{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

{-| Use this library to build @mvc@ applications that consume many individually
    `Updatable` values, such as:

    * spread sheets,

    * control panels, and:

    * data visualizations.

    * build systems

    This library builds on top of the @mvc@ library, so you may want to read
    the documentation in the "MVC" module if you haven't already.

    Here is an example program to illustrate how this library works:

> import Control.Applicative ((<$>), (<*>))
> import Control.Foldl (last, length)
> import MVC.Updates (Updatable, on, listen, runUpdatable)
> import MVC.Prelude (stdinLines, tick)
> import Prelude hiding (last, length)
>
> data Example = Example (Maybe String) Int deriving (Show)
>
> debug :: Show a => String -> Updatable a -> Updatable a
> debug label = listen (\x -> putStrLn (label ++ ": " ++ show x))
>
> lastLine :: Updatable (Maybe String)
> lastLine = debug "lastLine" (on last stdinLines)
>
> seconds  :: Updatable Int
> seconds  = debug "seconds " (on length (tick 1.0))
>
> example  :: Updatable Example
> example  = debug "example " (Example <$> lastLine <*> seconds)
>
> main :: IO ()
> main = runUpdatable example

    First we build two simple `Updatable` values:

    * @lastLine@ updates every time the user enters a new line at standard input

    * @seconds@ increments every second

    Additionally, the `debug` function attaches a listener to each value that
    prints updates to the console.  Every listener triggers once at the
    beginning of the program and once for each update to the attached value.

    Then we assemble these two `Updatable` values into a derived `Updatable`
    value using `Applicative` operations.  This derived value updates every
    time one of the two original values updates:

> $ ./example
> lastLine: Nothing
> seconds : 0
> example : Example Nothing 0
> Test<Enter>
> lastLine: Just "Test"
> example : Example (Just "Test") 0
> seconds : 1
> example : Example (Just "Test") 1
> seconds : 2
> example : Example (Just "Test") 2
> ABC<Enter>
> lastLine: Just "ABC"
> example : Example (Just "ABC") 2
> seconds : 3
> example : Example (Just "ABC") 3
> ...

    At the beginning of the program we see one debug output for each value's
    initialization.  Afterwards, we see updates every time the user enters a
    line of input or one second passes.

    Updates are efficient.  When the user enters a new line, the `Example` value
    reuses the cached value for seconds.  Similarly, when one second passes, the
    `Example` reuses the cached value for the last line.

    The Example section at the bottom of this module contains an extended
    example for how to build a GTK-based spreadsheet using this library.
-}

module MVC.Updates (
    -- * Updates
      -- $updates
      Updatable(..)
    , on
    , listen
    , runUpdatable
    , updates

    -- * Example
    -- $example

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import Control.Category (id)
import Control.Concurrent.Async (withAsync)
import Control.Foldl (FoldM(..), Fold(..))
import qualified Control.Foldl as Foldl
import Data.IORef (newIORef, readIORef, writeIORef)
import MVC
import Prelude hiding (id)

{- $updates
    You can combine smaller updates into larger updates using `Applicative`
    operations:

> _As :: Updatable A
> _Bs :: Updatable B
>
> _ABs :: Updatable (A, B)
> _ABs = liftA2 (,) _As _Bs

    @_ABs@ updates every time either @_As@ updates or @_Bs@ updates, caching and
    reusing values that do not update.  For example, if @_As@ emits a new @A@,
    then @_ABs@ reuses the old value for @B@.  Vice versa, if @_Bs@ emits a new
    @B@ then @_ABs@ reuses the old value for @A@.

    This caching behavior transitively works for any number of updates that you
    combine using `Applicative` operations.  Also, the internal code is
    efficient and does not introduce any new threads no matter how many updates
    you combine.  (Note: the `updates` function does introduce one additional
    thread)

    Tip: To efficiently merge a large number of updates, store them in a
    `Data.Sequence.Seq` and use `Data.Foldable.sequenceA` to merge them:

> sequenceA :: Seq (Updatable a) -> Updatable (Seq a)
-}

-- | A concurrent, updatable value
data Updatable a = forall e . On (FoldM IO e a) (Managed (Controller e))

instance Functor Updatable where
    fmap f (On fold mController) = On (fmap f fold) mController

-- _Left :: Traversable' (Either a b) a
_Left :: Applicative f => (a -> f a) -> (Either a b -> f (Either a b))
_Left k e = case e of
    Left  a -> fmap Left (k a)
    Right b -> pure (Right b)

-- _Right :: Traversable' (Either a b) b
_Right :: Applicative f => (b -> f b) -> (Either a b -> f (Either a b))
_Right k e = case e of
    Left  a -> pure (Left a)
    Right b -> fmap Right (k b)

instance Applicative Updatable where
    pure a = On (pure a) mempty

    (On foldL mControllerL) <*> (On foldR mControllerR) = On foldT mControllerT
      where
        foldT =
            Foldl.pretraverseM _Left foldL <*> Foldl.pretraverseM _Right foldR

        mControllerT =
            fmap (fmap Left) mControllerL <> fmap (fmap Right) mControllerR

-- | Create an `Updatable` value using a pure `Fold`
on :: Fold e a -> Managed (Controller e) -> Updatable a
on fold = On (Foldl.generalize fold)
{-# INLINABLE on #-}

{-| Attach a listener that runs every time an `Updatable` value updates

> -- Treating `a -> IO ()` as the `View a` `Monoid`:
>
> listen mempty = id
>
> listen (f <> g) = listen g . listen f
-}
listen :: (a -> IO ()) -> Updatable a -> Updatable a
listen handler (On (FoldM step begin done) mController) =
    On (FoldM step' begin' done) mController
  where
    begin' = do
        x <- begin
        b <- done x
        handler b
        return x
    step' x a = do
        x' <- step x a
        b  <- done x'
        handler b
        return x'
{-# INLINABLE listen #-}

{-| Run an `Updatable` value, discarding the result

    Use this if you only care about running the associated listeners
-}
runUpdatable :: Updatable a -> IO ()
runUpdatable (On (FoldM step begin done) mController) = runMVC () id $ do
    controller <- mController

    ioref <- liftIO $ do
        x     <- begin
        _     <- done x
        newIORef x

    let view = asSink $ \e -> do
            x  <- readIORef ioref
            x' <- step x e
            _  <- done x'
            writeIORef ioref x'

    return (view, controller)
{-# INLINABLE runUpdatable #-}

{-| Convert an `Updatable` value to a `Managed` `Controller` that emits updates

    You must specify how to `Buffer` the updates
-}
updates :: Buffer a -> Updatable a -> Managed (Controller a)
updates buffer (On (FoldM step begin done) mController) = do
    controller <- mController
    managed $ \k -> do
        (o, i, seal) <- spawn' buffer

        ioref <- liftIO $ do
            x <- begin
            a <- done x
            _ <- atomically $ send o a
            newIORef x

        let view = asSink $ \e -> do
                x  <- readIORef ioref
                x' <- step x e
                a  <- done x'
                _  <- atomically $ send o a
                writeIORef ioref x'

        let io = do
                _ <- runMVC begin id (pure (view, controller))
                atomically seal

        withAsync io $ \_ -> k (asInput i) <* atomically seal

-- $example
--
-- The following example program shows how to build a spreadsheet with input and
-- output cells using the @gtk@, @mvc@ and @mvc-updates@ libraries.
--
-- The first half of the program contains all the @gtk@-specific logic.  The
-- key function is @spreadsheet@, which returns high-level commands to build
-- multiple input and output cells.
--
-- > -- This must be compiled with the `-threaded` flag
-- >
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.Applicative (Applicative, (<$>), (<*>))
-- > import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
-- > import Control.Concurrent.Async (async, wait)
-- > import Control.Foldl (lastDef)
-- > import Graphics.UI.Gtk as GTK
-- > import Lens.Family.TH (makeLenses)
-- > import MVC
-- > import MVC.Updates as MVC
-- > 
-- > makeInCell :: VBox -> Updatable Double
-- > makeInCell vBox = MVC.on (lastDef 0) $ managed $ \k -> do
-- >     (output, input) <- spawn Unbounded
-- >     spinButton <- spinButtonNewWithRange 0 100 1
-- >     onValueSpinned spinButton $ do
-- >         n <- get spinButton spinButtonValue
-- >         _ <- atomically (send output n)
-- >         return ()
-- >     boxPackStartDefaults vBox spinButton
-- >     widgetShowAll vBox
-- >     k (asInput input)
-- > 
-- > makeOutCell :: VBox -> Managed (View Double)
-- > makeOutCell vBox = liftIO $ do
-- >     entry <- entryNew
-- >     boxPackStartDefaults vBox entry
-- >     return $ asSink $ \n -> postGUISync $ entrySetText entry (show n)
-- > 
-- > spreadsheet :: Managed (Updatable Double, Managed (View Double), IO ())
-- > spreadsheet = managed $ \k -> do
-- >     initGUI
-- >     window <- windowNew
-- >     hBox   <- hBoxNew False 0
-- >     vBoxL  <- vBoxNew False 0
-- >     vBoxR  <- vBoxNew False 0
-- >     set window [windowTitle := "Spreadsheet", containerChild := hBox]
-- >     boxPackStartDefaults hBox vBoxL
-- >     boxPackStartDefaults hBox vBoxR
-- > 
-- >     mvar <- newEmptyMVar
-- >     a    <- async $ k (makeInCell vBoxL, makeOutCell vBoxR, putMVar mvar ())
-- >     takeMVar mvar
-- > 
-- >     GTK.on window deleteEvent $ do
-- >         liftIO mainQuit
-- >         return False
-- >     widgetShowAll window
-- >     mainGUI
-- >     wait a
--
--     Input cells are `Updatable` values, and output cells are `Managed`
--     `View`s.  Since `Updatable` values are `Applicative`s, we can combine
--     input cells into a single `Updatable` value (represented by the @In@
--     type) that updates whenever any individual cell updates:
--
-- > data Out = O { _o1 :: Double, _o2 :: Double, _o3 :: Double, _o4 :: Double }
-- > 
-- > data In  = I { _i1 :: Double, _i2 :: Double, _i3 :: Double, _i4 :: Double }
-- > 
-- > makeLenses ''Out
-- > o1, o2, o3, o4 :: Functor f => (Double -> f Double) -> Out -> f Out
-- > 
-- > model :: Model () In Out
-- > model = asPipe $ loop $ \(I i1 i2 i3 i4) -> do
-- >     return $ O (i1 + i2) (i2 * i3) (i3 - i4) (max i4 i1)
-- > 
-- > main :: IO ()
-- > main = runMVC () model $ do
-- >     (inCell, outCell, go) <- spreadsheet
-- >     c <- updates Unbounded $ I <$> inCell <*> inCell <*> inCell <*> inCell
-- >     v <- fmap (handles o1) outCell
-- >       <> fmap (handles o2) outCell
-- >       <> fmap (handles o3) outCell
-- >       <> fmap (handles o4) outCell
-- >     liftIO go
-- >     return (v, c)
-- >
-- > -- This must be compiled with the `-threaded` flag
--
--     The @model@ contains the pure fragment of our program that relates input
--     cells to output cells.  In this example, each output cell is a function
--     of two input cells.
--
--     If you compile and run the above program with the @-threaded@ flag, a
--     small spread sheet window will open with input cells on the left-hand
--     side and output cells on the right-hand side.  Modifying any input cell
--     will automatically update all output cells.

{- $reexports
    "Control.Foldl" re-exports the `Fold` and `FoldM` types
-}
