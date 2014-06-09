{-# LANGUAGE ExistentialQuantification #-}

{-| Use this library to build @mvc@ applications that consume many individually
    `Updatable` values, such as:

    * spread sheets,

    * control panels, and:

    * data visualizations.

    This library builds on top of the @mvc@ library, so you may want to read
    the documentation in the "MVC" module if you haven't already.

    Here is an example program to illustrate how this library works:

> import Control.Applicative ((<$>), (<*>))
> import Control.Foldl (last, length)
> import MVC
> import MVC.Updates
> import MVC.Prelude (stdinLines, tick)
> import qualified Pipes.Prelude as Pipes
> import Prelude hiding (last, length)
> 
> data Example = Example (Maybe String) Int deriving (Show)
>
> lastLine :: Updatable (Maybe String)
> lastLine = On last stdinLines
>
> seconds :: Updatable Int
> seconds = On length (tick 1.0)
>
> example :: Updatable Example
> example = Example <$> lastLine <*> seconds
> 
> viewController :: Managed (View Example, Controller Example)
> viewController = do
>     controller <- updates Unbounded example
>     return (asSink print, controller)
> 
> model :: Model () Example Example
> model = asPipe $ Pipes.takeWhile (\(Example str _) -> str /= Just "quit")
> 
> main :: IO ()
> main = runMVC () model viewController

    First we build two simple `Updatable` values:

    * @lastLine@ updates every time the user enters a new line at standard input

    * @seconds@ increments every second

    Then we assemble them into a derived `Updatable` value using `Applicative`
    operations.  This derived value updates every time one of the two primitive
    values updates:

> $ ./example
> Example Nothing 0
> Test<Enter>
> Example (Just "Test") 0
> Example (Just "Test") 1
> Example (Just "Test") 2
> ABC<Enter>
> Example (Just "ABC") 2
> Example (Just "ABC") 3
> quit<Enter>
> $

    Every time the user types in a new line of input the @controller@ emits a
    new @Example@ value that overrides the first field.  Similarly, every time
    one second passes the @controller@ emits a new @Example@ value that
    overrides the second field.

    The Example section at the bottom of this module contains an extended
    example for how to build a GTK-based spreadsheet using this library.
-}

module MVC.Updates (
    -- * Updates
      -- $updates
      Updatable(..)
    , updates

    -- * Example
    -- $example

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import Control.Concurrent.Async (withAsync)
import Control.Foldl (Fold(..))
import Control.Monad (forever)
import Control.Monad.Trans.State.Strict (get, put)
import MVC

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
    efficient and only introduces one extra thread no matter how many updates
    you combine.  You can even skip the extra thread if you unpack the `Fold`
    type and use the fields directly within your @mvc@ program.  Study the
    source code for `updates` to see this in action.

    Tip: To efficiently merge a large number of updates, store them in a
    `Data.Sequence.Seq` and use `Data.Foldable.sequenceA` to merge them:

> sequenceA :: Seq (Updatable a) -> Updatable (Seq a)
-}

-- | A concurrent, updatable value
data Updatable a = forall e . On (Fold e a) (Managed (Controller e))

instance Functor Updatable where
    fmap f (On fold mController) = On (fmap f fold) mController

{-
> onLeft (f <*> x) = onLeft f <*> onLeft x
>
> onLeft (pure r) = pure r
-}
onLeft :: Fold a b -> Fold (Either a x) b
onLeft (Fold step begin done) = Fold step' begin done
  where
    step' x (Left a) = step x a
    step' x  _       = x

{-
> onRight (f <*> x) = onRight f <*> onRight x
>
> onRight (pure r) = pure r
-}
onRight :: Fold a b -> Fold (Either x a) b
onRight (Fold step begin done) = Fold step' begin done
  where
    step' x (Right a) = step x a
    step' x  _        = x

instance Applicative Updatable where
    pure a = On (pure a) mempty

    (On foldL mControllerL) <*> (On foldR mControllerR) = On foldT mControllerT
      where
        foldT = onLeft foldL <*> onRight foldR

        mControllerT =
            fmap (fmap Left) mControllerL <> fmap (fmap Right) mControllerR

{-| Convert an `Updatable` value to a `Managed` `Controller` that emits updates

    You must specify how to `Buffer` the updates
-}
updates :: Buffer a -> Updatable a -> Managed (Controller a)
updates buffer (On (Fold step begin done) mController) = do
    controller <- mController
    managed $ \k -> do
        (o, i, seal) <- spawn' buffer
    
        let model_ = asPipe $ forever $ do
                x <- lift get
                yield (done x)
                e <- await
                lift $ put $! step x e
    
            view_ = asSink $ \a -> do
                _ <- atomically (send o a)
                return ()
    
        let io = do
                _ <- runMVC begin model_ (pure (view_, controller))
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
-- > {-# LANGUAGE TemplateHaskell #-}
-- > 
-- > import Control.Applicative (Applicative, (<$>), (<*>))
-- > import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
-- > import Control.Concurrent.Async (async, wait)
-- > import Control.Foldl (latest)
-- > import Graphics.UI.Gtk
-- > import Lens.Family.TH (makeLenses)
-- > import MVC
-- > import MVC.Updates
-- > 
-- > makeInCell :: VBox -> Updatable Double
-- > makeInCell vBox = On (latest 0) $ managed $ \k -> do
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
-- >     on window deleteEvent $ do
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
--
--     The @model@ contains the pure fragment of our program that relates input
--     cells to output cells.  In this example, each output cell is a function
--     of two input cells.
--
--     If you compile and run the above program, a small spread sheet window
--     will open with input cells on the left-hand side and output cells on the
--     right-hand side.  Modifying any input cell will automatically update all
--     output cells.

{- $reexports
    "Control.Foldl" re-exports the `Fold` type
-}
