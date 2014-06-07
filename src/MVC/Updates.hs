{-# LANGUAGE ExistentialQuantification #-}

{-| Use this library to build @mvc@ applications that consume many `Updatable`
    values.  This documentation assumes familiarity with the @mvc@ library.

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

    First we build two primitive `Updatable` values:

    * @lastLine@ updates every time the user enters a new line at standard input

    * @seconds@ increments every second

    Then we assembles them into a derived `Updatable` value using `Applicative`
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
    new @Example@ value with a new value for the first field.  Similarly, every
    time one second passes the @controller@ emits a new @Example@ value that
    increments the second field.
-}

module MVC.Updates (
    -- * Updates
      -- $updates
      Updatable(..)
    , updates

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import Control.Concurrent.Async (withAsync)
import Control.Foldl (Fold(..))
import Control.Monad.Trans.State.Strict
import MVC

{- $updates
    You can combine smaller updates into larger updates using `Applicative`
    operations:

> as :: Updatable A
> bs :: Updatable B
>
> abs :: Updatable (A, B)
> abs = liftA2 (,) as bs

    @abs@ updates every time either @as@ updates or @bs@ updates, caching and
    reusing values that do not update.  For example, if @as@ emits a new @A@,
    then @abs@ reuses the old value for @B@.  Vice versa, if @bs@ emits a new
    @B@ then @abs@ reuses the old value for @A@.

    This caching behavior transitively works for any number of updates.  Also,
    the internal code is efficient and only introduces one extra thread no
    matter how many updates you combine.

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

{-| Convert an `Updatable` value to a `Managed` `Controller` that emits updates by
    specifying how to `Buffer` the updates
-}
updates :: Buffer a -> Updatable a -> Managed (Controller a)
updates buffer (On (Fold step begin done) mController) = do
    controller <- mController
    managed $ \k -> do
        (o, i, seal) <- spawn' buffer
    
        let model_ = asPipe $ do
                yield (done begin)
                for cat $ \e -> do
                    a <- lift (state (\x -> let x' = step x e in (done x', x')))
                    yield a
    
            view_ = asSink $ \a -> do
                _ <- atomically (send o a)
                return ()
    
        let io = do
                _ <- runMVC begin model_ (pure (view_, controller))
                atomically seal
    
        withAsync io $ \_ -> k (asInput i) <* atomically seal

{- $reexports
    "Control.Foldl" re-exports the `Fold` type
-}
