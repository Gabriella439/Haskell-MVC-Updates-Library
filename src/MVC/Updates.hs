{-# LANGUAGE ExistentialQuantification #-}

{-| Use this library to build @mvc@ applications that consume many `Updatable`
    values.  Here is an example program to illustrate how this library works:

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
> viewController :: Managed (View Example, Controller Example)
> viewController = do
>     lines <- stdinLines :: Managed (Controller String)
>     ticks <- tick 3.0   :: Managed (Controller ()    )
> 
>     let lastLine :: Updatable (Maybe String)
>         lastLine = On last lines
> 
>         numTicks :: Updatable Int
>         numTicks = On length ticks
> 
>         both :: Updatable Example
>         both = Example <$> lastLine <*> numTicks
> 
>     controller <- updates Unbounded both :: Managed (Controller Example)
> 
>     return (asSink print, controller)
> 
> model :: Model () Example Example
> model = asPipe $ Pipes.takeWhile (\(Example str _) -> str /= Just "quit")
> 
> main :: IO ()
> main = runMVC () model viewController

-}

module MVC.Updates (
    -- * Updates
      -- $updates
      Updatable(..)
    , updates
    ) where

import Control.Applicative (Applicative(pure, (<*>)), (<*))
import Control.Concurrent.Async (withAsync)
import Control.Foldl (Fold(Fold))
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
data Updatable a = forall e . On (Fold e a) (Controller e)

instance Functor Updatable where
    fmap f (On fold controller) = On (fmap f fold) controller

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

    (On foldL controllerL) <*> (On foldR controllerR) = On foldT controllerT
      where
        foldT = onLeft foldL <*> onRight foldR

        controllerT = fmap Left controllerL <> fmap Right controllerR

{-| Convert an `Updatable` value to a `Managed` `Controller` that emits updates by
    specifying how to `Buffer` the updates
-}
updates :: Buffer a -> Updatable a -> Managed (Controller a)
updates buffer (On (Fold step begin done) control) = managed $ \k -> do
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
            _ <- runMVC begin model_ (pure (view_, control))
            atomically seal

    withAsync io $ \_ -> k (asInput i) <* atomically seal
