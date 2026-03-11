{- |
This module is currently not in use and may be considered a design study.
Warning monad is like 'Control.Monad.Writer.Writer' monad,
it can be used to record exceptions that do not break program flow.

TODO:

* Better name for 'Warnable'
-}
module Control.Monad.Exception.Warning where

import qualified Control.Monad.Exception.Synchronous as Sync

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (mplus)
import Data.Maybe (catMaybes)


-- * Plain monad

{- |
Contains a value and
possibly warnings that were generated while the computation of that value.
-}
data Warnable e a =
   Warnable [Maybe e] a


{- |
Convert an exception to a warning.
-}
fromException :: a -> Sync.Exceptional e a -> Warnable e a
fromException deflt x =
{- Here the list item can only be constructed after the constructor of x is known
   case x of
      Sync.Success y   -> Warnable [Nothing] y
      Sync.Exception e -> Warnable [Just e] deflt
-}
   let (e,y) =
           case x of
              Sync.Success y0   -> (Nothing, y0)
              Sync.Exception e0 -> (Just e0, deflt)
   in  Warnable [e] y

fromExceptionNull :: Sync.Exceptional e () -> Warnable e ()
fromExceptionNull = fromException ()

toException :: ([e0] -> e1) -> Warnable e0 a -> Sync.Exceptional e1 a
toException summarize x =
   case x of
      Warnable mes y ->
         case catMaybes mes of
            [] -> Sync.Success y
            es -> Sync.Exception (summarize es)



warn :: e -> Warnable e ()
warn e = Warnable [Just e] ()



instance Functor (Warnable e) where
   fmap f x =
      case x of
         Warnable e a -> Warnable e (f a)

instance Applicative (Warnable e) where
   pure = Warnable [] -- [Nothing]?
   f <*> x =
      case f of
         Warnable e0 g ->
            case x of
               Warnable e1 y -> Warnable (mplus e0 e1) (g y)

instance Monad (Warnable e) where
   return = pure
   x >>= f =
      case x of
         Warnable e0 y ->
            case f y of
               Warnable e1 z -> Warnable (e0 ++ e1) z


-- * Monad transformer

newtype WarnableT e m a =
   WarnableT {runWarnableT :: m (Warnable e a)}


fromSynchronousT :: Functor m =>
   a -> Sync.ExceptionalT e m a -> WarnableT e m a
fromSynchronousT deflt (Sync.ExceptionalT mx) =
   WarnableT $ fmap (fromException deflt) mx



warnT :: (Monad m) =>
   e -> WarnableT e m ()
warnT = WarnableT . return . warn



instance Functor m => Functor (WarnableT e m) where
   fmap f (WarnableT x) =
      WarnableT (fmap (fmap f) x)

instance Applicative m => Applicative (WarnableT e m) where
   pure = WarnableT . pure . pure
   WarnableT f <*> WarnableT x =
      WarnableT (fmap (<*>) f <*> x)

instance Monad m => Monad (WarnableT e m) where
   return = WarnableT . return . return
   x0 >>= f =
      WarnableT $
      do Warnable ex x <- runWarnableT x0
         Warnable ey y <- runWarnableT (f x)
         return $ Warnable (ex ++ ey) y
