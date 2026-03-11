{- |
Module      :  Control.Monad.Trans.Indexed.Free.Wrap
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

An instance of the free indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.Free.Wrap
  ( FreeIx (..)
  , WrapIx (..)
  ) where

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

data WrapIx f i j m x where
  Unwrap :: x -> WrapIx f i i m x
  Wrap :: f i j (FreeIx f j k m x) -> WrapIx f i k m x
instance (IxFunctor f, Monad m)
  => Functor (WrapIx f i j m) where
    fmap f = \case
      Unwrap x -> Unwrap $ f x
      Wrap fm -> Wrap $ fmap (fmap f) fm

newtype FreeIx f i j m x = FreeIx {runFreeIx :: m (WrapIx f i j m x)}
instance (IxFunctor f, Monad m)
  => Functor (FreeIx f i j m) where
    fmap f (FreeIx m) = FreeIx $ fmap (fmap f) m
instance (IxFunctor f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure = FreeIx . pure . Unwrap
    (<*>) = apIx
instance (IxFunctor f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = pure
    (>>=) = flip bindIx
instance (IxFunctor f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift = FreeIx . fmap Unwrap
instance IxFunctor f
  => IxMonadTrans (FreeIx f) where
    joinIx (FreeIx mm) = FreeIx $ mm >>= \case
      Unwrap (FreeIx m) -> m
      Wrap fm -> return $ Wrap $ fmap joinIx fm
instance
  ( IxFunctor f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = FreeIx . return . Wrap
instance IxMonadTransFree FreeIx where
  liftFreeIx = FreeIx . return . Wrap . fmap return
  hoistFreeIx f (FreeIx m) = FreeIx (fmap hoist_f m)
    where
      hoist_f = \case
        Unwrap x -> Unwrap x
        Wrap y -> Wrap (f (fmap (hoistFreeIx f) y))
  foldFreeIx f (FreeIx m) = bindIx foldMap_f (lift m)
    where
      foldMap_f = \case
        Unwrap x -> return x
        Wrap y -> bindIx (foldFreeIx f) (f y)
