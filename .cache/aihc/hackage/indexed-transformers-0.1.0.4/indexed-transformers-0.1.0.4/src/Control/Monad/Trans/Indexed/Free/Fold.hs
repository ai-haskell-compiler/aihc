{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{- |
Module      :  Control.Monad.Trans.Indexed.Free.Fold
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

An instance of the free indexed monad transformer encoded as `foldFreeIx`.
-}

module Control.Monad.Trans.Indexed.Free.Fold
  ( FreeIx (..)
  ) where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.Indexed
import Control.Monad.Trans.Indexed.Free

{- |
`FreeIx` is the free indexed monad transformer encoded as its `foldFreeIx`.

prop> foldFreeIx f freeIx = runFreeIx freeIx f
-}
newtype FreeIx g i j m x = FreeIx
  {runFreeIx :: forall t. (IxMonadTrans t, Monad m)
    => (forall i j x. g i j x -> t i j m x) -> t i j m x}
instance (IxFunctor f, Monad m) => Functor (FreeIx f i j m) where
  fmap f (FreeIx k) = FreeIx $ \step -> fmap f (k step)
instance (IxFunctor f, i ~ j, Monad m)
  => Applicative (FreeIx f i j m) where
    pure x = FreeIx $ const $ pure x
    (<*>) = apIx
instance (IxFunctor f, i ~ j, Monad m)
  => Monad (FreeIx f i j m) where
    return = pure
    (>>=) = flip bindIx
instance (IxFunctor f, i ~ j)
  => MonadTrans (FreeIx f i j) where
    lift m = FreeIx $ const $ lift m
instance IxFunctor f
  => IxMonadTrans (FreeIx f) where
    joinIx (FreeIx g) = FreeIx $ \k -> bindIx (\(FreeIx f) -> f k) (g k)
instance
  ( IxFunctor f
  , Monad m
  , i ~ j
  ) => MonadFree (f i j) (FreeIx f i j m) where
    wrap = join . liftFreeIx
instance IxMonadTransFree FreeIx where
  liftFreeIx m = FreeIx $ \k -> k m
  hoistFreeIx f (FreeIx k) = FreeIx $ \g -> k (g . f)
  foldFreeIx f (FreeIx k) = k f
