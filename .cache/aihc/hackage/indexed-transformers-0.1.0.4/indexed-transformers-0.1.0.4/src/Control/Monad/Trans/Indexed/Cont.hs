{- |
Module      :  Control.Monad.Trans.Indexed.Cont
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The continuation indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.Cont
  ( ContIx (..)
  , callCCIx
  , evalContIx
  , mapContIx
  , withContIx
  , resetIx
  , shiftIx
  , toContT
  , fromContT
  ) where

import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.Trans.Indexed

newtype ContIx i j m x = ContIx {runContIx :: (x -> m j) -> m i}
  deriving Functor
instance IxMonadTrans ContIx where
  joinIx (ContIx k) = ContIx $ \f -> k $ \(ContIx g) -> g f
instance i ~ j => Applicative (ContIx i j m) where
  pure x = ContIx $ \k -> k x
  ContIx cf <*> ContIx cx = ContIx $ \ k -> cf $ \ f -> cx (k . f)
instance i ~ j => Monad (ContIx i j m) where
  return = pure
  ContIx cx >>= k = ContIx $ \ c -> cx (\ x -> runContIx (k x) c)
instance i ~ j => MonadTrans (ContIx i j) where
  lift = ContIx . (>>=)
instance i ~ j => MonadCont (ContIx i j m) where callCC = callCCIx

evalContIx :: Monad m => ContIx x j m j -> m x
evalContIx c = runContIx c return

mapContIx :: (m i -> m j) -> ContIx i k m x -> ContIx j k m x
mapContIx g (ContIx f) = ContIx $ g . f

withContIx :: ((y -> m k) -> x -> m j) -> ContIx i j m x -> ContIx i k m y
withContIx f (ContIx g) = ContIx $ g . f

callCCIx :: ((x -> ContIx j k m y) -> ContIx i j m x) -> ContIx i j m x
callCCIx f = ContIx $ \k -> runContIx (f (ContIx . const . k)) k

shiftIx :: Monad m => ((x -> m j) -> ContIx i k m k) -> ContIx i j m x
shiftIx f = ContIx (evalContIx . f)

resetIx :: Monad m => ContIx x j m j -> ContIx i i m x
resetIx = lift . evalContIx

toContT :: ContIx i i m x -> ContT i m x
toContT (ContIx f) = ContT f

fromContT :: ContT i m x -> ContIx i i m x
fromContT (ContT f) = ContIx f
