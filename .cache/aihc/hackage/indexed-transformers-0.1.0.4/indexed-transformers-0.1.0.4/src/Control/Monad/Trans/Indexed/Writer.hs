{- |
Module      :  Control.Monad.Trans.Indexed.Writer
Copyright   :  (C) 2024 Eitan Chatav
License     :  BSD 3-Clause License (see the file LICENSE)
Maintainer  :  Eitan Chatav <eitan.chatav@gmail.com>

The writer indexed monad transformer.
-}

module Control.Monad.Trans.Indexed.Writer
  ( WriterIx (..)
  , evalWriterIx
  , execWriterIx
  , mapWriterIx
  , tellIx
  , listenIx
  , listensIx
  , passIx
  , censorIx
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans
import Control.Monad.Trans.Indexed

newtype WriterIx w i j m x = WriterIx {runWriterIx :: m (x, w i j)}
  deriving Functor

instance Category w => IxMonadTrans (WriterIx w) where
  joinIx (WriterIx mm) = WriterIx $ do
    (WriterIx m, ij) <- mm
    (x, jk) <- m
    return (x, ij >>> jk)
instance (i ~ j, Applicative m, Category w) => Applicative (WriterIx w i j m) where
  pure x = WriterIx (pure (x, id))
  WriterIx mf <*> WriterIx mx =
    let
      apply (f, ij) (x, jk) = (f x, ij >>> jk)
    in
      WriterIx $ apply <$> mf <*> mx
instance (i ~ j, Monad m, Category w) => Monad (WriterIx w i j m) where
  return = pure
  (>>=) = flip bindIx
instance (i ~ j, Category w) => MonadTrans (WriterIx w i j) where
  lift m = WriterIx $ do
    x <- m
    return (x, id)

evalWriterIx :: Monad m => WriterIx w i j m x -> m x
evalWriterIx (WriterIx m) = fst <$> m

execWriterIx :: Monad m => WriterIx w i j m x -> m (w i j)
execWriterIx (WriterIx m) = snd <$> m

mapWriterIx
  :: (m (x, w i j) -> n (y, q i j))
  -> WriterIx w i j m x
  -> WriterIx q i j n y
mapWriterIx f m = WriterIx $ f (runWriterIx m)

tellIx :: Monad m => w i j -> WriterIx w i j m ()
tellIx w = WriterIx (return ((), w))

listenIx :: Monad m => WriterIx w i j m x -> WriterIx w i j m (x, w i j)
listenIx (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, w),w)

listensIx
  :: Monad m
  => (w i j -> y)
  -> WriterIx w i j m x
  -> WriterIx w i j m (x, y)
listensIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return ((x, f w), w)

passIx
  :: Monad m
  => WriterIx w i j m (x, w i j -> q i j)
  -> WriterIx q i j m x
passIx (WriterIx m) = WriterIx $ do
  ((x, f), w) <- m
  return (x, f w)

censorIx :: Monad m => (w i j -> w i j) -> WriterIx w i j m x -> WriterIx w i j m x
censorIx f (WriterIx m) = WriterIx $ do
  (x, w) <- m
  return (x, f w)
