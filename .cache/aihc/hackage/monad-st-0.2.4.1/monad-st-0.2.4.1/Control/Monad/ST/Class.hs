{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------
module Control.Monad.ST.Class (MonadST(..)) where

import Control.Monad.Trans.Class
#if MIN_VERSION_base(4,4,0)
import Control.Monad.ST.Safe
#else
import Control.Monad.ST
#endif

class Monad m => MonadST m where
  type World m :: *
  liftST :: ST (World m) a -> m a

instance MonadST IO where
  type World IO = RealWorld
  liftST = stToIO

instance MonadST (ST s) where
  type World (ST s) = s
  liftST = id

instance (MonadTrans t, MonadST m, Monad (t m)) => MonadST (t m) where
  type World (t m) = World m
  liftST = lift . liftST
