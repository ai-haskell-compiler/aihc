{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Accum.OrphanInstances where

import Control.Monad.Accum
import Control.Monad.Trans.Accum qualified as T

instance (Monoid w, Monad m) => MonadAccum w (T.AccumT w m) where
  look = T.look
  add = T.add
