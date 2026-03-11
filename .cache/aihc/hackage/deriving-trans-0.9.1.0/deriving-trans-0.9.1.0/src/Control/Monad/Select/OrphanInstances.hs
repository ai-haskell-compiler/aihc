{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Select.OrphanInstances where

import Control.Monad.Select
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Select qualified as T
import Data.Functor.Identity

instance MonadBaseControlIdentity Identity m => MonadSelect r (T.SelectT r m) where
  select f = T.SelectT $ \ k -> liftBaseWithIdentity $ \runInIdentity ->
    Identity $ f $ runIdentity . runInIdentity . k
