{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Implements a 'Handle' that uses Redis as a persistent data store.
-}
module KeyedVals.Handle.Redis (
  -- * create a Handle
  new,

  -- * module re-exports
  module KeyedVals.Handle,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import KeyedVals.Handle
import KeyedVals.Handle.Redis.Internal
import UnliftIO.Exception (throwIO)


{- | Construct a 'Handle' that uses Redis.

The instance is configured using the environment variable @REDIS_URL@.

If the value of @REDIS_URL@ is invalid or missing, this function throws an IOError.

The Handle uses a pool of connections; the size of the pool can be adjusted
by setting the environment variable @REDIS_MAX_CONNECTIONS@.
-}
new :: MonadUnliftIO m => m (Handle m)
new =
  liftIO readEnvConnectInfo >>= \case
    (Just i) -> fromConnectInfo i
    Nothing -> liftIO $ throwIO $ userError "could not create a Handle that uses redis"
