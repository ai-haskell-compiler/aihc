{-|
Module      : Control.Concurrent.Async.Refresh.Tokens
Description : This module exposes the API of the async-refresh-tokens package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

The async-refresh-tokens package is built on top of the async-refresh
package and provides the core logic for renewal of expiring access
tokens according to user-provided actions.
-}

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Control.Concurrent.Async.Refresh.Tokens
  ( IsToken(..)
  , Token(..)
  , TokenRefresher
  , tokenRefresherAsync
  , RequestToken(..)
  , RefreshResult(..)
  , TokenStore
  , TokenConf
  , newTokenRefresher
  , newEmptyTokenStore
  , defaultTokenConf
  , tokenConfSetFactor
  , tokenConfAddRequest
  ) where

import           Control.Concurrent.Async.Refresh.Tokens.Prelude

import           Control.Concurrent.Async.Refresh
import           Control.Concurrent.Async.Refresh.Tokens.Conf
import qualified Control.Concurrent.Async.Refresh.Tokens.Lenses  as Lens
import           Control.Concurrent.Async.Refresh.Tokens.Types
import           Control.Monad.IO.Unlift
import           Lens.Micro
import           UnliftIO.Async
import           UnliftIO.STM

-- | Start a new token refresher for the provided configuration.
-- Returns a 'TokenRefresher' handle representing the running token
-- refresher.
newTokenRefresher :: forall m.
                     ( MonadUnliftIO m
                     , MonadMask m
                     , MonadLogger m )
                  => TokenConf m -> m TokenRefresher
newTokenRefresher conf = do
  asyncHandle <- async $
    bracket (mapM spawnSingleTokenRefresher (conf ^. Lens.requests))
            (mapM cancel)
            waitForTermination
  return $ TokenRefresher asyncHandle
  where waitForTermination :: [Async ()] -> m ()
        waitForTermination asyncHandles = do
          void $ waitAny asyncHandles
          logErrorN "Token Refresher terminated unexpectedly"

-- | Spawn an async refresher for the provided token.
spawnSingleTokenRefresher :: forall m.
                             ( MonadUnliftIO m
                             , MonadMask m
                             , MonadLogger m )
                          => RequestToken m -> m (Async ())
spawnSingleTokenRefresher (RequestToken store action) = do
  let conf = newAsyncRefreshConf action
             & asyncRefreshConfSetCallback (tokenStoreCallback store)
  asyncRefreshAsync <$> newAsyncRefresh conf

-- | Extract the 'Async' handle from the provided 'TokenRefresher'.
tokenRefresherAsync :: TokenRefresher -> Async ()
tokenRefresherAsync (TokenRefresher asyncHandle) = asyncHandle

-- | Callback to be used by async-refresh package, which simply
-- updates the provided token store.
tokenStoreCallback :: forall m t.
                      (MonadIO m, IsToken t, MonadLogger m)
                   => TokenStore t
                   -> Either SomeException (RefreshResult (Token t)) -> m ()
tokenStoreCallback _ (Left exn) =
  logErrorN $ sformat ("Token refresh action failed: " % stext) (tshow exn)
tokenStoreCallback store res@(Right t) = do
  logDebugN $
    sformat ("Token refresh action succeeded: " % stext) (tshow (maskRefreshResult t))
  atomically $ writeTVar store (refreshResult <$> res)

maskRefreshResult :: RefreshResult (Token t) -> RefreshResult (Token t)
maskRefreshResult res = res { refreshResult = maskedToken }

maskedToken :: Token t
maskedToken = Token { unToken = "XXXXXXXXXXXXXXXX" }

-- | Create a new empty token store for the provided token type.
newEmptyTokenStore :: (MonadIO m, IsToken t)
                   => m (TVar (Either SomeException (Token t)))
newEmptyTokenStore = atomically $
  newTVar (Left (toException (TokenNotFound "")))
