-------------------------------------------------------------------------------
-- network-wait
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exports variants of the functions from "Network.Wait"
-- specialised for Redis servers. In addition to checking whether a
-- connection can be established, the functions in this module also check
-- whether the Redis server is ready to accept commands using
-- `checkedConnect`. Unlike `checkedConnect`, we don't give up if the
-- connection fails, but instead use the specified retry policy to try again.
-- All functions in this module return the established connection if
-- successful.
module Network.Wait.Redis (
    waitRedis,
    waitRedisVerbose,
    waitRedisVerboseFormat,
    waitRedisWith
) where

-------------------------------------------------------------------------------

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry

import Database.Redis

import Network.Wait

-------------------------------------------------------------------------------


-- | `waitRedis` @retryPolicy connectInfo@ is a variant of
-- `waitRedisWith` which does not install any additional handlers.
waitRedis
    :: (MonadIO m, MonadMask m)
    => RetryPolicyM m -> ConnectInfo -> m Connection
waitRedis = waitRedisWith []

-- | `waitRedisVerbose` @outputHandler retryPolicy connectInfo@ is a variant
-- of `waitRedisVerboseFormat` which catches all exceptions derived from
-- `SomeException` and formats retry attempt information using `defaultLogMsg`
-- before passing the resulting `String` to @out@.
waitRedisVerbose
    :: (MonadIO m, MonadMask m)
    => (String -> m ()) -> RetryPolicyM m -> ConnectInfo -> m Connection
waitRedisVerbose out =
    waitRedisVerboseFormat @SomeException $
    \b ex st -> out $ defaultLogMsg b ex st

-- | `waitRedisVerboseFormat` @outputHandler retryPolicy connectInfo@ is a
-- variant of `waitRedisWith` which installs an extra handler based on
-- `logRetries` which passes status information for each retry attempt
-- to @outputHandler@.
waitRedisVerboseFormat
    :: forall e m . (MonadIO m, MonadMask m, Exception e)
    => (Bool -> e -> RetryStatus -> m ())
    -> RetryPolicyM m
    -> ConnectInfo
    -> m Connection
waitRedisVerboseFormat out = waitRedisWith [h]
    where h = logRetries (const $ pure True) out

-- | `waitRedisWith` @extraHandlers retryPolicy connectInfo@ will attempt
-- to connect to the Redis server using @connectInfo@ and check that the
-- server is ready to accept commands. If this check fails, @retryPolicy@ is
-- used to determine whether (and how often) this function should attempt to
-- retry establishing the connection. By default, this function will retry
-- after all exceptions (except for those given by `skipAsyncExceptions`).
-- This behaviour may be customised with @extraHandlers@ which are installed
-- after `skipAsyncExceptions`, but before the default exception handler. The
--  @extraHandlers@ may also be used to report retry attempts to e.g. the
-- standard output or a logger.
waitRedisWith
    :: (MonadIO m, MonadMask m)
    => [RetryStatus -> Handler m Bool] -> RetryPolicyM m -> ConnectInfo
    -> m Connection
waitRedisWith hs policy = recoveringWith hs policy . liftIO . checkedConnect

-------------------------------------------------------------------------------
