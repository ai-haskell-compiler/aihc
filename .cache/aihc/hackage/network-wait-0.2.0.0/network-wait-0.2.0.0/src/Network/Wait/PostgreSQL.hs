-------------------------------------------------------------------------------
-- network-wait
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exports variants of the functions from "Network.Wait"
-- specialised for PostgreSQL servers. In addition to checking whether a
-- connection can be established, the functions in this module also check
-- whether the PostgreSQL server is ready to accept commands.
module Network.Wait.PostgreSQL (
    waitPostgreSql,
    waitPostgreSqlVerbose,
    waitPostgreSqlVerboseFormat,
    waitPostgreSqlWith
) where

-------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal

import Network.Wait

-------------------------------------------------------------------------------

-- | `waitPostgreSql` @retryPolicy connectInfo@ is a variant of
-- `waitPostgresWith` which does not install any additional handlers.
waitPostgreSql
    :: (MonadIO m, MonadMask m)
    => RetryPolicyM m -> ConnectInfo -> m Connection
waitPostgreSql = waitPostgreSqlWith []

-- | `waitPostgreSqlVerbose` @outputHandler retryPolicy connectInfo@ is a variant
-- of `waitPostgreSqlVerboseFormat` which catches all exceptions derived from
-- `SomeException` and formats retry attempt information using `defaultLogMsg`
-- before passing the resulting `String` to @out@.
waitPostgreSqlVerbose
    :: (MonadIO m, MonadMask m)
    => (String -> m ()) -> RetryPolicyM m -> ConnectInfo -> m Connection
waitPostgreSqlVerbose out =
    waitPostgreSqlVerboseFormat @SomeException $
    \b ex st -> out $ defaultLogMsg b ex st

-- | `waitPostgreSqlVerboseFormat` @outputHandler retryPolicy connectInfo@ is a
-- variant of `waitPostgreSqlWith` which installs an extra handler based on
-- `logRetries` which passes status information for each retry attempt
-- to @outputHandler@.
waitPostgreSqlVerboseFormat
    :: forall e m . (MonadIO m, MonadMask m, Exception e)
    => (Bool -> e -> RetryStatus -> m ())
    -> RetryPolicyM m
    -> ConnectInfo
    -> m Connection
waitPostgreSqlVerboseFormat out = waitPostgreSqlWith [h]
    where h = logRetries (const $ pure True) out

-- | `waitPostgreSqlWith` @extraHandlers retryPolicy connectInfo@ will attempt
-- to connect to the PostgreSQL server using @connectInfo@ and check that the
-- server is ready to accept commands. If this check fails, @retryPolicy@ is
-- used to determine whether (and how often) this function should attempt to
-- retry establishing the connection. By default, this function will retry
-- after all exceptions (except for those given by `skipAsyncExceptions`).
-- This behaviour may be customised with @extraHandlers@ which are installed
-- after `skipAsyncExceptions`, but before the default exception handler. The
--  @extraHandlers@ may also be used to report retry attempts to e.g. the
-- standard output or a logger.
waitPostgreSqlWith
    :: (MonadIO m, MonadMask m)
    => [RetryStatus -> Handler m Bool] -> RetryPolicyM m -> ConnectInfo
    -> m Connection
waitPostgreSqlWith hs policy info =
    recoveringWith hs policy $
    liftIO $
    bracket (connect info) close $ \con -> do
        rs <- query_ @[Int] con "SELECT 1;"
        unless (rs == [[1]]) $ throwM $
            fatalError "Unexpected result for SELECT 1."
        pure con

-------------------------------------------------------------------------------
