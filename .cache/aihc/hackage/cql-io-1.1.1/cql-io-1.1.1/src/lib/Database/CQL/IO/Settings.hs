-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.CQL.IO.Settings where

import Control.Exception (IOException)
import Control.Lens (makeLenses, set, over)
import Control.Monad.Catch
import Control.Retry hiding (retryPolicy)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Semigroup ((<>))
import Data.Time
import Database.CQL.Protocol
import Database.CQL.IO.Cluster.Policies (Policy, random)
import Database.CQL.IO.Connection.Socket (PortNumber)
import Database.CQL.IO.Connection.Settings as C
import Database.CQL.IO.Exception
import Database.CQL.IO.Log
import Database.CQL.IO.Pool as P
import Database.CQL.IO.Timeouts (Milliseconds (..))
import OpenSSL.Session (SSLContext, SomeSSLException)

import qualified Data.HashMap.Strict as HashMap

data Settings = Settings
    { _poolSettings  :: PoolSettings
    , _connSettings  :: ConnectionSettings
    , _retrySettings :: RetrySettings
    , _logger        :: Logger
    , _protoVersion  :: Version
    , _portnumber    :: PortNumber
    , _contacts      :: NonEmpty String
    , _policyMaker   :: IO Policy
    , _prepStrategy  :: PrepareStrategy
    }

-- | Strategy for the execution of 'PrepQuery's.
data PrepareStrategy
    = EagerPrepare -- ^ cluster-wide preparation
    | LazyPrepare  -- ^ on-demand per node preparation
    deriving (Eq, Ord, Show)

-- | Retry settings control if and how retries are performed
-- by the client upon encountering errors during query execution.
--
-- There are three aspects to the retry settings:
--
--   1. /What/ to retry. Determined by the retry handlers ('setRetryHandlers').
--   2. /How/ to perform the retries. Determined by the retry policy
--      ('setRetryPolicy').
--   3. Configuration adjustments to be performed before retrying. Determined by
--      'adjustConsistency', 'adjustSendTimeout' and 'adjustResponseTimeout'.
--      These adjustments are performed /once/ before the first retry and are
--      scoped to the retries only.
--
-- Retry settings can be scoped to a client action by 'Database.CQL.IO.Client.retry',
-- thus locally overriding the \"global\" retry settings configured by
-- 'setRetrySettings'.
data RetrySettings = RetrySettings
    { _retryPolicy        :: forall m. Monad m => RetryPolicyM m
    , _reducedConsistency :: (Maybe Consistency)
    , _sendTimeoutChange  :: Milliseconds
    , _recvTimeoutChange  :: Milliseconds
    , _retryHandlers      :: forall m. Monad m => [RetryStatus -> Handler m Bool]
    }

makeLenses ''RetrySettings
makeLenses ''Settings

-- | Default settings:
--
-- * The initial contact point is \"localhost\" on port 9042.
--
-- * The load-balancing policy is 'random'.
--
-- * The binary protocol version is 3.
--
-- * The connection idle timeout is 60s.
--
-- * The connection pool uses 4 stripes to mitigate thread contention.
--
-- * Connections use a connect timeout of 5s, a send timeout of 3s and
--   a receive timeout of 10s.
--
-- * 128 streams per connection are used.
--
-- * 16k receive buffer size.
--
-- * No compression is applied to frame bodies.
--
-- * No default keyspace is used.
--
-- * A single, immediate retry is performed for errors that are always safe to
--   retry and are known to have good chances of succeeding on a retry.
--   See 'defRetrySettings'.
--
-- * Query preparation is done lazily. See 'PrepareStrategy'.
defSettings :: Settings
defSettings = Settings
    P.defSettings
    C.defSettings
    defRetrySettings
    nullLogger
    V3
    9042
    ("localhost" :| [])
    random
    LazyPrepare

-----------------------------------------------------------------------------
-- Settings

-- | Set the binary protocol version to use.
setProtocolVersion :: Version -> Settings -> Settings
setProtocolVersion v = set protoVersion v

-- | Set the initial contact points (hosts) from which node discovery will
-- start.
setContacts :: String -> [String] -> Settings -> Settings
setContacts v vv = set contacts (v :| vv)

-- | Add an additional host to the contact list.
addContact :: String -> Settings -> Settings
addContact v = over contacts (v <|)

-- | Set the portnumber to use to connect on /every/ node of the cluster.
setPortNumber :: PortNumber -> Settings -> Settings
setPortNumber v = set portnumber v

-- | Set the load-balancing policy.
setPolicy :: IO Policy -> Settings -> Settings
setPolicy v = set policyMaker v

-- | Set strategy to use for preparing statements.
setPrepareStrategy :: PrepareStrategy -> Settings -> Settings
setPrepareStrategy v = set prepStrategy v

-- | Set the 'Logger' to use for processing log messages emitted by the client.
setLogger :: Logger -> Settings -> Settings
setLogger v = set logger v

-----------------------------------------------------------------------------
-- Pool Settings

-- | Set the connection idle timeout. Connections in a pool will be closed
-- if not in use for longer than this timeout.
setIdleTimeout :: NominalDiffTime -> Settings -> Settings
setIdleTimeout v = set (poolSettings.idleTimeout) v

-- | Maximum connections per pool /stripe/.
setMaxConnections :: Int -> Settings -> Settings
setMaxConnections v = set (poolSettings.maxConnections) v

-- | Set the number of pool stripes to use. A good setting is equal to the
-- number of CPU cores this codes is running on.
setPoolStripes :: Int -> Settings -> Settings
setPoolStripes v s
    | v < 1     = error "cql-io settings: stripes must be greater than 0"
    | otherwise = set (poolSettings.poolStripes) v s

-- | When receiving a response times out, we can no longer use the stream of the
-- connection that was used to make the request as it is uncertain if
-- a response will arrive later. Thus the bandwith of a connection will be
-- decreased. This settings defines a threshold after which we close the
-- connection to get a new one with all streams available.
setMaxTimeouts :: Int -> Settings -> Settings
setMaxTimeouts v = set (poolSettings.maxTimeouts) v

-----------------------------------------------------------------------------
-- Connection Settings

-- | Set the compression to use for frame body compression.
setCompression :: Compression -> Settings -> Settings
setCompression v = set (connSettings.compression) v

-- | Set the maximum number of streams per connection. In version 2 of the
-- binary protocol at most 128 streams can be used. Version 3 supports up
-- to 32768 streams.
setMaxStreams :: Int -> Settings -> Settings
setMaxStreams v s
    | v < 1 || v > 32768 = error "cql-io settings: max. streams must be within [1, 32768]"
    | otherwise          = set (connSettings.maxStreams) v s

-- | Set the connect timeout of a connection.
setConnectTimeout :: NominalDiffTime -> Settings -> Settings
setConnectTimeout v = set (connSettings.connectTimeout) (Ms $ round (1000 * v))

-- | Set the send timeout of a connection. Requests exceeding the send
-- timeout will cause the connection to be closed and fail with a
-- 'ConnectionClosed' exception.
setSendTimeout :: NominalDiffTime -> Settings -> Settings
setSendTimeout v = set (connSettings.sendTimeout) (Ms $ round (1000 * v))

-- | Set the response timeout of a connection. Requests exceeding the
-- response timeout will fail with a 'ResponseTimeout' exception.
setResponseTimeout :: NominalDiffTime -> Settings -> Settings
setResponseTimeout v = set (connSettings.responseTimeout) (Ms $ round (1000 * v))

-- | Set the default keyspace to use. Every new connection will be
-- initialised to use this keyspace.
setKeyspace :: Keyspace -> Settings -> Settings
setKeyspace v = set (connSettings.defKeyspace) (Just v)

-- | Set the retry settings to use.
setRetrySettings :: RetrySettings -> Settings -> Settings
setRetrySettings v = set retrySettings v

-- | Set maximum receive buffer size.
--
-- The actual buffer size used will be the minimum of the CQL response size
-- and the value set here.
setMaxRecvBuffer :: Int -> Settings -> Settings
setMaxRecvBuffer v = set (connSettings.maxRecvBuffer) v

-- | Set a fully configured SSL context.
--
-- This will make client server queries use TLS.
setSSLContext :: SSLContext -> Settings -> Settings
setSSLContext v = set (connSettings.tlsContext) (Just v)

-- | Set the supported authentication mechanisms.
--
-- When a Cassandra server requests authentication on a connection,
-- it specifies the requested 'AuthMechanism'. The client 'Authenticator'
-- is chosen based that name. If no authenticator with a matching
-- name is configured, an 'AuthenticationError' is thrown.
setAuthentication :: [C.Authenticator] -> Settings -> Settings
setAuthentication = set (connSettings.authenticators)
                  . HashMap.fromList
                  . map (\a -> (authMechanism a, a))

-----------------------------------------------------------------------------
-- Retry Settings

-- | Never retry.
noRetry :: RetrySettings
noRetry = RetrySettings
    { _retryPolicy        = RetryPolicyM $ const (return Nothing)
    , _reducedConsistency = Nothing
    , _sendTimeoutChange  = Ms 0
    , _recvTimeoutChange  = Ms 0
    , _retryHandlers      = []
    }

-- | Default retry settings, combining 'defRetryHandlers' with 'defRetryPolicy'.
-- Consistency is never reduced on retries and timeout values remain unchanged.
defRetrySettings :: RetrySettings
defRetrySettings = RetrySettings
    { _retryPolicy        = defRetryPolicy
    , _reducedConsistency = Nothing
    , _sendTimeoutChange  = Ms 0
    , _recvTimeoutChange  = Ms 0
    , _retryHandlers      = defRetryHandlers
    }

-- | Eager retry settings, combining 'eagerRetryHandlers' with
-- 'eagerRetryPolicy'. Consistency is never reduced on retries and timeout
-- values remain unchanged.
eagerRetrySettings :: RetrySettings
eagerRetrySettings = RetrySettings
    { _retryPolicy        = eagerRetryPolicy
    , _reducedConsistency = Nothing
    , _sendTimeoutChange  = Ms 0
    , _recvTimeoutChange  = Ms 0
    , _retryHandlers      = eagerRetryHandlers
    }

-- | The default retry policy permits a single, immediate retry.
defRetryPolicy :: RetryPolicy
defRetryPolicy = limitRetries 1

-- | The eager retry policy permits 5 retries with exponential
-- backoff (base-2) with an initial delay of 100ms, i.e. the
-- retries will be performed with 100ms, 200ms, 400ms, 800ms
-- and 1.6s delay, respectively, for a maximum delay of ~3s.
eagerRetryPolicy :: RetryPolicy
eagerRetryPolicy = limitRetries 5 <> exponentialBackoff 100000

-- | The default retry handlers permit a retry for the following errors:
--
--   * A 'HostError', since it always occurs before a query has been
--     sent to the server.
--
--   * A 'ConnectionError' that is a 'ConnectTimeout', since it always
--     occurs before a query has been sent to the server.
--
--   * A 'ResponseError' that is one of the following:
--
--       * 'Unavailable', since that is an error response from a coordinator
--         before the query is actually executed.
--       * A 'ReadTimeout' that indicates that the required consistency
--         level could be achieved but the data was unfortunately chosen
--         by the coordinator to be returned from a replica that turned
--         out to be unavailable. A retry has a good chance of getting the data
--         from one of the other replicas.
--       * A 'WriteTimeout' for a write to the batch log failed. The batch log
--         is written prior to execution of the statements of the batch and
--         hence these errors are safe to retry.
--
defRetryHandlers :: Monad m => [RetryStatus -> Handler m Bool]
defRetryHandlers =
    [ const $ Handler $ \(e :: ConnectionError) -> case e of
        ConnectTimeout {} -> return True
        _                 -> return False
    , const $ Handler $ \(e :: ResponseError) -> return $ case reCause e of
        Unavailable  {}   -> True
        ReadTimeout  {..} -> rTimeoutNumAck >= rTimeoutNumRequired &&
                             not rTimeoutDataPresent
        WriteTimeout {..} -> wTimeoutWriteType == WriteBatchLog
        _                 -> False
    , const $ Handler $ \(_ :: HostError)        -> return True
    , const $ Handler $ \(_ :: SomeSSLException) -> return True
    ]

-- | The eager retry handlers permit a superset of the errors
-- of 'defRetryHandlers', namely:
--
--   * Any 'ResponseError' that is a 'ReadTimeout', 'WriteTimeout',
--     'Overloaded', 'Unavailable' or 'ServerError'.
--
--   * Any 'ConnectionError'.
--
--   * Any 'IOException'.
--
--   * Any 'HostError'.
--
--   * Any 'SomeSSLException' (if an SSL context is configured).
--
-- Notably, these retry handlers are only safe to use for idempotent
-- queries, or if a duplicate write has no severe consequences in
-- the context of the application's data model.
eagerRetryHandlers :: Monad m => [RetryStatus -> Handler m Bool]
eagerRetryHandlers =
    [ const $ Handler $ \(e :: ResponseError) -> case reCause e of
        ReadTimeout  {} -> return True
        WriteTimeout {} -> return True
        Overloaded   {} -> return True
        Unavailable  {} -> return True
        ServerError  {} -> return True
        _               -> return False
    , const $ Handler $ \(_ :: ConnectionError)  -> return True
    , const $ Handler $ \(_ :: IOException)      -> return True
    , const $ Handler $ \(_ :: HostError)        -> return True
    , const $ Handler $ \(_ :: SomeSSLException) -> return True
    ]

-- | Set the 'RetryPolicy' to apply on retryable exceptions,
-- which determines the number and distribution of retries over time,
-- i.e. /how/ retries are performed. Configuring a retry policy
-- does not specify /what/ errors should actually be retried.
-- See 'setRetryHandlers'.
setRetryPolicy :: RetryPolicy -> RetrySettings -> RetrySettings
setRetryPolicy v s = s { _retryPolicy = v }

-- | Set the exception handlers that decide whether a request can be
-- retried by the client, i.e. /what/ errors are permissible to retry.
-- For configuring /how/ the retries are performed, see 'setRetryPolicy'.
setRetryHandlers :: (forall m. Monad m => [RetryStatus -> Handler m Bool])
    -> RetrySettings -> RetrySettings
setRetryHandlers v s = s { _retryHandlers = v }

-- | On retry, change the consistency to the given value.
adjustConsistency :: Consistency -> RetrySettings -> RetrySettings
adjustConsistency v = set reducedConsistency (Just v)

-- | On retry adjust the send timeout. See 'setSendTimeout'.
adjustSendTimeout :: NominalDiffTime -> RetrySettings -> RetrySettings
adjustSendTimeout v = set sendTimeoutChange (Ms $ round (1000 * v))

-- | On retry adjust the response timeout. See 'setResponseTimeout'.
adjustResponseTimeout :: NominalDiffTime -> RetrySettings -> RetrySettings
adjustResponseTimeout v = set recvTimeoutChange (Ms $ round (1000 * v))

