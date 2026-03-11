--------------------------------------------------------------------------------
-- Logstash client for Haskell                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | A simple Logstash client.
module Logstash (
    module Logstash.Connection,
    
    -- * Running Logstash actions
    LogstashContext(..),
    runLogstashConn,
    runLogstashPool,

    LogstashQueueCfg(..),
    defaultLogstashQueueCfg,
    withLogstashQueue,

    -- * Codecs
    stash, 
    stashJsonLine
) where 

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.Timeout
import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Retry

import Data.Aeson
import Data.Acquire
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import Data.Pool

import UnliftIO (MonadUnliftIO(..))

import Logstash.Connection

--------------------------------------------------------------------------------

-- | `stash` @timeout message@ is a computation which sends @message@ to 
-- the Logstash server. 
stash 
    :: MonadIO m 
    => BSL.ByteString 
    -> ReaderT LogstashConnection m ()
stash msg = ask >>= \con -> liftIO $ writeData con msg

-- | `stashJsonLine` @timeout document@ is a computation which serialises
-- @document@ and sends it to the Logstash server. This function is intended
-- to be used with the `json_lines` codec.
stashJsonLine 
    :: (MonadIO m, ToJSON a) 
    => a 
    -> ReaderT LogstashConnection m ()
stashJsonLine = stash . (<> "\n") . encode

--------------------------------------------------------------------------------

-- | A type of exceptions that can occur related to Logstash connections.
data LogstashException = LogstashTimeout
    deriving (Eq, Show) 

instance Exception LogstashException where 
    displayException _ = "Writing to Logstash timed out."

-- | A type class of types that provide Logstash connections.
class LogstashContext ctx where 
    runLogstash 
        :: (MonadMask m, MonadUnliftIO m) 
        => ctx 
        -> RetryPolicyM m
        -> Integer
        -> (RetryStatus -> ReaderT LogstashConnection m a) 
        -> m a

instance LogstashContext (Acquire LogstashConnection) where 
    runLogstash = runLogstashConn

instance LogstashContext LogstashPool where
    runLogstash = runLogstashPool

-- | `runLogstashConn` @connectionAcquire computation@ runs @computation@ using
-- a Logstash connection produced by @connectionAcquire@.
runLogstashConn 
    :: (MonadMask m, MonadUnliftIO m)
    => Acquire LogstashConnection
    -> RetryPolicyM m
    -> Integer 
    -> (RetryStatus -> ReaderT LogstashConnection m a) 
    -> m a 
runLogstashConn acq policy time action = 
    recoverAll policy $ \s -> 
    withRunInIO $ \runInIO -> do 
    -- run the Logstash action with the specified timeout
    mr <- timeout time $ runInIO $ withAcquire acq $ runReaderT (action s)
    
    -- raise an exception if a timeout occurred
    maybe (throw LogstashTimeout) pure mr

-- | `runLogstashPool` @pool computation@ takes a `LogstashConnection` from
-- @pool@ and runs @computation@ with it.
runLogstashPool 
    :: (MonadMask m, MonadUnliftIO m) 
    => LogstashPool 
    -> RetryPolicyM m
    -> Integer
    -> (RetryStatus -> ReaderT LogstashConnection m a)
    -> m a
runLogstashPool pool policy time action =
    recoverAll policy $ \s -> 
    withRunInIO $ \runInIO -> 
    mask $ \restore -> do
        -- acquire a connection from the resource pool
        (resource, local) <- takeResource pool

        mr <- restore (timeout time $ runInIO $ runReaderT (action s) resource) 
            `onException` destroyResource pool local resource

        -- return the resource to the pool
        putResource local resource

        -- raise an exception if a timeout occurred
        maybe (throw LogstashTimeout) pure mr

--------------------------------------------------------------------------------

-- | Configurations for `withLogstashQueue` which control the general Logstash
-- configuration as well as the size of the bounded queue and the number of
-- worker threads.
data LogstashQueueCfg ctx = MkLogstashQueueCfg {
    -- | The connection context for the worker threads.
    logstashQueueContext :: ctx,
    -- | The size of the queue.
    logstashQueueSize :: Int,
    -- | The number of workers. This must be at least 1.
    logstashQueueWorkers :: Int,
    -- | The retry policy.
    logstashQueueRetryPolicy :: RetryPolicyM IO,
    -- | The timeout for each attempt at sending data to the Logstash server.
    logstashQueueTimeout :: Integer
}

-- | `defaultLogstashQueueCfg` @ctx@ constructs a `LogstashQueueCfg` with
-- some default values for the Logstash context given by @ctx@:
-- 
-- - A queue size limited to 1000 entries
-- - Two worker threads
-- - 25ms exponential backoff with a maximum of five retries as retry policy
-- - 1s timeout for each logging attempt
--
-- You may wish to customise these settings to suit your needs.
defaultLogstashQueueCfg :: LogstashContext ctx => ctx -> LogstashQueueCfg ctx
defaultLogstashQueueCfg ctx = MkLogstashQueueCfg{
    logstashQueueContext = ctx,
    logstashQueueSize = 1000,
    logstashQueueWorkers = 2,
    logstashQueueRetryPolicy = exponentialBackoff 25000 <> limitRetries 5,
    logstashQueueTimeout = 1000000
}

-- | `withLogstashQueue` @cfg codec exceptionHandlers action@ initialises a
-- queue with space for a finite number of log messages given by @cfg@ to allow 
-- for log messages to be sent to Logstash asynchronously. This addresses the 
-- following issues with synchronous logging:
--
-- - Since writing log messages to Logstash involves network I/O, this may 
--   be slower than queueing up messages in memory and therefore synchronously 
--   sending messages may delay the computation that originated the log 
--   message.
-- - With a finite number of Logstash connections, synchronous logging may also
--   get blocked until a connection is available.
--
-- The queue is read from by a configurable number of worker threads which 
-- use Logstash connections from a `LogstashContext`. The queue is given
-- to @action@ as an argument. The @retryPolicy@ and @timeout@ parameters
-- serve the same purpose as for `runLogstash`. We recommend that, if the
-- `LogstashContext` is a `LogstashPool`, it should contain at least as many 
-- connections as the number of works to avoid contention between worker 
-- threads.
--
-- @codec@ is the handler for how messages should be sent to the Logstash 
-- server, this is typically a codec like `stashJsonLine`. The `RetryStatus`
-- is provided as an additional argument to @codec@ in case a handler wishes
-- to inspect this.
withLogstashQueue 
    :: (LogstashContext ctx, MonadUnliftIO m)
    => LogstashQueueCfg ctx
    -> (RetryStatus -> item -> ReaderT LogstashConnection IO ())
    -> [item -> Handler ()]
    -> (TBMQueue item -> m a)
    -> m a
withLogstashQueue MkLogstashQueueCfg{..} dispatch handlers action = 
    withRunInIO $ \runInIO -> do
        -- initialise a bounded queue with the specified size
        queue <- newTBMQueueIO logstashQueueSize

        let worker = do 
                -- [blocking] read the next item from the queue
                mitem <- atomically $ readTBMQueue queue 

                -- the item will be Nothing if the queue is empty and has
                -- been closed; otherwise we have an item that should be
                -- dispatched to Logstash
                case mitem of 
                    Nothing -> pure ()
                    Just item -> do 
                        -- dispatch the item using the policies from the
                        -- configuration and the provided dispatch handler
                        -- this may fail if the retry policy is exhausted,
                        -- in which case we use the provided exception
                        -- handlers to try and catch the exception
                        runLogstash logstashQueueContext 
                                    logstashQueueRetryPolicy 
                                    logstashQueueTimeout 
                                    (\status -> dispatch status item)
                            `catches` (map ($ item) handlers)
                        
                        -- loop
                        worker

        -- initialise the requested number of worker threads
        workers <- replicateM logstashQueueWorkers $ async worker

        -- run the main computation in the current thread with its original
        -- monad stack and give it the the queue to write log messages to
        runInIO (action queue) `finally` do 
            -- close the queue to allow the worker threads to gracefully 
            -- shut down
            atomically $ closeTBMQueue queue

            -- wait for the worker threads to terminate; we use `waitCatch` 
            -- here instead of `wait` because `waitCatch` does not raise any 
            -- exceptions that may occur in the worker threads in this thread
            mapM_ waitCatch workers

--------------------------------------------------------------------------------
