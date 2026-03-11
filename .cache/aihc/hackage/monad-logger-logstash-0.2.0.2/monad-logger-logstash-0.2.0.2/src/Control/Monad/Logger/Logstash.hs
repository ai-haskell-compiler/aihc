--------------------------------------------------------------------------------
-- Logstash backend for monad-logger                                          --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | This module implements `runLogstashLoggingT` which can be
-- used to write log messages that arise in a `LoggingT` computation to a
-- given `LogstashContext`. The following example demonstrates how to use the 
-- `runLogstashLoggingT` function with a TCP connection to Logstash, the
-- default retry policy from `Control.Retry`, a 1s timeout for each attempt,
-- and the @json_lines@ codec:
-- 
-- > main :: IO ()
-- > main = do 
-- >    let ctx = logstashTcp def
-- >    runLogstashLoggingT ctx retryPolicyDefault 1000000 (const stashJsonLine) $ 
-- >         logInfoN "Hello World"
--
-- Assuming a suitable Logstash server that can receive this message,
-- something like the following JSON document should be indexed (see the 
-- documentation for `Control.Monad.Logger` for information about how to include
-- more information in log messages):
--
-- > { 
-- >    "@version":"1",
-- >    "message":"Hello World",
-- >    "log.origin.file.line":0,
-- >    "log.origin.file.module":"<unknown>",
-- >    "log.origin.file.package":"<unknown>",
-- >    "log.origin.file.start.column":0,
-- >    "log.origin.file.start.line":0,
-- >    "log.origin.file.end.column":0,
-- >    "log.origin.file.end.line":0,
-- >    "log.origin.file.name":"<unknown>",
-- >    "log.logger":"",
-- >    "log.level":"info"
-- > }
--
-- If an error or a timeout occurs while writing to the Logstash connection,
-- the retry policy determines whether and when sending the message is
-- attempted again. If all attempts fail, the most recent exception is thrown
-- to the caller.
module Control.Monad.Logger.Logstash (
    runLogstashLoggingT,
    stashJsonLine,
    jsonLogLine,

    withLogstashLoggingT,
    runTBMQueueLoggingT,
    unTBMQueueLoggingT,

    -- * Re-exports
    LogstashContext(..)
) where 

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception (Handler)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Retry

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import UnliftIO (MonadIO(..), MonadUnliftIO)

import Logstash hiding (stashJsonLine)
import qualified Logstash as L (stashJsonLine)

--------------------------------------------------------------------------------

-- | `runLogstashLoggingT` @context retryPolicy time codec logger@ runs a 
-- `LoggingT` computation which writes all log entries to the Logstash 
-- @context@ using the given @codec@. The @retryPolicy@ determines whether 
-- and how the handler should deal with failures that arise. Each attempt
-- that is made by the @retryPolicy@ will have a timeout of @time@
-- microseconds applied to it. 
runLogstashLoggingT 
    :: LogstashContext ctx 
    => ctx 
    -> RetryPolicyM IO
    -> Integer
    -> ( RetryStatus -> 
         (Loc, LogSource, LogLevel, LogStr) -> 
         ReaderT LogstashConnection IO ()
       )
    -> LoggingT m a 
    -> m a
runLogstashLoggingT ctx policy time codec log = runLoggingT log $ 
    \logLoc logSource logLevel logStr -> runLogstash ctx policy time $ 
    \s -> codec s (logLoc, logSource, logLevel, logStr)

-- | `withLogstashLoggingT` @cfg codec exceptionHandlers logger@ is like
-- `withLogstashQueue` except for `LoggingT` computations so that log messages
-- are automatically added to the queue.
withLogstashLoggingT
    :: (LogstashContext ctx, MonadUnliftIO m)
    => LogstashQueueCfg ctx
    -> ( RetryStatus -> 
         (Loc, LogSource, LogLevel, LogStr) ->
         ReaderT LogstashConnection IO ()
       )
    -> [(Loc, LogSource, LogLevel, LogStr) -> Handler ()]
    -> LoggingT m a
    -> m a
withLogstashLoggingT cfg dispatch hs log = withLogstashQueue cfg dispatch hs $ 
    \queue -> runTBMQueueLoggingT queue log

-- | `runTBMQueueLoggingT` @queue logger@ runs @logger@ so that log messages
-- are automatically added to @queue@. This can be used if the same queue and 
-- consumer should be shared among multiple producer threads. The queue should
-- be initialised by `withLogstashQueue`.
runTBMQueueLoggingT 
    :: MonadUnliftIO m 
    => TBMQueue (Loc, LogSource, LogLevel, LogStr)
    -> LoggingT m a
    -> m a
runTBMQueueLoggingT queue log = runLoggingT log $
    \logLoc logSource logLevel logStr -> atomically $ 
        writeTBMQueue queue (logLoc, logSource, logLevel, logStr)

-- | `unTBMQueueLoggingT` @queue@ is like `unChanLoggingT` but for a 
-- `TBMQueue`. Since a `TBMQueue` can be closed, this function does not run
-- forever like `unChanLoggingT` and will return when @queue@ is closed.
unTBMQueueLoggingT 
    :: (MonadIO m, MonadLogger m)
    => TBMQueue (Loc, LogSource, LogLevel, LogStr)
    -> m () 
unTBMQueueLoggingT queue = do
    mLine <- liftIO $ atomically $ readTBMQueue queue

    case mLine of 
        Nothing -> pure ()
        Just (loc,src,lvl,msg) -> do 
            monadLoggerLog loc src lvl msg
            unTBMQueueLoggingT queue

--------------------------------------------------------------------------------

-- | `stashJsonLine` @entry@ serialises @entry@ as JSON using reasonable
-- defaults for Elasticsearch based on 
-- https://www.elastic.co/guide/en/ecs/current/ecs-field-reference.html
-- and sends the result to Logstash using the @json_lines@ codec.
stashJsonLine :: (Loc, LogSource, LogLevel, LogStr) 
              -> ReaderT LogstashConnection IO ()
stashJsonLine = L.stashJsonLine . jsonLogLine 
    
-- | `jsonLogLine` @entry@ serialises @entry@ as JSON using reasonable
-- defaults for Elasticsearch based on 
-- https://www.elastic.co/guide/en/ecs/current/ecs-field-reference.html
jsonLogLine :: (Loc, LogSource, LogLevel, LogStr) -> Value
jsonLogLine (loc, src, lvl, msg) = object 
    [ "message" .= decodeUtf8 (fromLogStr msg)
    , "log" .= object 
        [ "logger" .= src
        , "level" .= jsonLogLevel lvl
        , "origin" .= object 
            [ "file" .= object 
                [ "name" .= loc_filename loc 
                , "line" .= fst (loc_start loc)
                -- the following fields are not part of the ECS
                , "package" .= loc_package loc
                , "module" .= loc_module loc
                , "start" .= jsonCharPos (loc_start loc)
                , "end" .= jsonCharPos (loc_end loc)
                ] 
            ]
        ]
    ]
    where jsonLogLevel :: LogLevel -> Text
          jsonLogLevel LevelDebug = "debug"
          jsonLogLevel LevelInfo = "info"
          jsonLogLevel LevelWarn = "warn"
          jsonLogLevel LevelError = "error"
          jsonLogLevel (LevelOther x) = x

          jsonCharPos :: (Int, Int) -> Value
          jsonCharPos (line, column) =
              object [ "line" .= line, "column" .= column ]

--------------------------------------------------------------------------------
