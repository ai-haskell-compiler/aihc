{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Log.MonadLogger.Syslog
       ( runSyslogLoggingT
       , runCustomSyslogLoggingT
       , System.Posix.Syslog.Facility(..)
       )
       where

import Control.Monad.Logger
import Data.Text             ( unpack )
import System.Posix.Syslog

#if MIN_VERSION_hsyslog(5,0,0)
import qualified Data.ByteString.Unsafe as BSU
#else
import qualified Data.ByteString.Char8 as BS8
import           System.Log.FastLogger ( fromLogStr )
#endif

-- | Runs a 'LoggingT' action, sending its output to Syslog.
-- Logging will use service name (logger "tag") 'hslogger',
-- and Syslog facility 'User'. Log lines will have the same
-- format as 'runStdoutLoggingT', but the 'LogLevel' will
-- be converted to the matching Syslog priority value, and
-- will be included in the logged message.
runSyslogLoggingT :: LoggingT m a -> m a
#if MIN_VERSION_hsyslog(5,0,0)
runSyslogLoggingT = runCustomSyslogLoggingT "hslogger" User
#else
runSyslogLoggingT = runCustomSyslogLoggingT "hslogger" USER
#endif

-- | Like 'runSyslogLoggingT', but specifying desired service
-- name -- (logger "tag") and Syslog facility.
runCustomSyslogLoggingT :: String          -- ^ Logger tag.
                        -> Facility        -- ^ Syslog facility.
                        -> LoggingT m a
                        -> m a
runCustomSyslogLoggingT n f = (`runLoggingT` defaultSyslogOutput n f)

-- This invokes 'formattedSyslogOutput' with 'defaultLogStr'.
-- This means that the resulting log messages are the same as
-- the default format used by "Control.Monad.Logger".
defaultSyslogOutput :: String              -- Logger tag
                    -> Facility            -- Syslog facility
                    -> Loc
                    -> LogSource
                    -> LogLevel
                    -> LogStr
                    -> IO ()
defaultSyslogOutput n f = formattedSyslogOutput n f defaultLogStr

-- Given a "Control.Monad.Logger" log formatter, this writes
-- the log to the syslog,
formattedSyslogOutput :: String            -- Logger tag
                      -> Facility          -- Syslog facility
                      -> (Loc -> LogSource -> LogLevel -> LogStr -> LogStr)
                      -> Loc
                      -> LogSource
                      -> LogLevel
                      -> LogStr
                      -> IO ()
formattedSyslogOutput name facility f l s level msg =
#if MIN_VERSION_hsyslog(5,0,0)
    withSyslog name [DelayedOpen] facility $
    BSU.unsafeUseAsCStringLen (fromLogStr $ f l s level msg) $
    syslog
      (Just facility)
      (levelToPriority level)
#else
    withSyslog defaultConfig { identifier      = BS8.pack name
                             , defaultFacility = facility
                             } $ \syslog ->
        syslog facility
            (levelToPriority level)
            (fromLogStr $ f l s level msg)
#endif

levelToPriority :: LogLevel -> Priority
levelToPriority LevelDebug = Debug
levelToPriority LevelInfo  = Info
levelToPriority LevelWarn  = Warning
levelToPriority LevelError = Error
levelToPriority (LevelOther level) =
    case level of
        "Emergency" -> Emergency
        "Alert"     -> Alert
        "Critical"  -> Critical
        "Notice"    -> Notice
        _           -> error $ "unknown log level: " ++ unpack level
