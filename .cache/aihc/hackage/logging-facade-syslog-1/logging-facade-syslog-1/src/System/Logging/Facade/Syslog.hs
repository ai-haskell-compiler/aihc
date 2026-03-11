{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: POSIX

   A simple "System.Logging.Facade" back-end for @syslog(3)@ as specified in
   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html POSIX.1-2008>.
-}

module System.Logging.Facade.Syslog where

import System.Logging.Facade.Sink ( LogSink )
import System.Logging.Facade.Types ( LogRecord(LogRecord), LogLevel(..), Location(..) )
import System.Posix.Syslog ( Priority(..), Facility, syslog )
import Foreign.C.String ( withCStringLen )

-- | Pass this value to 'System.Logging.Facade.Sink.setLogSink' to switch
-- logging to 'syslog'. Messages of all priorities will be logged in the
-- processes' default syslog facility --- typically 'System.Posix.Syslog.User'.
-- If these default settings don't fit your needs, consider using 'syslogSink'
-- or call 'System.Posix.Syslog.withSyslog' or a similar funtion to configure
-- the system's @syslog@ implementation appropriately.
--
-- @
--   syslogSink = syslogSink' showWithLocation Nothing
-- @

syslogSink :: LogSink
syslogSink = syslogSink' showWithLocation Nothing

-- | A variant of 'syslogSink' that allows you to control how the location
-- information provided by @logger-facade@ is baked into the log message and
-- which @syslog@ 'Facility' will be used. If no explicit facility is set,
-- messages go out with whatever the process default is --- typically
-- 'System.Posix.Syslog.User'.

syslogSink' :: (Maybe Location -> String -> String) -> Maybe Facility -> LogSink
syslogSink' fmtMessage facil (LogRecord lvl loc msg) =
  withCStringLen (fmtMessage loc msg) (syslog facil (mapPrio lvl))
    where
      mapPrio :: LogLevel -> Priority
      mapPrio TRACE = Debug
      mapPrio DEBUG = Debug
      mapPrio INFO  = Info
      mapPrio WARN  = Warning
      mapPrio ERROR = Error

-- | The default message formatter used by 'syslogSink'. It will append the
-- location of the message to the string such that running
--
-- @
--   info "Hello, world."
-- @
--
-- yields:
--
-- @
--   May 15 10:59:41 myhost ghc[28074]: Hello, world. (from package \"interactive\", module \"Ghci2\", file \"\<interactive\>\", line 32, and column 1
-- @

showWithLocation :: Maybe Location -> String -> String
showWithLocation Nothing    msg = msg
showWithLocation (Just loc) msg = showString msg
                                . showString " (from package " . shows (locationPackage loc)
                                . showString ", module " . shows (locationModule loc)
                                . showString ", file " . shows (locationFile loc)
                                . showString ", line " . shows (locationLine loc)
                                . showString ", and column " $ show (locationColumn loc)
