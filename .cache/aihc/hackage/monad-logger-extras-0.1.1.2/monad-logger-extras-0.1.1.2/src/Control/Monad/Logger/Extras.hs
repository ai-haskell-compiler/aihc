{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description: Composable logging actions for monad-logger, with a few predefined loggers.
-}
module Control.Monad.Logger.Extras where

import Control.Monad.Logger

import Data.ByteString.Char8 as C8
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import System.IO
import qualified System.Posix.Syslog as Posix

import System.Console.ANSI

-- | Run a 'LoggingT' action using the provided 'Logger'
runLoggerLoggingT :: LoggingT m a -> Logger -> m a
runLoggerLoggingT f logger = f `runLoggingT` unLogger logger

-- | Type synonym for a logging action. See 'defaultLogStr' for the default
-- formatting of this data.
type LogF = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | A composable logging action.
newtype Logger = Logger { unLogger :: LogF }
  deriving (Semigroup, Monoid)

-- | Composable stderr logging action.
logToStderr :: Logger
logToStderr = Logger $ defaultOutput stderr

-- | Composable stdout logging action.
logToStdout :: Logger
logToStdout = Logger $ defaultOutput stdout

-- | This logger doesn't perform any logging action.
logToNowhere :: Logger
logToNowhere = mempty

-- | Log messages to a posix system log. The string argument is a tag that can
-- be used to identify log messages produced by this logger.
-- You can, for instance, run @journalctl --user -t mytag@ to see log messages
-- tagged with @"mytag"@.
logToSyslog :: String -> Logger
logToSyslog tagstr = Logger $ \loc src lvl str -> do
  let syslogPriority = case lvl of
        LevelDebug -> Posix.Debug
        LevelInfo -> Posix.Info
        LevelWarn -> Posix.Warning
        LevelError -> Posix.Error
        LevelOther _ -> Posix.Info
      out = defaultLogStr loc src lvl str
  Posix.withSyslog tagstr [Posix.DelayedOpen] Posix.User $
    unsafeUseAsCStringLen (fromLogStr out) $
      Posix.syslog Nothing syslogPriority

-- | Add colors to your log output based on 'LogLevel'. Colors can be
-- customized by using 'colorizeWith' instead.
colorize :: Logger -> Logger
colorize = colorizeWith defaultColors

-- | Add a custom set of colors to your log output. See 'defaultColors' for an
-- example.
colorizeWith :: [(LogLevel, Color)] -> Logger -> Logger
colorizeWith colorMap f = Logger $ \loc src lvl str ->
  let c s = case lookup lvl colorMap of
        Nothing -> str
        Just color -> mapLogStrBS (wrapSGRColor color) s
  in unLogger f loc src lvl $ c str

-- | The default color mapping used by 'colorize'.
defaultColors :: [(LogLevel, Color)]
defaultColors =
  [ (LevelDebug, Green)
  , (LevelInfo, Blue)
  , (LevelWarn, Yellow)
  , (LevelError, Red)
  ]

-- | Map a function over a log string.
mapLogStrBS :: ToLogStr msg => (ByteString -> msg) -> LogStr -> LogStr
mapLogStrBS f = toLogStr . f . fromLogStr

-- | Apply 'SGR' codes to a string to modify its display attributes, resetting
-- SGR codes afterward.
wrapSGRCode :: [SGR] -> ByteString -> ByteString
wrapSGRCode codes t = mconcat
  [ C8.pack $ setSGRCode codes
  , t
  , C8.pack $ setSGRCode [Reset]
  ]

-- | Apply an SGR color code to a string, unsetting the color after the string.
wrapSGRColor :: Color -> ByteString -> ByteString
wrapSGRColor c = wrapSGRCode [SetColor Foreground Vivid c]

-- | A handy test
test :: IO ()
test = do
  let logger = colorize logToStderr <> logToSyslog "log-test"
  flip runLoggerLoggingT logger $ do
    logDebugN "This is a debug message."
    logInfoN "This is an info message."
    logWarnN "This is a warning."
    logErrorN "This is an error!"
