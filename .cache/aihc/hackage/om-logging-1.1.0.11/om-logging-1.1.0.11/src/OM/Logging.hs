{-# LANGUAGE OverloadedStrings #-}

{- |
  Description: Logging utilities.

  Various combinators for composing "monad-logger" loggers.
-}
module OM.Logging (
  -- * Standard OM logging
  standardLogging,
  withStandardFormat,

  -- * Logging Combinators
  withTime,
  withThread,
  withLevel,
  withPrefix,
  withPackage,

  -- ** Filters
  filterLogging,
  levelFilter,

  -- ** Destinations
  teeLogging,
  stdoutLogging,
  fdLogging,

  -- * Other types
  parseLevel,
  JSONLevel(..),
) where


import Control.Concurrent (myThreadId)
import Control.Monad (when)
import Control.Monad.Logger (LogLevel(LevelDebug, LevelError, LevelInfo,
  LevelOther, LevelWarn), Loc, LogSource, LogStr, loc_package)
import Data.Aeson (FromJSON(parseJSON), Value(String))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import OM.Show (showt)
import System.IO (Handle, hFlush, stdout)
import System.Log.FastLogger (fromLogStr, toLogStr)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T


{- | Log to more than one logging destination. -}
teeLogging
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Destination 1. -}
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ Destination 2. -}
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
teeLogging logging1 logging2 loc src level msg = do
  logging1 loc src level msg
  logging2 loc src level msg


{- |
  Filter out some log messages. Only messages matching the predicate
  are logged to the underlying logger.
-}
filterLogging
  :: (Loc -> LogSource -> LogLevel -> LogStr -> Bool)
     {- ^ The filter to apply. -}
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
     {- ^ The downstream logging destination. -}
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
filterLogging p base loc src level msg =
  when (p loc src level msg)
    (base loc src level msg)


{- |
  @levelFilter level@ is a filter predicate that matches all log messages
  with @level@ or above.
-}
levelFilter :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> Bool
levelFilter target _ _ level _ = level >= target


{- | Prepend the 'Control.Concurrent.ThreadId' to the beginning of the log. -}
withThread
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withThread base loc src level msg = do
  tid <- myThreadId
  base loc src level (toLogStr (squareBracket (showt tid :: Text)) <> msg)


{- | Add timing information to the beginning of logs. -}
withTime
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withTime base loc src level msg = do
  now <- getCurrentTime
  let
    time :: LogStr
    time =
      toLogStr
      . squareBracket
      . T.pack
      . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%06Q %Z"
      $ now
  base loc src level (time <> msg)


{- | Add the originating package to the log message. -}
withPackage
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withPackage base loc src level msg =
  let
    {-
      The package information looks like this
      `om-legion-6.4.1.1-LnoDD8xLijN7DglolZGFIp`, but we only want the
      actual package name, which is why we do the `reverse . split`
      business.
    -}
    package =
      toLogStr
      . squareBracket
      $ case
          reverse
          . splitOn "-"
          . loc_package
          $ loc
        of
          _hash : _version : nameComponents ->
            intercalate "-" (reverse nameComponents)
          _ -> loc_package loc
  in base loc src level (package <> msg)


{- | Add the Logging level to the log output. -}
withLevel
  :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withLevel base loc src level msg =
  let
    levelStr :: LogStr
    levelStr = squareBracket . toLogStr . showLevel $ level
  in
    base loc src level (levelStr <> msg)


{- | Prefix a fixed string to the log output. -}
withPrefix
  :: LogStr
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withPrefix prefix base loc src level msg =
  base loc src level (prefix <> msg)


{- | Help with putting things in square brackets. -}
squareBracket :: (IsString s, Monoid s) => s -> s
squareBracket t = "[" <> t <> "]"


{- | Stringify a log level. -}
showLevel :: LogLevel -> Text
showLevel (LevelOther level) = T.toUpper level
showLevel level = T.toUpper . T.drop 5 . showt $ level


{- |
  Log messages to stdout. This is very bare bones. It only logs the
  message itself with no other information. It is meant to be used in
  conjunction with some of the other combinators, like `withLevel`.
-}
stdoutLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
stdoutLogging = fdLogging stdout


{- | Like 'stdoutLogging', but log to a file handle. -}
fdLogging :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
fdLogging fd _ _ _ msg = do
  BS8.hPutStr fd (fromLogStr msg <> "\n")
  hFlush fd


{- | The standard logging for most OM programs. -}
standardLogging
  :: LogLevel
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
standardLogging logLevel =
  withStandardFormat logLevel stdoutLogging


{- |
  Log to the indicated destination, applying the "standard" filters
  and formats.
-}
withStandardFormat
  :: LogLevel {- ^ The minimum log level that will be logged. -}
  -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) {- ^ The base logger. -}
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
withStandardFormat logLevel =
  filterLogging (levelFilter logLevel)
  . withPrefix ": "
  . withThread
  . withPackage
  . withLevel
  . withTime


{- | A FromJSON instance to figure out the logging level. -}
newtype JSONLevel = JSONLevel {
    unJSONLevel :: LogLevel
  }
instance FromJSON JSONLevel where
  parseJSON (String str) =
    return (JSONLevel (parseLevel str))
  parseJSON v =
    fail $ "Can't parse logging level from: " ++ show v


{- | Parse a logging level from a string. -}
parseLevel :: Text -> LogLevel
parseLevel "DEBUG" = LevelDebug
parseLevel "INFO" = LevelInfo
parseLevel "WARN" = LevelWarn
parseLevel "ERROR" = LevelError
parseLevel other = LevelOther other


