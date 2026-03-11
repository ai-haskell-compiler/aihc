{-|
Module      : System.Log.Caster.Core
Description : Multicast, thread-safe, and not slow logger.
Copyright   : (c) Akihito KIRISAKI

License     : BSD3
Maintainer  : Akihito KIRISAKI
Stability   : experimental
-}
{-# LANGUAGE UndecidableInstances #-}
module System.Log.Caster.Core
  ( -- * Basics
    LogMsg(..)
  , broadcastLog
  , LogQueue(..)
  , newLogQueue
  , LogChan(..)
  , newLogChan
  , Formatter
  , Listener
  , relayLog
  -- * Listeners
  , stdoutListener
  , stdoutListenerWith
  , terminalListener
  , handleListener
  , handleListenerFlush
  -- * Formatter
  , defaultFormatter
  , terminalFormatter
  , terminalFormatterWith
  -- * Log levels
  , LogLevel(..)
  -- * Useful string class and operator
  , ToBuilder(..)
  , fix
  , ($:)
  , (<:>)
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.FastBuilder as FB
import qualified Data.ByteString.Lazy        as LBS
import           Data.Semigroup
import qualified Data.Text                   as ST
import qualified Data.Text.Encoding          as STE
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LTE
import           Data.UnixTime               (Format, UnixTime (..),
                                              formatUnixTime)
import           GHC.IO.Unsafe               (unsafePerformIO)
import           System.IO                   (Handle, hFlush, stdout)

-- | Types which are able to be converted into @'FB.Builder' Builder@
--   @toBuilde@ encodes @String@ and @Text@ as utf-8.
class ToBuilder a where
  toBuilder :: a -> FB.Builder

instance ToBuilder FB.Builder where
  toBuilder = id

instance ToBuilder String where
  toBuilder = FB.stringUtf8

instance ToBuilder ST.Text where
  toBuilder = FB.byteString . STE.encodeUtf8

instance ToBuilder LT.Text where
  toBuilder = FB.byteString . LBS.toStrict . LTE.encodeUtf8

instance ToBuilder SBS.ByteString where
  toBuilder = FB.byteString

instance ToBuilder LBS.ByteString where
  toBuilder = FB.byteString . LBS.toStrict

instance ToBuilder BB.Builder where
  toBuilder = FB.byteString . LBS.toStrict . BB.toLazyByteString

instance {-# OVERLAPPABLE #-} Show a => ToBuilder a where
  toBuilder = FB.stringUtf8 . show

-- | If you turn @OverloadedStrings@ extension on, GHC can't deduce the type of string literal.
--   This function fix the type to @'FB.Builder' Builder@ without type annotation.
fix :: FB.Builder -> FB.Builder
fix = id

-- | Infix version of @fix@.
infixr 0 $:
($:) :: ToBuilder b => (FB.Builder -> b) -> FB.Builder -> b
($:) = ($)

-- | Concat @ToBuilder@ strings as @'FB.Builder' Builder@.
infixr 6 <:>
(<:>) :: (ToBuilder a, ToBuilder b) => a -> b -> FB.Builder
a <:> b = toBuilder a <> toBuilder b

-- |Log levels. These are matched to syslog.
data LogLevel
  = LogDebug
  | LogInfo
  | LogNotice
  | LogWarn
  | LogError
  | LogCritical
  | LogAlert
  | LogEmergency
  deriving (Show, Eq, Ord)

-- | Log message.
data LogMsg = LogMsg
  { logMsgLevel   :: LogLevel
  , logMsgTime    :: UnixTime
  , logMsgBuilder :: FB.Builder
  }

-- | Queue of @LogMsg@.
newtype LogQueue = LogQueue (TQueue LogMsg)

-- | Channel of @LogMsg@.
newtype LogChan = LogChan (TChan LogMsg)

-- | Make new @LogQueue@
newLogQueue :: IO LogQueue
newLogQueue = LogQueue <$> newTQueueIO

-- | Make new @LogChan@.
newLogChan :: IO LogChan
newLogChan = LogChan <$> newBroadcastTChanIO

-- | Connect @LogQueue@ and @TChan@ @LogMsg@.
broadcastLog :: LogQueue -> LogChan -> IO ()
broadcastLog (LogQueue q) (LogChan c) =  forever $
  atomically $ readTQueue q >>= writeTChan c

-- | Formatter.
type Formatter = LogMsg -> FB.Builder

-- | IO function takes @LogMsg@.
type Listener = LogMsg -> IO ()

-- | Listen @LogChan@ and give the @LogMsg@ to given @Listener@.
relayLog :: LogChan -> LogLevel -> Listener -> IO ()
relayLog (LogChan bchan) logLevel listener = do
  chan <- atomically $ dupTChan bchan
  forever $ do
    msg <- atomically $ readTChan chan
    when (logMsgLevel msg >= logLevel) $ listener msg

-- | Make @Listener@ from @Handle@
handleListener :: Formatter -> Handle -> Listener
handleListener f h = FB.hPutBuilder h . f

-- | Make @Listener@ flushing buffer after getting @LogMsg@
handleListenerFlush :: Formatter -> Handle -> Listener
handleListenerFlush f h msg = FB.hPutBuilder h (f msg) >> hFlush h

-- | Stdout listener with @Formatter@.
stdoutListenerWith :: Formatter -> Listener
stdoutListenerWith f = handleListenerFlush f stdout

-- | Stdout listener.
stdoutListener :: Listener
stdoutListener = stdoutListenerWith defaultFormatter

-- | Terminal listener. Log levels are colored.
terminalListener :: Listener
terminalListener = stdoutListenerWith terminalFormatter

-- | Default log formatter.
defaultFormatter :: Formatter
defaultFormatter (LogMsg lev ut str) =
  formatTime ut <> " - [" <> logLevelToBuilder lev <> "] " <> str <> "\n"

-- | Formatter for term.
--   It provides colored logs.
terminalFormatter :: Formatter
terminalFormatter = terminalFormatterWith
                    "\ESC[32m"
                    "\ESC[36m"
                    "\ESC[4m\ESC[36m"
                    "\ESC[4m\ESC[33m"
                    "\ESC[4m\ESC[31m"
                    "\ESC[1m\ESC[31m"
                    "\ESC[1m\ESC[35m"
                    "\ESC[5m\ESC[35m"

-- | Formatter with specified colors for log levels.
--   Parameters are just @FB.Builder@, so you can decorate as you like with ansi escaping.
terminalFormatterWith :: FB.Builder -> FB.Builder -> FB.Builder -> FB.Builder -> FB.Builder -> FB.Builder -> FB.Builder -> FB.Builder -> Formatter
terminalFormatterWith fDebug fInfo fNotice fWarn fError fCritical fAlert fEmergency (LogMsg lev ut str) =
  formatTime ut <> " - " <> fmt <>  "[" <> logLevelToBuilder lev <> "]\ESC[0m " <> str <> "\n"
  where
    fmt = case lev of
      LogDebug     -> fDebug
      LogInfo      -> fInfo
      LogNotice    -> fNotice
      LogWarn      -> fWarn
      LogError     -> fError
      LogCritical  -> fCritical
      LogAlert     -> fAlert
      LogEmergency -> fEmergency


logLevelToBuilder :: LogLevel -> FB.Builder
logLevelToBuilder = \case
  LogDebug -> "DEBUG"
  LogInfo  -> "INFO"
  LogNotice  -> "NOTICE"
  LogWarn  -> "WARN"
  LogError -> "ERROR"
  LogCritical -> "CRITICAL"
  LogAlert -> "ALERT"
  LogEmergency -> "EMERGENCY"


{-# NOINLINE formatTime #-}
formatTime :: UnixTime -> FB.Builder
formatTime ut =
  let
    ut' = FB.byteString . unsafePerformIO $ formatUnixTime "%Y-%m-%d %T" ut
    utMilli = FB.string7 . tail . show $ utMicroSeconds ut `div` 1000 + 1000
  in
    ut' <> "." <> utMilli
