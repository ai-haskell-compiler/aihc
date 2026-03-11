{-|
Module      : System.Log.Caster
Description : Multicast, thread-safe, and not slow logger.
Copyright   : (c) Akihito KIRISAKI

License     : BSD3
Maintainer  : Akihito KIRISAKI
Stability   : experimental
-}
module System.Log.Caster
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
  , logAs
  , debug
  , info
  , notice
  , warn
  , err
  , critical
  , alert
  , emergency
  -- * Useful string class and operator
  , ToBuilder(..)
  , fix
  , ($:)
  , (<:>)
  ) where

import           System.Log.Caster.Core

import           Control.Concurrent.STM
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.UnixTime          (getUnixTime)

-- | Push a message to @LogQueue@.
logAs :: (MonadIO m, ToBuilder s) => LogQueue -> LogLevel -> s -> m ()
logAs (LogQueue q) l s = liftIO $ do
  ut <- getUnixTime
  atomically $ writeTQueue q (LogMsg l ut (toBuilder s))

debug :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
debug q = logAs q LogDebug

info :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
info q = logAs q LogInfo

notice :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
notice q = logAs q LogNotice

warn :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
warn q = logAs q LogWarn

err :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
err q = logAs q LogError

critical :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
critical q = logAs q LogCritical

alert :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
alert q = logAs q LogAlert

emergency :: (MonadIO m, ToBuilder s) => LogQueue -> s -> m ()
emergency q = logAs q LogEmergency
