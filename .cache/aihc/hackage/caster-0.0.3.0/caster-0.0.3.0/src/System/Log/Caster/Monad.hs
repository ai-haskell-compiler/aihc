{-|
Module      : System.Log.Caster.Monad
Description : Multicast, thread-safe, and not slow logger.
Copyright   : (c) Akihito KIRISAKI

License     : BSD3
Maintainer  : Akihito KIRISAKI
Stability   : experimental
-}
module System.Log.Caster.Monad
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
  , MonadCaster(..)
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

-- | Logging monad for ReaderT pattern.
class MonadIO m => MonadCaster m where
  getLogQueue :: m LogQueue

  logAs :: ToBuilder s => LogLevel -> s -> m ()
  logAs l s = do
    ut <- liftIO getUnixTime
    LogQueue q <- getLogQueue
    liftIO . atomically $ writeTQueue q (LogMsg l ut (toBuilder s))

  debug :: ToBuilder s => s -> m ()
  debug = logAs LogDebug

  info :: ToBuilder s => s -> m ()
  info = logAs LogInfo

  notice :: ToBuilder s => s -> m ()
  notice = logAs LogNotice

  warn :: ToBuilder s => s -> m ()
  warn = logAs LogWarn

  err :: ToBuilder s => s -> m ()
  err = logAs LogError

  critical :: ToBuilder s => s -> m ()
  critical = logAs LogCritical

  alert :: ToBuilder s => s -> m ()
  alert = logAs LogAlert

  emergency :: ToBuilder s => s -> m ()
  emergency = logAs LogEmergency
