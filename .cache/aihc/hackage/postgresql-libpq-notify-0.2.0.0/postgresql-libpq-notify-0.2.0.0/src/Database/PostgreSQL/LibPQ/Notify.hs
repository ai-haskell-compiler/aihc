{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes, RecordWildCards #-}
{-|
Module      :  Database.PostgreSQL.LibPQ.Notify
Copyright   :  (c) 2020 Jonathan Fischoff
               (c) 2016 Moritz Kiefer
               (c) 2011-2015 Leon P Smith
               (c) 2012 Joey Adams
License     :  BSD3
Maintainer  :  Moritz Kiefer <moritz.kiefer@purelyfunctional.org>
Support for receiving asynchronous notifications via PostgreSQL's
Listen/Notify mechanism.  See
<http://www.postgresql.org/docs/9.4/static/sql-notify.html> for more
information.

Note that on Windows,  @getNotification@ currently uses a polling loop
of 1 second to check for more notifications,  due to some inadequacies
in GHC's IO implementation and interface on that platform.   See GHC
issue #7353 for more information.  While this workaround is less than
ideal,  notifications are still better than polling the database directly.
Notifications do not create any extra work for the backend,  and are
likely cheaper on the client side as well.

<http://hackage.haskell.org/trac/ghc/ticket/7353>

PostgreSQL notifications support using the same connection for sending and
receiving notifications.

However this implementation cannot support this usage pattern.

This implementation favors low latency by utilizing socket read notifications.
However a consequence of this implementation choice is the connection used
to wait for the notification cannot be used for anything else.

-}

module Database.PostgreSQL.LibPQ.Notify
     ( getNotification
     -- ** Advanced API
     , getNotificationWithConfig
     , defaultConfig
     , Config (..)
     ) where

import           Control.Exception (try, throwIO)
import qualified Database.PostgreSQL.LibPQ as PQ
import           GHC.IO.Exception (IOException(..),IOErrorType(ResourceVanished))
#if defined(mingw32_HOST_OS)
import           Control.Concurrent (threadDelay)
#else
import           Control.Concurrent
import           Control.Concurrent.STM(atomically)
#endif
import           Data.Function(fix)
import           Data.Bifunctor(first)

-- | Options for controlling and instrumenting the behavior of 'getNotificationWithConfig'
data Config = Config
  { startLoop              :: IO ()
  -- ^ Called each time 'getNotificationWithConfig' loops to look for another notification
  , beforeWait             :: IO ()
  -- ^ Event called before the thread will wait on 'threadWaitReadSTM' action.
#if defined(mingw32_HOST_OS)
  , retryDelay             :: Int
  -- ^ How long to wait in microseconds before retrying on Windows.
#endif
  }

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
  { startLoop              = pure ()
  , beforeWait             = pure ()
#if defined(mingw32_HOST_OS)
  , retryDelay             = 100000
#endif
  }

funcName :: String
funcName = "Hasql.Notification.getNotification"

setLoc :: IOError -> IOError
setLoc err = err {ioe_location = funcName}

fdError :: IOError
fdError =
  IOError { ioe_handle = Nothing
          , ioe_type = ResourceVanished
          , ioe_location = funcName
          , ioe_description =
              "failed to fetch file descriptor (did the connection time out?)"
          , ioe_errno = Nothing
          , ioe_filename = Nothing
          }

{-|
Returns a single notification.  If no notifications are
available, 'getNotificationWithConfig' blocks until one arrives.
Unlike 'getNotification', 'getNotificationWithConfig' takes in an
additional 'Config' parameter provides event hooks for operational insight.

The connection passed in cannot be used for anything else
while waiting on the notification or this call might never return.


Note that PostgreSQL does not
deliver notifications while a connection is inside a transaction.
-}
getNotificationWithConfig
  :: Config
  -- ^ 'Config' to instrument and configure the retry period on Windows.
  -> PQ.Connection
  -- ^ The connection. The connection cannot be used for anything else
  --   while waiting on the notification or this call might never return.
  -> IO (Either IOError PQ.Notify)
getNotificationWithConfig Config {..} c = fmap (first setLoc) $ try $ fix $ \next -> do
    startLoop
    PQ.notifies c >>= \case
      -- We found a notification just return it
      Just x -> pure x
      -- There wasn't a notification so we need to register to wait on the file handle
      Nothing -> PQ.socket c >>= \case
        -- This is an odd error
        Nothing -> throwIO fdError
        -- Typical case. Register to wait on more data.
        Just fd -> do
#if defined(mingw32_HOST_OS)
          let fileNotification = threadDelay retryDelay
#else
          -- We use threadWaitReadSTM instead of threadWaitRead to ensure the
          -- read callback is register while the look is held.
          action <-fst <$> threadWaitReadSTM fd
          -- Either wait for the file notification or race against the notification
          -- with the custom interrupt event if one is provided
          let fileNotification = atomically action
#endif
          beforeWait
          fileNotification
          _ <- PQ.consumeInput c
          next

{-|
Returns a single notification.  If no notifications are
available, 'getNotification' blocks until one arrives.

The connection passed in cannot be used for anything else
while waiting on the notification or this call might never return.

Note that PostgreSQL does not
deliver notifications while a connection is inside a transaction.
-}
getNotification
  :: PQ.Connection
  -- ^ The connection. The connection cannot be used for anything else
  --   while waiting on the notification or this call might never return.
  -> IO (Either IOError PQ.Notify)
getNotification = getNotificationWithConfig defaultConfig
