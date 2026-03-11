{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

{-|
Module      : Control.Concurrent.Async.Refresh
Description : This module exposes the API of the async-refresh package.
Copyright   : (c) Moritz Clasmeier, 2017-2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

The async-refresh package provides the logic for periodic refreshing
of arbitrary data. This module implements the core of the package and
exposes its API.
-}

module Control.Concurrent.Async.Refresh
  ( AsyncRefreshConf
  , AsyncRefresh
  , AsyncRefreshCallback
  , RefreshResult(..)
  , defaultAsyncRefreshInterval
  , newAsyncRefreshConf
  , asyncRefreshConfSetDefaultInterval
  , asyncRefreshConfSetLabel
  , asyncRefreshConfSetFactor
  , asyncRefreshConfSetCallback
  , newAsyncRefresh
  , asyncRefreshAsync
  ) where

import qualified Control.Concurrent.Async.Refresh.Lenses  as Lens
import           Control.Concurrent.Async.Refresh.Prelude
import           Control.Concurrent.Async.Refresh.Types
import           Control.Concurrent.Async.Refresh.Util
import           Control.Monad.IO.Unlift
import           Lens.Micro

-- | Given a refresh action, create a new configuration.
newAsyncRefreshConf :: MonadIO m => m (RefreshResult a) -> AsyncRefreshConf m a
newAsyncRefreshConf action =
  AsyncRefreshConf { _asyncRefreshConfDefaultInterval = defaultAsyncRefreshInterval
                   , _asyncRefreshConfAction          = action
                   , _asyncRefreshConfCallback        = defaultAsyncRefreshCallback
                   , _asyncRefreshConfLabel           = defaultAsyncRefreshLabel
                   , _asyncRefreshConfFactor          = defaultAsyncRefreshFactor }

-- | Default refresh interval is one minute (in milliseconds).
defaultAsyncRefreshInterval :: Int
defaultAsyncRefreshInterval = 60 * 10^3

-- | Default refresh factor. See documentation for 'asyncRefreshConfSetFactor'.
defaultAsyncRefreshFactor :: Double
defaultAsyncRefreshFactor = 0.8

-- | The default refresh callback is a no-op.
defaultAsyncRefreshCallback :: Monad m => Either SomeException (RefreshResult a) -> m ()
defaultAsyncRefreshCallback _ = return ()

-- | The default label for configuration is 'Nothing'.
defaultAsyncRefreshLabel :: Maybe Text
defaultAsyncRefreshLabel = Nothing

-- | Set default refresh interval, specified in milliseconds, in the
-- given configuration. If a refresh action fails or does not produce
-- an expiry time, this interval will be used.
asyncRefreshConfSetDefaultInterval :: Int
                                   -> AsyncRefreshConf m a
                                   -> AsyncRefreshConf m a
asyncRefreshConfSetDefaultInterval = (Lens.defaultInterval .~)

-- | Set the label in the provided configuration. This is a human
-- readable text, used for logging purposes.
asyncRefreshConfSetLabel :: Text
                         -> AsyncRefreshConf m a
                         -> AsyncRefreshConf m a
asyncRefreshConfSetLabel label = Lens.label .~ Just label

-- | Extract a human readable label from the provided configuration.
asyncRefreshConfGetLabel :: AsyncRefreshConf m a -> Text
asyncRefreshConfGetLabel conf = fromMaybe "Nothing" (conf ^. Lens.label)

-- | Set the refresh factor. When a refresh gives an explicit expiry
-- time after a succesful refresh run, then this expiry time will be
-- multiplied by this factor, yielding the effective expiry time after
-- which a new refresh run will be scheduled.
asyncRefreshConfSetFactor :: Double
                          -> AsyncRefreshConf m a
                          -> AsyncRefreshConf m a
asyncRefreshConfSetFactor factor = Lens.factor .~ restrictToInterval 0 1 factor

-- | Set a refresh callback for the provided configuration. This
-- callback will be called after a refresh run.
asyncRefreshConfSetCallback :: AsyncRefreshCallback m a
                            -> AsyncRefreshConf m a
                            -> AsyncRefreshConf m a
asyncRefreshConfSetCallback = (Lens.callback .~)

-- | Start a new thread taking care of refreshing of data according to
-- the given configuration.
newAsyncRefresh :: ( MonadUnliftIO m
                   , MonadCatch m
                   , MonadMask m
                   , MonadLogger m )
                => AsyncRefreshConf m a
                -> m AsyncRefresh
newAsyncRefresh conf = AsyncRefresh <$> async (asyncRefreshCtrlThread conf)

-- | Main function of the refresh control thread. Acts as a simple
-- watchdog for the thread defined by 'asyncRefreshThread' doing the
-- actual work.
asyncRefreshCtrlThread :: ( MonadUnliftIO m
                          , MonadCatch m
                          , MonadMask m
                          , MonadLogger m )
                       => AsyncRefreshConf m a
                       -> m ()
asyncRefreshCtrlThread conf = do
  withAsync (asyncRefreshThread conf) $ \asyncHandle -> wait asyncHandle
  logErrorN "Unexpected termination of async refresh thread"

-- | Main function for the thread implementing the refreshing logic.
asyncRefreshThread :: ( MonadUnliftIO m
                      , MonadCatch m
                      , MonadLogger m )
                   => AsyncRefreshConf m a -> m ()
asyncRefreshThread conf = forever $
  tryAny (asyncRefreshDo conf) >>= \case
    Right res -> do
      let delay = fromMaybe (conf ^. Lens.defaultInterval) (refreshExpiry res)
      logDebugN $
        sformat ("Refreshing done for refreshing request '" % stext % "'")
                (asyncRefreshConfGetLabel conf)
      threadDelay (computeRefreshTime conf delay * 10^3)
    Left  exn -> do
      logErrorN $
        sformat ("Refresh action failed for token request '" % stext % "': " % stext)
                (asyncRefreshConfGetLabel conf) (tshow exn)
      threadDelay (conf ^. Lens.defaultInterval * 10^3)

-- | This function does the actual refreshing work.
asyncRefreshDo :: ( MonadUnliftIO m
                  , MonadCatch m
                  , MonadLogger m )
               => AsyncRefreshConf m a -> m (RefreshResult a)
asyncRefreshDo conf = do
  tryA <- tryAny (conf ^. Lens.action)
    `logOnError` sformat ("Failed to execute refresh action for token request '"
                          % stext % "'") (asyncRefreshConfGetLabel conf)
  void $ tryAny ((conf ^. Lens.callback) tryA)
    `logOnError` "User provided callback threw exception"
  either throw return tryA

-- | Scale the given duration according to the factor specified in the
-- configuration.
computeRefreshTime :: AsyncRefreshConf m a -> Int -> Int
computeRefreshTime conf duration =
  floor $ (conf ^. Lens.factor) * fromIntegral duration
