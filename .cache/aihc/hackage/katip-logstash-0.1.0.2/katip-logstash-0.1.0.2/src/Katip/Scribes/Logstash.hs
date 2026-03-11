--------------------------------------------------------------------------------
-- Logstash backend for katip                                                 --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

-- | This module implements convenience functions for using the `Logstash`
-- module with the katip logging framework.
module Katip.Scribes.Logstash (
    withLogstashScribe
) where 

--------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad.Trans.Reader
import Control.Retry

import UnliftIO (MonadUnliftIO)
import Katip
import Logstash

--------------------------------------------------------------------------------

-- | `withLogstashScribe` @cfg permitFunc toItem codec handlers cont@ is like
-- `withLogstashQueue` except that a `Scribe` is passed to @cont@ instead of
-- the raw queue. The @toItem@ function is required to convert `Item` values
-- to whatever input @codec@ expects. A reasonable default might be `toObject`
-- or a wrapper around the `LogItem` type class. 
withLogstashScribe 
    :: (LogstashContext ctx, MonadUnliftIO m)
    => LogstashQueueCfg ctx -- ^ The configuration for the Logstash client.
    -> PermitFunc -- ^ A function which determines which entries to include.
    -> (forall a. LogItem a => Item a -> item) 
    -> (RetryStatus -> item -> ReaderT LogstashConnection IO ())
    -> [item -> Handler ()] -- ^ Exception handlers.
    -> (Scribe -> m r) -- ^ A continuation which the `Scribe` is given to.
    -> m r
withLogstashScribe cfg permit f codec hs cont = 
    withLogstashQueue cfg codec hs $ \queue -> cont $ Scribe{
        liPush = atomically . writeTBMQueue queue . f,
        -- we set the finaliser to close the queue, even though this handled
        -- by `withLogstashQueue` already since it is possible to close a
        -- given `Scribe` long before returning from `cont`.
        scribeFinalizer = atomically $ closeTBMQueue queue,
        scribePermitItem = permit
    }

--------------------------------------------------------------------------------
