{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2020 Kowainik, (c) 2020 Alexander Vershilov
SPDX-License-Identifier: MPL-2.0
Maintainer: Alexander Vershilov <alexander.vershilov@gmail.com>

This is internal module, use it on your own risk. The implementation here may be
changed without a version bump.
-}

module Colog.Concurrent.Internal
       ( BackgroundWorker (..)
       , Capacity (..)
       , mkCapacity
       ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (STM, TVar)
import Numeric.Natural (Natural)


{- | A wrapper type that carries capacity. The internal type may be
differrent for the different GHC versions.
-}
#if MIN_VERSION_stm(2,5,0)
data Capacity = Capacity Natural (Maybe Natural)
  deriving stock Show
#else
data Capacity = Capacity Int (Maybe Natural)
  deriving stock Show
#endif

-- | Creates new capacity.
--
-- @since 0.5.0.0
mkCapacity
  :: Natural -- ^ Size of the queue. Number of the messages in flight
  -> Maybe Natural -- ^ Maximum number of messages that logger can read in a chunk.
  -> Capacity
mkCapacity n = Capacity (mk n)  where
#if MIN_VERSION_stm(2,5,0)
  mk = id
#else
  mk = fromIntegral
#endif

{- | Wrapper for the background thread that may receive messages to
process.
-}
data BackgroundWorker msg = BackgroundWorker
    { backgroundWorkerThreadId :: !ThreadId
      -- ^ Background 'ThreadId'.
    , backgroundWorkerWrite    :: msg -> STM ()
      -- ^ Method for communication with the thread.
    , backgroundWorkerIsAlive  :: TVar Bool
    }
