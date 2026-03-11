module Control.Concurrent.STM.TMVar.Extras where

import Control.Concurrent.STM

-- | Non-blocking swap of TMVar irreguardless if it previously contained a value or not.
-- Returns what was in the TMVar (if exists)
forceSwapTMVar :: TMVar a -> a -> STM (Maybe a)
forceSwapTMVar v a = (Just <$> swapTMVar v a) `orElse` (const Nothing <$> putTMVar v a)

-- | Block until TMVar is empty.
-- The argument a is used to try to put into TMVar, but is taken out again in the same
-- transaction
waitTillEmptyTMVar :: TMVar a -> a -> STM ()
waitTillEmptyTMVar v a = putTMVar v a >> takeTMVar v >> pure ()

-- | Block until TMVar is full.
waitTillFullTMVar :: TMVar a -> STM ()
waitTillFullTMVar v = takeTMVar v >>= putTMVar v
