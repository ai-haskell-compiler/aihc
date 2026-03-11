-- | For full documentation please refer to "Control.Concurrent.MVar".
module Control.Concurrent.MVar.Strict
  ( MVar'

    -- * Operations
  , newEmptyMVar'
  , newMVar'
  , takeMVar'
  , putMVar'
  , readMVar'
  , swapMVar'
  , tryTakeMVar'
  , tryPutMVar'
  , tryReadMVar'
  , isEmptyMVar'
  , withMVar'
  , withMVar'Masked
  , modifyMVar'_
  , modifyMVar'
  , modifyMVar'Masked_
  , modifyMVar'Masked
  , mkWeakMVar'
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.MVar (MVar(..))
import GHC.Weak (Weak(..))
import qualified Control.Concurrent.MVar as Base

-- | Strict (WHNF) version of 'MVar'.
newtype MVar' a = MVar' (MVar a)
  deriving (Eq, NFData, NFData1)

-- | 'Base.newEmptyMVar' for an 'MVar''.
newEmptyMVar' :: IO (MVar' a)
newEmptyMVar' = MVar' <$> Base.newEmptyMVar

-- | 'Base.newMVar' for an 'MVar''.
--
-- Evaluates the initial value to WHNF.
newMVar' :: a -> IO (MVar' a)
newMVar' a = fmap MVar' . Base.newMVar =<< evaluate a

-- | 'Base.takeMVar' for an 'MVar''.
takeMVar' :: MVar' a -> IO a
takeMVar' (MVar' var) = Base.takeMVar var

-- | 'Base.putMVar' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
putMVar' :: MVar' a -> a -> IO ()
putMVar' (MVar' var) a = Base.putMVar var =<< evaluate a

-- | 'Base.readMVar' for an 'MVar''.
readMVar' :: MVar' a -> IO a
readMVar' (MVar' var) = Base.readMVar var

-- | 'Base.swapMVar' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
swapMVar' :: MVar' a -> a -> IO a
swapMVar' (MVar' var) a = Base.swapMVar var =<< evaluate a

-- | 'Base.tryTakeMVar' for an 'MVar''.
tryTakeMVar' :: MVar' a -> IO (Maybe a)
tryTakeMVar' (MVar' var) = Base.tryTakeMVar var

-- | 'Base.tryPutMVar' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
tryPutMVar' :: MVar' a -> a -> IO Bool
tryPutMVar' (MVar' var) a = Base.tryPutMVar var =<< evaluate a

-- | 'Base.tryReadMVar' for an 'MVar''.
tryReadMVar' :: MVar' a -> IO (Maybe a)
tryReadMVar' (MVar' var) = Base.tryReadMVar var

-- | 'Base.isEmptyMVar' for an 'MVar''.
isEmptyMVar' :: MVar' a -> IO Bool
isEmptyMVar' (MVar' var) = Base.isEmptyMVar var

-- | 'Base.withMVar' for an 'MVar''.
withMVar' :: MVar' a -> (a -> IO b) -> IO b
withMVar' (MVar' var) action = Base.withMVar var action
{-# INLINE withMVar' #-}

-- | 'Base.withMVarMasked' for an 'MVar''.
withMVar'Masked :: MVar' a -> (a -> IO b) -> IO b
withMVar'Masked (MVar' var) action = Base.withMVarMasked var action
{-# INLINE withMVar'Masked #-}

-- | 'Base.modifyMVar_' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
modifyMVar'_ :: MVar' a -> (a -> IO a) -> IO ()
modifyMVar'_ (MVar' var) action = Base.modifyMVar_ var $ \a0 -> do
  a <- action a0
  evaluate a
{-# INLINE modifyMVar'_ #-}

-- | 'Base.modifyMVar' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
modifyMVar' :: MVar' a -> (a -> IO (a, b)) -> IO b
modifyMVar' (MVar' var) action = Base.modifyMVar var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> evaluate a
{-# INLINE modifyMVar' #-}

-- | 'Base.modifyMVarMasked_' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
modifyMVar'Masked_ :: MVar' a -> (a -> IO a) -> IO ()
modifyMVar'Masked_ (MVar' var) action = Base.modifyMVarMasked_ var $ \a0 -> do
  a <- action a0
  evaluate a
{-# INLINE modifyMVar'Masked_ #-}

-- | 'Base.modifyMVarMasked' for an 'MVar''.
--
-- Evaluates the new value to WHNF.
modifyMVar'Masked :: MVar' a -> (a -> IO (a, b)) -> IO b
modifyMVar'Masked (MVar' var) action = Base.modifyMVarMasked var $ \a0 -> do
  (a, b) <- action a0
  (, b) <$> evaluate a
{-# INLINE modifyMVar'Masked #-}

-- | 'Base.mkWeakMVar' for an 'MVar''.
mkWeakMVar' :: MVar' a -> IO () -> IO (Weak (MVar' a))
mkWeakMVar' var@(MVar' (MVar var#)) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
