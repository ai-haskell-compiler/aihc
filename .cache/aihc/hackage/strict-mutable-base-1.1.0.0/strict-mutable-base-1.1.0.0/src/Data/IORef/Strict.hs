-- | For full documentation please refer to "Data.IORef".
module Data.IORef.Strict
  ( IORef'

    -- * Operations
  , newIORef'
  , readIORef'
  , writeIORef'
  , modifyIORef'
  , atomicModifyIORef'
  , atomicWriteIORef'
  , mkWeakIORef'
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Exts (mkWeak#)
import GHC.IO (IO(..))
import GHC.IORef (IORef(..))
import GHC.STRef (STRef(..))
import GHC.Weak (Weak(..))
import qualified Data.IORef as Base

-- | A strict (WHNF) variant of 'IORef'.
newtype IORef' a = IORef' (Base.IORef a)
  deriving (Eq, NFData, NFData1)

-- | 'Base.newIORef' for 'IORef''.
--
-- Evaluates the initial value to WHNF.
newIORef' :: a -> IO (IORef' a)
newIORef' a = fmap IORef' . Base.newIORef =<< evaluate a

-- | 'Base.readIORef' for 'IORef''.
readIORef' :: IORef' a -> IO a
readIORef' (IORef' var) = Base.readIORef var

-- | 'Base.writeIORef' for 'IORef''.
--
-- Evaluates the new value to WHNF.
writeIORef' :: IORef' a -> a -> IO ()
writeIORef' (IORef' var) a = Base.writeIORef var =<< evaluate a

-- | 'Base.modifyIORef' for 'IORef''.
modifyIORef' :: IORef' a -> (a -> a) -> IO ()
modifyIORef' (IORef' var) f = Base.modifyIORef' var f

-- | 'Base.atomicModifyIORef' for 'IORef''.
--
-- Evaluates the new value to WHNF.
atomicModifyIORef' :: IORef' a -> (a -> (a, b)) -> IO b
atomicModifyIORef' (IORef' var) f = Base.atomicModifyIORef' var f

-- | 'Base.atomicWriteIORef' for 'IORef''.
--
-- Evaluates the new value to WHNF.
atomicWriteIORef' :: IORef' a -> a -> IO ()
atomicWriteIORef' (IORef' var) a = Base.atomicWriteIORef var =<< evaluate a

-- | 'Base.mkWeakIORef' for 'IORef''.
mkWeakIORef' :: IORef' a -> IO () -> IO (Weak (IORef' a))
mkWeakIORef' var@(IORef' (IORef (STRef var#))) (IO finalizer) = IO $ \s0 ->
  case mkWeak# var# var finalizer s0 of
    (# s1, w #) -> (# s1, Weak w #)
