module Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
    modifyIORef,
    modifyIORef',
    atomicModifyIORef,
    atomicModifyIORef',
    atomicWriteIORef,
  )
where

import GHC.IO (IO)
import GHC.IORef (IORef, atomicModifyIORef', atomicModifyIORef2, atomicSwapIORef, newIORef, readIORef, writeIORef)
import Prelude (return, seq, (>>=))

-- | Mutate the contents of an 'IORef' without forcing the new value.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef reference transform =
  readIORef reference >>= \value -> writeIORef reference (transform value)

-- | Mutate the contents of an 'IORef', forcing the new value to weak head
-- normal form before storing it.
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' reference transform =
  readIORef reference >>= \value ->
    let updated = transform value
     in updated `seq` writeIORef reference updated

-- | Atomically replace the contents of an 'IORef' and return an auxiliary
-- result. The new value and result remain lazy.
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef reference transform =
  atomicModifyIORef2 reference transform >>= \(_old, inner) ->
    case inner of
      (_new, extra) -> return extra

-- | Atomically write a new value without forcing it.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef reference value =
  atomicSwapIORef reference value >>= \_old -> return ()
