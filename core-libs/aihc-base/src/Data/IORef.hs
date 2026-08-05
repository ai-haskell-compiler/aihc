{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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

import GHC.IO (IO (..))
import GHC.IORef (IORef (..), newIORef, readIORef, writeIORef)
import GHC.Prim (MutVar#, RealWorld, State#, casMutVar#, readMutVar#)
import GHC.STRef (STRef (..))
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
atomicModifyIORef (IORef (STRef reference)) transform =
  IO (retryAtomicModify reference transform)

retryAtomicModify :: MutVar# RealWorld a -> (a -> (a, b)) -> State# RealWorld -> (# State# RealWorld, b #)
retryAtomicModify reference transform state =
  case readMutVar# reference state of
    (# readState, old #) ->
      retryAtomicModifyExpected reference transform old readState

retryAtomicModifyExpected :: MutVar# RealWorld a -> (a -> (a, b)) -> a -> State# RealWorld -> (# State# RealWorld, b #)
retryAtomicModifyExpected reference transform old state =
  case transform old of
    (new, result) ->
      case casMutVar# reference old new state of
        (# nextState, failed, current #) ->
          case failed of
            0# -> (# nextState, result #)
            _ -> retryAtomicModifyExpected reference transform current nextState

-- | A strict version of 'atomicModifyIORef'. The new value is forced before
-- it can be installed, while the returned value is forced after installation.
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' reference transform =
  atomicModifyIORef reference strictTransform >>= \result ->
    result `seq` return result
  where
    strictTransform old =
      case transform old of
        (new, result) -> new `seq` (new, result)

-- | Atomically write a new value without forcing it.
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef (IORef (STRef reference)) value =
  IO (retryAtomicWrite reference value)

retryAtomicWrite :: MutVar# RealWorld a -> a -> State# RealWorld -> (# State# RealWorld, () #)
retryAtomicWrite reference value state =
  case readMutVar# reference state of
    (# readState, old #) ->
      retryAtomicWriteExpected reference value old readState

retryAtomicWriteExpected :: MutVar# RealWorld a -> a -> a -> State# RealWorld -> (# State# RealWorld, () #)
retryAtomicWriteExpected reference value old state =
  case casMutVar# reference old value state of
    (# nextState, failed, current #) ->
      case failed of
        0# -> (# nextState, () #)
        _ -> retryAtomicWriteExpected reference value current nextState
