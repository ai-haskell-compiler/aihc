{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IORef
  ( IORef (..),
    newIORef,
    readIORef,
    writeIORef,
    atomicModifyIORef2Lazy,
    atomicModifyIORef2,
    atomicModifyIORefLazy_,
    atomicModifyIORef'_,
    atomicSwapIORef,
    atomicModifyIORef',
  )
where

import Data.Kind (Type)
import GHC.IO (IO (..))
import GHC.Internal.Classes (Eq (..))
import GHC.Prim (MutVar#, RealWorld, State#, casMutVar#, newMutVar#, readMutVar#, seq, writeMutVar#)
import GHC.STRef (STRef (..))

-- | A mutable variable in the 'IO' monad. The representation deliberately
-- shares the 'STRef' boundary used by @base@.
newtype IORef (a :: Type) = IORef (STRef RealWorld a)
  deriving newtype (Eq)

-- | Build a new 'IORef'.
newIORef :: a -> IO (IORef a)
newIORef initial =
  IO
    ( \state ->
        case newMutVar# initial state of
          (# nextState, reference #) -> (# nextState, IORef (STRef reference) #)
    )

-- | Read the value of an 'IORef'.
readIORef :: IORef a -> IO a
readIORef (IORef (STRef reference)) = IO (readMutVar# reference)

-- | Write a new value into an 'IORef' without forcing it.
writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef (STRef reference)) value =
  IO
    ( \state ->
        case writeMutVar# reference value state of
          nextState -> (# nextState, () #)
    )

-- | Atomically apply a function to the contents of an 'IORef', returning the
-- old value paired with the pair the function produced. Nothing is forced:
-- the installed value is a thunk that selects the first component, exactly as
-- GHC\'s @atomicModifyMutVar2#@ arranges.
--
-- GHC implements this with a dedicated primop; here the atomicity comes from
-- a compare-and-swap retry loop, which keeps the same observable behaviour.
atomicModifyIORef2Lazy :: IORef a -> (a -> (a, b)) -> IO (a, (a, b))
atomicModifyIORef2Lazy (IORef (STRef reference)) transform =
  IO (retryModify2 reference transform)

retryModify2 ::
  MutVar# RealWorld a ->
  (a -> (a, b)) ->
  State# RealWorld ->
  (# State# RealWorld, (a, (a, b)) #)
retryModify2 reference transform state =
  case readMutVar# reference state of
    (# readState, old #) -> retryModify2Expected reference transform old readState

retryModify2Expected ::
  MutVar# RealWorld a ->
  (a -> (a, b)) ->
  a ->
  State# RealWorld ->
  (# State# RealWorld, (a, (a, b)) #)
retryModify2Expected reference transform old state =
  let pair = transform old
      new = case pair of (next, _) -> next
   in case casMutVar# reference old new state of
        (# nextState, failed, current #) ->
          case failed of
            0# -> (# nextState, (old, pair) #)
            _ -> retryModify2Expected reference transform current nextState

-- | As 'atomicModifyIORef2Lazy', but the two pairs are forced to weak head
-- normal form before the result is returned. Their components stay lazy.
atomicModifyIORef2 :: IORef a -> (a -> (a, b)) -> IO (a, (a, b))
atomicModifyIORef2 (IORef (STRef reference)) transform =
  IO
    ( \state ->
        case retryModify2 reference transform state of
          (# nextState, result #) ->
            case result of
              (_old, inner) ->
                case inner of
                  (_new, _extra) -> (# nextState, result #)
    )

-- | Atomically apply a function to the contents of an 'IORef' without
-- forcing anything, returning the old and the new value.
atomicModifyIORefLazy_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORefLazy_ reference transform =
  IO
    ( \state ->
        case retryModify2 (unwrapIORef reference) (\old -> (transform old, ())) state of
          (# nextState, result #) ->
            case result of
              (old, inner) -> (# nextState, (old, case inner of (new, _) -> new) #)
    )

-- | As 'atomicModifyIORefLazy_', but the new value is forced to weak head
-- normal form before it becomes visible.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORef'_ reference transform =
  IO
    ( \state ->
        case retryModify2 (unwrapIORef reference) strictTransform state of
          (# nextState, result #) ->
            case result of
              (old, inner) ->
                case inner of
                  (new, _) -> (# nextState, (old, new) #)
    )
  where
    strictTransform old =
      let new = transform old
       in new `seq` (new, ())

-- | Atomically replace the contents of an 'IORef', returning the old value.
atomicSwapIORef :: IORef a -> a -> IO a
atomicSwapIORef (IORef (STRef reference)) value =
  IO (retrySwap reference value)

retrySwap :: MutVar# RealWorld a -> a -> State# RealWorld -> (# State# RealWorld, a #)
retrySwap reference value state =
  case readMutVar# reference state of
    (# readState, old #) -> retrySwapExpected reference value old readState

retrySwapExpected :: MutVar# RealWorld a -> a -> a -> State# RealWorld -> (# State# RealWorld, a #)
retrySwapExpected reference value old state =
  case casMutVar# reference old value state of
    (# nextState, failed, current #) ->
      case failed of
        0# -> (# nextState, old #)
        _ -> retrySwapExpected reference value current nextState

-- | A strict version of @atomicModifyIORef@: the new value is forced before
-- it is installed and the auxiliary result is forced before it is returned.
atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' reference transform =
  IO
    ( \state ->
        case retryModify2 (unwrapIORef reference) strictTransform state of
          (# nextState, result #) ->
            case result of
              (_old, inner) ->
                case inner of
                  (_new, extra) -> extra `seq` (# nextState, extra #)
    )
  where
    strictTransform old =
      case transform old of
        (new, extra) -> new `seq` (new, extra)

unwrapIORef :: IORef a -> MutVar# RealWorld a
unwrapIORef (IORef (STRef reference)) = reference
