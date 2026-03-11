{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}
{-# language KindSignatures #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language UnliftedNewtypes #-}
{-# language RoleAnnotations #-}
{-# language DataKinds #-}

module Data.Primitive.Unlifted.MVar.Primops
  ( UnliftedMVar#
  , newUnliftedMVar#
  , takeUnliftedMVar#
  , tryTakeUnliftedMVar#
  , putUnliftedMVar#
  , tryPutUnliftedMVar#
  , readUnliftedMVar#
  , tryReadUnliftedMVar#
  , sameUnliftedMVar#
  , isEmptyUnliftedMVar#
  ) where

import GHC.Exts (MVar#, State#, Int#, newMVar#, takeMVar#, tryTakeMVar#, putMVar#, tryPutMVar#, readMVar#, tryReadMVar#, reallyUnsafePtrEquality#, isEmptyMVar#)

import Data.Primitive.Unlifted.Type

newtype UnliftedMVar# s (a :: UnliftedType) = UnliftedMVar# (MVar# s a)
type role UnliftedMVar# nominal representational

newUnliftedMVar# :: State# s -> (# State# s, UnliftedMVar# s a #)
{-# INLINE newUnliftedMVar# #-}
newUnliftedMVar# s = case newMVar# s of
  (# s', mv #) -> (# s', UnliftedMVar# mv #)

takeUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, a #)
{-# INLINE takeUnliftedMVar# #-}
takeUnliftedMVar# (UnliftedMVar# mv) s = takeMVar# mv s

tryTakeUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, (# (##) | a #) #)
{-# INLINE tryTakeUnliftedMVar# #-}
tryTakeUnliftedMVar# (UnliftedMVar# mv) s =
  case tryTakeMVar# mv s of
    (# s', 0#, _ #) -> (# s', (#(##)| #)#)
    (# s', _, a #) -> (# s', (#|a #) #)

putUnliftedMVar# :: UnliftedMVar# s a -> a -> State# s -> State# s
{-# INLINE putUnliftedMVar# #-}
putUnliftedMVar# (UnliftedMVar# mv) a s = putMVar# mv a s

tryPutUnliftedMVar# :: UnliftedMVar# s a -> a -> State# s -> (# State# s, Int# #)
{-# INLINE tryPutUnliftedMVar# #-}
tryPutUnliftedMVar# (UnliftedMVar# mv) a s = tryPutMVar# mv a s

readUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, a #)
{-# INLINE readUnliftedMVar# #-}
readUnliftedMVar# (UnliftedMVar# mv) s = readMVar# mv s

tryReadUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, (# (##) | a #) #)
{-# INLINE tryReadUnliftedMVar# #-}
tryReadUnliftedMVar# (UnliftedMVar# mv) s =
  case tryReadMVar# mv s of
    (# s', 0#, _ #) -> (# s', (#(##)| #)#)
    (# s', _, a #) -> (# s', (#|a #) #)

sameUnliftedMVar# :: UnliftedMVar# s a -> UnliftedMVar# s a -> Int#
{-# INLINE sameUnliftedMVar# #-}
sameUnliftedMVar# (UnliftedMVar# mv1) (UnliftedMVar# mv2)
  = reallyUnsafePtrEquality# mv1 mv2

isEmptyUnliftedMVar# :: UnliftedMVar# s a -> State# s -> (# State# s, Int# #)
{-# INLINE isEmptyUnliftedMVar# #-}
isEmptyUnliftedMVar# (UnliftedMVar# mv) s
  = isEmptyMVar# mv s
