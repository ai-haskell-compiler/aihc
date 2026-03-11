{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Op
  ( writeCharArray#
  , copyByteArray#
  , copyMutableByteArray#
  ) where

import GHC.Exts (ByteArray#, Char#, Int#, MutableByteArray#, State#, (<#), (>=#))
import qualified GHC.Exts as Exts
import GHC.Int (Int (I#))

writeCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeCharArray# arr i v st = case i <# 0# of
  1# -> error ("writeCharArray#: negative index " ++ show (I# i))
  _ -> case Exts.getSizeofMutableByteArray# arr st of
    (# st', sz #) -> case i >=# sz of
      1# -> error ("writeCharArray#: index " ++ show (I# i) ++ " >= length " ++ show (I# sz))
      _ -> Exts.writeCharArray# arr i v st'

copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyByteArray# src soff dst doff len s0 =
  case Exts.getSizeofMutableByteArray# dst s0 of
    (# s1, sz #)
      | I# soff >= 0
      , I# doff >= 0
      , I# len >= 0
      , I# doff + I# len <= I# sz
      , I# soff + I# len <= I# (Exts.sizeofByteArray# src) ->
          Exts.copyByteArray# src soff dst doff len s1
      | otherwise -> error "copyByteArray#: index range out of bounds"

copyMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableByteArray# src soff dst doff len s0 =
  case Exts.getSizeofMutableByteArray# dst s0 of
    (# s1, szDst #) -> case Exts.getSizeofMutableByteArray# src s1 of
      (# s2, szSrc #)
        | I# soff >= 0
        , I# doff >= 0
        , I# len >= 0
        , I# doff + I# len <= I# szDst
        , I# soff + I# len <= I# szSrc ->
            Exts.copyMutableByteArray# src soff dst doff len s2
        | otherwise -> error "copyMutableByteArray#: index range out of bounds"
