{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Raehik.Compat.Data.Primitive.Types
  ( Prim'(..)
  , P.Prim(..)
  , P.sizeOf
  ) where

import Data.Primitive.Types qualified as P
import Raehik.Compat.GHC.Exts.GHC910UnalignedAddrPrimops
import GHC.Exts
import GHC.Word
import GHC.Int
import Numeric.Natural

-- | 'P.Prim' extension class providing unaligned accesses
--
-- hoping to get this merged in https://github.com/haskell/primitive/issues/409
--
-- (also includes Addr# primops which that issue/PR may not)
--
-- Also includes an associated type for size in bytes. Another thing that maybe
-- primitive could provide. (Wouldn't be hard!)
class P.Prim a => Prim' a where
    type SizeOf a :: Natural
    -- | Read a value from the array. The offset is in bytes.
    indexWord8ByteArrayAs# :: ByteArray# -> Int# -> a

    readWord8ByteArrayAs#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    writeWord8ByteArrayAs# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

    indexWord8OffAddrAs# :: Addr# -> Int# -> a

    readWord8OffAddrAs#  :: Addr# -> Int# -> State# s -> (# State# s, a #)
    writeWord8OffAddrAs# :: Addr# -> Int# -> a -> State# s -> State# s

instance Prim' Word8 where
    type SizeOf Word8 = SIZEOF_WORD8
    indexWord8ByteArrayAs# = P.indexByteArray#
    readWord8ByteArrayAs#  = P.readByteArray#
    writeWord8ByteArrayAs# = P.writeByteArray#
    indexWord8OffAddrAs#   = P.indexOffAddr#
    readWord8OffAddrAs#    = P.readOffAddr#
    writeWord8OffAddrAs#   = P.writeOffAddr#

instance Prim' Word16 where
    type SizeOf Word16 = SIZEOF_WORD16
    indexWord8ByteArrayAs# arr# os# = W16# (indexWord8ArrayAsWord16# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsWord16# arr# os# s0 of
          (# s1, w# #) -> (# s1, W16# w# #)
    writeWord8ByteArrayAs# arr# os# (W16# w#) = \s0 ->
        writeWord8ArrayAsWord16# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = W16# (indexWord8OffAddrAsWord16# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsWord16# addr# os# s0 of
          (# s1, w# #) -> (# s1, W16# w# #)
    writeWord8OffAddrAs# addr# os# (W16# w#) s# =
        writeWord8OffAddrAsWord16# addr# os# w# s#

instance Prim' Word32 where
    type SizeOf Word32 = SIZEOF_WORD32
    indexWord8ByteArrayAs# arr# os# = W32# (indexWord8ArrayAsWord32# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsWord32# arr# os# s0 of
          (# s1, w# #) -> (# s1, W32# w# #)
    writeWord8ByteArrayAs# arr# os# (W32# w#) = \s0 ->
        writeWord8ArrayAsWord32# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = W32# (indexWord8OffAddrAsWord32# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsWord32# addr# os# s0 of
          (# s1, w# #) -> (# s1, W32# w# #)
    writeWord8OffAddrAs# addr# os# (W32# w#) s# =
        writeWord8OffAddrAsWord32# addr# os# w# s#

instance Prim' Word64 where
    type SizeOf Word64 = SIZEOF_WORD64
    indexWord8ByteArrayAs# arr# os# = W64# (indexWord8ArrayAsWord64# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsWord64# arr# os# s0 of
          (# s1, w# #) -> (# s1, W64# w# #)
    writeWord8ByteArrayAs# arr# os# (W64# w#) = \s0 ->
        writeWord8ArrayAsWord64# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = W64# (indexWord8OffAddrAsWord64# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsWord64# addr# os# s0 of
          (# s1, w# #) -> (# s1, W64# w# #)
    writeWord8OffAddrAs# addr# os# (W64# w#) s# =
        writeWord8OffAddrAsWord64# addr# os# w# s#

instance Prim' Int8 where
    type SizeOf Int8 = SIZEOF_INT8
    indexWord8ByteArrayAs# = P.indexByteArray#
    readWord8ByteArrayAs#  = P.readByteArray#
    writeWord8ByteArrayAs# = P.writeByteArray#
    indexWord8OffAddrAs#   = P.indexOffAddr#
    readWord8OffAddrAs#    = P.readOffAddr#
    writeWord8OffAddrAs#   = P.writeOffAddr#

instance Prim' Int16 where
    type SizeOf Int16 = SIZEOF_INT16
    indexWord8ByteArrayAs# arr# os# = I16# (indexWord8ArrayAsInt16# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsInt16# arr# os# s0 of
          (# s1, w# #) -> (# s1, I16# w# #)
    writeWord8ByteArrayAs# arr# os# (I16# w#) = \s0 ->
        writeWord8ArrayAsInt16# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = I16# (indexWord8OffAddrAsInt16# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsInt16# addr# os# s0 of
          (# s1, w# #) -> (# s1, I16# w# #)
    writeWord8OffAddrAs# addr# os# (I16# w#) s# =
        writeWord8OffAddrAsInt16# addr# os# w# s#

instance Prim' Int32 where
    type SizeOf Int32 = SIZEOF_INT32
    indexWord8ByteArrayAs# arr# os# = I32# (indexWord8ArrayAsInt32# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsInt32# arr# os# s0 of
          (# s1, w# #) -> (# s1, I32# w# #)
    writeWord8ByteArrayAs# arr# os# (I32# w#) = \s0 ->
        writeWord8ArrayAsInt32# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = I32# (indexWord8OffAddrAsInt32# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsInt32# addr# os# s0 of
          (# s1, w# #) -> (# s1, I32# w# #)
    writeWord8OffAddrAs# addr# os# (I32# w#) s# =
        writeWord8OffAddrAsInt32# addr# os# w# s#

instance Prim' Int64 where
    type SizeOf Int64 = SIZEOF_INT64
    indexWord8ByteArrayAs# arr# os# = I64# (indexWord8ArrayAsInt64# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsInt64# arr# os# s0 of
          (# s1, w# #) -> (# s1, I64# w# #)
    writeWord8ByteArrayAs# arr# os# (I64# w#) = \s0 ->
        writeWord8ArrayAsInt64# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = I64# (indexWord8OffAddrAsInt64# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsInt64# addr# os# s0 of
          (# s1, w# #) -> (# s1, I64# w# #)
    writeWord8OffAddrAs# addr# os# (I64# w#) s# =
        writeWord8OffAddrAsInt64# addr# os# w# s#

instance Prim' Word where
    type SizeOf Word = SIZEOF_HSWORD
    indexWord8ByteArrayAs# arr# os# = W# (indexWord8ArrayAsWord# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsWord# arr# os# s0 of
          (# s1, w# #) -> (# s1, W# w# #)
    writeWord8ByteArrayAs# arr# os# (W# w#) = \s0 ->
        writeWord8ArrayAsWord# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = W# (indexWord8OffAddrAsWord# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsWord# addr# os# s0 of
          (# s1, w# #) -> (# s1, W# w# #)
    writeWord8OffAddrAs# addr# os# (W# w#) s# =
        writeWord8OffAddrAsWord# addr# os# w# s#

instance Prim' Int where
    type SizeOf Int = SIZEOF_HSINT
    indexWord8ByteArrayAs# arr# os# = I# (indexWord8ArrayAsInt# arr# os#)
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ArrayAsInt# arr# os# s0 of
          (# s1, w# #) -> (# s1, I# w# #)
    writeWord8ByteArrayAs# arr# os# (I# w#) = \s0 ->
        writeWord8ArrayAsInt# arr# os# w# s0
    indexWord8OffAddrAs# addr# os# = I# (indexWord8OffAddrAsInt# addr# os#)
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAsInt# addr# os# s0 of
          (# s1, w# #) -> (# s1, I# w# #)
    writeWord8OffAddrAs# addr# os# (I# w#) s# =
        writeWord8OffAddrAsInt# addr# os# w# s#
