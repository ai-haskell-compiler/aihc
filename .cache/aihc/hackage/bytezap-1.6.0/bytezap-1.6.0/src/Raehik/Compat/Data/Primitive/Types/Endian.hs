-- | I think this should be in primitive.

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}

module Raehik.Compat.Data.Primitive.Types.Endian where

import Raehik.Compat.Data.Primitive.Types
import               Data.Word
import Raehik.Compat.Data.Word.ByteSwap qualified as X
import               Data.Int
import Raehik.Compat.Data.Int.ByteSwap
import GHC.ByteOrder
import Data.Kind
import GHC.Float

-- | Boxed types which permit reversing byte order ("byte swapping").
class ByteSwap a where byteSwap :: a -> a
instance ByteSwap Word16 where byteSwap = byteSwap16
instance ByteSwap Word32 where byteSwap = byteSwap32
instance ByteSwap Word64 where byteSwap = byteSwap64
instance ByteSwap Word   where byteSwap = X.byteSwap
instance ByteSwap  Int16 where byteSwap = byteSwapI16
instance ByteSwap  Int32 where byteSwap = byteSwapI32
instance ByteSwap  Int64 where byteSwap = byteSwapI64
instance ByteSwap  Int   where byteSwap = byteSwapI

-- I think these two are well-founded. No tests currently though.
instance ByteSwap Float  where
    byteSwap = castWord32ToFloat  . byteSwap . castFloatToWord32
instance ByteSwap Double where
    byteSwap = castWord64ToDouble . byteSwap . castDoubleToWord64

newtype ByteOrdered (end :: ByteOrder) a = ByteOrdered
  { unByteOrdered :: a
  } deriving (Ord, Eq, Show, Num, Real, Enum, Integral) via a

-- | Newtype for easier instance derivation.
newtype PrimByteSwapped a = PrimByteSwapped { unPrimByteSwapped :: a }

-- | Prim instance where we byte swap at accesses.
instance (Prim a, ByteSwap a) => Prim (PrimByteSwapped a) where
    sizeOf# (PrimByteSwapped a) = sizeOf# a
    alignment# (PrimByteSwapped a) = alignment# a
    indexByteArray# arr# i# =
        PrimByteSwapped (byteSwap (indexByteArray# arr# i#))
    readByteArray# arr# i# = \s0 ->
        case readByteArray# arr# i# s0 of
          (# s1, a #) -> (# s1, PrimByteSwapped (byteSwap a) #)
    writeByteArray# arr# i# (PrimByteSwapped a) = \s0 ->
        writeByteArray# arr# i# (byteSwap a) s0
    setByteArray# arr# i# len# (PrimByteSwapped a) = \s0 ->
        setByteArray# arr# i# len# (byteSwap a) s0
    indexOffAddr# addr# i# =
        PrimByteSwapped (byteSwap (indexOffAddr# addr# i#))
    readOffAddr# addr# i# = \s0 ->
        case readOffAddr# addr# i# s0 of
          (# s1, a #) -> (# s1, PrimByteSwapped (byteSwap a) #)
    writeOffAddr# addr# i# (PrimByteSwapped a) = \s0 ->
        writeOffAddr# addr# i# (byteSwap a) s0
    setOffAddr# arr# i# len# (PrimByteSwapped a) = \s0 ->
        setOffAddr# arr# i# len# (byteSwap a) s0

instance (Prim' a, ByteSwap a) => Prim' (PrimByteSwapped a) where
    type SizeOf (PrimByteSwapped a) = SizeOf a
    indexWord8ByteArrayAs# arr# os# =
        PrimByteSwapped (byteSwap (indexWord8ByteArrayAs# arr# os#))
    readWord8ByteArrayAs# arr# os# = \s0 ->
        case readWord8ByteArrayAs# arr# os# s0 of
          (# s1, a #) -> (# s1, PrimByteSwapped (byteSwap a) #)
    writeWord8ByteArrayAs# arr# os# (PrimByteSwapped a) = \s0 ->
        writeWord8ByteArrayAs# arr# os# (byteSwap a) s0
    indexWord8OffAddrAs# addr# os# =
        PrimByteSwapped (byteSwap (indexWord8OffAddrAs# addr# os#))
    readWord8OffAddrAs# addr# os# = \s0 ->
        case readWord8OffAddrAs# addr# os# s0 of
          (# s1, a #) -> (# s1, PrimByteSwapped (byteSwap a) #)
    writeWord8OffAddrAs# addr# os# (PrimByteSwapped a) = \s0 ->
        writeWord8OffAddrAs# addr# os# (byteSwap a) s0

-- idk why I gotta (a :: Type) why is GHC going kind-polymorphic there lol
#if defined(WORDS_BIGENDIAN)
deriving via (PrimByteSwapped a) instance
    (Prim  a, ByteSwap a) => Prim  (ByteOrdered 'LittleEndian a)
deriving via (PrimByteSwapped a) instance
    (Prim' a, ByteSwap a) => Prim' (ByteOrdered 'LittleEndian a)
deriving via (a :: Type) instance
    Prim  a => Prim  (ByteOrdered 'BigEndian a)
deriving via (a :: Type) instance
    Prim' a => Prim' (ByteOrdered 'BigEndian a)
#else
deriving via (a :: Type) instance
    Prim  a => Prim  (ByteOrdered 'LittleEndian a)
deriving via (a :: Type) instance
    Prim' a => Prim' (ByteOrdered 'LittleEndian a)
deriving via (PrimByteSwapped a) instance
    (Prim  a, ByteSwap a) => Prim  (ByteOrdered 'BigEndian a)
deriving via (PrimByteSwapped a) instance
    (Prim' a, ByteSwap a) => Prim' (ByteOrdered 'BigEndian a)
#endif
