{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module      : System.Endian
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module System.Endian
    ( Endianness(..)
    , getSystemEndianness
    -- little endian to and from cpu
    , fromLE32
    , fromLE64
    , fromLE16
    , toLE32
    , toLE64
    , toLE16
    -- big endian to and from cpu
    , fromBE32
    , fromBE64
    , fromBE16
    , toBE32
    , toBE64
    , toBE16
    ) where

#include "MachDeps.h"

import Foreign.C.Types
import Data.Word

-- | represent the CPU endianness
--
-- Big endian system stores bytes with the MSB as the first byte.
-- Little endian system stores bytes with the LSB as the first byte.
--
-- middle endian is purposely avoided.
data Endianness = LittleEndian
                | BigEndian
                deriving (Show,Eq)

-- | return the system endianness
getSystemEndianness :: Endianness
#ifdef WORDS_BIGENDIAN
getSystemEndianness = BigEndian
#else
getSystemEndianness = LittleEndian
#endif

-- | Convert from a big endian 64 bit value to the cpu endianness
fromBE64 :: Word64 -> Word64
fromBE64 = if getSystemEndianness == BigEndian then id else swap64

-- | Convert from a little endian 64 bit value to the cpu endianness
fromLE64 :: Word64 -> Word64
fromLE64 = if getSystemEndianness == LittleEndian then id else swap64

-- | Convert from a big endian 32 bit value to the cpu endianness
fromBE32 :: Word32 -> Word32
fromBE32 = if getSystemEndianness == BigEndian then id else swap32

-- | Convert from a little endian 32 bit value to the cpu endianness
fromLE32 :: Word32 -> Word32
fromLE32 = if getSystemEndianness == LittleEndian then id else swap32

-- | Convert from a big endian 16 bit value to the cpu endianness
fromBE16 :: Word16 -> Word16
fromBE16 = if getSystemEndianness == BigEndian then id else swap16

-- | Convert from a little endian 16 bit value to the cpu endianness
fromLE16 :: Word16 -> Word16
fromLE16 = if getSystemEndianness == LittleEndian then id else swap16


-- | Convert a 64 bit value in cpu endianess to big endian
toBE64 :: Word64 -> Word64
toBE64 = fromBE64

-- | Convert a 64 bit value in cpu endianess to little endian
toLE64 :: Word64 -> Word64
toLE64 = fromLE64

-- | Convert a 32 bit value in cpu endianess to big endian
toBE32 :: Word32 -> Word32
toBE32 = fromBE32

-- | Convert a 32 bit value in cpu endianess to little endian
toLE32 :: Word32 -> Word32
toLE32 = fromLE32

-- | Convert a 16 bit value in cpu endianness to big endian
toBE16 :: Word16 -> Word16
toBE16 = fromBE16

-- | Convert a 16 bit value in cpu endianness to little endian
toLE16 :: Word16 -> Word16
toLE16 = fromLE16

#if MIN_VERSION_base(4,7,0)

-- | Transform a 16 bit value bytes from a.b to b.a
swap16 :: Word16 -> Word16
swap16 = byteSwap16
    
-- | Transform a 32 bit value bytes from a.b.c.d to d.c.b.a
swap32 :: Word32 -> Word32
swap32 = byteSwap32

-- | Transform a 64 bit value bytes from a.b.c.d.e.f.g.h to h.g.f.e.d.c.b.a
swap64 :: Word64 -> Word64
swap64 = byteSwap64

#else

-- | Transform a 16 bit value bytes from a.b to b.a
{-# INLINE swap16 #-}
swap16 :: Word16 -> Word16
swap16 = fromIntegral . c_swap16 . fromIntegral

-- | Transform a 32 bit value bytes from a.b.c.d to d.c.b.a
{-# INLINE swap32 #-}
swap32 :: Word32 -> Word32
swap32 = fromIntegral . c_swap32 . fromIntegral

-- | Transform a 64 bit value bytes from a.b.c.d.e.f.g.h to h.g.f.e.d.c.b.a
{-# INLINE swap64 #-}
swap64 :: Word64 -> Word64
swap64 = fromIntegral . c_swap64 . fromIntegral

foreign import ccall unsafe "bitfn_swap16" c_swap16 :: CUShort -> CUShort
foreign import ccall unsafe "bitfn_swap32" c_swap32 :: CUInt -> CUInt
foreign import ccall unsafe "bitfn_swap64" c_swap64 :: CULLong -> CULLong

#endif


