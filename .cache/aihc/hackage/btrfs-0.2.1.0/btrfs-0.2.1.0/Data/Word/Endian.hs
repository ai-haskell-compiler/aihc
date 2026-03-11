{-# LANGUAGE CPP #-}

module Data.Word.Endian where

import Data.Word (Word16, Word32, Word64)
#if MIN_VERSION_base(4,7,0)
import Data.Word (byteSwap16, byteSwap32, byteSwap64)
#else
import Data.Bits (rotateL, unsafeShiftL, unsafeShiftR, (.&.), (.|.))
#endif
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

#define MAKE_ENDIAN_WORD(native_type, endian_type, from, to, convert) \
newtype endian_type = endian_type {from :: native_type} deriving (Show, Eq); \
to :: native_type -> endian_type; \
to = endian_type; \
instance Storable endian_type where { \
    sizeOf _ = sizeOf (undefined :: native_type); \
    {-# INLINE sizeOf #-}; \
    alignment _ = alignment (undefined :: native_type); \
    {-# INLINE alignment #-}; \
    peek ptr = do {x <- peek (castPtr ptr); return (endian_type (convert x))}; \
    {-# INLINE peek #-}; \
    poke ptr (endian_type x) = poke (castPtr ptr) (convert x); \
    {-# INLINE poke #-}; \
}

#include "MachDeps.h"

#ifdef WORDS_BIGENDIAN
MAKE_ENDIAN_WORD(Word16, LE16, fromLE16, toLE16, invert16)
MAKE_ENDIAN_WORD(Word32, LE32, fromLE32, toLE32, invert32)
MAKE_ENDIAN_WORD(Word64, LE64, fromLE64, toLE64, invert64)
MAKE_ENDIAN_WORD(Word16, BE16, fromBE16, toBE16, id)
MAKE_ENDIAN_WORD(Word32, BE32, fromBE32, toBE32, id)
MAKE_ENDIAN_WORD(Word64, BE64, fromBE64, toBE64, id)
#else
MAKE_ENDIAN_WORD(Word16, LE16, fromLE16, toLE16, id)
MAKE_ENDIAN_WORD(Word32, LE32, fromLE32, toLE32, id)
MAKE_ENDIAN_WORD(Word64, LE64, fromLE64, toLE64, id)
MAKE_ENDIAN_WORD(Word16, BE16, fromBE16, toBE16, invert16)
MAKE_ENDIAN_WORD(Word32, BE32, fromBE32, toBE32, invert32)
MAKE_ENDIAN_WORD(Word64, BE64, fromBE64, toBE64, invert64)
#endif

invert16 :: Word16 -> Word16
invert32 :: Word32 -> Word32
invert64 :: Word64 -> Word64

#if MIN_VERSION_base(4,7,0)
invert16 = byteSwap16
{-# INLINE invert16 #-}
invert32 = byteSwap32
{-# INLINE invert32 #-}
invert64 = byteSwap64
{-# INLINE invert64 #-}
#else
invert16 x = x `rotateL` 8

invert32 x =
    ((x               ) `unsafeShiftR` 24) .|.
    ((x .&. 0x00ff0000) `unsafeShiftR`  8) .|.
    ((x .&. 0x0000ff00) `unsafeShiftL`  8) .|.
    ((x               ) `unsafeShiftL` 24)

invert64 x =
    ((x                       ) `unsafeShiftR` 56) .|.
    ((x .&. 0x00ff000000000000) `unsafeShiftR` 40) .|.
    ((x .&. 0x0000ff0000000000) `unsafeShiftR` 24) .|.
    ((x .&. 0x000000ff00000000) `unsafeShiftR`  8) .|.
    ((x .&. 0x00000000ff000000) `unsafeShiftL`  8) .|.
    ((x .&. 0x0000000000ff0000) `unsafeShiftL` 24) .|.
    ((x .&. 0x000000000000ff00) `unsafeShiftL` 40) .|.
    ((x                       ) `unsafeShiftL` 56)
#endif
