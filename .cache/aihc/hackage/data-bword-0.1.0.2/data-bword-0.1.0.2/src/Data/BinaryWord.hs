{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
#if __GLASGOW_HASKELL__ >= 705
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

#include "MachDeps.h"

-- | Extra operations on binary words of fixed length.
module Data.BinaryWord
  ( BinaryWord(..)
  , lMsb
  , lLsb
  ) where

import Data.Int
import Data.Word
import Data.Bits (Bits(..))
#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(..))
#endif
#if __GLASGOW_HASKELL__ >= 705
import GHC.Prim (plusWord2#, timesWord2#)
# if __GLASGOW_HASKELL__ >= 904
import GHC.Prim (word64ToWord#, wordToWord64#)
# endif
# if WORD_SIZE_IN_BITS == 32
import GHC.Word (Word32(..))
# endif
# if WORD_SIZE_IN_BITS == 64
import GHC.Word (Word64(..))
# endif
#endif

-- | Two's complement, fixed length binary words.
#if MIN_VERSION_base(4,7,0)
class (FiniteBits w, FiniteBits (UnsignedWord w), FiniteBits (SignedWord w))
#else
class (Bits w, Bits (UnsignedWord w), Bits (SignedWord w))
#endif
      ⇒ BinaryWord w where
  -- | The unsigned variant type
  type UnsignedWord w
  -- | The signed variant type
  type SignedWord w
  -- | Convert the word to the unsigned type (identical to 'fromIntegral')
  unsignedWord ∷ w → UnsignedWord w
  -- | Convert the word to the signed type (identical to 'fromIntegral')
  signedWord ∷ w → SignedWord w
  -- | Unwrapped addition
  unwrappedAdd ∷ w → w → (w, UnsignedWord w)
  -- | Unwrapped multiplication
  unwrappedMul ∷ w → w → (w, UnsignedWord w)
  -- | Number of leading (from MSB) zero bits
  leadingZeroes ∷ w → Int
  -- | Number or trailing (from LSB) zero bits
  trailingZeroes ∷ w → Int
  -- | The word with all bits set to 0
  allZeroes ∷ w
  -- | The word with all bits set to 1
  allOnes ∷ w
  -- | The word with MSB set to 1 and all the other bits set to 0
  msb ∷ w
  -- | The word with LSB set to 1 and all the other bits set to 0
  lsb ∷ w
  lsb = bit 0
  {-# INLINE lsb #-}
  -- | Test if the MSB is 1
  testMsb ∷ w → Bool
  -- | Test if the LSB is 1
  testLsb ∷ w → Bool
  testLsb = flip testBit 0
  {-# INLINE testLsb #-}
  -- | Set the MSB to 1
  setMsb ∷ w → w
  -- | Set the LSB to 1
  setLsb ∷ w → w
  setLsb = flip setBit 0
  {-# INLINE setLsb #-}
  -- | Set the MSB to 0
  clearMsb ∷ w → w
  -- | Set the LSB to 0
  clearLsb ∷ w → w
  clearLsb = flip clearBit 0
  {-# INLINE clearLsb #-}

-- | MSB lens.
lMsb ∷ (Functor f, BinaryWord w) ⇒ (Bool → f Bool) → w → f w
lMsb f w = fmap (\x → if x then setMsb w else clearMsb w) (f (testMsb w))

-- | LSB lens.
lLsb ∷ (Functor f, BinaryWord w) ⇒ (Bool → f Bool) → w → f w
lLsb f w = fmap (\x → if x then setLsb w else clearLsb w) (f (testLsb w))

instance BinaryWord Word8 where
  type UnsignedWord Word8 = Word8
  type SignedWord Word8 = Int8
  unsignedWord = id
  {-# INLINE unsignedWord #-}
  signedWord = fromIntegral
  {-# INLINE signedWord #-}
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s = fromIntegral x + fromIntegral y ∷ Word16
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 8)
  {-# INLINE unwrappedAdd #-}
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p = fromIntegral x * fromIntegral y ∷ Word16
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 8)
  {-# INLINE unwrappedMul #-}
#if MIN_VERSION_base(4,8,0)
  leadingZeroes = countLeadingZeros
  {-# INLINE leadingZeroes #-}
  trailingZeroes = countTrailingZeros
  {-# INLINE trailingZeroes #-}
#else
  leadingZeroes w | w .&. 0xF0 == 0 = go4 4 w
                  | otherwise       = go4 0 (shiftR w 4)
    where go4 off w' | w' .&. 8 /= 0 = off
                     | w' .&. 4 /= 0 = off + 1
                     | w' .&. 2 /= 0 = off + 2
                     | w' .&. 1 /= 0 = off + 3
                     | otherwise     = off + 4
  trailingZeroes w | w .&. 0x0F == 0 = go4 4 (shiftR w 4)
                   | otherwise       = go4 0 w
    where go4 off w' | w' .&. 1 /= 0 = off
                     | w' .&. 2 /= 0 = off + 1
                     | w' .&. 4 /= 0 = off + 2
                     | w' .&. 8 /= 0 = off + 3
                     | otherwise     = off + 4
#endif
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = 0xFF
  {-# INLINE allOnes #-}
  msb = 0x80
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 7
  {-# INLINE testMsb #-}
  setMsb x = setBit x 7
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 7
  {-# INLINE clearMsb #-}

instance BinaryWord Word16 where
  type UnsignedWord Word16 = Word16
  type SignedWord Word16 = Int16
  unsignedWord = id
  {-# INLINE unsignedWord #-}
  signedWord = fromIntegral
  {-# INLINE signedWord #-}
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s  = fromIntegral x + fromIntegral y ∷ Word32
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 16)
  {-# INLINE unwrappedAdd #-}
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p  = fromIntegral x * fromIntegral y ∷ Word32
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 16)
  {-# INLINE unwrappedMul #-}
#if MIN_VERSION_base(4,8,0)
  leadingZeroes = countLeadingZeros
  {-# INLINE leadingZeroes #-}
  trailingZeroes = countTrailingZeros
  {-# INLINE trailingZeroes #-}
#else
  leadingZeroes w | w .&. 0xFF00 == 0 = go8 8 w
                  | otherwise         = go8 0 (shiftR w 8)
    where
      go8 off w' | w' .&. 0xF0 == 0 = go4 (off + 4) w'
                 | otherwise        = go4 off (shiftR w' 4)
      go4 off w' | w' .&. 8 /= 0    = off
                 | w' .&. 4 /= 0    = off + 1
                 | w' .&. 2 /= 0    = off + 2
                 | w' .&. 1 /= 0    = off + 3
                 | otherwise        = off + 4
  trailingZeroes w | w .&. 0x00FF == 0 = go8 8 (shiftR w 8)
                   | otherwise         = go8 0 w
    where
      go8 off w' | w' .&. 0x0F == 0 = go4 (off + 4) (shiftR w' 4)
                 | otherwise        = go4 off w'
      go4 off w' | w' .&. 1 /= 0    = off
                 | w' .&. 2 /= 0    = off + 1
                 | w' .&. 4 /= 0    = off + 2
                 | w' .&. 8 /= 0    = off + 3
                 | otherwise        = off + 4
#endif
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = 0xFFFF
  {-# INLINE allOnes #-}
  msb = 0x8000
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 15
  {-# INLINE testMsb #-}
  setMsb x = setBit x 15
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 15
  {-# INLINE clearMsb #-}

instance BinaryWord Word32 where
  type UnsignedWord Word32 = Word32
  type SignedWord Word32 = Int32
  unsignedWord = id
  {-# INLINE unsignedWord #-}
  signedWord = fromIntegral
  {-# INLINE signedWord #-}
#if __GLASGOW_HASKELL__ >= 705 && WORD_SIZE_IN_BITS == 32
  unwrappedAdd (W32# x) (W32# y) = hi `seq` lo `seq` (hi, lo)
    where !(# hi', lo' #) = plusWord2# x y
          lo = W32# lo'
          hi = W32# hi'
#else
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s  = fromIntegral x + fromIntegral y ∷ Word64
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 32)
#endif
  {-# INLINE unwrappedAdd #-}
#if __GLASGOW_HASKELL__ >= 705 && WORD_SIZE_IN_BITS == 32
  unwrappedMul (W32# x) (W32# y) = hi `seq` lo `seq` (hi, lo)
    where !(# hi', lo' #) = timesWord2# x y
          lo = W32# lo'
          hi = W32# hi'
#else
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p  = fromIntegral x * fromIntegral y ∷ Word64
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 32)
#endif
  {-# INLINE unwrappedMul #-}
#if MIN_VERSION_base(4,8,0)
  leadingZeroes = countLeadingZeros
  {-# INLINE leadingZeroes #-}
  trailingZeroes = countTrailingZeros
  {-# INLINE trailingZeroes #-}
#else
  leadingZeroes w | w .&. 0xFFFF0000 == 0 = go16 16 w
                  | otherwise             = go16 0 (shiftR w 16)
    where
      go16 off w' | w' .&. 0xFF00 == 0 = go8 (off + 8) w'
                  | otherwise          = go8 off (shiftR w' 8)
      go8  off w' | w' .&. 0xF0 == 0   = go4 (off + 4) w'
                  | otherwise          = go4 off (shiftR w' 4)
      go4  off w' | w' .&. 8 /= 0      = off
                  | w' .&. 4 /= 0      = off + 1
                  | w' .&. 2 /= 0      = off + 2
                  | w' .&. 1 /= 0      = off + 3
                  | otherwise          = off + 4
  trailingZeroes w | w .&. 0x0000FFFF == 0 = go16 16 (shiftR w 16)
                   | otherwise             = go16 0 w
    where
      go16 off w' | w' .&. 0x00FF == 0 = go8 (off + 8) (shiftR w' 8)
                  | otherwise          = go8 off w'
      go8  off w' | w' .&. 0x0F == 0   = go4 (off + 4) (shiftR w' 4)
                  | otherwise          = go4 off w'
      go4  off w' | w' .&. 1 /= 0      = off
                  | w' .&. 2 /= 0      = off + 1
                  | w' .&. 4 /= 0      = off + 2
                  | w' .&. 8 /= 0      = off + 3
                  | otherwise          = off + 4
#endif
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = 0xFFFFFFFF
  {-# INLINE allOnes #-}
  msb = 0x80000000
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 31
  {-# INLINE testMsb #-}
  setMsb x = setBit x 31
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 31
  {-# INLINE clearMsb #-}

instance BinaryWord Word64 where
  type UnsignedWord Word64 = Word64
  type SignedWord Word64 = Int64
  unsignedWord = id
  {-# INLINE unsignedWord #-}
  signedWord = fromIntegral
  {-# INLINE signedWord #-}
#if __GLASGOW_HASKELL__ >= 705 && WORD_SIZE_IN_BITS == 64
  unwrappedAdd (W64# x) (W64# y) = hi `seq` lo `seq` (hi, lo)
# if __GLASGOW_HASKELL__ >= 904
    where !(# hi', lo' #) = plusWord2# (word64ToWord# x) (word64ToWord# y)
          lo = W64# (wordToWord64# lo')
          hi = W64# (wordToWord64# hi')
# else
    where !(# hi', lo' #) = plusWord2# x y
          lo = W64# lo'
          hi = W64# hi'
# endif
  {-# INLINE unwrappedAdd #-}
#else
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where lo = x + y
          hi = if lo < x then 1 else 0
  {-# INLINABLE unwrappedAdd #-}
#endif
#if __GLASGOW_HASKELL__ >= 705 && WORD_SIZE_IN_BITS == 64
  unwrappedMul (W64# x) (W64# y) = hi `seq` lo `seq` (hi, lo)
# if __GLASGOW_HASKELL__ >= 904
    where !(# hi', lo' #) = timesWord2# (word64ToWord# x) (word64ToWord# y)
          lo = W64# (wordToWord64# lo')
          hi = W64# (wordToWord64# hi')
# else
    where !(# hi', lo' #) = timesWord2# x y
          lo = W64# lo'
          hi = W64# hi'
# endif
  {-# INLINE unwrappedMul #-}
#else
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where xHi = shiftR x 32
          xLo = x .&. 0xFFFFFFFF
          yHi = shiftR y 32
          yLo = y .&. 0xFFFFFFFF
          hi0 = xHi * yHi
          lo0 = xLo * yLo
          p1  = xHi * yLo
          p2  = xLo * yHi
          hi  = hi0 + fromIntegral (uHi1 ∷ Word32) + fromIntegral uHi2 +
                shiftR p1 32 + shiftR p2 32
          lo  = shiftL (fromIntegral lo') 32 .|. (lo0 .&. 0xFFFFFFFF)
          (uHi1, uLo) = unwrappedAdd (fromIntegral p1) (fromIntegral p2)
          (uHi2, lo') = unwrappedAdd (fromIntegral (shiftR lo0 32)) uLo
#endif
#if MIN_VERSION_base(4,8,0)
  leadingZeroes = countLeadingZeros
  {-# INLINE leadingZeroes #-}
  trailingZeroes = countTrailingZeros
  {-# INLINE trailingZeroes #-}
#else
# if WORD_SIZE_IN_BITS == 64
  leadingZeroes w | w .&. 0xFFFFFFFF00000000 == 0 = go32 32 w
                  | otherwise                     = go32 0 (shiftR w 32)
    where
      go32 off w' | w' .&. 0xFFFF0000 == 0 = go16 (off + 16) w'
                  | otherwise              = go16 off (shiftR w' 16)
      go16 off w' | w' .&. 0xFF00 == 0     = go8 (off + 8) w'
                  | otherwise              = go8 off (shiftR w' 8)
      go8  off w' | w' .&. 0xF0 == 0       = go4 (off + 4) w'
                  | otherwise              = go4 off (shiftR w' 4)
      go4  off w' | w' .&. 8 /= 0          = off
                  | w' .&. 4 /= 0          = off + 1
                  | w' .&. 2 /= 0          = off + 2
                  | w' .&. 1 /= 0          = off + 3
                  | otherwise              = off + 4
  trailingZeroes w | w .&. 0x00000000FFFFFFFF == 0 = go32 32 (shiftR w 32)
                   | otherwise                     = go32 0 w
    where
      go32 off w' | w' .&. 0x0000FFFF == 0 = go16 (off + 16) (shiftR w' 16)
                  | otherwise              = go16 off w'
      go16 off w' | w' .&. 0x00FF == 0     = go8 (off + 8) (shiftR w' 8)
                  | otherwise              = go8 off w'
      go8  off w' | w' .&. 0x0F == 0       = go4 (off + 4) (shiftR w' 4)
                  | otherwise              = go4 off w'
      go4  off w' | w' .&. 1 /= 0          = off
                  | w' .&. 2 /= 0          = off + 1
                  | w' .&. 4 /= 0          = off + 2
                  | w' .&. 8 /= 0          = off + 3
                  | otherwise              = off + 4
# else
  leadingZeroes w | hiZeroes == 32 = 32 + leadingZeroes lo
                  | otherwise      = hiZeroes
    where lo = fromIntegral w ∷ Word32
          hi = fromIntegral (shiftR w 32) ∷ Word32
          hiZeroes = leadingZeroes hi
  trailingZeroes w | loZeroes == 32 = 32 + trailingZeroes hi
                   | otherwise      = loZeroes
    where lo = fromIntegral w ∷ Word32
          hi = fromIntegral (shiftR w 32) ∷ Word32
          loZeroes = trailingZeroes lo
# endif
#endif
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = 0xFFFFFFFFFFFFFFFF
  {-# INLINE allOnes #-}
  msb = 0x8000000000000000
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 63
  {-# INLINE testMsb #-}
  setMsb x = setBit x 63
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 63
  {-# INLINE clearMsb #-}

instance BinaryWord Int8 where
  type UnsignedWord Int8 = Word8
  type SignedWord Int8 = Int8
  unsignedWord = fromIntegral
  {-# INLINE unsignedWord #-}
  signedWord = id
  {-# INLINE signedWord #-}
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s = fromIntegral x + fromIntegral y ∷ Int16
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 8)
  {-# INLINE unwrappedAdd #-}
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p = fromIntegral x * fromIntegral y ∷ Int16
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 8)
  {-# INLINE unwrappedMul #-}
  leadingZeroes = leadingZeroes . unsignedWord
  {-# INLINE leadingZeroes #-}
  trailingZeroes = trailingZeroes . unsignedWord
  {-# INLINE trailingZeroes #-}
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = -1
  {-# INLINE allOnes #-}
  msb = minBound
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 7
  {-# INLINE testMsb #-}
  setMsb x = setBit x 7
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 7
  {-# INLINE clearMsb #-}

instance BinaryWord Int16 where
  type UnsignedWord Int16 = Word16
  type SignedWord Int16 = Int16
  unsignedWord = fromIntegral
  {-# INLINE unsignedWord #-}
  signedWord = id
  {-# INLINE signedWord #-}
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s  = fromIntegral x + fromIntegral y ∷ Int32
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 16)
  {-# INLINE unwrappedAdd #-}
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p  = fromIntegral x * fromIntegral y ∷ Int32
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 16)
  {-# INLINE unwrappedMul #-}
  leadingZeroes = leadingZeroes . unsignedWord
  {-# INLINE leadingZeroes #-}
  trailingZeroes = trailingZeroes . unsignedWord
  {-# INLINE trailingZeroes #-}
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = -1
  {-# INLINE allOnes #-}
  msb = minBound
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 15
  {-# INLINE testMsb #-}
  setMsb x = setBit x 15
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 15
  {-# INLINE clearMsb #-}

instance BinaryWord Int32 where
  type UnsignedWord Int32 = Word32
  type SignedWord Int32 = Int32
  unsignedWord = fromIntegral
  {-# INLINE unsignedWord #-}
  signedWord = id
  {-# INLINE signedWord #-}
#if WORD_SIZE_IN_BITS == 32
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where extX = if x < 0 then maxBound else 0
          extY = if y < 0 then maxBound else 0
          (hi', lo) = unsignedWord x `unwrappedAdd` unsignedWord y
          hi = signedWord $ hi' + extX + extY
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where extX = if x < 0 then negate y else 0
          extY = if y < 0 then negate x else 0
          (hi', lo) = unsignedWord x `unwrappedMul` unsignedWord y
          hi = signedWord hi' + extX + extY
#else
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where s  = fromIntegral x + fromIntegral y ∷ Int64
          lo = fromIntegral s
          hi = fromIntegral (shiftR s 32)
  {-# INLINE unwrappedAdd #-}
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where p  = fromIntegral x * fromIntegral y ∷ Int64
          lo = fromIntegral p
          hi = fromIntegral (shiftR p 32)
  {-# INLINE unwrappedMul #-}
#endif
  leadingZeroes = leadingZeroes . unsignedWord
  {-# INLINE leadingZeroes #-}
  trailingZeroes = trailingZeroes . unsignedWord
  {-# INLINE trailingZeroes #-}
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = -1
  {-# INLINE allOnes #-}
  msb = minBound
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 31
  {-# INLINE testMsb #-}
  setMsb x = setBit x 31
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 31
  {-# INLINE clearMsb #-}

instance BinaryWord Int64 where
  type UnsignedWord Int64 = Word64
  type SignedWord Int64 = Int64
  unsignedWord = fromIntegral
  {-# INLINE unsignedWord #-}
  signedWord = id
  {-# INLINE signedWord #-}
  unwrappedAdd x y = hi `seq` lo `seq` (hi, lo)
    where extX = if x < 0 then maxBound else 0
          extY = if y < 0 then maxBound else 0
          (hi', lo) = unsignedWord x `unwrappedAdd` unsignedWord y
          hi = signedWord $ hi' + extX + extY
  unwrappedMul x y = hi `seq` lo `seq` (hi, lo)
    where extX = if x < 0 then negate y else 0
          extY = if y < 0 then negate x else 0
          (hi', lo) = unsignedWord x `unwrappedMul` unsignedWord y
          hi = signedWord hi' + extX + extY
  leadingZeroes = leadingZeroes . unsignedWord
  {-# INLINE leadingZeroes #-}
  trailingZeroes = trailingZeroes . unsignedWord
  {-# INLINE trailingZeroes #-}
  allZeroes = 0
  {-# INLINE allZeroes #-}
  allOnes = -1
  {-# INLINE allOnes #-}
  msb = minBound
  {-# INLINE msb #-}
  lsb = 1
  {-# INLINE lsb #-}
  testMsb x = testBit x 63
  {-# INLINE testMsb #-}
  setMsb x = setBit x 63
  {-# INLINE setMsb #-}
  clearMsb x = clearBit x 63
  {-# INLINE clearMsb #-}
