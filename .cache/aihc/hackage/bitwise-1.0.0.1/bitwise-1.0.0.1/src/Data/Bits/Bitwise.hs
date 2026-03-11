{-# LANGUAGE PatternGuards #-}
{-|

Module      :  Data.Bits.Bitwise
Copyright   :  (c) Claude Heiland-Allen 2012
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Lifting boolean operations on 'Bool' to bitwise operations on 'Bits'.

Packing bits into words, and unpacking words into bits.

-}
module Data.Bits.Bitwise
  (
  -- * Boolean operations lifted to bitwise operations.
    repeat
  , map
  , zipWith
  , or
  , and
  , any
  , all
  , isUniform
  -- * Splitting\/joining 'Bits' to\/from (lsb, msb).
  , mask
  , splitAt
  , joinAt
  , fromBool
  -- * (Un)packing 'Bits' to\/from lists of 'Bool'.
  , fromListLE
  , toListLE
  , fromListBE
  , toListBE
  -- * (Un)packing 'Word8' to\/from 8-tuples of 'Bool'.
  , packWord8LE
  , unpackWord8LE
  , packWord8BE
  , unpackWord8BE
  ) where

import Prelude hiding (repeat, map, zipWith, any, all, or, and, splitAt)
import qualified Prelude as P

import Data.Bits (Bits(complement, (.&.), (.|.), xor, bit, shiftL, shiftR, testBit, bitSizeMaybe, zeroBits),
                  FiniteBits(finiteBitSize))
import Data.List (foldl')
import Data.Word (Word8)

-- | Lift a boolean constant to a bitwise constant.
{-# INLINE repeat #-}
repeat :: (Bits b) => Bool -> b
repeat False = zeroBits
repeat True = complement zeroBits

-- | Lift a unary boolean operation to a bitwise operation.
--
--   The implementation is by exhaustive input\/output case analysis:
--   thus the operation provided must be total.
--
{-# INLINE map #-}
map :: (Bits b) => (Bool -> Bool) {- ^ operation -} -> b -> b
map f = case (f False, f True) of
  (False, False) -> \_ -> zeroBits
  (False, True ) -> id
  (True,  False) -> complement
  (True,  True ) -> \_ -> complement zeroBits

-- | Lift a binary boolean operation to a bitwise operation.
--
--   The implementation is by exhaustive input\/output case analysis:
--   thus the operation provided must be total.
--
{-# INLINE zipWith #-}
zipWith :: (Bits b) => (Bool -> Bool -> Bool) {- ^ operation -} -> b -> b -> b
zipWith f = case (f False False, f False True, f True False, f True True) of
  (False, False, False, False) -> \_ _ -> zeroBits
  (False, False, False, True ) -> (.&.)
  (False, False, True,  False) -> \x y -> x .&. complement y
  (False, False, True,  True ) -> \x _ -> x
  (False, True,  False, False) -> \x y -> complement x .&. y
  (False, True,  False, True ) -> \_ y -> y
  (False, True,  True,  False) -> xor
  (False, True,  True,  True ) -> (.|.)
  (True,  False, False, False) -> \x y -> complement (x .|. y)
  (True,  False, False, True ) -> \x y -> complement (x `xor` y)
  (True,  False, True,  False) -> \_ y -> complement y
  (True,  False, True,  True ) -> \x y -> x .|. complement y
  (True,  True,  False, False) -> \x _ -> complement x
  (True,  True,  False, True ) -> \x y -> complement x .|. y
  (True,  True,  True,  False) -> \x y -> complement (x .&. y)
  (True,  True,  True,  True ) -> \_ _ -> complement zeroBits

-- zipWith3 would have 256 cases? not sure..

-- | True when any bit is set.
{-# INLINE or #-}
or  :: (Bits b) => b -> Bool
or  b = b /= zeroBits

-- | True when all bits are set.
{-# INLINE and #-}
and :: (Bits b) => b -> Bool
and b = b == complement zeroBits

-- | True when the predicate is true for any bit.
{-# INLINE any #-}
any :: (Bits b) => (Bool -> Bool) {- ^ predicate -} -> b -> Bool
any f = or  . map f

-- | True when the predicate is true for all bits.
{-# INLINE all #-}
all :: (Bits b) => (Bool -> Bool) {- ^ predicate -} -> b -> Bool
all f = and . map f

-- | Determine if a 'Bits' is all 1s, all 0s, or neither.
{-# INLINE isUniform #-}
isUniform :: (Bits b) => b -> Maybe Bool
isUniform b
  | b == zeroBits            = Just False
  | b == complement zeroBits = Just True
  | otherwise                = Nothing

-- | A mask with count least significant bits set.
{-# INLINE mask #-}
mask :: (Num b, Bits b) => Int {- ^ count -} -> b
mask n = bit n - bit 0

-- | Split a word into (lsb, msb).  Ensures lsb has no set bits
--   above the split point.
{-# INLINE splitAt #-}
splitAt :: (Num b, Bits b) => Int {- ^ split point -} -> b {- ^ word -} -> (b, b) {- ^ (lsb, msb) -}
splitAt n b = (b .&. mask n, b `shiftR` n)

-- | Join lsb with msb to make a word.  Assumes lsb has no set bits
--   above the join point.
{-# INLINE joinAt #-}
joinAt :: (Bits b) => Int {- ^ join point -} -> b {- ^ least significant bits -} -> b {- ^ most significant bits -} -> b {- ^ word -}
joinAt n lsb msb = lsb .|. (msb `shiftL` n)

-- | Pack bits into a byte in little-endian order.
{-# INLINE packWord8LE #-}
packWord8LE :: Bool {- ^ least significant bit -} -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool {- ^ most significant bit -} -> Word8
packWord8LE  a b c d e f g h = z a 1 .|. z b 2 .|. z c 4 .|. z d 8 .|. z e 16 .|. z f 32 .|. z g 64 .|. z h 128
  where z False _ = 0
        z True  n = n

-- | Pack bits into a byte in big-endian order.
{-# INLINE packWord8BE #-}
packWord8BE :: Bool {- ^ most significant bit -} -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool {- ^ least significant bit -} -> Word8
packWord8BE a b c d e f g h = packWord8LE h g f e d c b a

-- | Extract the bits from a byte in little-endian order.
{-# INLINE unpackWord8LE #-}
unpackWord8LE :: Word8 -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) {- ^ (least significant bit, ..., most significant bit) -}
unpackWord8LE w = (b 1, b 2, b 4, b 8, b 16, b 32, b 64, b 128)
  where b z = w .&. z /= 0

-- | Extract the bits from a byte in big-endian order.
{-# INLINE unpackWord8BE #-}
unpackWord8BE :: Word8 -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) {- ^ (most significant bit, ..., least significant bit) -}
unpackWord8BE w = (b 128, b 64, b 32, b 16, b 8, b 4, b 2, b 1)
  where b z = w .&. z /= 0

-- | The least significant bit.
{-# INLINE fromBool #-}
fromBool :: (Bits b) => Bool -> b
fromBool False = zeroBits
fromBool True  = bit 0

-- | Convert a little-endian list of bits to 'Bits'.
{-# INLINE fromListLE #-}
fromListLE :: (Bits b) => [Bool] {- ^ \[least significant bit, ..., most significant bit\] -} -> b
fromListLE = foldr f zeroBits
  where
    f b i = fromBool b .|. (i `shiftL` 1)

-- | Convert a 'Bits' to a list of bits, in
--   little-endian order.
{-# INLINE toListLE #-}
toListLE :: (Bits b) => b -> [Bool] {- ^ \[least significant bit, ..., most significant bit\] -}
toListLE b0 | Just n <- bitSizeMaybe b0 = P.map (testBit b0) [0..n-1]
            | otherwise = go b0
  where go b | zeroBits == b = []
             | otherwise = testBit b 0 : go (b `shiftR` 1)

-- | Convert a big-endian list of bits to 'Bits'.
{-# INLINE fromListBE #-}
fromListBE :: (Bits b) => [Bool] {- ^ \[most significant bit, ..., least significant bit\] -} -> b
fromListBE = foldl' f zeroBits
  where
    f i b = (i `shiftL` 1) .|. fromBool b

-- | Convert a 'FiniteBits' to a list of bits, in
--   big-endian order.
{-# INLINE toListBE #-}
toListBE :: (FiniteBits b) => b -> [Bool] {- ^ \[most significant bit, ..., least significant bit\] -}
toListBE b = P.map (testBit b) [finiteBitSize b - 1, finiteBitSize b - 2 .. 0]
