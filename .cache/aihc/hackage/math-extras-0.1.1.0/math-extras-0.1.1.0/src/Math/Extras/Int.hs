{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Extras.Int
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Arithmetic on integral numbers.
module Math.Extras.Int (
  -- * Saturated arithmetic
    saturatedFromInteger
  , saturatedAdd
  , saturatedSubtract
  , saturatedMultiply
  , saturatedPow

  -- * Other helper functions
  , toBinaryString
  ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromMaybe)

-- | Like 'fromInteger', but returns 'maxBound' and 'minBound' in case of
-- overflow and underflow, respectively.
saturatedFromInteger :: forall a. (Integral a, Bounded a) => Integer -> a
saturatedFromInteger x
  | x > toInteger (maxBound @a) = maxBound
  | x < toInteger (minBound @a) = minBound
  | otherwise = fromInteger x

-- | Sum of two integral numbers. Returns 'maxBound' and 'minBound' in case of
-- overflow and underflow, respectively.
saturatedAdd :: (Integral a, Bounded a) => a -> a -> a
saturatedAdd x y = saturatedFromInteger (toInteger x + toInteger y)

-- | Difference of two integral numbers. Returns 'maxBound' and 'minBound' in case of
-- overflow and underflow, respectively.
saturatedSubtract :: (Integral a, Bounded a) => a -> a -> a
saturatedSubtract x y = saturatedFromInteger (toInteger x - toInteger y)

-- | Product of two integral numbers. Returns 'maxBound' and 'minBound' in case of
-- overflow and underflow, respectively.
saturatedMultiply :: (Integral a, Bounded a) => a -> a -> a
saturatedMultiply x y = saturatedFromInteger (toInteger x * toInteger y)

-- | @saturatedPow a b@ computes @a@ '^' @b@. Returns 'maxBound' and 'minBound' in case of
-- overflow and underflow, respectively.
--
-- __NB:__ Like '^', the exponent must be non-negative.
saturatedPow :: Int -> Int -> Int
saturatedPow _ b | b < 0 = error "Negative exponent"
saturatedPow 0 0 = 1
saturatedPow 0 _ = 0
saturatedPow 1 _ = 1
saturatedPow (-1) b = if even b then 1 else -1
saturatedPow 2 b
  | b >= finiteBitSize (0 :: Int) - 1 = maxBound
  | otherwise = unsafeShiftL 1 b
saturatedPow (-2) b
  | b >= finiteBitSize (0 :: Int) = if even b then maxBound else minBound
  | otherwise = unsafeShiftL (if even b then 1 else -1) b
saturatedPow a b = go a b 1
  where
    bound = if a < 0 && odd b then minBound else maxBound
    sqrtMaxBound = floor . sqrt @Double . fromIntegral $ maxBound @Int
    go a b acc
      | b == 0 = acc
      | b == 1 = saturatedMultiply acc a
      | a > sqrtMaxBound || a < -sqrtMaxBound = bound
      | otherwise =
          let acc' = if even b then acc else saturatedMultiply acc a
           in go (a * a) (b `div` 2) acc'

-- | Returns the bit representation of the input as a 'NonEmpty' 'Char' consisting of @0@s and @1@s,
-- without leading zeros. The length of the result is at most 'finiteBitSize' @b@.
--
-- > Data.List.NonEmpty.toList (toBinaryString (100 :: Int)) == "1100100"
-- > Data.List.NonEmpty.toList (toBinaryString (-1 :: Int8)) == "11111111"
toBinaryString :: FiniteBits b => b -> NonEmpty Char
toBinaryString b = f <$> fromMaybe (pure 0) (nonEmpty [len-1, len-2 .. 0])
  where
    len = finiteBitSize b - countLeadingZeros b
    f i = if testBit b i then '1' else '0'
