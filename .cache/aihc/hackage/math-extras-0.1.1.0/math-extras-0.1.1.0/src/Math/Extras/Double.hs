{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Math.Extras.Double
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Arithmetic on doubles. Some functions use more general types than 'Double'.
module Math.Extras.Double (
  -- * Constant double values
    positiveInfinity
  , negativeInfinity
  , nan

  -- * Fuzzy comparison
  , Tolerance
  , fuzzyEq
  , fuzzyCompare
  , (~=)
  , (/~=)

  -- * Rounding
  , RoundingMode(..)
  , roundToInteger
  , saturatedRound

  -- * Predicates
  , isFinite
  , isInteger

  -- * Conversion
  , toInt64RawBits
  , toWord64RawBits
  , fromInt64RawBits
  , fromWord64RawBits
  ) where

import Data.Int (Int64)
import Data.Word (Word64)
import Unsafe.Coerce (unsafeCoerce)

import Math.Extras.Int (saturatedFromInteger)


-- | A constant holding the positive infinity of type 'Double'.
positiveInfinity :: Double
positiveInfinity = 1 / 0

-- | A constant holding the negative infinity of type 'Double'.
negativeInfinity :: Double
negativeInfinity = (-1) / 0

-- | A constant holding a NaN value of type 'Double'.
nan :: Double
nan = 0 / 0

type Tolerance = Double

defaultTolerance :: Tolerance
defaultTolerance = 1.0e-6

-- | Returns @True@ if the difference of the two numbers are no more than 'abs'@(tolerance)@.
--
--    * All NaNs are fuzzily equal.
--    * With finite tolerance, 'positiveInfinity' and 'negativeInfinity' are fuzzily equal
--      only to themselves.
--    * With 'positiveInfinity' or 'negativeInfinity' tolerance, all non-NaN values are
--      fuzzily equal.
--    * NaN tolerance behaves the same as zero tolerance.
--
-- @fuzzyEq tolerance@ is reflexive and symmetric, but /not/ transitive unless @tolerance@ is
-- zero, 'positiveInfinity', 'negativeInfinity', or NaN.
fuzzyEq :: Tolerance -> Double -> Double -> Bool
fuzzyEq tolerance x y =
  (abs (x-y) <= abs tolerance)
    || (x == y)
    || (isNaN x && isNaN y)

-- | Compare two 'Double's with a tolerance.
--
-- If 'fuzzyEq' @tolerance x y@ then @fuzzyCompare tolerance x y == @ 'EQ'; otherwise
-- @fuzzyCompare tolerance x y == @ 'compare' @x y@.
fuzzyCompare :: Tolerance -> Double -> Double -> Ordering
fuzzyCompare tolerance x y
  | fuzzyEq tolerance x y = EQ
  | otherwise = compare x y

-- | @(~=)@ is equivalent to 'fuzzyEq' @1.0e-6@.
(~=) :: Double -> Double -> Bool
(~=) = fuzzyEq defaultTolerance

-- | @x /~= y@ is equivalent to @not (x ~= y)@.
(/~=) :: Double -> Double -> Bool
(/~=) x y = not (x ~= y)

data RoundingMode
  = Up
  -- ^ Round away from 0
  | Down
  -- ^ Round towards 0
  | Ceiling
  -- ^ Round towards positive infinity
  | Floor
  -- ^ Round towards negative infinity
  | HalfUp
  -- ^ Round towards nearest integer; round 'Up' in case of a tie
  | HalfDown
  -- ^ Round towards nearest integer; round 'Down' in case of a tie
  | HalfEven
  -- ^ Round towards nearest integer; round towards the even neighbor in case of a tie
  deriving (Eq, Ord, Show)

-- | Round a 'Double' to an 'Integer' using the specified 'RoundingMode'.
roundToInteger :: RoundingMode -> Double -> Integer
roundToInteger mode x = case mode of
  Ceiling -> ceiling x
  Floor -> floor x
  Up | x >= 0 -> ceiling x
     | otherwise -> floor x
  Down | x >= 0 -> floor x
       | otherwise -> ceiling x
  HalfUp | abs (fromIntegral rounded - x) == 0.5 -> roundToInteger Up x
         | otherwise -> rounded
  HalfDown | abs (fromIntegral rounded - x) == 0.5 -> roundToInteger Down x
         | otherwise -> rounded
  HalfEven -> rounded
  where rounded = round x :: Integer

-- | Round a 'Double' to an integral number using the specified 'RoundingMode'.
-- Returns 'maxBound' and 'minBound' in case of overflow and underflow, respectively.
saturatedRound :: forall a. (Bounded a, Integral a) => RoundingMode -> Double -> a
saturatedRound mode x = saturatedFromInteger (roundToInteger mode x)

-- | Returns @False@ for NaN and infinity, and @True@ otherwise.
isFinite :: RealFloat a => a -> Bool
isFinite x = not (isInfinite x) && not (isNaN x)

-- | Returns @True@ if the input represents an integer.
isInteger :: (Eq a, Num a, RealFloat a) => a -> Bool
isInteger x = isFinite x && fromIntegral (round x :: Int) == x

-- | Returns an 'Int64' with the same bit representation as the given 'Double'.
toInt64RawBits :: Double -> Int64
toInt64RawBits = unsafeCoerce

-- | Returns a 'Word64' with the same bit representation as the given 'Double'.
toWord64RawBits :: Double -> Word64
toWord64RawBits = unsafeCoerce

-- | Returns a 'Double' with the same bit representation as the given 'Int64'.
fromInt64RawBits :: Int64 -> Double
fromInt64RawBits = unsafeCoerce

-- | Returns a 'Double' with the same bit representation as the given 'Word64'.
fromWord64RawBits :: Word64 -> Double
fromWord64RawBits = unsafeCoerce
