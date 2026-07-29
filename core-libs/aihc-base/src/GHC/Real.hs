{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RebindableSyntax #-}

module GHC.Real
  ( Fractional (..),
    Integral (..),
    Real (..),
    RealFrac (..),
    Rational,
    Ratio,
    denominator,
    even,
    fromIntegral,
    gcd,
    lcm,
    numerator,
    odd,
    realToFrac,
    (%),
    (^),
    (^^),
  )
where

import Data.Bool (Bool (..), not, (&&))
import GHC.Enum (Enum (..))
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..), Ordering (..))
import GHC.Internal.Integer (Integer (..), integerQuotRem, integerToInt#)
import GHC.Num (Num (..))

data Ratio a = Ratio a a

type Rational = Ratio Integer

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational

class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer

class (Num a) => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

infixl 7 /

class (Real a, Fractional a) => RealFrac a where
  properFraction :: (Integral b) => a -> (b, a)
  truncate :: (Integral b) => a -> b
  round :: (Integral b) => a -> b
  ceiling :: (Integral b) => a -> b
  floor :: (Integral b) => a -> b

instance Real Int where
  toRational (I# value) = Ratio (IS value) (IS 1#)

instance Real Integer where
  toRational value = Ratio value 1

instance Integral Int where
  quot numerator denominator = firstOfPair (intQuotRem numerator denominator)
  rem numerator denominator = secondOfPair (intQuotRem numerator denominator)
  div numerator denominator = firstOfPair (integralDivMod numerator denominator)
  mod numerator denominator = secondOfPair (integralDivMod numerator denominator)
  quotRem = intQuotRem
  divMod = integralDivMod

  toInteger (I# value) = IS value

instance Integral Integer where
  quot numerator denominator = firstOfPair (integerQuotRemBoxed numerator denominator)
  rem numerator denominator = secondOfPair (integerQuotRemBoxed numerator denominator)
  div numerator denominator = firstOfPair (integralDivMod numerator denominator)
  mod numerator denominator = secondOfPair (integralDivMod numerator denominator)
  quotRem = integerQuotRemBoxed
  divMod = integralDivMod

  toInteger value = value

intQuotRem :: Int -> Int -> (Int, Int)
intQuotRem (I# numerator) (I# denominator) =
  case integerQuotRem (IS numerator) (IS denominator) of
    (quotient, intRemainder) -> (I# (integerToInt# quotient), I# (integerToInt# intRemainder))

integerQuotRemBoxed :: Integer -> Integer -> (Integer, Integer)
integerQuotRemBoxed = integerQuotRem

integralDivMod :: (Integral a) => a -> a -> (a, a)
integralDivMod numerator denominator =
  case quotRem numerator denominator of
    (quotient, divisionRemainder) ->
      case signum divisionRemainder == negate (signum denominator) of
        True -> (quotient - 1, divisionRemainder + denominator)
        False -> (quotient, divisionRemainder)

firstOfPair :: (a, b) -> a
firstOfPair (first, _) = first

secondOfPair :: (a, b) -> b
secondOfPair (_, second) = second

instance (Eq a) => Eq (Ratio a) where
  (==) = equalRatio
  left /= right = not (left == right)

equalRatio :: (Eq a) => Ratio a -> Ratio a -> Bool
equalRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  leftNumerator == rightNumerator && leftDenominator == rightDenominator

instance (Integral a) => Ord (Ratio a) where
  compare = compareRatio
  left < right = ratioLessBy compare left right
  left <= right = ratioLessOrEqualBy compare left right
  left > right = ratioGreaterBy compare left right
  left >= right = ratioGreaterOrEqualBy compare left right
  max = ratioMaxBy compare
  min = ratioMinBy compare

compareRatio :: (Integral a) => Ratio a -> Ratio a -> Ordering
compareRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  compare (leftNumerator * rightDenominator) (rightNumerator * leftDenominator)

ratioLessBy :: (a -> a -> Ordering) -> a -> a -> Bool
ratioLessBy comparison left right =
  case comparison left right of
    LT -> True
    _ -> False

ratioLessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
ratioLessOrEqualBy comparison left right =
  case comparison left right of
    GT -> False
    _ -> True

ratioGreaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
ratioGreaterBy comparison left right =
  case comparison left right of
    GT -> True
    _ -> False

ratioGreaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
ratioGreaterOrEqualBy comparison left right =
  case comparison left right of
    LT -> False
    _ -> True

ratioMaxBy :: (a -> a -> Ordering) -> a -> a -> a
ratioMaxBy comparison left right =
  case comparison left right of
    GT -> left
    _ -> right

ratioMinBy :: (a -> a -> Ordering) -> a -> a -> a
ratioMinBy comparison left right =
  case comparison left right of
    GT -> right
    _ -> left

instance (Integral a) => Num (Ratio a) where
  (+) = addRatio
  (-) = subtractRatio
  (*) = multiplyRatio
  negate = negateRatio
  abs = absRatio
  signum = signumRatio
  fromInteger value = Ratio (fromInteger value) 1

addRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
addRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce
    (leftNumerator * rightDenominator + rightNumerator * leftDenominator)
    (leftDenominator * rightDenominator)

subtractRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
subtractRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce
    (leftNumerator * rightDenominator - rightNumerator * leftDenominator)
    (leftDenominator * rightDenominator)

multiplyRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
multiplyRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  reduce (leftNumerator * rightNumerator) (leftDenominator * rightDenominator)

negateRatio :: (Num a) => Ratio a -> Ratio a
negateRatio (Ratio valueNumerator valueDenominator) = Ratio (negate valueNumerator) valueDenominator

absRatio :: (Num a) => Ratio a -> Ratio a
absRatio (Ratio valueNumerator valueDenominator) = Ratio (abs valueNumerator) valueDenominator

signumRatio :: (Num a) => Ratio a -> Ratio a
signumRatio (Ratio valueNumerator _) = Ratio (signum valueNumerator) 1

instance (Integral a) => Fractional (Ratio a) where
  (/) = divideRatio
  recip = reciprocalRatio
  fromRational = ratioFromRational

divideRatio :: (Integral a) => Ratio a -> Ratio a -> Ratio a
divideRatio (Ratio leftNumerator leftDenominator) (Ratio rightNumerator rightDenominator) =
  (leftNumerator * rightDenominator) % (leftDenominator * rightNumerator)

reciprocalRatio :: (Integral a) => Ratio a -> Ratio a
reciprocalRatio (Ratio valueNumerator valueDenominator) =
  case valueNumerator == 0 of
    True -> ratioZeroDenominatorError
    False ->
      case valueNumerator < 0 of
        True -> Ratio (negate valueDenominator) (negate valueNumerator)
        False -> Ratio valueDenominator valueNumerator

ratioFromRational :: (Integral a) => Rational -> Ratio a
ratioFromRational (Ratio valueNumerator valueDenominator) = fromInteger valueNumerator % fromInteger valueDenominator

instance (Integral a) => Real (Ratio a) where
  toRational = ratioToRational

ratioToRational :: (Integral a) => Ratio a -> Rational
ratioToRational (Ratio valueNumerator valueDenominator) = Ratio (toInteger valueNumerator) (toInteger valueDenominator)

instance (Integral a) => RealFrac (Ratio a) where
  properFraction = ratioProperFraction
  truncate value = firstOfPair (ratioProperFraction value)
  round = ratioRound
  ceiling = ratioCeiling
  floor = ratioFloor

ratioProperFraction :: (Integral a, Integral b) => Ratio a -> (b, Ratio a)
ratioProperFraction (Ratio valueNumerator valueDenominator) =
  case quotRem valueNumerator valueDenominator of
    (quotient, fractionRemainder) -> (fromInteger (toInteger quotient), Ratio fractionRemainder valueDenominator)

ratioRound :: (Integral a, Integral b) => Ratio a -> b
ratioRound value =
  case ratioProperFraction value of
    (integral, roundRemainder) ->
      case compare (abs roundRemainder + abs roundRemainder) 1 of
        LT -> integral
        EQ ->
          case even integral of
            True -> integral
            False -> integral + ratioDirection roundRemainder
        GT -> integral + ratioDirection roundRemainder

ratioCeiling :: (Integral a, Integral b) => Ratio a -> b
ratioCeiling value =
  case ratioProperFraction value of
    (integral, ceilingRemainder) ->
      case ceilingRemainder > 0 of
        True -> integral + 1
        False -> integral

ratioFloor :: (Integral a, Integral b) => Ratio a -> b
ratioFloor value =
  case ratioProperFraction value of
    (integral, floorRemainder) ->
      case floorRemainder < 0 of
        True -> integral - 1
        False -> integral

ratioDirection :: (Integral a, Num b) => Ratio a -> b
ratioDirection value =
  case value < 0 of
    True -> negate 1
    False -> 1

instance (Integral a) => Enum (Ratio a) where
  succ value = value + 1
  pred value = value - 1
  toEnum value = Ratio (fromIntegral value) 1
  fromEnum value = fromInteger (truncate value)
  enumFrom value = numericEnumFromThen value (value + 1)
  enumFromThen = numericEnumFromThen
  enumFromTo first = numericEnumFromThenTo first (first + 1)
  enumFromThenTo = numericEnumFromThenTo

infixl 7 %

(%) :: (Integral a) => a -> a -> Ratio a
valueNumerator % valueDenominator = reduce (valueNumerator * signum valueDenominator) (abs valueDenominator)

reduce :: (Integral a) => a -> a -> Ratio a
reduce _ 0 = ratioZeroDenominatorError
reduce valueNumerator valueDenominator =
  case gcd valueNumerator valueDenominator of
    divisor -> Ratio (quot valueNumerator divisor) (quot valueDenominator divisor)

numerator :: Ratio a -> a
numerator (Ratio value _) = value

denominator :: Ratio a -> a
denominator (Ratio _ value) = value

ratioZeroDenominatorError :: a
ratioZeroDenominatorError = ratioZeroDenominatorError

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral value = fromInteger (toInteger value)

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac value = fromRational (toRational value)

even :: (Integral a) => a -> Bool
even value = rem value 2 == 0

odd :: (Integral a) => a -> Bool
odd value = not (even value)

gcd :: (Integral a) => a -> a -> a
gcd left right = gcdPositive (abs left) (abs right)

gcdPositive :: (Integral a) => a -> a -> a
gcdPositive left right =
  case right == 0 of
    True -> left
    False -> gcdPositive right (rem left right)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm left right = abs (quot left (gcd left right) * right)

infixr 8 ^, ^^

(^) :: (Num a, Integral b) => a -> b -> a
base ^ exponent =
  case exponent < 0 of
    True -> negativeExponentError
    False -> positivePower base exponent 1

positivePower :: (Num a, Integral b) => a -> b -> a -> a
positivePower base exponent accumulator =
  case exponent == 0 of
    True -> accumulator
    False ->
      case quotRem exponent 2 of
        (halfExponent, powerRemainder) ->
          case powerRemainder == 0 of
            True -> positivePower (base * base) halfExponent accumulator
            False -> positivePower (base * base) halfExponent (accumulator * base)

(^^) :: (Fractional a, Integral b) => a -> b -> a
base ^^ exponent =
  case exponent < 0 of
    True -> recip (base ^ negate exponent)
    False -> base ^ exponent

negativeExponentError :: a
negativeExponentError = negativeExponentError

numericEnumFromThen :: (Fractional a) => a -> a -> [a]
numericEnumFromThen first second = first : numericEnumFromThen second (second + (second - first))

numericEnumFromThenTo :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> value : go (value + step)
            False -> []
        False ->
          case value >= last of
            True -> value : go (value + step)
            False -> []
