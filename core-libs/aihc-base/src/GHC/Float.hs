{-# LANGUAGE MagicHash #-}

module GHC.Float
  ( Double (..),
    Float (..),
    Floating (..),
    RealFloat (..),
    roundTo,
  )
where

import Data.Bool (Bool (..), not)
import GHC.Int (Int)
import GHC.Integer (Integer)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Integer (integerToInt#)
import GHC.Num (Num (..))
import GHC.Prim
  ( Double#,
    Float#,
    Int#,
    eqFloat#,
    fabsDouble#,
    fabsFloat#,
    gtFloat#,
    int2Double#,
    int2Float#,
    ltFloat#,
    minusFloat#,
    negateDouble#,
    negateFloat#,
    plusFloat#,
    timesFloat#,
    (*##),
    (+##),
    (-#),
    (-##),
    (<##),
    (==##),
    (>##),
  )
import GHC.Real (Fractional, Integral (..), RealFrac)
import GHC.Types (Double (..), Float (..), Ordering (..))

-- | Trigonometric and transcendental operations.
class (Fractional a) => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  log1p :: a -> a
  expm1 :: a -> a
  log1pexp :: a -> a
  log1mexp :: a -> a

infixr 8 **

-- | Machine-independent decomposition of real floating-point values.
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a

-- | Round a list of digits in the given base to the given number of digits.
-- The result carries a leading one when the rounding overflows.
roundTo :: Int -> Int -> [Int] -> (Int, [Int])
roundTo base digitCount digits =
  case roundDigits base digitCount digits of
    (0, rounded) -> (0, rounded)
    (_, rounded) -> (1, 1 : rounded)

roundDigits :: Int -> Int -> [Int] -> (Int, [Int])
roundDigits base remaining digits =
  case remaining == 0 of
    True -> (roundCarry base digits, [])
    False ->
      case digits of
        [] -> (0, replicateZero remaining)
        (digit : rest) ->
          case roundDigits base (remaining - 1) rest of
            (carry, roundedRest) ->
              let total = digit + carry
               in case total == base of
                    True -> (1, 0 : roundedRest)
                    False -> (0, total : roundedRest)

roundCarry :: Int -> [Int] -> Int
roundCarry _ [] = 0
roundCarry base (digit : rest) =
  let half = base `quot` 2
   in case digit > half of
        True -> 1
        False ->
          case digit == half of
            True -> roundEven rest
            False -> 0

roundEven :: [Int] -> Int
roundEven [] = 0
roundEven (digit : rest) =
  case digit == 0 of
    True -> roundEven rest
    False -> 1

replicateZero :: Int -> [Int]
replicateZero count =
  case count <= 0 of
    True -> []
    False -> 0 : replicateZero (count - 1)

-- | Convert a primitive comparison result to a 'Bool'.
isTrue :: Int# -> Bool
isTrue value =
  case value of
    0# -> False
    _ -> True

instance Eq Float where
  F# left == F# right = isTrue (eqFloat# left right)
  left /= right = not (left == right)

instance Ord Float where
  compare (F# left) (F# right) =
    case ltFloat# left right of
      0# ->
        case gtFloat# left right of
          0# -> EQ
          _ -> GT
      _ -> LT
  F# left < F# right = isTrue (ltFloat# left right)
  F# left > F# right = isTrue (gtFloat# left right)
  left <= right = not (left > right)
  left >= right = not (left < right)
  max left right = if left < right then right else left
  min left right = if left < right then left else right

floatSignum :: Float# -> Float#
floatSignum value =
  case gtFloat# value (int2Float# 0#) of
    0# ->
      case ltFloat# value (int2Float# 0#) of
        0# -> value
        _ -> int2Float# ((-#) 0# 1#)
    _ -> int2Float# 1#

instance Num Float where
  F# left + F# right = F# (plusFloat# left right)
  F# left - F# right = F# (minusFloat# left right)
  F# left * F# right = F# (timesFloat# left right)
  negate (F# value) = F# (negateFloat# value)
  abs (F# value) = F# (fabsFloat# value)
  signum (F# value) = F# (floatSignum value)
  fromInteger value = F# (int2Float# (integerToInt# value))

instance Eq Double where
  D# left == D# right = isTrue ((==##) left right)
  left /= right = not (left == right)

instance Ord Double where
  compare (D# left) (D# right) =
    case (<##) left right of
      0# ->
        case (>##) left right of
          0# -> EQ
          _ -> GT
      _ -> LT
  D# left < D# right = isTrue ((<##) left right)
  D# left > D# right = isTrue ((>##) left right)
  left <= right = not (left > right)
  left >= right = not (left < right)
  max left right = if left < right then right else left
  min left right = if left < right then left else right

doubleSignum :: Double# -> Double#
doubleSignum value =
  case (>##) value (int2Double# 0#) of
    0# ->
      case (<##) value (int2Double# 0#) of
        0# -> value
        _ -> int2Double# ((-#) 0# 1#)
    _ -> int2Double# 1#

instance Num Double where
  D# left + D# right = D# ((+##) left right)
  D# left - D# right = D# ((-##) left right)
  D# left * D# right = D# ((*##) left right)
  negate (D# value) = D# (negateDouble# value)
  abs (D# value) = D# (fabsDouble# value)
  signum (D# value) = D# (doubleSignum value)
  fromInteger value = D# (int2Double# (integerToInt# value))
