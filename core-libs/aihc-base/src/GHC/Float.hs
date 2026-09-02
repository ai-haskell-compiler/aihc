{-# LANGUAGE MagicHash #-}

module GHC.Float
  ( Double (..),
    Float (..),
    Floating (..),
    RealFloat (..),
    castDoubleToWord64,
    castFloatToWord32,
    castWord32ToFloat,
    castWord64ToDouble,
    double2Float,
    double2Int,
    float2Double,
    float2Int,
    int2Double,
    int2Float,
    roundTo,
  )
where

import Data.Bool (Bool (..))
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Prim
  ( castDoubleToWord64#,
    castFloatToWord32#,
    castWord32ToFloat#,
    castWord64ToDouble#,
    double2Float#,
    double2Int#,
    float2Double#,
    float2Int#,
    int2Double#,
    int2Float#,
  )
import GHC.Real (Fractional, Integral (..), RealFrac)
import GHC.Types (Double (..), Float (..))
import GHC.Word (Word32 (..), Word64 (..))

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

-- | Give the IEEE 754 bit pattern of a single-precision value.
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# value) = W32# (castFloatToWord32# value)

-- | Give the single-precision value of an IEEE 754 bit pattern.
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# value) = F# (castWord32ToFloat# value)

-- | Give the IEEE 754 bit pattern of a double-precision value.
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# value) = W64# (castDoubleToWord64# value)

-- | Give the double-precision value of an IEEE 754 bit pattern.
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# value) = D# (castWord64ToDouble# value)

int2Double :: Int -> Double
int2Double (I# value) = D# (int2Double# value)

int2Float :: Int -> Float
int2Float (I# value) = F# (int2Float# value)

double2Int :: Double -> Int
double2Int (D# value) = I# (double2Int# value)

float2Int :: Float -> Int
float2Int (F# value) = I# (float2Int# value)

double2Float :: Double -> Float
double2Float (D# value) = F# (double2Float# value)

float2Double :: Float -> Double
float2Double (F# value) = D# (float2Double# value)
