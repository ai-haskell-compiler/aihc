{-# LANGUAGE MagicHash #-}

module GHC.Num.Natural
  ( Natural (..),
  )
where

import Data.Bits (Bits (..))
import GHC.Exception (ArithException (..), throw)
import GHC.Num.BigNat (BigNat#)
import GHC.Prim (Word#, eqWord#, indexWordArray#, int2Word#, ltWord#, sizeofByteArray#, (<#), (==#))
import GHC.Prim.Integer (Integer (..), integerFromWord#)
import GHC.Prim.Natural (Natural (..))
import GHC.Read ()
import GHC.Types (isTrue#)
import Text.ParserCombinators.ReadPrec (ReadPrec, pfail)
import Prelude

-- The type itself is declared in @GHC.Prim.Natural@, because the compiler
-- names it as the kind of a type-level natural literal. The arithmetic
-- below goes through 'Integer' rather than working on the limbs directly:
-- only the representation has to match GHC, and the operations are free to
-- reuse the magnitude code that already backs 'Integer'.

underflow :: a
underflow = throw Underflow

naturalZero :: Natural
naturalZero = NS (int2Word# 0#)

naturalOne :: Natural
naturalOne = NS (int2Word# 1#)

-- | The 'Integer' with the same value.
naturalToInteger :: Natural -> Integer
naturalToInteger (NS word) = integerFromWord# 1# word
naturalToInteger (NB magnitude) = IP magnitude

-- | The 'Natural' with the same value; a negative argument underflows.
naturalFromInteger :: Integer -> Natural
naturalFromInteger (IS value) =
  case isTrue# ((<#) value 0#) of
    True -> underflow
    False -> NS (int2Word# value)
naturalFromInteger (IP magnitude) =
  -- A single-limb magnitude is only ever an 'Integer' above @maxBound :: Int@,
  -- which still fits a 'Word#'.
  case isTrue# ((==#) (sizeofByteArray# magnitude) 8#) of
    True -> NS (indexWordArray# magnitude 0#)
    False -> NB magnitude
naturalFromInteger (IN _) = underflow

instance Eq Natural where
  NS left == NS right = isTrue# (eqWord# left right)
  left == right = naturalToInteger left == naturalToInteger right

instance Ord Natural where
  compare (NS left) (NS right) = compareWord# left right
  compare left right = compare (naturalToInteger left) (naturalToInteger right)

compareWord# :: Word# -> Word# -> Ordering
compareWord# left right =
  case isTrue# (ltWord# left right) of
    True -> LT
    False ->
      case isTrue# (eqWord# left right) of
        True -> EQ
        False -> GT

instance Show Natural where
  showsPrec precedence value = showsPrec precedence (naturalToInteger value)

instance Num Natural where
  left + right = naturalFromInteger (naturalToInteger left + naturalToInteger right)
  left - right =
    case left < right of
      True -> underflow
      False -> naturalFromInteger (naturalToInteger left - naturalToInteger right)
  left * right = naturalFromInteger (naturalToInteger left * naturalToInteger right)
  negate value =
    case value == naturalZero of
      True -> naturalZero
      False -> underflow
  abs value = value
  signum value = naturalFromInteger (signum (naturalToInteger value))
  fromInteger = naturalFromInteger

instance Real Natural where
  toRational value = toRational (naturalToInteger value)

instance Integral Natural where
  quot left right = naturalFromInteger (quot (naturalToInteger left) (naturalToInteger right))
  rem left right = naturalFromInteger (rem (naturalToInteger left) (naturalToInteger right))
  div left right = naturalFromInteger (div (naturalToInteger left) (naturalToInteger right))
  mod left right = naturalFromInteger (mod (naturalToInteger left) (naturalToInteger right))
  quotRem left right = naturalPair (quotRem (naturalToInteger left) (naturalToInteger right))
  divMod left right = naturalPair (divMod (naturalToInteger left) (naturalToInteger right))
  toInteger = naturalToInteger

naturalPair :: (Integer, Integer) -> (Natural, Natural)
naturalPair (left, right) = (naturalFromInteger left, naturalFromInteger right)

instance Enum Natural where
  succ value = value + naturalOne
  pred value = value - naturalOne
  toEnum value = naturalFromInteger (toInteger value)
  fromEnum value = fromInteger (naturalToInteger value)
  enumFrom first = naturalsFromThen (naturalToInteger first) (naturalToInteger first + 1)
  enumFromThen first second = naturalsFromThen (naturalToInteger first) (naturalToInteger second)
  enumFromTo first last = naturalsFromThenTo (naturalToInteger first) (naturalToInteger first + 1) (naturalToInteger last)
  enumFromThenTo first second last = naturalsFromThenTo (naturalToInteger first) (naturalToInteger second) (naturalToInteger last)

naturalsFromThen :: Integer -> Integer -> [Natural]
naturalsFromThen first second =
  case second >= first of
    True -> naturalFromInteger first : naturalsFromThen second (second + (second - first))
    False -> naturalsFromThenTo first second 0

naturalsFromThenTo :: Integer -> Integer -> Integer -> [Natural]
naturalsFromThenTo first second last = go first
  where
    step = second - first

    go value =
      case step >= 0 of
        True ->
          case value <= last of
            True -> naturalFromInteger value : go (value + step)
            False -> []
        False ->
          case value >= last && value >= 0 of
            True -> naturalFromInteger value : go (value + step)
            False -> []

instance Read Natural where
  readPrec = do
    value <- readPrec :: ReadPrec Integer
    case value < 0 of
      True -> pfail
      False -> return (naturalFromInteger value)

instance Bits Natural where
  left .&. right = naturalFromInteger (naturalToInteger left .&. naturalToInteger right)
  left .|. right = naturalFromInteger (naturalToInteger left .|. naturalToInteger right)
  xor left right = naturalFromInteger (xor (naturalToInteger left) (naturalToInteger right))
  complement _ = underflow
  shift value amount = naturalFromInteger (shift (naturalToInteger value) amount)
  rotate value amount = naturalFromInteger (rotate (naturalToInteger value) amount)
  zeroBits = naturalZero
  bit index = naturalFromInteger (bit index)
  testBit value = testBit (naturalToInteger value)
  bitSizeMaybe _ = Nothing
  bitSize _ = bitSize (0 :: Integer)
  isSigned _ = False
  shiftL value amount = naturalFromInteger (shiftL (naturalToInteger value) amount)
  unsafeShiftL value amount = naturalFromInteger (unsafeShiftL (naturalToInteger value) amount)
  shiftR value amount = naturalFromInteger (shiftR (naturalToInteger value) amount)
  unsafeShiftR value amount = naturalFromInteger (unsafeShiftR (naturalToInteger value) amount)
  popCount value = popCount (naturalToInteger value)
