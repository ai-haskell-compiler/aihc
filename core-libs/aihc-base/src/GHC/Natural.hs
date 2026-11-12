{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE ViewPatterns #-}

-- | The legacy interface to 'Natural'.
module GHC.Natural
  ( BigNat (..),
    Natural (NatS#, NatJ#),
    isValidNatural,

    -- * Arithmetic
    plusNatural,
    minusNatural,
    minusNaturalMaybe,
    timesNatural,
    negateNatural,
    signumNatural,
    quotRemNatural,
    quotNatural,
    remNatural,
    gcdNatural,
    lcmNatural,

    -- * Bits
    andNatural,
    orNatural,
    xorNatural,
    bitNatural,
    testBitNatural,
    popCountNatural,
    shiftLNatural,
    shiftRNatural,

    -- * Conversions
    naturalToInteger,
    naturalToWord,
    naturalToWordMaybe,
    naturalFromInteger,
    wordToNatural,
    wordToNatural#,

    -- * Modular arithmetic
    powModNatural,
  )
where

import Data.Bits (Bits (..))
import GHC.Exception (ArithException (..), throw)
import GHC.Num.BigNat (BigNat (..))
import GHC.Num.Integer (integerFromNatural, integerPowMod#, integerToNatural)
import GHC.Num.Natural (Natural (..), naturalCheck)
import GHC.Prim (Int#, Word#)
import GHC.Types (Bool, Int (I#))
import Prelude

pattern NatS# :: Word# -> Natural
pattern NatS# word = NS word

pattern NatJ# :: BigNat -> Natural
pattern NatJ# magnitude <- NB (BN# -> magnitude)
  where
    NatJ# (BN# magnitude) = NB magnitude

{-# COMPLETE NatS#, NatJ# #-}

-- | Compute a power modulo a natural number.
powModNatural :: Natural -> Natural -> Natural -> Natural
powModNatural base exponent modulus =
  case integerPowMod# (integerFromNatural base) (integerFromNatural exponent) modulus of
    (# result | #) -> result
    (# | () #) -> throw DivideByZero

-- | Whether a natural number has the canonical representation.
isValidNatural :: Natural -> Bool
isValidNatural = naturalCheck

-- | Add two natural numbers.
plusNatural :: Natural -> Natural -> Natural
plusNatural = (+)

-- | Subtract two natural numbers. A negative result throws 'Underflow'.
minusNatural :: Natural -> Natural -> Natural
minusNatural = (-)

-- | Subtract two natural numbers. A negative result gives 'Nothing'.
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe left right
  | left < right = Nothing
  | otherwise = Just (left - right)

-- | Multiply two natural numbers.
timesNatural :: Natural -> Natural -> Natural
timesNatural = (*)

-- | Negate a natural number. A value that is not zero throws 'Underflow'.
negateNatural :: Natural -> Natural
negateNatural = negate

-- | The sign of a natural number: zero or one.
signumNatural :: Natural -> Natural
signumNatural = signum

-- | The quotient and the remainder of two natural numbers.
quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural = quotRem

-- | The quotient of two natural numbers.
quotNatural :: Natural -> Natural -> Natural
quotNatural = quot

-- | The remainder of two natural numbers.
remNatural :: Natural -> Natural -> Natural
remNatural = rem

-- | The greatest common divisor of two natural numbers.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural = gcd

-- | The least common multiple of two natural numbers.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural = lcm

-- | The bitwise AND of two natural numbers.
andNatural :: Natural -> Natural -> Natural
andNatural = (.&.)

-- | The bitwise OR of two natural numbers.
orNatural :: Natural -> Natural -> Natural
orNatural = (.|.)

-- | The bitwise exclusive OR of two natural numbers.
xorNatural :: Natural -> Natural -> Natural
xorNatural = xor

-- | The natural number with only the given bit set.
bitNatural :: Int# -> Natural
bitNatural index = bit (I# index)

-- | Whether the given bit of a natural number is set.
testBitNatural :: Natural -> Int -> Bool
testBitNatural = testBit

-- | The number of set bits of a natural number.
popCountNatural :: Natural -> Int
popCountNatural = popCount

-- | Shift a natural number to the left.
shiftLNatural :: Natural -> Int -> Natural
shiftLNatural = shiftL

-- | Shift a natural number to the right.
shiftRNatural :: Natural -> Int -> Natural
shiftRNatural = shiftR

-- | The 'Integer' with the same value.
naturalToInteger :: Natural -> Integer
naturalToInteger = integerFromNatural

-- | The low word of a natural number. A larger value wraps around.
naturalToWord :: Natural -> Word
naturalToWord = fromIntegral

-- | The 'Word' with the same value, if the value fits in a word.
naturalToWordMaybe :: Natural -> Maybe Word
naturalToWordMaybe value
  | value <= fromIntegral (maxBound :: Word) = Just (fromIntegral value)
  | otherwise = Nothing

-- | The absolute value of an 'Integer' as a 'Natural'. GHC gives the same
-- result for a negative argument.
naturalFromInteger :: Integer -> Natural
naturalFromInteger = integerToNatural

-- | The 'Natural' with the same value as a 'Word'.
wordToNatural :: Word -> Natural
wordToNatural = fromIntegral

-- | The 'Natural' with the same value as a 'Word'.
wordToNatural# :: Word -> Natural
wordToNatural# = fromIntegral
