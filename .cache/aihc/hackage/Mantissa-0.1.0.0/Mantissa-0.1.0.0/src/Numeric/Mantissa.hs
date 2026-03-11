{-#
    LANGUAGE
      BangPatterns,
      MagicHash,
      UnboxedTuples,
      UnliftedNewtypes
  #-}

-- | Real numbers in [0,1), represented as fixed-point reals stored in
-- a machine word.
--
-- `Fractional` would arguably be a better name, but is of course
-- already in use.
module Numeric.Mantissa (
  Mantissa(..),
  oneMinusE,
  mantissaFromWord,
  mantissaToWord,
  mantissaToFractional,
  ) where

import Data.Ratio ((%), numerator, denominator)
import GHC.Num (integerToWord, integerFromWord)
import GHC.Exts (Word(..), isTrue#, not#)
import Numeric.Mantissa.Unboxed


-- | A real number in [0,1), represented as a boxed word
data Mantissa = M# Mantissa#

-- | The largest possible mantissa, just a tiny bit less than one
oneMinusE :: Mantissa
oneMinusE = M# (Mantissa# (not# 0##))

instance Num Mantissa where

  -- | Addition is modulo 1
  M# a + M# b = M# (plusMod1Mantissa# a b)
  
  -- | Subtraction is modulo 1
  M# a - M# b = M# (minusMod1Mantissa# a b)

  M# a * M# b = M# (timesMantissa# a b)

  -- | All mantissas are positive, so `abs` is the identity function
  abs = id

  -- | The only integer representable as a mantissa is 0
  fromInteger 0 = M# (Mantissa# 0##)
  fromInteger _ = error "fromInteger: can only convert 0 to mantissa"

  -- | Note that `signum` does not return 1 for positive numbers,
  -- since 1 is not available
  signum 0 = 0
  signum _ = oneMinusE

instance Eq Mantissa where
  M# a == M# b = isTrue# (eqMantissa# a b)

instance Ord Mantissa where
  M# a < M# b = isTrue# (ltMantissa# a b)
  M# a > M# b = isTrue# (gtMantissa# a b)
  M# a <= M# b = isTrue# (leMantissa# a b)
  M# a >= M# b = isTrue# (geMantissa# a b)

mantissaFromWord :: Word -> Mantissa
mantissaFromWord (W# w) = M# (Mantissa# w)

mantissaToWord :: Mantissa -> Word
mantissaToWord (M# (Mantissa# w)) = W# w

-- | The unsigned integer stored in a mantissa ("what multiple of the
-- smallest possible mantissa is this?"). This is used as a step in
-- some more mathematically meaningful functionality.
mantissaSerial :: Mantissa -> Integer
mantissaSerial = integerFromWord . mantissaToWord

-- | One more than the largest integer in a word (2^64 on a 64-bit
-- machine, for example).
--
-- I'd be interested to know if there's a more idiomatic way of doing
-- this.
wordMultiple :: Integer
wordMultiple = 1 + mantissaSerial oneMinusE

instance Fractional Mantissa where

  -- | The user is responsible for ensuring that, when they take a/b,
  -- then a is less than b.
  M# a / M# b = M# (quotMantissa# a b)

  recip _ = error "recip: cannot take reciprocal of a mantissa"

  -- | The user is responsible for ensuring that, when they use
  -- fromRational on a fraction, the numerator is less than the
  -- denominator (and both are positive).
  fromRational u = let
    q = quot (wordMultiple * numerator u) (denominator u)
    in mantissaFromWord (integerToWord q)

-- | Convert a mantissa to any Fractional type, implemented using
-- `fromRational`
mantissaToFractional :: Fractional a => Mantissa -> a
mantissaToFractional m = fromRational (mantissaSerial m % wordMultiple)
