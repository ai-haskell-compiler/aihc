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
module Numeric.Mantissa.Unboxed (
  Mantissa#(..),
  eqMantissa#,
  neMantissa#,
  gtMantissa#,
  ltMantissa#,
  geMantissa#,
  leMantissa#,
  plusMod1Mantissa#,
  minusMod1Mantissa#,
  plusWithOverflowMantissa#,
  minusWithOverflowMantissa#,
  timesMantissa#,
  quotMantissa#,
  ) where

import GHC.Exts


-- | A real number in [0,1), represented as an unboxed word
newtype Mantissa# = Mantissa# Word#

eqMantissa# :: Mantissa# -> Mantissa# -> Int#
eqMantissa# (Mantissa# a) (Mantissa# b) = eqWord# a b

neMantissa# :: Mantissa# -> Mantissa# -> Int#
neMantissa# (Mantissa# a) (Mantissa# b) = neWord# a b

ltMantissa# :: Mantissa# -> Mantissa# -> Int#
ltMantissa# (Mantissa# a) (Mantissa# b) = ltWord# a b

gtMantissa# :: Mantissa# -> Mantissa# -> Int#
gtMantissa# (Mantissa# a) (Mantissa# b) = gtWord# a b

leMantissa# :: Mantissa# -> Mantissa# -> Int#
leMantissa# (Mantissa# a) (Mantissa# b) = leWord# a b

geMantissa# :: Mantissa# -> Mantissa# -> Int#
geMantissa# (Mantissa# a) (Mantissa# b) = geWord# a b

-- | Addition modulo 1
plusMod1Mantissa# :: Mantissa# -> Mantissa# -> Mantissa#
plusMod1Mantissa# (Mantissa# a) (Mantissa# b) = Mantissa# (plusWord# a b)

-- | Subtraction modulo 1
minusMod1Mantissa# :: Mantissa# -> Mantissa# -> Mantissa#
minusMod1Mantissa# (Mantissa# a) (Mantissa# b) = Mantissa# (minusWord# a b)

-- | The second Int# component is nonzero on overflow
plusWithOverflowMantissa# :: Mantissa# -> Mantissa# -> (# Mantissa#, Int# #)
plusWithOverflowMantissa# (Mantissa# a) (Mantissa# b) = let
  !(# c, i #) = addWordC# a b
  in (# Mantissa# c, i #)

-- | The second Int# component is nonzero on overflow
minusWithOverflowMantissa# :: Mantissa# -> Mantissa# -> (# Mantissa#, Int# #)
minusWithOverflowMantissa# (Mantissa# a) (Mantissa# b) = let
  !(# c, i #) = subWordC# a b
  in (# Mantissa# c, i #)

timesMantissa# :: Mantissa# -> Mantissa# -> Mantissa#
timesMantissa# (Mantissa# a) (Mantissa# b) = let
  !(# c, _ #) = timesWord2# a b
  in Mantissa# c

-- | The user is responsible for ensuring that, when they take
-- dividend/divisor, then the dividend is less than the divisor.
quotMantissa# :: Mantissa# -> Mantissa# -> Mantissa#
quotMantissa# (Mantissa# a) (Mantissa# b) = let
  !(# c, _ #) = quotRemWord2# a 0## b
  in Mantissa# c
