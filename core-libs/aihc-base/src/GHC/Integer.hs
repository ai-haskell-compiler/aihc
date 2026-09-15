{-# LANGUAGE UnboxedTuples #-}

-- | The legacy @integer-gmp@ interface to 'Integer'.  Packages still using it
-- get the operations from here; new code should use "GHC.Num.Integer".
module GHC.Integer
  ( Integer,
    divInteger,
    divModInteger,
    modInteger,
    quotInteger,
    quotRemInteger,
    remInteger,
  )
where

import GHC.Internal.Integer (Integer)
import GHC.Real (div, divMod, mod, quot, quotRem, rem)

-- | Truncating division, rounding towards zero.
quotInteger :: Integer -> Integer -> Integer
quotInteger = quot

-- | The remainder of 'quotInteger'.
remInteger :: Integer -> Integer -> Integer
remInteger = rem

-- | 'quotInteger' and 'remInteger' paired.
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger numerator denominator =
  case quotRem numerator denominator of
    (quotient, remainder) -> (# quotient, remainder #)

-- | Integer division, rounding towards negative infinity.
divInteger :: Integer -> Integer -> Integer
divInteger = div

-- | The modulus of 'divInteger'.
modInteger :: Integer -> Integer -> Integer
modInteger = mod

-- | 'divInteger' and 'modInteger' paired.
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger numerator denominator =
  case divMod numerator denominator of
    (quotient, modulus) -> (# quotient, modulus #)
