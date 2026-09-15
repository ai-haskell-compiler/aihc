{-# LANGUAGE UnboxedTuples #-}

-- | The legacy @integer-gmp@ operation names.
--
-- GHC keeps this module as a compatibility shim over the @Integer@
-- operations, and packages that predate @ghc-bignum@ still import it.
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
import GHC.Real (Integral (..))

quotInteger :: Integer -> Integer -> Integer
quotInteger = quot

remInteger :: Integer -> Integer -> Integer
remInteger = rem

divInteger :: Integer -> Integer -> Integer
divInteger = div

modInteger :: Integer -> Integer -> Integer
modInteger = mod

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger numerator denominator =
  case quotRem numerator denominator of
    (quotient, remainder) -> (# quotient, remainder #)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger numerator denominator =
  case divMod numerator denominator of
    (quotient, modulus) -> (# quotient, modulus #)
