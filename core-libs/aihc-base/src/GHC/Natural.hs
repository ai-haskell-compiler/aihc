{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE ViewPatterns #-}

-- | The legacy interface to 'Natural'.
module GHC.Natural
  ( BigNat (..),
    Natural (NatS#, NatJ#),
    powModNatural,
    isValidNatural,
    naturalFromInteger,
  )
where

import GHC.Exception (ArithException (..), throw)
import GHC.Num.BigNat (BigNat (..))
import GHC.Num.Integer (Integer, integerFromNatural, integerPowMod#, integerToNatural)
import GHC.Num.Natural (Natural (..), naturalCheck)
import GHC.Prim (Word#)
import GHC.Types (Bool)

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

-- | The 'Natural' with the absolute value of an 'Integer', as in GHC.
naturalFromInteger :: Integer -> Natural
naturalFromInteger = integerToNatural
