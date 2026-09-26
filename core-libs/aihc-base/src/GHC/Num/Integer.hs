{-# LANGUAGE MagicHash #-}

module GHC.Num.Integer
  ( Integer (..),
    integerFromBigNat#,
    integerToBigNatClamp#,
    integerLog2#,
    integerLogBase#,
  )
where

import GHC.Internal.Integer (Integer (..), integerFromMagnitude#, integerLog2#, integerLogBase#)
import GHC.Num.BigNat (BigNat (..), BigNat#, bigNatFromWord#, bigNatZero)
import GHC.Prim (int2Word#, (<#))

-- | The non-negative 'Integer' with a magnitude.
integerFromBigNat# :: BigNat# -> Integer
integerFromBigNat# = integerFromMagnitude# 1#

-- | The magnitude of a non-negative 'Integer', and zero for a negative one.
integerToBigNatClamp# :: Integer -> BigNat#
integerToBigNatClamp# value =
  case value of
    IP magnitude -> magnitude
    IS small ->
      case small <# 0# of
        1# -> case bigNatZero of BN# zero -> zero
        _ -> bigNatFromWord# (int2Word# small)
    IN _ -> case bigNatZero of BN# zero -> zero
