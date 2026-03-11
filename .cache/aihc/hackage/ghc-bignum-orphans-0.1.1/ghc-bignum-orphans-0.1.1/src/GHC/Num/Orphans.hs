{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Exports orphan instances that mimic instances available in later versions of
@ghc-bignum@. To use them, simply @import GHC.Num.Orphans ()@.
-}
module GHC.Num.Orphans () where

#if !(MIN_VERSION_ghc_bignum(1,1,0))
import GHC.Num.BigNat
#endif

#if !(MIN_VERSION_ghc_bignum(1,1,0))
instance Eq BigNat where
   BN# a == BN# b = bigNatEq a b
   BN# a /= BN# b = bigNatNe a b

instance Ord BigNat where
   (BN# a) `compare` (BN# b) = bigNatCompare a b
   BN# a <  BN# b = bigNatLt a b
   BN# a <= BN# b = bigNatLe a b
   BN# a >  BN# b = bigNatGt a b
   BN# a >= BN# b = bigNatGe a b
#endif
