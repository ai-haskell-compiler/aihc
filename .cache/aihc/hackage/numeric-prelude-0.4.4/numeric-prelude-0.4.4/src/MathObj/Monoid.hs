{-# LANGUAGE RebindableSyntax #-}
module MathObj.Monoid where

import qualified Algebra.PrincipalIdealDomain as PID

import Algebra.PrincipalIdealDomain (gcd, lcm, )
import Algebra.Additive (zero, )
import Algebra.Monoid (C, idt, (<*>), )

import NumericPrelude.Base

{- |
It is only a monoid for non-negative numbers.

> idt <*> GCD (-2) = GCD 2

Thus, use this Monoid only for non-negative numbers!
-}
newtype GCD a = GCD {runGCD :: a}
   deriving (Show, Eq)

instance PID.C a => C (GCD a) where
   idt = GCD zero
   (GCD x) <*> (GCD y) = GCD (gcd x y)


newtype LCM a = LCM {runLCM :: a}
   deriving (Show, Eq)

instance PID.C a => C (LCM a) where
   idt = LCM zero
   (LCM x) <*> (LCM y) = LCM (lcm x y)


{- |
@Nothing@ is the largest element.
-}
newtype Min a = Min {runMin :: Maybe a}
   deriving (Show, Eq)

instance Ord a => C (Min a) where
   idt = Min Nothing
   (Min x) <*> (Min y) = Min $
      maybe y (\x' -> maybe x (Just . min x') y) x


{- |
@Nothing@ is the smallest element.
-}
newtype Max a = Max {runMax :: Maybe a}
   deriving (Show, Eq)

instance Ord a => C (Max a) where
   idt = Max Nothing
   (Max x) <*> (Max y) = Max $
      maybe y (\x' -> maybe x (Just . max x') y) x
