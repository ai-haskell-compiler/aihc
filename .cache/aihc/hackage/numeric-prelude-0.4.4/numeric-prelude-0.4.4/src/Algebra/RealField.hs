{-# LANGUAGE RebindableSyntax #-}
module Algebra.RealField (
   C,
   ) where

import qualified Algebra.Field as Field
import qualified Algebra.RealRing as RealRing
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.ToInteger as ToInteger

import qualified Number.Ratio as Ratio

import Prelude (Float, Double, )

{- |
This is a convenient class for common types
that both form a field and have a notion of ordering by magnitude.
-}
class (RealRing.C a, Field.C a) => C a where

instance C Float where
instance C Double where

instance (ToInteger.C a, PID.C a) => C (Ratio.T a) where
