{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Test.Utility where

import qualified Numeric.Netlib.Class as Class
import Numeric.BLAS.Scalar (RealOf, absolute)

import qualified Test.QuickCheck as QC


approx ::
   (Class.Floating a, RealOf a ~ ar, Class.Real ar, Show a) =>
   ar -> a -> a -> QC.Property
approx tol x y =
   QC.counterexample (show (x,y)) $ absolute (x-y) <= tol

approxReal :: (Class.Real a, Show a) => a -> a -> a -> QC.Property
approxReal tol x y =
   QC.counterexample (show (x,y)) $ abs (x-y) <= tol
