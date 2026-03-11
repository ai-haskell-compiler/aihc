{- |
Generate lists of basis functions with respect to interpolation nodes
and generate functions from coefficients with respect to these bases.

A basis function is one where all but one features are zero.
E.g. in a linear basis a basis function is one at one node,
and zero at all the other interpolation nodes.

You need the basis functions
for setting up the matrix for a linear least-squares solver for curve fitting.
The solver computes some coefficients
and in a second step you convert these coefficients
to the piecewise interpolation function.
-}
module Numeric.Interpolation.Basis (
   -- * Interpolation basis functions
   Compact.linear,
   Compact.hermite1,
   Compact.cubicLinear,
   Compact.cubicParabola,
   -- * Construct functions from the coefficients with respect to a basis
   coefficientsToLinear,
   coefficientsToHermite1,
   coefficientsToCubicLinear,
   coefficientsToCubicParabola,
   ) where

import qualified Numeric.Interpolation.Basis.Compact as Compact
import qualified Numeric.Interpolation.NodeList as Nodes
import Numeric.Interpolation.Private.Basis
          (parabolaDerivativeCenterNode, hermite1Split)
import Numeric.Interpolation.Private.List (mapAdjacent3, )


{- |
@coefficientsToLinear nodes coefficients@
creates an interpolation function for @nodes@,
where the @coefficients@ correspond to the basis functions
constructed with @Basis.linear nodes@.
-}
coefficientsToLinear :: [a] -> [b] -> Nodes.T a b
coefficientsToLinear xs = Nodes.fromList . zip xs

{- |
Cf. 'coefficientsToLinear'
-}
coefficientsToHermite1 :: [a] -> [b] -> Nodes.T a (b, b)
coefficientsToHermite1 xs =
   Nodes.fromList . zip xs . hermite1Split xs



{- |
Cf. 'coefficientsToLinear'
-}
coefficientsToCubicLinear :: (Fractional a) => [a] -> [a] -> Nodes.T a (a, a)
coefficientsToCubicLinear xs =
   Nodes.fromList .
   mapAdjacent3 (\(xl,yl) (xn,yn) (xr,yr) -> (xn, (yn, (yr-yl)/(xr-xl)))) .
   zip xs

{- |
Cf. 'coefficientsToLinear'
-}
coefficientsToCubicParabola :: (Fractional a) => [a] -> [a] -> Nodes.T a (a, a)
coefficientsToCubicParabola xs =
   Nodes.fromList .
   mapAdjacent3
      (\pl pn@(xn,yn) pr ->
         (xn, (yn, parabolaDerivativeCenterNode pl pn pr))) .
   zip xs
