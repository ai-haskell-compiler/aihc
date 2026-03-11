{-# LANGUAGE RebindableSyntax #-}
{- |
Lazy evaluation allows for the solution
 of differential equations in terms of power series.
Whenever you can express the highest derivative of the solution
 as explicit expression of the lower derivatives
 where each coefficient of the solution series
 depends only on lower coefficients,
 the recursive algorithm will work.
-}

module MathObj.PowerSeries.DifferentialEquation where

import qualified MathObj.PowerSeries.Core    as PS
import qualified MathObj.PowerSeries.Example as PSE

import qualified Algebra.Field        as Field
import qualified Algebra.ZeroTestable as ZeroTestable

import NumericPrelude.Numeric
import NumericPrelude.Base


{- |
Example for a linear equation:
   Setup a differential equation for @y@ with

>    y   t = (exp (-t)) * (sin t)
>    y'  t = -(exp (-t)) * (sin t) + (exp (-t)) * (cos t)
>    y'' t = -2 * (exp (-t)) * (cos t)

Thus the differential equation

>    y'' = -2 * (y' + y)

holds.

The following function generates
a power series for @exp (-t) * sin t@
by solving the differential equation.
-}

solveDiffEq0 :: (Field.C a) => [a]
solveDiffEq0 =
   let -- the initial conditions are passed to "PS.integrate"
       y   = PS.integrate 0 y'
       y'  = PS.integrate 1 y''
       y'' = PS.scale (-2) (PS.add y' y)
   in  y

verifyDiffEq0 :: (Field.C a) => [a]
verifyDiffEq0 =
   PS.mul (zipWith (*) (iterate negate 1) PSE.exp) PSE.sin

propDiffEq0 :: Bool
propDiffEq0 =  solveDiffEq0 == (verifyDiffEq0 :: [Rational])


{- |
We are not restricted to linear equations!
 Let the solution be y with
  y   t =   (1-t)^-1
  y'  t =   (1-t)^-2
  y'' t = 2*(1-t)^-3
 then it holds
  y'' = 2 * y' * y
-}

solveDiffEq1 :: (ZeroTestable.C a, Field.C a) => [a]
solveDiffEq1 =
   let -- the initial conditions are passed to "PS.integrate"
       y   = PS.integrate 1 y'
       y'  = PS.integrate 1 y''
       y'' = PS.scale 2 (PS.mul y' y)
   in  y

verifyDiffEq1 :: (ZeroTestable.C a, Field.C a) => [a]
verifyDiffEq1 = PS.divide [1] [1, -1]

propDiffEq1 :: Bool
propDiffEq1 =  solveDiffEq1 == (verifyDiffEq1 :: [Rational])
