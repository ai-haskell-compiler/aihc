{-# LANGUAGE RebindableSyntax #-}
module Main where

import Number.Complex((+:), (-:), )
import qualified Number.Complex as Complex
import Number.Physical      as Value
import Number.SI            as SIValue -- units
import Number.SI.Unit       as SIUnit  -- unit prefixes
          (pico, nano, micro, milli, centi, deci,
           deca, hecto, kilo, mega, giga, tera, peta)
import Number.OccasionallyScalarExpression as Expr

import qualified Number.NonNegativeChunky as Chunky
import qualified Number.NonNegative       as NonNegW
import qualified Number.Positional.Check  as Real
import qualified Number.FixedPoint.Check  as FixedPoint
import qualified Number.ResidueClass.Func as ResidueClass
import qualified Number.Peano             as Peano

import qualified MathObj.Polynomial          as Polynomial
import qualified MathObj.LaurentPolynomial   as LaurentPolynomial
import qualified MathObj.PowerSeries         as PowerSeries
import qualified MathObj.PowerSeries.Example as PowerSeriesExample
import qualified MathObj.PartialFraction     as PartialFraction

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Field                as Field
import qualified Algebra.ZeroTestable         as ZeroTestable
import qualified Algebra.Indexable            as Indexable

import Data.List (genericTake, genericLength)

import NumericPrelude.Base
import NumericPrelude.Numeric


{- * Physical units -}

-- some shorthands for common usage
type SIDouble  = SIValue.T Double Double
type SIComplex = SIValue.T Double (Complex.T Double)

{- this advice seems not to be targeted to ghc's interactive mode
default (SIDouble)
-}




test :: [SIDouble]
test =
   let lengthScales = map (\n->10^-n*meter) [-10..6]
       areaScales = map (\n->10^-n*meter^2) [-10..6]
   in  lengthScales ++ map recip lengthScales ++
       areaScales   ++ map recip areaScales ++
       map ((meter*gramm/second)^-) [-5..5] ++
       take 16 (iterate (10*) (10e-10*meter/gramm)) ++
       [1/meter^2, 1/meter, meter, meter^2,
        second, hertz,
        meter*second, second/meter, meter/second, 1/meter/second,
        volt/meter,newton/meter,
        gramm]

testComplex :: SIComplex
testComplex = (2 :: Double) *> (SIValue.fromScalarSingle (3+:4)*milli*second)

testMagnitude :: SIDouble
testMagnitude = SIValue.lift (Value.lift Complex.magnitude) testComplex

testExpr :: Expr.T Double SIDouble
testExpr = sin (5 / (3+1) * fromValue meter)

testPrefixes :: [SIDouble]
testPrefixes =
   [pico, nano, micro, milli, centi, deci,
    deca, hecto, kilo, mega, giga, tera, peta]


{- * Reals -}

testReal :: String
testReal = Real.defltShow (sqrt 2 + log 2 * pi)

testComplexReal :: Complex.T Real.T
testComplexReal = exp (0 +: pi) + exp (0 -: pi)

showReal :: Real.T -> String
showReal = Real.defltShow


{- * Fixed point numbers -}

testFixedPoint :: String
testFixedPoint = FixedPoint.defltShow (sqrt 2 + log 2 * pi)

showFixedPoint :: FixedPoint.T -> String
showFixedPoint = FixedPoint.defltShow


{- * Residue classes -}

testResidueClass :: Integer
testResidueClass = ResidueClass.concrete 7 (5*3/2)

polyResidueClass :: (ZeroTestable.C a, Field.C a) =>
   [a] -> ResidueClass.T (Polynomial.T a)
polyResidueClass = ResidueClass.fromRepresentative . polynomial

{- That's strange:
The residue class implementation should constantly compute mod
and thus should be much faster.
I assume that this is an effect of missing sharing.
The functions which represent a residue class are shared,
but not their values.

*Main> mod (3^3000000) 2 :: Integer
1
(2.47 secs, 24541080 bytes)
*Main> ResidueClass.concrete 2 (3^3000000) :: Integer
1
(7.33 secs, 515047072 bytes)
-}


{- * Polynomials and power series -}

polynomial :: [a] -> Polynomial.T a
polynomial = Polynomial.fromCoeffs

powerSeries :: [a] -> PowerSeries.T a
powerSeries = PowerSeries.fromCoeffs

laurentPolynomial :: Int -> [a] -> LaurentPolynomial.T a
laurentPolynomial = LaurentPolynomial.fromShiftCoeffs

tanSeries :: PowerSeries.T Rational
tanSeries = powerSeries PowerSeriesExample.tan


{- * Partial fractions -}

partialFraction :: (PID.C a, Indexable.C a) =>
   [a] -> a -> PartialFraction.T a
partialFraction = PartialFraction.fromFactoredFraction

{- |
An example from wavelet theory: lifting coefficients of the CDF wavelet family.
-}
cdfFraction :: PartialFraction.T (Polynomial.T Rational)
cdfFraction =
   partialFraction
      (map polynomial [[-4,1],[0,1],[4,1]])
      (product (map polynomial [[-2,1],[2,1]]))

{- |
The same example with different notation,
that relies on numerical literals being used for polynomials.
-}
cdfFractionNum :: PartialFraction.T (Polynomial.T Rational)
cdfFractionNum =
   let x = polynomial [0,1]
   in  partialFraction [x-4,x,x+4] ((x-2)*(x+2))


{- * Peano numbers -}
testPeano :: Peano.T
testPeano = minimum [Peano.infinity, 2, Peano.infinity, 4]

testPeanoList :: [Char]
testPeanoList =
   genericTake (genericLength (repeat 'a') :: Peano.T) ['a'..'z']

testChunky :: Chunky.T NonNegW.Int
testChunky = (2+3)*(1+5)


main :: IO ()
main = print test
