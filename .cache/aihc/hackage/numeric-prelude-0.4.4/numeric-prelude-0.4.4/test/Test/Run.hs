-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Algebra.Additive
import qualified Test.Algebra.IntegralDomain
import qualified Test.Algebra.PrincipalIdealDomain
import qualified Test.Algebra.RealRing
import qualified Test.MathObj.Gaussian.Bell
import qualified Test.MathObj.Gaussian.Polynomial
import qualified Test.MathObj.Gaussian.ExponentTuple
import qualified Test.MathObj.Gaussian.Variance
import qualified Test.MathObj.Matrix
import qualified Test.MathObj.PartialFraction
import qualified Test.MathObj.Polynomial
import qualified Test.MathObj.Polynomial.Core
import qualified Test.MathObj.PowerSeries
import qualified Test.MathObj.PowerSeries.Core
import qualified Test.MathObj.PowerSeries.Example
import qualified Test.MathObj.RefinementMask2
import qualified Test.Number.ComplexSquareRoot
import qualified Test.Number.GaloisField2p32m5

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Algebra.Additive.test
    Test.Algebra.IntegralDomain.test
    Test.Algebra.PrincipalIdealDomain.test
    Test.Algebra.RealRing.test
    Test.MathObj.Gaussian.Bell.test
    Test.MathObj.Gaussian.Polynomial.test
    Test.MathObj.Gaussian.ExponentTuple.test
    Test.MathObj.Gaussian.Variance.test
    Test.MathObj.Matrix.test
    Test.MathObj.PartialFraction.test
    Test.MathObj.Polynomial.test
    Test.MathObj.Polynomial.Core.test
    Test.MathObj.PowerSeries.test
    Test.MathObj.PowerSeries.Core.test
    Test.MathObj.PowerSeries.Example.test
    Test.MathObj.RefinementMask2.test
    Test.Number.ComplexSquareRoot.test
    Test.Number.GaloisField2p32m5.test
