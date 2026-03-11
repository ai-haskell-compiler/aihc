-- Do not edit! Automatically created with doctest-extract.
module Test.ComplexDouble.Main where

import qualified Test.ComplexDouble.Numeric.BLAS.Matrix.RowMajor
import qualified Test.ComplexDouble.Numeric.BLAS.Vector.Slice
import qualified Test.ComplexDouble.Numeric.BLAS.Vector

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Test.ComplexDouble.Numeric.BLAS.Matrix.RowMajor.test
    Test.ComplexDouble.Numeric.BLAS.Vector.Slice.test
    Test.ComplexDouble.Numeric.BLAS.Vector.test
