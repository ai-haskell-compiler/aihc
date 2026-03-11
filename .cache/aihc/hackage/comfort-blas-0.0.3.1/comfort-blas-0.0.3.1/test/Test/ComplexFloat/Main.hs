-- Do not edit! Automatically created with doctest-extract.
module Test.ComplexFloat.Main where

import qualified Test.ComplexFloat.Numeric.BLAS.Matrix.RowMajor
import qualified Test.ComplexFloat.Numeric.BLAS.Vector.Slice
import qualified Test.ComplexFloat.Numeric.BLAS.Vector

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Test.ComplexFloat.Numeric.BLAS.Matrix.RowMajor.test
    Test.ComplexFloat.Numeric.BLAS.Vector.Slice.test
    Test.ComplexFloat.Numeric.BLAS.Vector.test
