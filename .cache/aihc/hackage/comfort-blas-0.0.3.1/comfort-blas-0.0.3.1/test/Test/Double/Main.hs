-- Do not edit! Automatically created with doctest-extract.
module Test.Double.Main where

import qualified Test.Double.Numeric.BLAS.Matrix.RowMajor
import qualified Test.Double.Numeric.BLAS.Vector.Slice
import qualified Test.Double.Numeric.BLAS.Vector

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Test.Double.Numeric.BLAS.Matrix.RowMajor.test
    Test.Double.Numeric.BLAS.Vector.Slice.test
    Test.Double.Numeric.BLAS.Vector.test
