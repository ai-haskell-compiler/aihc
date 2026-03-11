-- Do not edit! Automatically created with doctest-extract.
module Test.Float.Main where

import qualified Test.Float.Numeric.BLAS.Matrix.RowMajor
import qualified Test.Float.Numeric.BLAS.Vector.Slice
import qualified Test.Float.Numeric.BLAS.Vector

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Test.Float.Numeric.BLAS.Matrix.RowMajor.test
    Test.Float.Numeric.BLAS.Vector.Slice.test
    Test.Float.Numeric.BLAS.Vector.test
