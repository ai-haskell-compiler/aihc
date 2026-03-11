module Main where

import qualified Test.Numeric.BLAS.Slice as Slice

import qualified Test.Float.Main as TestFloat
import qualified Test.Double.Main as TestDouble
import qualified Test.ComplexFloat.Main as TestComplexFloat
import qualified Test.ComplexDouble.Main as TestComplexDouble

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
   DocTest.printLine "\nSlice"
   Slice.test
   DocTest.printLine "\nFloat"
   TestFloat.main
   DocTest.printLine "\nDouble"
   TestDouble.main
   DocTest.printLine "\nComplex Float"
   TestComplexFloat.main
   DocTest.printLine "\nComplex Double"
   TestComplexDouble.main
