-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Numeric.GLPK
import qualified Test.Numeric.GLPK.Monad

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Numeric.GLPK.test
    Test.Numeric.GLPK.Monad.test
