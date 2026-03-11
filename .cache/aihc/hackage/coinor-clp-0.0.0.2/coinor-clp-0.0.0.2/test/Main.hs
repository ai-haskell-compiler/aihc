-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Numeric.COINOR.CLP
import qualified Test.Numeric.COINOR.CLP.Monad

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Numeric.COINOR.CLP.test
    Test.Numeric.COINOR.CLP.Monad.test
