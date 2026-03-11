-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Math.Apportionment
import qualified Test.QuickCheck as QC

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.runWith QC.stdArgs{QC.maxSuccess=3000} $ do
    Test.Math.Apportionment.test
