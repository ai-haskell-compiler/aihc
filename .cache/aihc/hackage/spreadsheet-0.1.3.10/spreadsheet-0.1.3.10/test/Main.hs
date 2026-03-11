-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Data.Spreadsheet

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Data.Spreadsheet.test
