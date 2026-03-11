-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Data.NonEmptyPrivate
import qualified Test.Data.NonEmpty.Map

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Data.NonEmptyPrivate.test
    Test.Data.NonEmpty.Map.test
