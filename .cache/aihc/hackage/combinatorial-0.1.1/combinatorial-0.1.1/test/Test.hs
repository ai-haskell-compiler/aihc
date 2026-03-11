-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Combinatorics
import qualified Test.Combinatorics.Private
import qualified Test.Combinatorics.BellNumbers
import qualified Test.Combinatorics.CardPairs
import qualified Test.Combinatorics.Mastermind
import qualified Test.Combinatorics.Partitions
import qualified Test.Combinatorics.Permutation.WithoutSomeFixpoints

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Combinatorics.test
    Test.Combinatorics.Private.test
    Test.Combinatorics.BellNumbers.test
    Test.Combinatorics.CardPairs.test
    Test.Combinatorics.Mastermind.test
    Test.Combinatorics.Partitions.test
    Test.Combinatorics.Permutation.WithoutSomeFixpoints.test
