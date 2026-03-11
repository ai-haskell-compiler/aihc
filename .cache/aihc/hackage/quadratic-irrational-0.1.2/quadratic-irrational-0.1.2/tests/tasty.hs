module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified CyclicList
import qualified QuadraticIrrational

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "quadratic-irrational"
    [ CyclicList.tests
    , QuadraticIrrational.tests
    ]
