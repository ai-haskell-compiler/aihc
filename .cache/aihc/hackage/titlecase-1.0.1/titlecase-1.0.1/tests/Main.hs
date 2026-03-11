module Main where

import qualified Property
import qualified Unit

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Property.tests, Unit.tests]
