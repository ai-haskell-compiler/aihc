{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main :: IO ()
main = $(defaultMainGenerator)

{-
Properties in comments are not run:

prop_comment :: Assertion
prop_comment = assertFailure "property in comment should not be run"
-}

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]

test_plus :: [TestTree]
test_plus =
  [ testCase "3 + 4" (7 @=? (3 + 4))
    -- ...
  ]
