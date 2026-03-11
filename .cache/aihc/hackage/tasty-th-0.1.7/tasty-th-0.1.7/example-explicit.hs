{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main :: IO ()
main = $(defaultMainGeneratorFor "explicit" ["prop_length_append", "case_length_1", "test_plus"])

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]

case_add :: Assertion
case_add = 7 @=? (3 + 4)

test_plus :: [TestTree]
test_plus =
  [ $(testGroupGeneratorFor "case_add" ["case_add"])
    -- ...
  ]
