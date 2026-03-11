{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.TH
import Test.Tasty.HUnit
import Data.List (sort)

main :: IO ()
main = $(defaultMainGenerator)

case_example_test_functions :: Assertion
case_example_test_functions = do
  functions <- extractTestFunctions "example.hs"
  let expected = [ "prop_length_append", "case_length_1", "test_plus" ]
  sort expected @=? sort functions

case_example_explicit_test_functions :: Assertion
case_example_explicit_test_functions = do
  functions <- extractTestFunctions "example-explicit.hs"
  let expected = [ "case_add", "prop_length_append", "case_length_1", "test_plus" ]
  sort expected @=? sort functions

case_example_literate_test_functions :: Assertion
case_example_literate_test_functions = do
  functions <- extractTestFunctions "example-literate.lhs"
  let expected = [ "prop_length_append", "case_length_1", "test_plus" ]
  sort expected @=? sort functions
