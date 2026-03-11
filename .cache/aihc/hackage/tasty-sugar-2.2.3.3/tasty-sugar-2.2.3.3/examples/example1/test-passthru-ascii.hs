module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Sugar

import NiftyText


cube = mkCUBE { inputDirs = [ "examples/example1/testdata" ] }


main = do sweets <- findSugar cube
          defaultMain . testGroup "passthru ascii tests" =<<
            withSugarGroups sweets testGroup test_passthru_ascii


test_passthru_ascii sweet cnt exp = do
  inp <- readFile $ rootFile sweet
  return [ testCase ("checking #" <> show cnt <> ": " <> expectedFile exp) $ do
             let testout = NiftyText.processText "passthru" "ascii" inp
             out <- readFile $ expectedFile exp
             out @=? testout
         ]
