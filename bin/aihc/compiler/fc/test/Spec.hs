module Main (main) where

import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcFixtureTests, fcGoldenTests, fcLintTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  fc <- fcFixtureTests
  fcLint <- fcLintTests
  fcGolden <- fcGoldenTests
  defaultMain (testGroup "aihc-fc" [fc, fcLint, fcGolden, fcPropertyTests])
