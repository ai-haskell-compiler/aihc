module Main (main) where

import Test.Fc2.Properties (fc2PropertyTests)
import Test.Fc2.Suite (fc2FixtureTests, fc2GoldenTests, fc2LintTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  fc2 <- fc2FixtureTests
  fc2Lint <- fc2LintTests
  fc2Golden <- fc2GoldenTests
  defaultMain (testGroup "aihc-fc" [fc2, fc2Lint, fc2Golden, fc2PropertyTests])
