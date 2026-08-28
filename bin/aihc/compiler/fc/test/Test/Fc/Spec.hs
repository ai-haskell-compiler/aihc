module Test.Fc.Spec (tests) where

import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcFixtureTests, fcGoldenTests, fcLintTests)
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  fc <- fcFixtureTests
  fcLint <- fcLintTests
  fcGolden <- fcGoldenTests
  pure (testGroup "aihc-fc" [fc, fcLint, fcGolden, fcPropertyTests])
