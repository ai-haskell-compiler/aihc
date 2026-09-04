module Test.Arm64.Spec (tests) where

import Test.Arm64.LirSuite qualified as LirSuite
import Test.Arm64.Suite qualified as Suite
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  lir <- LirSuite.tests
  pure (testGroup "aihc-arm64" [Suite.tests, lir])
