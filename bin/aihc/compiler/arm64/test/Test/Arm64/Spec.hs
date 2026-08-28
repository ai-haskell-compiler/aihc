module Test.Arm64.Spec (tests) where

import Test.Arm64.Suite qualified as Suite
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "aihc-arm64"
    [Suite.tests]
