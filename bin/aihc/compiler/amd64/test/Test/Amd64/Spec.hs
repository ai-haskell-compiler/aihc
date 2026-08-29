module Test.Amd64.Spec (tests) where

import Test.Amd64.Suite qualified as Suite
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "aihc-amd64"
    [Suite.tests]
