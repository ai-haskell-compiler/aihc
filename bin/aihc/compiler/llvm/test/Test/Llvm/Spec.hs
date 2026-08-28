module Test.Llvm.Spec (tests) where

import Test.Llvm.Suite qualified as Suite
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "aihc-llvm"
    [Suite.tests]
