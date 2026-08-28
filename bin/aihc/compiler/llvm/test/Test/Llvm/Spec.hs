module Test.Llvm.Spec (tests) where

import Hedgehog (Property, property, success)
import Test.Llvm.Suite qualified as Suite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "aihc-llvm"
    [ Suite.tests,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the workspace-wide Hedgehog controls accepted by this suite.
prop_dummy :: Property
prop_dummy = property success
