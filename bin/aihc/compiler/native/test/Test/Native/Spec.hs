module Test.Native.Spec (tests) where

import Hedgehog (Property, property, success)
import Test.Native.BlockLayout qualified as BlockLayout
import Test.Native.Compiler qualified as Compiler
import Test.Native.Primitive qualified as Primitive
import Test.Native.RegisterAllocate qualified as RegisterAllocate
import Test.Native.Runtime qualified as Runtime
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "aihc-native"
    [ BlockLayout.tests,
      Compiler.tests,
      Primitive.tests,
      Runtime.tests,
      RegisterAllocate.tests,
      testProperty "Hedgehog options" prop_dummy
    ]

-- | Keep the workspace-wide Hedgehog controls accepted by this suite.
prop_dummy :: Property
prop_dummy = property success
