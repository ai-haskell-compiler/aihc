module Test.Native.Spec (tests) where

import Test.Native.BlockLayout qualified as BlockLayout
import Test.Native.Compiler qualified as Compiler
import Test.Native.GcFuzz qualified as GcFuzz
import Test.Native.Primitive qualified as Primitive
import Test.Native.RegisterAllocate qualified as RegisterAllocate
import Test.Native.Runtime qualified as Runtime
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "aihc-native"
    [ BlockLayout.tests,
      Compiler.tests,
      GcFuzz.tests,
      Primitive.tests,
      Runtime.tests,
      RegisterAllocate.tests
    ]
