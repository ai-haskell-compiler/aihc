module Main (main) where

import Hedgehog (Property, property, success)
import Test.Native.BlockLayout qualified as BlockLayout
import Test.Native.Compiler qualified as Compiler
import Test.Native.Primitive qualified as Primitive
import Test.Native.RegisterAllocate (tests)
import Test.Native.Runtime qualified as Runtime
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-native"
        [ BlockLayout.tests,
          Compiler.tests,
          Primitive.tests,
          Runtime.tests,
          tests,
          testProperty "Hedgehog options" prop_dummy
        ]
    )

-- | Keep the workspace-wide Hedgehog controls accepted by this suite.
prop_dummy :: Property
prop_dummy = property success
