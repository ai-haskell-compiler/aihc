module Main (main) where

import Test.Native.BlockLayout qualified as BlockLayout
import Test.Native.Primitive qualified as Primitive
import Test.Native.RegisterAllocate (tests)
import Test.Native.RuntimePlan qualified as RuntimePlan
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-native"
        [ BlockLayout.tests,
          Primitive.tests,
          RuntimePlan.tests,
          tests
        ]
    )
