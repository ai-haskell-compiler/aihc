module Main (main) where

import Test.Native.BlockLayout qualified as BlockLayout
import Test.Native.Primitive qualified as Primitive
import Test.Native.RegisterAllocate (tests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-native"
        [ BlockLayout.tests,
          Primitive.tests,
          tests,
          QC.testProperty "dummy quickcheck property" prop_dummy
        ]
    )

-- | Keep the workspace-wide QuickCheck controls accepted by this suite.
prop_dummy :: Bool -> Bool
prop_dummy _ = True
