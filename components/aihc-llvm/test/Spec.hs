module Main (main) where

import Test.Llvm.Suite qualified as Suite
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-llvm"
        [ Suite.tests,
          QC.testProperty "dummy quickcheck property" prop_dummy
        ]
    )

-- | Keep the workspace-wide QuickCheck controls accepted by this suite.
prop_dummy :: Bool -> Bool
prop_dummy _ = True
