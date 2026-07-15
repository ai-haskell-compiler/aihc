module Main (main) where

import Test.Grin.Suite (grinEvalFixtureTests, grinGoldenTests, grinUnitTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  goldenFixtures <- grinGoldenTests
  evalFixtures <- grinEvalFixtureTests
  defaultMain
    ( testGroup
        "aihc-grin"
        [ grinUnitTests,
          goldenFixtures,
          evalFixtures,
          QC.testProperty "dummy quickcheck property" prop_dummy
        ]
    )

-- | Keep the workspace-wide QuickCheck controls accepted by this suite.
prop_dummy :: Bool -> Bool
prop_dummy _ = True
