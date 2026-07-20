module Main (main) where

import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
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
          QC.testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip
        ]
    )
