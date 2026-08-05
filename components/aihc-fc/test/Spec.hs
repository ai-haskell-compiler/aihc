module Main (main) where

import Test.Fc.Arbitrary (prop_fcTextRoundTrip)
import Test.Fc.Suite (fcDesugarTests, fcEvalFixtureTests, fcEvalTests, fcGoldenTests, fcLoweringTests, fcOptimizationTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  golden <- fcGoldenTests
  evalFixtures <- fcEvalFixtureTests
  defaultMain
    ( testGroup
        "aihc-fc"
        [ golden,
          fcDesugarTests,
          fcEvalTests,
          fcLoweringTests,
          fcOptimizationTests,
          evalFixtures,
          QC.testProperty "generated System FC text round-trip" prop_fcTextRoundTrip
        ]
    )
