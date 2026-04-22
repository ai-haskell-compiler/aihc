module Main (main) where

import Test.Fc.Suite (fcGoldenTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  golden <- fcGoldenTests
  defaultMain
    ( testGroup
        "aihc-fc"
        [ golden,
          QC.testProperty "dummy quickcheck property" prop_dummy
        ]
    )

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool -> Bool
prop_dummy _ = True
