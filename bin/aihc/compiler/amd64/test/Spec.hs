module Main (main) where

import Hedgehog (Property, property, success)
import Test.Amd64.Suite (tests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-amd64"
        [ tests,
          testProperty "Hedgehog options" prop_dummy
        ]
    )

-- | Keep the workspace-wide Hedgehog controls accepted by this suite.
prop_dummy :: Property
prop_dummy = property success
