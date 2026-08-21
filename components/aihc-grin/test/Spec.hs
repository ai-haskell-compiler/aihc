module Main (main) where

import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "aihc-grin"
        [testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip]
    )
