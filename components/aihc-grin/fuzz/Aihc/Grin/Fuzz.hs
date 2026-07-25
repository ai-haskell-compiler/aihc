-- | Continuously runnable QuickCheck properties owned by @aihc-grin@.
module Aihc.Grin.Fuzz
  ( grinFuzzProperties,
  )
where

import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.QuickCheck (Property)

grinFuzzProperties :: [(String, Property)]
grinFuzzProperties =
  [("generated GRIN pretty-printer round-trip", prop_grinPrettyRoundTrip)]
