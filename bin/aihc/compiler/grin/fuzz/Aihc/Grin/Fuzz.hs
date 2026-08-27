-- | Continuously runnable Hedgehog properties owned by @aihc-grin@.
module Aihc.Grin.Fuzz
  ( grinFuzzProperties,
  )
where

import Hedgehog (Property)
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)

grinFuzzProperties :: [(String, Property)]
grinFuzzProperties =
  [("generated GRIN pretty-printer round-trip", prop_grinPrettyRoundTrip)]
