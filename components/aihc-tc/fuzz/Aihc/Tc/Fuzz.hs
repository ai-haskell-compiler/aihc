-- | Continuously runnable Hedgehog properties owned by @aihc-tc@.
module Aihc.Tc.Fuzz
  ( tcFuzzProperties,
  )
where

import Hedgehog (Property)
import Test.Tc.Properties (prop_reflexiveEq, prop_zonkIdempotent)

tcFuzzProperties :: [(String, Property)]
tcFuzzProperties =
  [ ("zonking idempotent", prop_zonkIdempotent),
    ("reflexive equality solved", prop_reflexiveEq)
  ]
