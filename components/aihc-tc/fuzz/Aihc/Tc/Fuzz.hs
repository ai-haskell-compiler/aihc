-- | Continuously runnable QuickCheck properties owned by @aihc-tc@.
module Aihc.Tc.Fuzz
  ( tcFuzzProperties,
  )
where

import Test.QuickCheck (Property, Testable, property)
import Test.Tc.Properties (prop_reflexiveEq, prop_zonkIdempotent)

tcFuzzProperties :: [(String, Property)]
tcFuzzProperties =
  [ named "zonking idempotent" prop_zonkIdempotent,
    named "reflexive equality solved" prop_reflexiveEq
  ]
  where
    named :: (Testable prop) => String -> prop -> (String, Property)
    named name value = (name, property value)
