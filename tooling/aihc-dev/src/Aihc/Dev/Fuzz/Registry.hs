-- | Workspace-wide QuickCheck property registry.
module Aihc.Dev.Fuzz.Registry
  ( FuzzProperty (..),
    fuzzProperties,
    fuzzPropertyId,
  )
where

import Aihc.Grin.Fuzz (grinFuzzProperties)
import Aihc.Tc.Fuzz (tcFuzzProperties)
import Test.QuickCheck (Property)

data FuzzProperty = FuzzProperty
  { fuzzPropertyComponent :: String,
    fuzzPropertyName :: String,
    fuzzPropertyValue :: Property
  }

fuzzPropertyId :: FuzzProperty -> String
fuzzPropertyId fuzzProperty =
  fuzzPropertyComponent fuzzProperty <> "." <> fuzzPropertyName fuzzProperty

-- | All continuously runnable properties, grouped by their owning component.
fuzzProperties :: [FuzzProperty]
fuzzProperties =
  qualify "aihc-tc" tcFuzzProperties
    ++ qualify "aihc-grin" grinFuzzProperties
  where
    qualify component = map (uncurry (FuzzProperty component))
