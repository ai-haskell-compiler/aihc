{-# LANGUAGE RankNTypes #-}

-- | Workspace-wide Hedgehog property registry.
module Aihc.Dev.Fuzz.Registry
  ( FuzzProperty (..),
    fuzzProperties,
    fuzzPropertyId,
  )
where

import Aihc.Grin.Fuzz (grinFuzzProperties)
import Aihc.Tc.Fuzz (tcFuzzProperties)
import Hedgehog (Property)
import Test.Native.GcFuzz (withGcFuzzProperty)

data FuzzProperty = FuzzProperty
  { fuzzPropertyComponent :: String,
    fuzzPropertyName :: String,
    fuzzPropertyWeight :: Rational,
    withFuzzProperty :: forall a. (Property -> IO a) -> IO a
  }

fuzzPropertyId :: FuzzProperty -> String
fuzzPropertyId fuzzProperty =
  fuzzPropertyComponent fuzzProperty <> "." <> fuzzPropertyName fuzzProperty

-- | All continuously runnable properties, grouped by their owning component.
fuzzProperties :: [FuzzProperty]
fuzzProperties =
  qualify "aihc-tc" tcFuzzProperties
    ++ qualify "aihc-grin" grinFuzzProperties
    ++ [FuzzProperty "aihc-gc" "collects random heaps and stacks" 0.1 withGcFuzzProperty]
  where
    qualify component = map (\(name, value) -> FuzzProperty component name 1 (\run -> run value))
