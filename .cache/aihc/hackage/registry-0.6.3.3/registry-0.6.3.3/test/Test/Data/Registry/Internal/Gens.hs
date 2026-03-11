{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Internal.Gens where

import Data.Registry
import Hedgehog
import Protolude
import Test.Data.Registry.Internal.GensRegistry

-- Hedgehog generators for the internal types
registry = normalize gensRegistry

for_all :: forall a. _ => PropertyT IO a
for_all = forAll $ gen @a

gen :: forall a. _ => Gen a
gen = make registry
