{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cereal.UnorderedContainers.Get where

import qualified Cereal.UnorderedContainers.Extras.StrictHashMap as StrictHashMap
import Cereal.UnorderedContainers.Prelude

strictHashMap :: (Eq key, Hashable key) => Get Int -> Get key -> Get value -> Get (StrictHashMap.HashMap key value)
strictHashMap int key value = size >>= associations
  where
    size = int
    associations size = StrictHashMap.replicateM size ((,) <$> key <*> value)
