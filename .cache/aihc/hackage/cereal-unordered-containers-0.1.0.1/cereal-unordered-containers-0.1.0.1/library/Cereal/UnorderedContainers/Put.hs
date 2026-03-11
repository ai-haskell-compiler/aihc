module Cereal.UnorderedContainers.Put where

import qualified Cereal.UnorderedContainers.Extras.StrictHashMap as StrictHashMap
import Cereal.UnorderedContainers.Prelude

strictHashMap :: Putter Int -> Putter key -> Putter value -> Putter (StrictHashMap.HashMap key value)
strictHashMap int key value x = size *> associations
  where
    size = int (StrictHashMap.size x)
    associations = StrictHashMap.traverse_ association x
    association keyValue valueValue = key keyValue *> value valueValue
