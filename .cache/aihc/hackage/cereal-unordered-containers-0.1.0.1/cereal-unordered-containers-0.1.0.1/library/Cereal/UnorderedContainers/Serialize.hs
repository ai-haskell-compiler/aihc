{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Orphan instances of "Serialize".
module Cereal.UnorderedContainers.Serialize where

import qualified Cereal.UnorderedContainers.Extras.StrictHashMap as StrictHashMap
import qualified Cereal.UnorderedContainers.Get as Get
import Cereal.UnorderedContainers.Prelude
import qualified Cereal.UnorderedContainers.Put as Put

instance (Eq key, Hashable key, Serialize key, Serialize value) => Serialize (StrictHashMap.HashMap key value) where
  put = Put.strictHashMap put put put
  get = Get.strictHashMap get get get
