module YamlUnscrambler.Util.HashMap where

import Data.HashMap.Strict
import YamlUnscrambler.Prelude hiding (lookup)

lookupFirst :: (Hashable k) => [k] -> HashMap k v -> Maybe (k, v)
lookupFirst keys map =
  getFirst (foldMap (\k -> First (fmap (k,) (lookup k map))) keys)
