-- | A 'Semigroup'-based counter type.
module Data.Map.Counter where

import           Data.Map.Append.Strict
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Monoid

-- | A 'Semigroup'-based counter type.
--
-- > let counts = mkCounter 1 <> mkCounter 2 <> mkCounter 1 :: Counter Int
-- > getCounts counts === Map.fromList [(1, 2), (2, 1)]
type Counter key = AppendMap key (Sum Int)

-- | Counter that has a single occurrence for a single item.
mkCounter :: Ord key => key -> Counter key
mkCounter key = AppendMap $ Map.singleton key (Sum 1)

-- | Get counts as a map.
getCounts :: Counter key -> Map key Int
getCounts = fmap getSum . unAppendMap
