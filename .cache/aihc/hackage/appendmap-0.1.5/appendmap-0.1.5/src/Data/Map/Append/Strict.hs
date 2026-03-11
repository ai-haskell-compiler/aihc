{-| A wrapper for 'Map' with a 'Semigroup' and 'Monoid' instances that delegate to
  the individual keys.
-}
module Data.Map.Append.Strict where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map

import           Data.Monoid        hiding ((<>))
import           Data.Semigroup

import qualified Data.List.NonEmpty as NE

-- | Map wrapper with 'Semigroup' and 'Monoid' instances that delegate to the
-- keys. It satisfies the following property:
--
-- > lookup k (m1 <> m2) === lookup k m1 <> lookup k m2
-- >   where
-- >     lookup key = Map.lookup key . unAppendMap
newtype AppendMap k v = AppendMap
  { unAppendMap :: Map k v
  } deriving (Ord, Eq, Show)

instance (Ord k, Semigroup v) => Semigroup (AppendMap k v) where
  AppendMap a <> AppendMap b = AppendMap $ Map.unionWith (<>) a b
  sconcat = AppendMap . Map.unionsWith (<>) . NE.toList . fmap unAppendMap

instance (Ord k, Semigroup v) => Monoid (AppendMap k v) where
  mempty = AppendMap Map.empty
  mappend = (<>)
  mconcat = AppendMap . Map.unionsWith (<>) . fmap unAppendMap
