-- | This module provides set-specific multimap functionality.
module Data.Multimap.Set (
  SetMultimap,
  map,
  delete,
  member', notMember'
) where

import Prelude hiding (map)

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Multimap.Generic (Multimap(..), modifyMany)

-- | A multimap with 'Set' values. This multimap implementation will automatically deduplicate
-- values per key. For example:
--
-- > let mm = fromList [('a', 1), ('a', 1)] :: SetMultimap Char Int
-- > size mm == 1 -- True
--
--  See "Data.Multimap.Set" for operations specific to this type.
type SetMultimap = Multimap Set

-- | /O(n * log n)/ Maps over the multimap's values. This function is useful since 'Set' is not a
-- functor.
map :: (Ord k, Ord v1 , Ord v2) => (v1 -> v2) -> SetMultimap k v1 -> SetMultimap k v2
map f (Multimap m) = Multimap $ fmap (Set.map f) m

-- | /O(log m)/ Deletes an entry from the multimap.
delete :: (Ord k, Ord v) => k -> v -> SetMultimap k v -> SetMultimap k v
delete k v = modifyMany (Set.delete v) k

-- | /O(log n)/ Checks whether an entry (key plus value) is a member of the multimap.
member' :: (Ord k, Ord v) => k -> v -> SetMultimap k v -> Bool
member' k v (Multimap m) = maybe False (Set.member v) $ Map.lookup k m

-- | /O(log n)/ Checks whether an entry (key plus value) is a not member of the multimap.
notMember' :: (Ord k, Ord v) => k -> v -> SetMultimap k v -> Bool
notMember' k v mm = not $ member' k v mm
