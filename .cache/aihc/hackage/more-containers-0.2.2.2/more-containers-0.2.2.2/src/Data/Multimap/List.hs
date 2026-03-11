-- | This module provides list-specific multimap functionality.
module Data.Multimap.List (
  ListMultimap,
  cons, uncons
) where

import Data.Functor.Compose (Compose(..))
import qualified Data.List as List

import Data.Multimap.Generic (Multimap(..), modifyMany, modifyManyF)

-- | A multimap with list values. Note that lists do not support efficient appends or sizing, so
-- several multimap operations will have higher complexity than for other collections. If
-- performance is a concern, consider using a 'Data.Multimap.SeqMultimap' instead.
--
-- See "Data.Multimap.List" for operations specific to this type.
type ListMultimap = Multimap []

-- | /O(log m)/ Prepends a value to a given key.
cons :: (Ord k) => k -> v -> ListMultimap k v -> ListMultimap k v
cons k v = modifyMany (v:) k

-- | /O(log m)/ Extracts the first value associated with a given key, if possible.
uncons :: (Ord k, Ord v) => k -> ListMultimap k v -> Maybe (v, ListMultimap k v)
uncons k = getCompose . modifyManyF (Compose . List.uncons) k
