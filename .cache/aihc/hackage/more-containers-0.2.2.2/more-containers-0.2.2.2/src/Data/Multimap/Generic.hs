{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

-- | This internal module implements functionality shared by all multimaps.
module Data.Multimap.Generic (
  Multimap(..), Group,
  null, size, distinctSize,
  empty, singleton,
#if __GLASGOW_HASKELL__ >= 708
  fromList, inverse,
#endif
  fromListWith, fromGroupList, fromMap,
  member, notMember, count,
  find, (!),
  prepend, prependMany, append, appendMany,
  deleteMany,
  inverseWith,
  filter, filterGroups,
  mapGroups,
  toList, toGroupList, toMap,
  keys, keysSet, keysMultiset,
  maxViewWith, minViewWith,
  modifyMany, modifyManyF
) where

import Data.Multimap.Collection (Collection)
import qualified Data.Multimap.Collection as Col
import Data.Multiset (Multiset)
import qualified Data.Multiset as Mset

import Prelude hiding (filter, null)
import qualified Prelude as Prelude
import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup, (<>))
#endif
import Data.Set (Set)
import Data.Tuple (swap)
import qualified GHC.Exts

-- | A map where the same key can be present multiple times.
newtype Multimap c k v = Multimap
  { _toMap :: Map k (c v)
  } deriving (
    Eq, Ord, Read, Show, Functor,
    {-| @since 0.2.1.1 -} Data, {-| @since 0.2.1.1 -} Typeable
  )

-- | A group of values.
type Group k cv = (k, cv)

instance (Ord k, Semigroup (c v)) => Semigroup (Multimap c k v) where
  Multimap m1 <> Multimap m2 = Multimap $ Map.unionWith (<>) m1 m2

instance (Ord k, Monoid (c v)) => Monoid (Multimap c k v) where
  mempty = empty

instance Foldable c => Foldable (Multimap c k) where
  foldr f r0 (Multimap m) = Map.foldr (\c r -> foldr f r c) r0 m

instance Traversable c => Traversable (Multimap c k) where
  sequenceA (Multimap m) = Multimap <$> sequenceA (fmap sequenceA m)

-- | @since 0.2.1.0
instance (Binary k, Binary (c v)) => Binary (Multimap c k v) where
  put (Multimap m) = put m
  get = Multimap <$> get

#if __GLASGOW_HASKELL__ >= 708
instance (Collection c, GHC.Exts.IsList (c v), GHC.Exts.Item (c v) ~ v, Ord k) => GHC.Exts.IsList (Multimap c k v) where
  type Item (Multimap c k v) = (k, v)
  fromList = fromList
  toList = toList

-- | /O(n * log n)/ Builds a multimap from a list of key, value tuples. The values are in the same
-- order as in the original list.
fromList
  :: (Collection c, GHC.Exts.IsList (c v), GHC.Exts.Item (c v) ~ v, Ord k)
  => [(k, v)] -> Multimap c k v
fromList = fromListWith GHC.Exts.fromList

-- | /O(n)/ Inverts keys and values inside a multimap.
inverse
  :: (Collection c, GHC.Exts.IsList (c k), GHC.Exts.Item (c k) ~ k, Ord k, Ord v)
  => Multimap c k v -> Multimap c v k
inverse = inverseWith GHC.Exts.fromList
#endif

-- | /O(1)/ Checks whether the multimap is empty.
null :: Multimap c k v -> Bool
null = Map.null . _toMap

-- | /O(m * C)/ Returns the size of the multimap.
size :: Collection c => Multimap c k v -> Int
size (Multimap m) = Map.foldl' (\n c -> n + Col.size c) 0 m

-- | /O(1)/ Returns the number of distinct keys in the multimap.
distinctSize :: Multimap c k v -> Int
distinctSize = Map.size . _toMap

-- | /O(1)/ Creates an empty multimap.
empty :: Multimap c k v
empty = Multimap Map.empty

-- | /O(1)/ Creates a multimap with a single entry.
singleton :: Collection c => k -> v -> Multimap c k v
singleton k v = Multimap $ Map.singleton k (Col.singleton v)

-- | /O(m)/ Builds a multimap from already grouped collections.
fromGroupList :: (Collection c, Monoid (c v), Ord k) => [Group k (c v)] -> Multimap c k v
fromGroupList = fromMap . Map.fromListWith (<>)

-- | /O(n * log n)/ Transforms a list of entries into a multimap, combining the values for each key
-- into the chosen collection. The values are in the same order as in the original list.
fromListWith :: Ord k => ([v] -> c v) -> [(k, v)] -> Multimap c k v
fromListWith f ts = Multimap $ Map.map (f . reverse) $ m where
  Multimap m = foldl' (\r (k, v) -> modifyMany (v:) k r) empty ts

-- | /O(m * C)/ Transforms a map of collections into a multimap.
fromMap :: Collection c => Map k (c v) -> Multimap c k v
fromMap = Multimap . Map.filter (not . Col.null)

-- | /O(log m)/ Returns the collection of values associated with a key.
find :: (Monoid (c v), Ord k) => k -> Multimap c k v -> c v
find k (Multimap m) = Map.findWithDefault mempty k m

-- | /O(log m)/ Infix version of 'find'.
(!) :: (Monoid (c v), Ord k) => Multimap c k v -> k -> c v
(!) = flip find

-- | /O(log m * C)/ Returns the number of times a key is present in a multimap. The complexity of
-- the operation depends on the complexity of the underlying collection's 'Data.Collection.size'
-- operation.
count :: (Collection c, Ord k) => k -> Multimap c k v -> Int
count k (Multimap m) = maybe 0 Col.size $ Map.lookup k m

-- | /O(log m)/ Checks whether a key is present at least once in a multimap.
member :: Ord k => k -> Multimap c k v -> Bool
member k = Map.member k . _toMap

-- | /O(log m)/ Checks whether a key is absent from a multimap.
notMember :: Ord k => k -> Multimap c k v -> Bool
notMember k = Map.notMember k . _toMap

-- | Modifies a key's collection using an arbitrary function. More specifically, this function lifts
-- an operation over a collection of values into a multimap operation.
--
-- Sample use to filter even values from a 'Data.Multimap.SetMultimap':
--
-- @
--    let ms = fromList [(\'a\', 1), (\'a\', 2)] :: SetMultimap Char Int
--    modifyMany (Set.filter even) \'a\' ms == fromList [(\'a\', 1)]
-- @
modifyMany
  :: (Collection c, Monoid (c v), Ord k)
  => (c v -> c v)
  -> k
  -> Multimap c k v
  -> Multimap c k v
modifyMany f k (Multimap m) = Multimap $ Map.alter (wrap . f . unwrap) k m where
  unwrap = fromMaybe mempty
  wrap c = if Col.null c then Nothing else Just c

-- | Modifies a key's collection using an arbitrary function. This is the applicative version of
-- 'modifyMany'.
modifyManyF
  :: (Collection c, Monoid (c v), Ord k, Functor f)
  => (c v -> f (c v))
  -> k
  -> Multimap c k v
  -> f (Multimap c k v)
modifyManyF f k (Multimap m) = Multimap <$> Map.alterF (fmap wrap . f . unwrap) k m where
  unwrap = fromMaybe mempty
  wrap c = if Col.null c then Nothing else Just c

-- | /O(log m * C)/ Prepends a value to a key's collection.
prepend :: (Collection c, Monoid (c v), Ord k) => k -> v -> Multimap c k v -> Multimap c k v
prepend k v = prependMany k (Col.singleton v)

-- | /O(log m * C)/ Prepends a collection of values to a key's collection.
prependMany :: (Collection c, Monoid (c v), Ord k) => k -> c v -> Multimap c k v -> Multimap c k v
prependMany k c = modifyMany (c <>) k

-- | /O(log m * C)/ Appends a value to a key's collection.
append :: (Collection c, Monoid (c v), Ord k) => k -> v -> Multimap c k v -> Multimap c k v
append k v = appendMany k (Col.singleton v)

-- | /O(log m * C)/ Appends a collection of values to a key's collection.
appendMany :: (Collection c, Monoid (c v), Ord k) => k -> c v -> Multimap c k v -> Multimap c k v
appendMany k c = modifyMany (<> c) k

-- | /O(log m)/ Removes all entries for the given key.
deleteMany :: Ord k => k -> Multimap c k v -> Multimap c k v
deleteMany k = Multimap . Map.delete k . _toMap

-- | /O(n)/ Inverts keys and values inside a multimap, potentially changing the collection type.
inverseWith :: (Collection c1, Ord k, Ord v) => ([k] -> c2 k) -> Multimap c1 k v -> Multimap c2 v k
inverseWith f = fromListWith f . fmap swap . toList

-- | /O(n)/ Filters multimap entries by value.
filter :: (Collection c, Monoid (c v), Ord k) => (v -> Bool) -> Multimap c k v -> Multimap c k v
filter f = fromGroupList . fmap (fmap (Col.filter f)) . toGroupList

-- | /O(m)/ Filters multimap groups. This enables filtering by key and collection.
filterGroups
  :: (Collection c, Monoid (c v), Ord k)
  => (Group k (c v) -> Bool) -> Multimap c k v -> Multimap c k v
filterGroups f = fromGroupList . Prelude.filter f . toGroupList

-- | Maps over the multimap's groups. This method can be used to convert between specific multimaps,
-- for example:
--
-- > let m1 = fromList [('a', 1), ('a', 1)] :: ListMultimap Char Int
-- > let m2 = mapGroups (fmap Set.fromList) m1 :: SetMultimap Char Int
mapGroups
  :: (Collection c2, Monoid (c2 v2), Ord k2)
  => (Group k1 (c1 v1) -> Group k2 (c2 v2)) -> Multimap c1 k1 v1 -> Multimap c2 k2 v2
mapGroups f = fromGroupList . fmap f . toGroupList

-- | /O(n)/ Converts a multimap into its list of entries. Note that this is different from
-- 'Data.Foldable.toList' which returns the multimap's values (similar to "Data.Map").
toList :: Collection c => Multimap c k v -> [(k, v)]
toList (Multimap m) = concat $ fmap go $ Map.toList m where
  go (k,c) = foldr (\v a -> (k,v) : a) [] c

-- | /O(m)/ Converts a multimap into its list of collections.
toGroupList :: Multimap c k v -> [Group k (c v)]
toGroupList = Map.toList . _toMap

-- | /O(1)/ Converts a multimap into a map of collections.
toMap :: Multimap c k v -> Map k (c v)
toMap = _toMap

viewWith
  :: (Collection c, Ord k)
  => (Map k (c v) -> Maybe ((k, c v), Map k (c v)))
  -> (c v -> Maybe (v, c v)) -> Multimap c k v -> Maybe ((k, v), Multimap c k v)
viewWith mapView f (Multimap m) = case mapView m of
  Nothing -> Nothing
  Just ((k, c), m') -> case f c of
    Nothing -> Nothing
    Just (v, c') -> Just ((k, v), Multimap (if Col.null c' then m' else Map.insert k c' m'))

-- | Returns the multimap's maximum key and value. The input function is guaranteed to be called
-- with a non-empty collection.
--
-- @since 0.2.1.2
maxViewWith :: (Collection c, Ord k) => (c v -> Maybe (v, c v)) -> Multimap c k v -> Maybe ((k, v), Multimap c k v)
maxViewWith = viewWith Map.maxViewWithKey

-- | Returns the multimap's minimum key and value. The input function is guaranteed to be called
-- with a non-empty collection.
--
-- @since 0.2.1.2
minViewWith :: (Collection c, Ord k) => (c v -> Maybe (v, c v)) -> Multimap c k v -> Maybe ((k, v), Multimap c k v)
minViewWith = viewWith Map.minViewWithKey

-- | /O(m)/ Returns a list of the multimap's keys. Each key will be repeated as many times as it is
-- present in the multimap.
keys :: Collection c => Multimap c k v -> [k]
keys (Multimap m) = Map.foldrWithKey go [] m where
  go k c r = replicate (Col.size c) k <> r

-- | /O(m)/ Returns a set of the multimap's (distinct) keys.
keysSet :: Multimap c k v -> Set k
keysSet = Map.keysSet . _toMap

-- | /O(m * C)/ Returns a multiset of the map's keys with matching multiplicities.
keysMultiset :: (Collection c, Ord k) => Multimap c k v -> Multiset k
keysMultiset = Mset.fromCountMap . Map.map Col.size . _toMap
