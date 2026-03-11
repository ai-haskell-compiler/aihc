{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | This modules provides a strict multiset implementation. To avoid collision with Prelude
-- functions, it is recommended to import this module qualified:
--
-- > import qualified Data.Multiset as Mset
--
-- All complexities below use /m/ for the number of distinct elements and /n/ for the total number
-- of elements.
module Data.Multiset (
  Multiset, Group,
  -- * Construction
  empty, singleton, replicate,
  fromList, fromGroupList,
  fromCountMap,
  -- * Tests and accessors
  null,
  size, distinctSize,
  member, notMember,
  isSubsetOf, isProperSubsetOf,
  count, (!),
  -- * Update
  insert, remove, removeAll, modify,
  -- * Maps and filters
  map, mapCounts, mapGroups,
  filter, filterGroups,
  -- * Combination
  max, min, difference, unionWith, intersectionWith,
  -- * Conversions
  toSet,
  toGroupList, toGrowingGroupList, toShrinkingGroupList,
  toCountMap,
  -- * Other
  elems, distinctElems,
  maxView, minView,
  mostCommon
) where

import Prelude hiding (filter, foldr, map, max, min, null, replicate)
import qualified Prelude as Prelude

import Data.Binary (Binary(..))
import Data.Data (Data, Typeable)
import Data.Foldable (foldl', foldr, toList)
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup, (<>))
#endif
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.Exts

-- | A strict implementation of a multiset. It is backed by a 'Data.Map.Strict.Map' and inherits
-- several of its properties and operation's complexities. In particular, the number of elements in
-- a multiset must not exceed @maxBound :: Int@.
data Multiset v = Multiset
  { _toMap :: !(Map v Int)
  , _size :: !Int
  } deriving (
    Eq, Ord, Read, Show,
    {-| @since 0.2.1.1 -} Data, {-| @since 0.2.1.1 -} Typeable
  )

-- | A group of values of a given size.
type Group v = (v, Int)

instance Ord v => Semigroup (Multiset v) where
  (<>) = unionWith' (+)

instance Ord v => Monoid (Multiset v) where
  mempty = empty

instance Foldable Multiset where
  foldr f r0 (Multiset m _) = Map.foldrWithKey go r0 m where
    go v n r1 = foldl' (flip f) r1 $ Prelude.replicate n v

-- | @since 0.2.1.0
instance Binary v => Binary (Multiset v) where
  put (Multiset m s) = put m <> put s
  get = Multiset <$> get <*> get

#if __GLASGOW_HASKELL__ >= 708
instance Ord v => GHC.Exts.IsList (Multiset v) where
  type Item (Multiset v) = v
  fromList = fromList
  toList = toList
#endif

-- | /O(1)/ Checks whether a multiset is empty.
null :: Multiset v -> Bool
null = Map.null . _toMap

-- | /O(1)/ Returns the total number of elements in the multiset. Note that this isn't the number of
-- /distinct/ elements, see 'distinctSize' for that.
size :: Multiset v -> Int
size = _size

-- | /O(1)/ Returns the number of distinct elements in the multiset.
distinctSize :: Multiset v -> Int
distinctSize = Map.size . _toMap

-- | /O(1)/ Returns an empty multiset.
empty :: Multiset v
empty = Multiset Map.empty 0

-- | /O(1)/ Returns a multiset with a single element.
singleton :: v -> Multiset v
singleton = replicate 1

-- | /O(1)/ Returns a multiset with the same element repeated. If n is zero or negative, 'replicate'
-- returns an empty multiset.
replicate :: Int -> v -> Multiset v
replicate n v = if n > 0
  then Multiset (Map.singleton v n) n
  else empty

-- | /O(m * log m)/ Builds a multiset from a map. Negative counts are ignored.
fromCountMap :: Ord v => Map v Int -> Multiset v
fromCountMap = Map.foldlWithKey' go empty where
  go ms v n = if n > 0
    then modify (+ n) v ms
    else ms

-- | /O(n * log n)/ Builds a multiset from values.
fromList :: Ord v => [v] -> Multiset v
fromList = foldl' (flip insert) empty

-- | /O(m * log m)/ Builds a multiset from a list of groups. Counts of duplicate groups are added
-- together and elements with negative total count are omitted.
fromGroupList :: Ord v => [Group v] -> Multiset v
fromGroupList = foldl' go empty where
  go ms (v,n) = modify (+ n) v ms

-- Access

-- | /O(log m)/ Checks whether the element is present at least once.
member :: Ord v => v -> Multiset v -> Bool
member v = Map.member v . _toMap

-- | /O(log m)/ Checks whether the element is not present.
notMember :: Ord v => v -> Multiset v -> Bool
notMember v = Map.notMember v . _toMap

-- | /O(log m)/ Returns the number of times the element is present in the multiset, or 0 if absent.
count :: Ord v => v -> Multiset v -> Int
count v = Map.findWithDefault 0 v . _toMap

-- | /O(log m)/ Infix version of 'count'.
(!) :: Ord v => Multiset v -> v -> Int
(!) = flip count

-- | /O(log m)/ Modifies the count of an element. If the resulting element's count is zero or
-- negative, it will be removed.
modify :: Ord v => (Int -> Int) -> v -> Multiset v -> Multiset v
modify f v ms@(Multiset m s) = Multiset m' s' where
  n = count v ms
  n' = Prelude.max 0 (f n)
  m' = if n' > 0 then Map.insert v n' m else Map.delete v m
  s' = s - n + n'

-- | /O(log m)/ Inserts an element.
insert :: Ord v => v -> Multiset v -> Multiset v
insert = modify (+1)

-- | /O(log m)/ Removes a single element. Does nothing if the element isn't present.
remove :: Ord v => v -> Multiset v -> Multiset v
remove = modify (subtract 1)

-- | /O(log m)/ Removes all occurrences of a given element.
removeAll :: Ord v => v -> Multiset v -> Multiset v
removeAll = modify (const 0)

-- | Filters a multiset by value.
filter :: Ord v => (v -> Bool) -> Multiset v -> Multiset v
filter f = filterGroups (f . fst)

-- | Filters a multiset by group.
filterGroups :: Ord v => (Group v -> Bool) -> Multiset v -> Multiset v
filterGroups f (Multiset m _) = Map.foldlWithKey' go empty m where
  go ms v n = if f (v,n)
    then modify (+ n) v ms
    else ms

-- | Maps on the multiset's values.
map :: (Ord v1, Ord v2) => (v1 -> v2) -> Multiset v1 -> Multiset v2
map f (Multiset m s) = Multiset (Map.mapKeysWith (+) f m) s

-- | Maps on the multiset's counts. Groups with resulting non-positive counts will be removed from
-- the final multiset.
--
-- @since 0.2.2.0
mapCounts :: Ord v => (Int -> Int) -> Multiset v -> Multiset v
mapCounts f = mapGroups (\(v, n) -> (v, f n))

-- | Maps on the multiset's groups. Groups with resulting non-positive counts will be removed from
-- the final multiset.
mapGroups :: Ord v => (Group v -> Group v) -> Multiset v -> Multiset v
mapGroups f ms = fromGroupList $ fmap f $ toGroupList ms

-- | Combines two multisets, returning the max count of each element.
max :: Ord v => Multiset v -> Multiset v -> Multiset v
max = unionWith' Prelude.max

-- | Combines two multisets, returning the minimum count of each element (or omitting it if the
-- element is present in only one of the two multisets).
min :: Ord v => Multiset v -> Multiset v -> Multiset v
min = intersectionWith Prelude.min

-- | Unions two multisets with a generic function. The combining function will be called with a
-- count of 0 when an element is only present in one set.
unionWith :: Ord v => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
unionWith f ms1 ms2 = fromGroupList $ fmap go $ toList vs where
  vs = Set.union (toSet ms1) (toSet ms2)
  go v = (v, (f (count v ms1) (count v ms2)))

-- | Intersects two multisets with a generic function. The combining function is guaranteed to be
-- called only with positive counts.
intersectionWith :: Ord v => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
intersectionWith f (Multiset m1 _) (Multiset m2 _) = fromCountMap $ Map.intersectionWith f m1 m2

-- | /O(m * log m)/ Returns the first set minus the second. Resulting negative counts are ignored.
difference :: Ord v => Multiset v -> Multiset v -> Multiset v
difference (Multiset m1 _) (Multiset m2 _) = fromCountMap $ Map.differenceWith go m1 m2 where
  go n1 n2 = let n = n1 - n2 in if n > 0 then Just n else Nothing

-- | /O(m * log m)/ Checks whether the first subset is a subset of the second (potentially equal to
-- it).
isSubsetOf :: Ord v => Multiset v -> Multiset v -> Bool
isSubsetOf (Multiset m _) ms = Map.foldrWithKey go True m where
  go v n r = count v ms >= n && r

-- | /O(m * log m)/ Checks whether the first subset is a strict subset of the second.
isProperSubsetOf :: Ord v => Multiset v -> Multiset v -> Bool
isProperSubsetOf ms1 ms2 = size ms1 < size ms2 && ms1 `isSubsetOf` ms2

-- | /O(1)/ Converts the multiset to a map of (positive) counts.
toCountMap :: Multiset v -> Map v Int
toCountMap = _toMap

-- | /O(m)/ Returns the 'Set' of all distinct elements in the multiset.
toSet :: Multiset v -> Set v
toSet = Map.keysSet . _toMap

-- | /O(m)/ Converts the multiset to a list of values and associated counts. The groups are in
-- undefined order; see 'toGrowingGroupList' and 'toShrinkingGroupList' for sorted versions.
toGroupList :: Multiset v -> [Group v]
toGroupList = Map.toList . _toMap

-- | /O(m * log m)/ Converts the multiset into a list of values and counts, from least common to
-- most.
toGrowingGroupList :: Multiset v -> [Group v]
toGrowingGroupList = sortOn snd . toGroupList

-- | /O(m * log m)/ Converts the multiset into a list of values and counts, from most common to
-- least.
toShrinkingGroupList :: Multiset v -> [Group v]
toShrinkingGroupList = sortOn (negate . snd) . toGroupList

-- Other

-- | /O(n)/ Returns the multiset's elements as a list where each element is repeated as many times
-- as its number of occurrences. This is a synonym for 'toList'.
--
-- @since 0.2.2.0
elems :: Multiset v -> [v]
elems = toList

-- | /O(m)/ Returns a list of the distinct elements in the multiset.
--
-- @since 0.2.2.0
distinctElems :: Multiset v -> [v]
distinctElems = Map.keys . _toMap

view :: Ord v => (Map v Int -> Maybe ((v, Int), Map v Int)) -> Multiset v -> Maybe (v, Multiset v)
view mapView (Multiset m s) = case mapView m of
  Nothing -> Nothing
  Just ((v, n), m') ->
    let
      s' = s - 1
      ms = if n == 1 then Multiset m' s' else Multiset (Map.insert v (n - 1) m') s'
    in Just (v, ms)

-- | /O(log m)/ Takes an element of maximum value from the multiset and the remaining multiset, or
-- 'Nothing' if the multiset was already empty.
--
-- @since 0.2.1.2
maxView :: Ord v => Multiset v -> Maybe (v, Multiset v)
maxView = view Map.maxViewWithKey

-- | /O(log m)/ Takes an element of minimum value from the multiset and the remaining multiset, or
-- 'Nothing' if the multiset was already empty.
--
-- @since 0.2.1.2
minView :: Ord v => Multiset v -> Maybe (v, Multiset v)
minView = view Map.minViewWithKey

-- | /O(m)/ Returns the multiset's elements grouped by count, most common first.
mostCommon :: Multiset v -> [(Int, [v])]
mostCommon = fmap go . groupBy (\e1 e2 -> snd e1 == snd e2) . toShrinkingGroupList where
  go ((v, n) : groups) = (n, v : fmap fst groups)
  go _ = error "unreachable"

-- Internal

unionWith' :: Ord v => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
unionWith' f (Multiset m1 _) (Multiset m2 _) = fromCountMap $ Map.unionWith f m1 m2
