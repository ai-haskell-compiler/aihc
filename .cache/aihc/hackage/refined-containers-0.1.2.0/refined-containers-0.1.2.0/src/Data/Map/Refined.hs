-- | This module defines a way to prove that a key exists in a map, so that the
-- key can be used to index into the map without using a 'Maybe', or manually
-- handling the \"impossible\" case with 'error' or other partial functions.
--
-- To do this, @'Map' s k v@ has a type parameter @s@ that identifies its set of
-- keys, so that if another map has the same type parameter, you know that map
-- has the same set of keys. There is @'Key' s k@, a type of keys that have been
-- validated to belong to the set identified by @s@, and for which the operation
-- of indexing into a @'Map' s k v@ (only for the same @s@) can proceed without
-- failure (see '!'). The type @s@ itself has no internal structure, rather it
-- is merely a skolem type variable (rank-2 polymorphism
-- 'Control.Monad.ST.runST' trick) introduced by "Data.Reflection".
--
-- Like "Data.Map", functions in this module are strict in the keys but lazy in
-- the values. The "Data.Map.Strict.Refined" module reuses the same 'Map' type
-- but provides functions that operate strictly on the values.
--
-- = Warning
-- This module together with "Data.Map" rely on 'Eq' and 'Ord' instances being
-- lawful: that '==' is an equivalence relation, and that the 'Ord' operations
-- define a total order on the quotient defined by this equivalence relation; at
-- least for the subset of keys that are actually encountered at runtime. If
-- this assumption is violated, this module may not be able to uphold its
-- invariants and may throw errors. In particular beware of NaN in 'Float' and
-- 'Double'.
module Data.Map.Refined
  (
  -- * Map type
    Common.Map
  , Common.Key
  -- * Existentials and common proofs
  , Common.SomeMap(..)
  , Common.withMap
  , Common.SomeMapWith(..)
  , Common.withMapWith
  , Common.Some2MapWith(..)
  , Common.with2MapWith
  , SupersetProof(..)
  , EmptyProof(..)
  -- * Construction
  , Common.empty
  , singleton
  , SingletonProof(..)
  , fromSet
  , Common.fromMap
  , Common.verifyMap
  , fromTraversable
  , fromTraversableWith
  , fromTraversableWithKey
  , FromTraversableProof(..)
  -- * Insertion
  , insert
  , insertWith
  , insertWithKey
  , InsertProof(..)
  , reinsert
  , insertLookupWithKey
  -- * Deletion/Update
  , Common.delete
  , adjust'
  , adjust
  , adjustWithKey
  , update'
  , update
  , updateWithKey
  , updateLookupWithKey
  -- * Query
  , Common.lookup
  , (Common.!)
  , Common.member
  , Common.lookupLT
  , Common.lookupGT
  , Common.lookupLE
  , Common.lookupGE
  , Common.null
  , Common.isSubmapOfBy
  , SubsetProof(..)
  , Common.disjoint
  , DisjointProof(..)
  -- * Combine
  , zipWith
  , zipWithKey
  , bind
  , Common.union
  , unionWith
  , unionWithKey
  , UnionProof(..)
  , Common.difference
  , DifferenceProof(..)
  , differenceWith
  , differenceWithKey
  , PartialDifferenceProof(..)
  , Common.intersection
  , intersectionWith
  , intersectionWithKey
  , IntersectionProof(..)
  -- * Traversal
  , map
  , mapWithKey
  , traverseWithKey
  , mapAccumLWithKey
  , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , MapProof(..)
  , backpermuteKeys
  -- * Folds
  , Common.foldMapWithKey
  , Common.foldrWithKey
  , Common.foldlWithKey
  , Common.foldrWithKey'
  , Common.foldlWithKey'
  -- * Conversion
  , Common.toMap
  , Common.keysSet
  , Common.toList
  , Common.toDescList
  -- * Filter
  , Common.restrictKeys
  , Common.withoutKeys
  , Common.filter
  , Common.filterKeys
  , Common.filterWithKey
  , Common.partition
  , Common.partitionWithKey
  , PartitionProof(..)
  , Common.takeWhileAntitone
  , Common.dropWhileAntitone
  , Common.spanAntitone
  , PartialPartitionProof(..)
  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey
  , Common.splitLookup
  , SplitProof(..)
  -- * Min/Max
  , updateMinWithKey
  , updateMaxWithKey
  , adjustMinWithKey
  , adjustMaxWithKey
  , Common.minViewWithKey
  , Common.maxViewWithKey
  -- * Casts
  , Common.castKey
  , Common.cast
  , castFlavor
  ) where

import           Data.Coerce
import           Data.Container.Refined.Proofs
import           Data.Container.Refined.Unsafe
import           Data.Functor
import qualified Data.Map as Map
import           Data.Map.Common.Refined
  ( Map(..), Key, unsafeCastKey, unsafeKey, SomeMapWith(..), Some2MapWith(..)
  , fromSet, (!), zipWith, mapWithKey, traverseWithKey, bind
  )
import qualified Data.Map.Common.Refined as Common
import           Data.Traversable
import           Data.Type.Coercion
import           Prelude hiding (lookup, map, null, zipWith)
import           Refined
import           Refined.Unsafe


-- | Create a map with a single key-value pair, and return a proof that the
-- key is in the resulting map.
singleton :: forall k a. k -> a -> SomeMapWith (SingletonProof 'Regular k) k a
singleton k v = SomeMapWith (Map $ Map.singleton k v)
  $ SingletonProof (unsafeKey k)

-- | Create a map from an arbitrary traversable of key-value pairs. If a key is
-- repeated, the retained value is the last one in traversal order. If you're
-- looking for @fromList@, this is the function you want.
fromTraversable
  :: forall t k a. (Traversable t, Ord k)
  => t (k, a) -> SomeMapWith (FromTraversableProof 'Regular t k) k a
fromTraversable xs = SomeMapWith (Map m) $ FromTraversableProof proof
  where
    (m, proof) = mapAccumL
      (\s (k, v) -> let !s' = Map.insert k v s in (s', unsafeKey k))
      Map.empty
      xs

-- | Create a map from an arbitrary traversable of key-value pairs, with a
-- function for combining values for repeated keys. The function is called as if
-- by 'foldl1', but flipped:
--
-- @
-- 'fromTraversableWith' f [(k, x1), (k, x2), (k, x3)]
--   = 'singleton' k (f x3 (f x2 x1))
-- @
fromTraversableWith
  :: forall t k a. (Traversable t, Ord k)
  => (a -> a -> a)
  -> t (k, a)
  -> SomeMapWith (FromTraversableProof 'Regular t k) k a
fromTraversableWith f xs = SomeMapWith (Map m) $ FromTraversableProof proof
  where
    (m, proof) = mapAccumL
      (\s (k, v) -> let !s' = Map.insertWith f k v s in (s', unsafeKey k))
      Map.empty
      xs

-- | Create a map from an arbitrary traversable of key-value pairs. Like
-- 'fromTraversableWith', but the combining function has access to the key.
fromTraversableWithKey
  :: forall t k a. (Traversable t, Ord k)
  => (k -> a -> a -> a)
  -> t (k, a)
  -> SomeMapWith (FromTraversableProof 'Regular t k) k a
fromTraversableWithKey f xs = SomeMapWith (Map m) $ FromTraversableProof proof
  where
    (m, proof) = mapAccumL
      (\s (k, v) -> let !s' = Map.insertWithKey f k v s in (s', unsafeKey k))
      Map.empty
      xs

-- | Insert a key-value pair into the map to obtain a potentially larger map,
-- guaranteed to contain the given key. If the key was already present, the
-- associated value is replaced with the supplied value.
insert
  :: forall s k a. Ord k
  => k -> a -> Map s k a -> SomeMapWith (InsertProof 'Regular k s) k a
insert k v (Map m) = SomeMapWith (Map $ Map.insert k v m)
  $ InsertProof (unsafeKey k) unsafeSubset

-- | Insert a key-value pair into the map to obtain a potentially larger map,
-- guaranteed to contain the given key. If the key was already present, the
-- supplied function is used to combine the new value with the old (in that
-- order).
insertWith
  :: forall s k a. Ord k
  => (a -> a -> a)
  -> k
  -> a
  -> Map s k a
  -> SomeMapWith (InsertProof 'Regular k s) k a
insertWith f k v (Map m) = SomeMapWith (Map $ Map.insertWith f k v m)
  $ InsertProof (unsafeKey k) unsafeSubset

-- | Insert a key-value pair into the map to obtain a potentially larger map,
-- guaranteed to contain the given key. Like 'insertWith', but the combining
-- function has access to the key, which is guaranteed to be in the old map.
insertWithKey
  :: forall s k a. Ord k
  => (Key s k -> a -> a -> a)
  -> k
  -> a
  -> Map s k a
  -> SomeMapWith (InsertProof 'Regular k s) k a
insertWithKey f k v (Map m) = SomeMapWith
  (Map $ Map.insertWithKey (f . unsafeKey) k v m)
  $ InsertProof (unsafeKey k) unsafeSubset

-- | Overwrite a key-value pair that is known to already be in the map. The set
-- of keys remains the same.
--
-- @
-- 'reinsert' k v = 'adjust (const v) k'
-- @
reinsert
  :: forall s k a. Ord k
  => Key s k -> a -> Map s k a -> Map s k a
reinsert = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.insert @k @a

-- | Insert a key-value pair into the map using a combining function, and if
-- the key was already present, the old value is returned along with the proof
-- that the key was present.
insertLookupWithKey
  :: forall s k a. Ord k
  => (Key s k -> a -> a -> a)
  -> k
  -> a
  -> Map s k a
  -> (Maybe (Key s k, a), SomeMapWith (InsertProof 'Regular k s) k a)
insertLookupWithKey f k v (Map m)
  = case Map.insertLookupWithKey (f . unsafeKey) k v m of
    (v', !m') -> ((unsafeKey k,) <$> v',)
      $ SomeMapWith (Map m') $ InsertProof (unsafeKey k) unsafeSubset

-- | If the given key is in the map, update the value at that key using the
-- given function. In any case, the set of keys remains the same.
adjust' :: forall s k a. Ord k => (a -> a) -> k -> Map s k a -> Map s k a
adjust' = coerce $ Map.adjust @k @a

-- | Update the value at a specific key known the be in the map using the given
-- function. The set of keys remains the same.
adjust :: forall s k a. Ord k => (a -> a) -> Key s k -> Map s k a -> Map s k a
adjust = gcoerceWith (unsafeCastKey @s @k) $ coerce $ adjust' @s @k @a

-- | If the given key is in the map, update the associated value using the given
-- function with a proof that the key was in the map; otherwise return the map
-- unchanged. In any case the set of keys remains the same.
adjustWithKey
  :: forall s k a. Ord k => (Key s k -> a -> a) -> k -> Map s k a -> Map s k a
adjustWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.adjustWithKey @k @a

-- | If a key is present in the map, update its value or delete it using the
-- given function, returning a potentially smaller map.
update'
  :: forall s k a. Ord k
  => (a -> Maybe a)
  -> k
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
update' f k (Map m) = SomeMapWith (Map $ Map.update f k m)
  $ SupersetProof unsafeSubset

-- | Update or delete a key known to be in the map using the given function,
-- returning a potentially smaller map.
update
  :: forall s k a. Ord k
  => (a -> Maybe a)
  -> Key s k
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
update = gcoerceWith (unsafeCastKey @s @k) $ coerce $ update' @s @k @a

-- | If a key is present in the map, update its value or delete it using the
-- given function with a proof that the key was in the map, returning a
-- potentially smaller map.
updateWithKey
  :: forall s k a. Ord k
  => (Key s k -> a -> Maybe a)
  -> k
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
updateWithKey f k (Map m) = SomeMapWith
  (Map $ Map.updateWithKey (f . unsafeKey) k m)
  $ SupersetProof unsafeSubset

-- | If the given key is in the map, update or delete it using the given
-- function with a proof that the key was in the map; otherwise the map is
-- unchanged. Alongside return the new value if it was updated, or the old value
-- if it was deleted, and a proof that the key was in the map.
updateLookupWithKey
  :: forall s k a. Ord k
  => (Key s k -> a -> Maybe a)
  -> k
  -> Map s k a
  -> (Maybe (Key s k, a), SomeMapWith (SupersetProof 'Regular s) k a)
updateLookupWithKey f k (Map m)
  = case Map.updateLookupWithKey (f . unsafeKey) k m of
    (v', !m') -> ((unsafeKey k,) <$> v',)
      $ SomeMapWith (Map m') $ SupersetProof unsafeSubset

-- | Given two maps proven to have the same keys, for each key apply the
-- function to the associated values and the key, to obtain a new map with the
-- same keys.
zipWithKey
  :: forall s k a b c. Ord k
  => (Key s k -> a -> b -> c) -> Map s k a -> Map s k b -> Map s k c
zipWithKey f (Map m1) (Map m2) = Map
  $ Map.mergeWithKey (\k x y -> Just $ f (unsafeKey k) x y)
    (\m -> if Map.null m
      then Map.empty
      else error "zipWithKey: bug: Data.Map.Refined has been subverted")
    (\m -> if Map.null m
      then Map.empty
      else error "zipWithKey: bug: Data.Map.Refined has been subverted")
    --  ^ Work around https://github.com/haskell/containers/issues/979
    m1
    m2

-- | Return the union of two maps, with a given combining function for keys that
-- exist in both maps simultaneously.
unionWith
  :: forall s t k a. Ord k
  => (a -> a -> a)
  -> Map s k a
  -> Map t k a
  -> SomeMapWith (UnionProof 'Regular s t) k a
unionWith f (Map m1) (Map m2) = SomeMapWith (Map $ Map.unionWith f m1 m2)
  $ UnionProof unsafeSubset unsafeSubsetWith2

-- | Return the union of two maps, with a given combining function for keys that
-- exist in both maps simultaneously.
--
-- You can use 'andLeft' and 'andRight' to obtain @'Key' s k@ and @'Key' t k@
-- respectively.
unionWithKey
  :: forall s t k a. Ord k
  => (Refined (InSet 'Regular s && InSet 'Regular t) k -> a -> a -> a)
  -> Map s k a
  -> Map t k a
  -> SomeMapWith (UnionProof 'Regular s t) k a
unionWithKey f (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.unionWithKey (f . reallyUnsafeRefine) m1 m2)
    $ UnionProof unsafeSubset unsafeSubsetWith2

-- | Return the first map, but for keys that appear in both maps, the given
-- function decides whether the key is removed.
differenceWith
  :: forall s t k a b. Ord k
  => (a -> b -> Maybe a)
  -> Map s k a
  -> Map t k b
  -> SomeMapWith (PartialDifferenceProof 'Regular s t) k a
differenceWith f (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.differenceWith f m1 m2)
    $ PartialDifferenceProof unsafeSubset unsafeSubset

-- | Return the first map, but for keys that appear in both maps, the given
-- function decides whether the key is removed.
--
-- You can use 'andLeft' and 'andRight' to obtain @'Key' s k@ and @'Key' t k@
-- respectively.
differenceWithKey
  :: forall s t k a b. Ord k
  => (Refined (InSet 'Regular s && InSet 'Regular t) k -> a -> b -> Maybe a)
  -> Map s k a
  -> Map t k b
  -> SomeMapWith (PartialDifferenceProof 'Regular s t) k a
differenceWithKey f (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.differenceWithKey (f . reallyUnsafeRefine) m1 m2)
    $ PartialDifferenceProof unsafeSubset unsafeSubset

-- | Return the intersection of two maps with the given combining function.
intersectionWith
  :: forall s t k a b c. Ord k
  => (a -> b -> c)
  -> Map s k a
  -> Map t k b
  -> SomeMapWith (IntersectionProof 'Regular s t) k c
intersectionWith f (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.intersectionWith f m1 m2)
    $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Return the intersection of two maps with the given combining function.
--
-- You can use 'andLeft' and 'andRight' to obtain @'Key' s k@ and @'Key' t k@
-- respectively.
intersectionWithKey
  :: forall s t k a b c. Ord k
  => (Refined (InSet 'Regular s && InSet 'Regular t) k -> a -> b -> c)
  -> Map s k a
  -> Map t k b
  -> SomeMapWith (IntersectionProof 'Regular s t) k c
intersectionWithKey f (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.intersectionWithKey (f . reallyUnsafeRefine) m1 m2)
    $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Apply a function to all values in a map. The set of keys remains the same.
map :: forall s k a b. (a -> b) -> Map s k a -> Map s k b
map = coerce $ Map.map @a @b @k

-- | Thread an accumularing argument through the map in ascending order of keys.
mapAccumLWithKey
  :: forall s k a b c. (a -> Key s k -> b -> (a, c))
  -> a
  -> Map s k b
  -> (a, Map s k c)
mapAccumLWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.mapAccumWithKey @a @k @b @c

-- | Thread an accumularing argument through the map in descending order of
-- keys.
mapAccumRWithKey
  :: forall s k a b c. (a -> Key s k -> b -> (a, c))
  -> a
  -> Map s k b
  -> (a, Map s k c)
mapAccumRWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.mapAccumRWithKey @a @k @b @c

-- | @'mapKeys' f m@ applies @f@ to each key of @m@ and collects the results
-- into a new map. For keys that were mapped to the same new key, the value
-- corresponding to the greatest of the original keys is retained.
mapKeys
  :: forall s k1 k2 a. Ord k2
  => (Key s k1 -> k2)
  -> Map s k1 a
  -> SomeMapWith (MapProof 'Regular s k1 k2) k2 a
mapKeys g (Map m)
  = SomeMapWith (Map $ Map.mapKeys (g . unsafeKey) m)
    $ MapProof (unsafeKey . g) \k2 -> case Map.lookup (unrefine k2) backMap of
      Nothing -> error "mapKeys: bug: Data.Map.Refined has been subverted"
      Just k1 -> k1
  where
    ~backMap = Map.fromList
      [ (k2, unsafeKey k1)
      | k1 <- Map.keys m
      , let !k2 = g $ unsafeKey k1
      ]

-- | @'mapKeysWith' c f m@ applies @f@ to each key of @m@ and collects the
-- results into a new map. For keys that were mapped to the same new key, @c@
-- acts as the combining function for corresponding values.
mapKeysWith
  :: forall s k1 k2 a. Ord k2
  => (a -> a -> a)
  -> (Key s k1 -> k2)
  -> Map s k1 a
  -> SomeMapWith (MapProof 'Regular s k1 k2) k2 a
mapKeysWith f g (Map m)
  = SomeMapWith (Map $ Map.mapKeysWith f (g . unsafeKey) m)
    $ MapProof (unsafeKey . g) \k2 -> case Map.lookup (unrefine k2) backMap of
      Nothing -> error "mapKeysWith: bug: Data.Map.Refined has been subverted"
      Just k1 -> k1
  where
    ~backMap = Map.fromList
      [ (k2, unsafeKey k1)
      | k1 <- Map.keys m
      , let !k2 = g $ unsafeKey k1
      ]

-- | Apply a function to all values in a map and collect only the 'Just'
-- results, returning a potentially smaller map.
mapMaybe
  :: forall s k a b. (a -> Maybe b)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k b
mapMaybe f (Map m) = SomeMapWith (Map $ Map.mapMaybe f m)
  $ SupersetProof unsafeSubset

-- | Apply a function to all values in a map, together with their corresponding
-- keys, and collect only the 'Just' results, returning a potentially smaller
-- map.
mapMaybeWithKey
  :: forall s k a b. (Key s k -> a -> Maybe b)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k b
mapMaybeWithKey f (Map m)
  = SomeMapWith (Map $ Map.mapMaybeWithKey (f . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Apply a function to all values in a map and collect the 'Left' and 'Right'
-- results into separate (disjoint) maps.
mapEither
  :: forall s k a b c. Ord k -- TODO: this is only used in the proof
  => (a -> Either b c)
  -> Map s k a
  -> Some2MapWith (PartitionProof 'Regular s k) k b c
mapEither p (Map m) = case Map.mapEither p m of
  (m1, m2) -> Some2MapWith (Map m1) (Map m2) $ PartitionProof
    do \k -> case Map.lookup (unrefine k) m of
        Nothing -> error
          "mapEither: bug: Data.Map.Refined has been subverted"
        Just x -> case p x of
          Left _ -> Left $ unsafeKey $ unrefine k
          Right _ -> Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Apply a function to all values in a map, together with their corresponding
-- keys, and collect the 'Left' and 'Right' results into separate (disjoint)
-- maps.
mapEitherWithKey
  :: forall s k a b c. Ord k -- TODO: this is only used in the proof
  => (Key s k -> a -> Either b c)
  -> Map s k a
  -> Some2MapWith (PartitionProof 'Regular s k) k b c
mapEitherWithKey p (Map m) = case Map.mapEitherWithKey (p . unsafeKey) m of
  (m1, m2) -> Some2MapWith (Map m1) (Map m2) $ PartitionProof
    do \k -> case Map.lookup (unrefine k) m of
        Nothing
          -> error "mapEitherWithKey: bug: Data.Map.Refined has been subverted"
        Just x -> case p k x of
          Left _ -> Left $ unsafeKey $ unrefine k
          Right _ -> Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Update or delete the value at the smallest key, returning a potentially
-- smaller map.
updateMinWithKey
  :: forall s k a. (Key s k -> a -> Maybe a)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
updateMinWithKey f (Map m)
  = SomeMapWith (Map $ Map.updateMinWithKey (f . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Update or delete the value at the largest key, returning a potentially
-- smaller map.
updateMaxWithKey
  :: forall s k a. (Key s k -> a -> Maybe a)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
updateMaxWithKey f (Map m)
  = SomeMapWith (Map $ Map.updateMaxWithKey (f . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Adjust the value at the smallest key. The set of keys remains unchanged.
adjustMinWithKey :: forall s k a. (Key s k -> a -> a) -> Map s k a -> Map s k a
adjustMinWithKey f (Map m)
  = Map $ Map.updateMinWithKey ((Just .) . f . unsafeKey) m

-- | Adjust the value at the greatest key. The set of keys remains unchanged.
adjustMaxWithKey :: forall s k a. (Key s k -> a -> a) -> Map s k a -> Map s k a
adjustMaxWithKey f (Map m)
  = Map $ Map.updateMaxWithKey ((Just .) . f . unsafeKey) m

-- | Apply the inverse image of the given function to the keys of the given map,
-- so that for all @k :: 'Key' s2 k2@,
-- @'backpermuteKeys' f m '!' k = m '!' f k@.
--
-- If maps are identified with functions, this computes the composition.
backpermuteKeys
  :: forall s1 s2 k1 k2 a. (Ord k1, KnownSet s2 k2)
  => (Key s2 k2 -> Key s1 k1) -> Map s1 k1 a -> Map s2 k2 a
backpermuteKeys f m = fromSet \k -> m ! f k
