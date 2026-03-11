{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Map.Common.Refined where

import           Control.Monad.Reader
import           Control.DeepSeq
import           Data.Coerce
import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Proofs
import           Data.Container.Refined.Unsafe
import           Data.Distributive
import           Data.Foldable.WithIndex
import           Data.Functor.Rep
import           Data.Functor.WithIndex
import qualified Data.Hashable as Hashable
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection
import qualified Data.Set as Set
import           Data.Traversable.WithIndex
import           Data.Type.Coercion
import           Data.Type.Equality ((:~:)(..))
import           Prelude hiding (zipWith)
import           Refined
import           Refined.Unsafe
import           Unsafe.Coerce

#if MIN_VERSION_containers(0, 6, 3)
import           Data.Bifoldable
#endif

#if MIN_VERSION_containers(0, 6, 2)
#elif MIN_VERSION_containers(0, 5, 8)
import           Data.Functor.Const (Const(..))
import           Data.Monoid (Any(..))
import qualified Data.Map.Merge.Lazy as Map
#else
import qualified Data.List as List
import qualified Data.Map.Strict as MapStrict
#endif

#if MIN_VERSION_containers(0, 5, 9) || MIN_VERSION_hashable(1, 4, 0)
import           Data.Functor.Classes
#endif


-- | A wrapper around a regular 'Data.Map.Map' with a type parameter @s@
-- identifying the set of keys present in the map.
--
-- A key of type @k@ may not be present in the map, but a @'Key' s k@ is
-- guaranteed to be present (if the @s@ parameters match). Thus the map is
-- isomorphic to a (total) function @'Key' s k -> a@, which motivates many of
-- the instances below.
--
-- A 'Map' always knows its set of keys, so given @'Map' s k a@ we can always
-- derive @'KnownSet' s k@ by pattern matching on the 'Dict' returned by
-- 'keysSet'.
newtype Map s k a = Map (Map.Map k a)
  deriving newtype (Eq, Ord, Show, Functor, Foldable, NFData)
#if MIN_VERSION_hashable(1, 3, 4)
  deriving newtype (Hashable.Hashable)
#endif
#if MIN_VERSION_containers(0, 6, 3)
  deriving newtype (Bifoldable)
#endif
#if MIN_VERSION_containers(0, 5, 9) || MIN_VERSION_hashable(1, 4, 0)
  deriving newtype (Eq1, Eq2, Ord1, Ord2, Show1, Show2)
#endif
  deriving stock (Traversable)
type role Map nominal nominal representational

-- | Convert to a regular 'Data.Map.Map', forgetting its set of keys.
toMap :: forall s k a. Map s k a -> Map.Map k a
toMap (Map m) = m

-- | @'Key' s k@ is a key of type @k@ that has been verified to be an element
-- of the set @s@, and thus verified to be present in all @'Map' s k@ maps.
--
-- Thus, @'Key' s k@ is a \"refinement\" type of @k@, and this library
-- integrates with an implementation of refimenement types in "Refined", so
-- the machinery from there can be used to manipulate 'Key's (however see
-- 'Data.Set.Refined.revealPredicate').
--
-- The underlying @k@ value can be obtained with 'unrefine'. A @k@ can be
-- validated into an @'Key' s k@ with 'member'.
type Key s = Refined (InSet 'Regular s)

unsafeCastKey :: forall s k. Coercion k (Key s k)
unsafeCastKey = reallyUnsafeUnderlyingRefined

unsafeKey :: k -> Key s k
unsafeKey = coerceWith unsafeCastKey

-- | An existential wrapper for a 'Map' with an as-yet-unknown set of keys.
-- Pattern maching on it gives you a way to refer to the set (the parameter
-- @s@), e.g.
--
-- @
-- case 'fromMap' ... of
--   'SomeMap' \@s m -> doSomethingWith \@s
--
-- case 'fromMap' ... of
--   'SomeMap' (m :: 'Map' s k a) -> doSomethingWith \@s
-- @
data SomeMap k a where
  SomeMap :: forall s k a. !(Map s k a) -> SomeMap k a

-- | Apply a map with an unknown set of keys to a continuation that can accept
-- a map with any set of keys. This gives you a way to refer to the set (the
-- parameter @s@), e.g.:
--
-- @
-- 'withMap' ('fromMap' ...) $ \\(m :: 'Map' s k a) -> doSomethingWith \@s
-- @
withMap :: forall k a r. SomeMap k a -> (forall s. Map s k a -> r) -> r
withMap (SomeMap m) k = k m

-- | Construct a map from a regular 'Data.Map.Map'.
fromMap :: forall k a. Map.Map k a -> SomeMap k a
fromMap m = SomeMap (Map m)

-- | Given a set of keys @s@ known ahead of time, verify whether a regular
-- 'Data.Map.Map' has exactly that set of keys.
verifyMap
  :: forall s k a. (Eq k, KnownSet s k) => Map.Map k a -> Maybe (Map s k a)
verifyMap m
  | Map.keys m == Set.toList (reflect $ Proxy @s) = Just (Map m)
  | otherwise = Nothing

-- | An existential wrapper for a 'Map' with an as-yet-unknown set of keys,
-- together with a proof of some fact @p@ about the set. Pattern matching on it
-- gives you a way to refer to the set (the parameter @s@). Functions that
-- change the set of keys in a map will return the map in this way, together
-- with a proof that somehow relates the keys set to the function's inputs.
data SomeMapWith p k a where
  SomeMapWith :: forall s k a p. !(Map s k a) -> !(p s) -> SomeMapWith p k a

-- | Apply a map with proof for an unknown set of keys to a continuation that
-- can accept a map with any set of keys satisfying the proof. This gives you a
-- way to refer to the set (the parameter @s@).
withMapWith
  :: forall k a r p. SomeMapWith p k a -> (forall s. Map s k a -> p s -> r) -> r
withMapWith (SomeMapWith m p) k = k m p

-- | An existential wrapper for a pair of maps with as-yet-unknown sets of keys,
-- together with a proof of some fact @p@ relating them.
data Some2MapWith p k a b where
  Some2MapWith
    :: forall s t k a b p. !(Map s k a)
    -> !(Map t k b)
    -> !(p s t)
    -> Some2MapWith p k a b

-- | Apply a pair of maps with proof for unknown sets of keys to a continuation
-- that can accept any pair of maps with any sets of keys satisfying the proof.
-- This gives you a way to refer to the sets (the parameters @s@ and @t@).
with2MapWith
  :: forall k a b r p. Some2MapWith p k a b
  -> (forall s t. Map s k a -> Map t k b -> p s t -> r)
  -> r
with2MapWith (Some2MapWith m1 m2 p) k = k m1 m2 p

-- | An empty map.
empty :: forall k a. SomeMapWith (EmptyProof 'Regular) k a
empty = SomeMapWith (Map Map.empty) $ EmptyProof unsafeSubset

-- | Create a map from a set of keys, and a function that for each key computes
-- the corresponding value.
fromSet :: forall s k a. KnownSet s k => (Key s k -> a) -> Map s k a
fromSet f = Map $ Map.fromSet (f . unsafeKey) (reflect $ Proxy @s)

-- | Delete a key and its value from the map if present, returning a potentially
-- smaller map.
delete
  :: forall s k a. Ord k
  => k -> Map s k a -> SomeMapWith (SupersetProof 'Regular s) k a
delete k (Map m) = SomeMapWith (Map $ Map.delete k m)
  $ SupersetProof unsafeSubset

-- | If the key is in the map, return the proof of this, and the associated
-- value; otherwise return 'Nothing'.
lookup :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k, a)
lookup k (Map m) = (unsafeKey k,) <$> Map.lookup k m

-- | Given a key that is proven to be in the map, return the associated value.
--
-- Unlike 'Data.Map.!' from "Data.Map", this function is total, as it is
-- impossible to obtain a @'Key' s k@ for a key that is not in the map
-- @'Map' s k a@.
(!) :: forall s k a. Ord k => Map s k a -> Key s k -> a
(!) (Map m) k = case Map.lookup (unrefine k) m of
  Nothing -> error "(!): bug: Data.Map.Refined has been subverted"
  Just x -> x

-- | If a key is in the map, return the proof that it is.
member :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k)
member k (Map m)
  | k `Map.member` m = Just (unsafeKey k)
  | otherwise = Nothing

-- | Find the largest key smaller than the given one, and return the
-- associated key-value pair.
lookupLT :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k, a)
lookupLT = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.lookupLT @k @a

-- | Find the smallest key greater than the given one, and return the
-- associated key-value pair.
lookupGT :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k, a)
lookupGT = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.lookupGT @k @a

-- | Find the largest key smaller or equal to the given one, and return the
-- associated key-value pair.
lookupLE :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k, a)
lookupLE = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.lookupLE @k @a

-- | Find the smallest key greater or equal to the given one, and return the
-- associated key-value pair.
lookupGE :: forall s k a. Ord k => k -> Map s k a -> Maybe (Key s k, a)
lookupGE = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.lookupGE @k @a

-- | If a map is empty, return a proof that it is.
null :: forall s k a. Map s k a -> Maybe (EmptyProof 'Regular s)
null (Map m)
  | Map.null m = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If all keys of the first map are also present in the second map, and the
-- given function returns 'True' for their associated values, return a proof
-- that the keys form a subset.
isSubmapOfBy
  :: forall s t k a b. Ord k
  => (a -> b -> Bool)
  -> Map s k a
  -> Map t k b
  -> Maybe (SubsetProof 'Regular s t)
isSubmapOfBy f (Map m1) (Map m2)
  | Map.isSubmapOfBy f m1 m2 = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If two maps are disjoint (i.e. their intersection is empty), return a proof
-- of that.
disjoint
  :: forall s t k a b. Ord k
  => Map s k a -> Map t k b -> Maybe (DisjointProof 'Regular s t)
disjoint (Map m1) (Map m2)
#if MIN_VERSION_containers(0, 6, 2)
  | Map.disjoint m1 m2
#elif MIN_VERSION_containers(0, 5, 8)
  | Const (Any False) <- Map.mergeA
    (Map.traverseMissing \_ _ -> Const mempty)
    (Map.traverseMissing \_ _ -> Const mempty)
    (Map.zipWithAMatched \_ _ _ -> Const $ Any True)
    m1
    m2
#else
  | Map.null $ MapStrict.intersectionWith (\_ _ -> ()) m1 m2
#endif
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | Given two maps proven to have the same keys, for each key apply the
-- function to the associated values, to obtain a new map with the same keys.
zipWith
  :: forall s k a b c. Ord k
  => (a -> b -> c) -> Map s k a -> Map s k b -> Map s k c
zipWith f (Map m1) (Map m2) = Map
  $ Map.mergeWithKey (\_ x y -> Just $ f x y)
    (\m -> if Map.null m
      then Map.empty
      else error "zipWith: bug: Data.Map.Refined has been subverted")
    (\m -> if Map.null m
      then Map.empty
      else error "zipWith: bug: Data.Map.Refined has been subverted")
    --  ^ Work around https://github.com/haskell/containers/issues/979
    m1
    m2

-- | Return the union of two maps. For keys that exist in both maps, the value
-- is taken from the first map.
--
-- @
-- 'union' = unionWith 'const'
-- @
union
  :: forall s t k a. Ord k
  => Map s k a -> Map t k a -> SomeMapWith (UnionProof 'Regular s t) k a
union (Map m1) (Map m2) = SomeMapWith (Map $ Map.union m1 m2)
  $ UnionProof unsafeSubset unsafeSubsetWith2

-- | Remove the keys that appear in the second map from the first map.
difference
  :: forall s t k a b. Ord k
  => Map s k a -> Map t k b -> SomeMapWith (DifferenceProof 'Regular s t) k a
difference (Map m1) (Map m2) = SomeMapWith (Map $ Map.difference m1 m2)
  $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Return the intersection of two maps, taking values from the first map.
--
-- @
-- 'intersection' = intersectionWith 'const'
-- @
intersection
  :: forall s t k a b. Ord k
  => Map s k a
  -> Map t k b
  -> SomeMapWith (IntersectionProof 'Regular s t) k a
intersection (Map m1) (Map m2)
  = SomeMapWith (Map $ Map.intersection m1 m2)
    $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Apply a function to all values in a map, together with their corresponding
-- keys, that are proven to be in the map. The set of keys remains the same.
mapWithKey :: forall s k a b. (Key s k -> a -> b) -> Map s k a -> Map s k b
mapWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.mapWithKey @k @a @b

-- | Map an 'Applicative' transformation in ascending order of keys, with access
-- to each value's corresponding key and a proof that it is in the map. The set
-- of keys remains unchanged.
traverseWithKey
  :: forall s f k a b. Applicative f
  => (Key s k -> a -> f b) -> Map s k a -> f (Map s k b)
traverseWithKey f (Map m) = Map <$> Map.traverseWithKey (f . unsafeKey) m

-- | Map each key-value pair of a map into a monoid (with proof that the key was
-- in the map), and combine the results using '<>'.
foldMapWithKey
  :: forall s k a m. Monoid m => (Key s k -> a -> m) -> Map s k a -> m
foldMapWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.foldMapWithKey @m @k @a

-- | Right associative fold with a lazy accumulator.
foldrWithKey :: forall s k a b. (Key s k -> a -> b -> b) -> b -> Map s k a -> b
foldrWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.foldrWithKey @k @a @b

-- | Left associative fold with a lazy accumulator.
foldlWithKey :: forall s k a b. (b -> Key s k -> a -> b) -> b -> Map s k a -> b
foldlWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.foldlWithKey @b @k @a

-- | Right associative fold with a strict accumulator.
foldrWithKey' :: forall s k a b. (Key s k -> a -> b -> b) -> b -> Map s k a -> b
foldrWithKey' = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.foldrWithKey' @k @a @b

-- | Left associative fold with a strict accumulator.
foldlWithKey' :: forall s k a b. (b -> Key s k -> a -> b) -> b -> Map s k a -> b
foldlWithKey' = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ Map.foldlWithKey' @b @k @a

-- | Return the set of keys in the map, with the contents of the set still
-- tracked by the @s@ parameter. See "Data.Set.Refined".
keysSet :: forall s k a. Map s k a -> Set s k
keysSet (Map m) = reify (Map.keysSet m)
  \(_ :: Proxy s') -> case unsafeCoerce Refl :: s :~: s' of
    Refl -> Dict

-- | Convert to a list of key-value pairs in ascending order of keys.
toList :: forall s k a. Map s k a -> [(Key s k, a)]
toList = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.toAscList @k @a

-- | Convert to a list of key-value pairs in descending order of keys.
toDescList :: forall s k a. Map s k a -> [(Key s k, a)]
toDescList = gcoerceWith (unsafeCastKey @s @k) $ coerce $ Map.toDescList @k @a

-- | Retain only the values that satisfy the predicate, returning a potentially
-- smaller map.
filter
  :: forall s k a. (a -> Bool)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
filter p (Map m) = SomeMapWith (Map $ Map.filter p m)
  $ SupersetProof unsafeSubset

-- | Retain only the keys that satisfy the predicate, returning a potentially
-- smaller map.
filterKeys
  :: forall s k a. (Key s k -> Bool)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
filterKeys p (Map m) = SomeMapWith
#if MIN_VERSION_containers(0, 8, 0)
  (Map $ Map.filterKeys (p . unsafeKey) m)
#else
  (Map $ Map.filterWithKey (\k _ -> p (unsafeKey k)) m)
#endif
  $ SupersetProof unsafeSubset

-- | Retain only the key-value pairs that satisfy the predicate, returning a
-- potentially smaller map.
filterWithKey
  :: forall s k a. (Key s k -> a -> Bool)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
filterWithKey p (Map m)
  = SomeMapWith (Map $ Map.filterWithKey (p . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Restrict a map to only those keys that are elements of @t@.
restrictKeys
  :: forall s t k a. (Ord k, KnownSet t k)
  => Map s k a -> SomeMapWith (IntersectionProof 'Regular s t) k a
restrictKeys (Map m) = SomeMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (Map $ Map.restrictKeys m $ reflect $ Proxy @t)
#else
  (Map $ Map.intersectionWith const m $ Map.fromSet id $ reflect $ Proxy @t)
#endif
  $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Remove all keys that are elements of @t@ from the map.
withoutKeys
  :: forall s t k a. (Ord k, KnownSet t k)
  => Map s k a -> SomeMapWith (DifferenceProof 'Regular s t) k a
withoutKeys (Map m) = SomeMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (Map $ Map.withoutKeys m $ reflect $ Proxy @t)
#else
  (Map $ Map.difference m $ Map.fromSet id $ reflect $ Proxy @t)
#endif
  $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Partition a map into two disjoint submaps: those whose values satisfy the
-- predicate, and those whose don't.
partition
  :: forall s k a. Ord k -- TODO: this is only used in the proof
  => (a -> Bool)
  -> Map s k a
  -> Some2MapWith (PartitionProof 'Regular s k) k a a
partition p (Map m) = case Map.partition p m of
  (m1, m2) -> Some2MapWith (Map m1) (Map m2) $ PartitionProof
    do \k -> case Map.lookup (unrefine k) m of
        Nothing -> error "partition: bug: Data.Map.Refined has been subverted"
        Just x -> if p x
          then Left $ unsafeKey $ unrefine k
          else Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Partition a map into two disjoint submaps: those whose key-value pairs
-- satisfy the predicate, and those whose don't.
partitionWithKey
  :: forall s k a. Ord k -- TODO: this is only used in the proof
  => (Key s k -> a -> Bool)
  -> Map s k a
  -> Some2MapWith (PartitionProof 'Regular s k) k a a
partitionWithKey p (Map m) = case Map.partitionWithKey (p . unsafeKey) m of
  (m1, m2) -> Some2MapWith (Map m1) (Map m2) $ PartitionProof
    do \k -> case Map.lookup (unrefine k) m of
        Nothing
          -> error "partitionWithKey: bug: Data.Map.Refined has been subverted"
        Just x -> if p k x
          then Left $ unsafeKey $ unrefine k
          else Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Take the a submap of keys up to a point where the predicate stops holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
takeWhileAntitone
  :: forall s k a. (Key s k -> Bool)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
takeWhileAntitone p (Map m) = SomeMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (Map $ Map.takeWhileAntitone (p . unsafeKey) m)
#else
  (Map $ Map.fromDistinctAscList
    $ List.takeWhile (p . unsafeKey . fst) $ Map.toAscList m)
#endif
  $ SupersetProof unsafeSubset

-- | Take the a submap of keys starting from a point where the predicate stops
-- holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
dropWhileAntitone
  :: forall s k a. (Key s k -> Bool)
  -> Map s k a
  -> SomeMapWith (SupersetProof 'Regular s) k a
dropWhileAntitone p (Map m) = SomeMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (Map $ Map.dropWhileAntitone (p . unsafeKey) m)
#else
  (Map $ Map.fromDistinctAscList
    $ List.dropWhile (p . unsafeKey . fst) $ Map.toAscList m)
#endif
  $ SupersetProof unsafeSubset

-- | Divide a map into two disjoint submaps at a point where the predicate on
-- the keys stops holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
spanAntitone
  :: forall s k a. (Key s k -> Bool)
  -> Map s k a
  -> Some2MapWith (PartialPartitionProof 'Regular s) k a a
spanAntitone p (Map m) =
#if MIN_VERSION_containers(0, 5, 8)
  case Map.spanAntitone (p . unsafeKey) m of
    (m1, m2)
#else
  case List.span (p . unsafeKey . fst) $ Map.toAscList m of
    (xs1, xs2)
      | let m1 = Map.fromDistinctAscList xs1
      , let m2 = Map.fromDistinctAscList xs2
#endif
      -> Some2MapWith (Map m1) (Map m2) $ PartialPartitionProof
        unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Return two disjoint submaps: those whose keys are less than the given key,
-- and those whose keys are greater than the given key. If the key was in the
-- map, also return the associated value and the proof that it was in the map.
splitLookup
  :: forall s k a. Ord k
  => k -> Map s k a -> Some2MapWith (SplitProof 'Regular s (Key s k, a)) k a a
splitLookup k (Map m) = case Map.splitLookup k m of
  (!m1, v, !m2) -> Some2MapWith (Map m1) (Map m2) $ SplitProof
    ((unsafeKey k,) <$> v) unsafeSubset \f g -> unsafeSubsetWith2 f g

-- | Retrieves the key-value pair corresponding to the smallest key of the map,
-- and the map with that pair removed; or a proof that the map was empty.
minViewWithKey
  :: forall s k a. Map s k a
  -> Either
    (EmptyProof 'Regular s)
    ((Key s k, a), SomeMapWith (SupersetProof 'Regular s) k a)
minViewWithKey (Map m) = case Map.minViewWithKey m of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (kv, m') -> Right $ (gcoerceWith (unsafeCastKey @s @k) $ coerce kv,)
    $ SomeMapWith (Map m') $ SupersetProof unsafeSubset

-- | Retrieves the key-value pair corresponding to the greatest key of the map,
-- and the map with that pair removed; or a proof that the map was empty.
maxViewWithKey
  :: forall s k a. Map s k a
  -> Either
    (EmptyProof 'Regular s)
    ((Key s k, a), SomeMapWith (SupersetProof 'Regular s) k a)
maxViewWithKey (Map m) = case Map.maxViewWithKey m of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (kv, m') -> Right $ (gcoerceWith (unsafeCastKey @s @k) $ coerce kv,)
    $ SomeMapWith (Map m') $ SupersetProof unsafeSubset

-- | If elements of @s@ can be weakened to elements of @t@ and vice versa, then
-- @s@ and @t@ actually stand for the same set and @'Key' s@ can be safely
-- interconverted with @'Key' t@.
--
-- The requirement that the weakenings are natural transformations ensures that
-- they don't actually alter the keys. To build these you can compose ':->''s
-- from proofs returned by functions in this module, or "Refined" functions like
-- 'andLeft' or 'leftOr'.
castKey
  :: forall s t k. (forall x. Key s x -> Key t x)
  -> (forall x. Key t x -> Key s x)
  -> Coercion (Key s k) (Key t k)
castKey = castRefined

-- | If keys can be interconverted (e.g. as proved by 'castKey'), then the maps
-- can be interconverted too. For example, 'Data.Map.Refined.zipWithKey' can be
-- implemented via 'Data.Map.Refined.intersectionWithKey' by proving that the
-- set of keys remains unchanged:
--
-- @
-- 'Data.Map.Refined.zipWithKey'
--   :: forall s k a b c. 'Ord' k
--   => ('Key' s k -> a -> b -> c) -> 'Map' s k a -> 'Map' s k b -> 'Map' s k c
-- 'Data.Map.Refined.zipWithKey' f m1 m2
--   | v'SomeMapWith' @r m proof <- 'Data.Map.Refined.intersectionWithKey' (f . 'andLeft') m1 m2
--   , v'IntersectionProof' p1 p2 <- proof
--   , ( v'Coercion' :: t'Coercion' ('Map' r k c) ('Map' s k c))
--     <- app $ 'cast' $ 'castKey' ('andLeft' . p1) (p2 'id' 'id')
--   = 'coerce' m
--   where
--     app :: t'Coercion' f g -> t'Coercion' (f x) (g x)
--     app v'Coercion' = v'Coercion'
-- @
cast
  :: forall s t k. (forall x. Coercion (Key s x) (Key t x))
  -> Coercion (Map s k) (Map t k)
cast Coercion = Coercion

instance FunctorWithIndex (Key s k) (Map s k) where
  imap = mapWithKey

instance FoldableWithIndex (Key s k) (Map s k) where
  ifoldMap = foldMapWithKey

instance TraversableWithIndex (Key s k) (Map s k) where
  itraverse = traverseWithKey

-- | Similar to the instance for functions -- zip corresponding keys. To use
-- '<*>'/'Control.Applicative.liftA2' without 'KnownSet' see 'zipWith'.
instance (Ord k, KnownSet s k) => Applicative (Map s k) where
  pure x = fromSet \_ -> x
  (<*>) = zipWith id

-- | @'bind' m f@ is a map that for each key @k :: 'Key' s k@, contains the
-- value @f (m '!' k) '!' k@, similar to @'>>='@ for functions.
bind :: forall s k a b. Ord k => Map s k a -> (a -> Map s k b) -> Map s k b
bind m f = mapWithKey (\k x -> f x ! k) m

-- | Similar to the instance for functions. To use '>>=' without 'KnownSet' see
-- 'bind'.
instance (Ord k, KnownSet s k) => Monad (Map s k) where
  (>>=) = bind

-- | Similar to the instance for functions. See also
-- 'Data.Map.Refined.backpermuteKeys'.
instance (Ord k, KnownSet s k) => MonadReader (Key s k) (Map s k) where
  ask = fromSet id
  local f m = mapWithKey (\k _ -> m ! f k) m

-- | Append the values at the corresponding keys
instance (Ord k, Semigroup a) => Semigroup (Map s k a) where
  (<>) = zipWith (<>)

instance (Ord k, KnownSet s k, Monoid a) => Monoid (Map s k a) where
  mempty = fromSet \_ -> mempty

-- | Similar to the instance for functions
instance (Ord k, KnownSet s k) => Distributive (Map s k) where
  collect = collectRep
  distribute = distributeRep

-- | Witness isomorphism with functions from @'Key' s k@
instance (Ord k, KnownSet s k) => Representable (Map s k) where
  type Rep (Map s k) = Key s k
  index = (!)
  tabulate = fromSet

#if MIN_VERSION_hashable(1, 3, 4)
#else
instance (Hashable.Hashable a, Hashable.Hashable k)
  => Hashable.Hashable (Map s k a) where
  hashWithSalt s (Map m) = Map.foldlWithKey'
    (\s' k v -> Hashable.hashWithSalt (Hashable.hashWithSalt s' k) v)
    (Hashable.hashWithSalt s (Map.size m))
    m
#endif
