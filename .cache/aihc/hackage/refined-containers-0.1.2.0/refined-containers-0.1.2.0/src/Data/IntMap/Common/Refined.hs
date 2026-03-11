{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.IntMap.Common.Refined where

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
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Proxy
import           Data.Reflection
import           Data.Traversable.WithIndex
import           Data.Type.Coercion
import           Data.Type.Equality ((:~:)(..))
import           Prelude hiding (zipWith)
import           Refined
import           Refined.Unsafe
import           Unsafe.Coerce

#if MIN_VERSION_containers(0, 6, 7)
#elif MIN_VERSION_containers(0, 6, 2)
import qualified Data.List as List
#elif MIN_VERSION_containers(0, 5, 8)
import           Data.Functor.Const (Const(..))
import qualified Data.List as List
import           Data.Monoid (Any(..))
import qualified Data.IntMap.Merge.Lazy as IntMap
#else
import qualified Data.IntMap.Strict as IntMapStrict
import qualified Data.List as List
#endif

#if MIN_VERSION_containers(0, 5, 9) || MIN_VERSION_hashable(1, 4, 0)
import           Data.Functor.Classes
#endif

-- | A wrapper around a regular 'Data.IntMap.IntMap' with a type parameter @s@
-- identifying the set of keys present in the map.
--
-- An t'Int' key may not be present in the map, but a @'Key' s@ is guaranteed to
-- be present (if the @s@ parameters match). Thus the map is isomorphic to a
-- (total) function @'Key' s -> a@, which motivates many of the instances below.
--
-- An 'IntMap' always knows its set of keys, so given @'IntMap' s a@ we can
-- always derive @'KnownIntSet' s@ by pattern matching on the 'Dict' returned by
-- 'keysSet'.
newtype IntMap s a = IntMap (IntMap.IntMap a)
  deriving newtype (Eq, Ord, Show, Functor, Foldable, NFData)
#if MIN_VERSION_hashable(1, 3, 4)
  deriving newtype (Hashable.Hashable)
#endif
#if MIN_VERSION_containers(0, 5, 9) || MIN_VERSION_hashable(1, 4, 0)
  deriving newtype (Eq1, Ord1, Show1)
#endif
  deriving stock (Traversable)
type role IntMap nominal representational

-- | Convert to a regular 'Data.IntMap.IntMap', forgetting its set of keys.
toIntMap :: forall s a. IntMap s a -> IntMap.IntMap a
toIntMap (IntMap m) = m

-- | @'Key' s@ is a key of type t'Int' that has been verified to be an element
-- of the set @s@, and thus verified to be present in all @'IntMap' s k@ maps.
--
-- Thus, @'Key' s@ is a \"refinement\" type of t'Int', and this library
-- integrates with an implementation of refimenement types in "Refined", so
-- the machinery from there can be used to manipulate 'Key's (however see
-- 'Data.IntSet.Refined.revealPredicate').
--
-- The underlying t'Int' value can be obtained with 'unrefine'. An t'Int' can be
-- validated into an @'Key' s@ with 'member'.
type Key s = Refined (InSet 'Int s) Int

unsafeCastKey :: forall s. Coercion Int (Key s)
unsafeCastKey = reallyUnsafeUnderlyingRefined

unsafeKey :: Int -> Key s
unsafeKey = coerceWith unsafeCastKey

-- | An existential wrapper for an 'IntMap' with an as-yet-unknown set of keys.
-- Pattern maching on it gives you a way to refer to the set (the parameter
-- @s@), e.g.
--
-- @
-- case 'fromIntMap' ... of
--   'SomeIntMap' \@s m -> doSomethingWith \@s
--
-- case 'fromIntMap' ... of
--   'SomeIntMap' (m :: 'IntMap' s a) -> doSomethingWith \@s
-- @
data SomeIntMap a where
  SomeIntMap :: forall s a. !(IntMap s a) -> SomeIntMap a

-- | Apply a map with an unknown set of keys to a continuation that can accept
-- a map with any set of keys. This gives you a way to refer to the set (the
-- parameter @s@), e.g.:
--
-- @
-- 'withIntMap' ('fromIntMap' ...)
--   $ \\(m :: 'IntMap' s a) -> doSomethingWith \@s
-- @
withIntMap :: forall a r. SomeIntMap a -> (forall s. IntMap s a -> r) -> r
withIntMap (SomeIntMap m) k = k m

-- | Construct a map from a regular 'Data.IntMap.IntMap'.
fromIntMap :: forall a. IntMap.IntMap a -> SomeIntMap a
fromIntMap m = SomeIntMap (IntMap m)

-- | Given a set of keys @s@ known ahead of time, verify whether a regular
-- 'Data.IntMap.IntMap' has exactly that set of keys.
verifyIntMap
  :: forall s a. KnownIntSet s => IntMap.IntMap a -> Maybe (IntMap s a)
verifyIntMap m
  | IntMap.keys m == IntSet.toList (reflect $ Proxy @s) = Just (IntMap m)
  | otherwise = Nothing

-- | An existential wrapper for an 'IntMap' with an as-yet-unknown set of keys,
-- together with a proof of some fact @p@ about the set. Pattern matching on it
-- gives you a way to refer to the set (the parameter @s@). Functions that
-- change the set of keys in a map will return the map in this way, together
-- with a proof that somehow relates the keys set to the function's inputs.
data SomeIntMapWith p a where
  SomeIntMapWith :: forall s a p. !(IntMap s a) -> !(p s) -> SomeIntMapWith p a

-- | Apply a map with proof for an unknown set of keys to a continuation that
-- can accept a map with any set of keys satisfying the proof. This gives you a
-- way to refer to the set (the parameter @s@).
withIntMapWith
  :: forall a r p. SomeIntMapWith p a -> (forall s. IntMap s a -> p s -> r) -> r
withIntMapWith (SomeIntMapWith m p) k = k m p

-- | An existential wrapper for a pair of maps with as-yet-unknown sets of keys,
-- together with a proof of some fact @p@ relating them.
data Some2IntMapWith p a b where
  Some2IntMapWith
    :: forall s t a b p. !(IntMap s a)
    -> !(IntMap t b)
    -> !(p s t)
    -> Some2IntMapWith p a b

-- | Apply a pair of maps with proof for unknown sets of keys to a continuation
-- that can accept any pair of maps with any sets of keys satisfying the proof.
-- This gives you a way to refer to the sets (the parameters @s@ and @t@).
with2IntMapWith
  :: forall a b r p. Some2IntMapWith p a b
  -> (forall s t. IntMap s a -> IntMap t b -> p s t -> r)
  -> r
with2IntMapWith (Some2IntMapWith m1 m2 p) k = k m1 m2 p

-- | An empty map.
empty :: forall a. SomeIntMapWith (EmptyProof 'Int) a
empty = SomeIntMapWith (IntMap IntMap.empty) $ EmptyProof unsafeSubset

-- | Create a map from a set of keys, and a function that for each key computes
-- the corresponding value.
fromSet :: forall s a. KnownIntSet s => (Key s -> a) -> IntMap s a
fromSet f = IntMap $ IntMap.fromSet (f . unsafeKey) (reflect $ Proxy @s)

-- | Delete a key and its value from the map if present, returning a potentially
-- smaller map.
delete :: forall s a. Int -> IntMap s a -> SomeIntMapWith (SupersetProof 'Int s) a
delete k (IntMap m) = SomeIntMapWith (IntMap $ IntMap.delete k m)
  $ SupersetProof unsafeSubset

-- | If the key is in the map, return the proof of this, and the associated
-- value; otherwise return 'Nothing'.
lookup :: forall s a. Int -> IntMap s a -> Maybe (Key s, a)
lookup k (IntMap m) = (unsafeKey k,) <$> IntMap.lookup k m

-- | Given a key that is proven to be in the map, return the associated value.
--
-- Unlike 'Data.IntMap.!' from "Data.IntMap", this function is total, as it is
-- impossible to obtain a @'Key' s@ for a key that is not in the map
-- @'IntMap' s a@.
(!) :: forall s a. IntMap s a -> Key s -> a
(!) (IntMap m) k = case IntMap.lookup (unrefine k) m of
  Nothing -> error "(!): bug: Data.IntMap.Refined has been subverted"
  Just x -> x

-- | If a key is in the map, return the proof that it is.
member :: forall s a. Int -> IntMap s a -> Maybe (Key s)
member k (IntMap m)
  | k `IntMap.member` m = Just (unsafeKey k)
  | otherwise = Nothing

-- | Find the largest key smaller than the given one, and return the
-- associated key-value pair.
lookupLT :: forall s a. Int -> IntMap s a -> Maybe (Key s, a)
lookupLT = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.lookupLT @a

-- | Find the smallest key greater than the given one, and return the
-- associated key-value pair.
lookupGT :: forall s a. Int -> IntMap s a -> Maybe (Key s, a)
lookupGT = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.lookupGT @a

-- | Find the largest key smaller or equal to the given one, and return the
-- associated key-value pair.
lookupLE :: forall s a. Int -> IntMap s a -> Maybe (Key s, a)
lookupLE = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.lookupLE @a

-- | Find the smallest key greater or equal to the given one, and return the
-- associated key-value pair.
lookupGE :: forall s a. Int -> IntMap s a -> Maybe (Key s, a)
lookupGE = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.lookupGE @a

-- | If a map is empty, return a proof that it is.
null :: forall s a. IntMap s a -> Maybe (EmptyProof 'Int s)
null (IntMap m)
  | IntMap.null m = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If all keys of the first map are also present in the second map, and the
-- given function returns 'True' for their associated values, return a proof
-- that the keys form a subset.
isSubmapOfBy
  :: forall s t a b. (a -> b -> Bool)
  -> IntMap s a
  -> IntMap t b
  -> Maybe (SubsetProof 'Int s t)
isSubmapOfBy f (IntMap m1) (IntMap m2)
  | IntMap.isSubmapOfBy f m1 m2 = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If two maps are disjoint (i.e. their intersection is empty), return a proof
-- of that.
disjoint
  :: forall s t a b. IntMap s a -> IntMap t b -> Maybe (DisjointProof 'Int s t)
disjoint (IntMap m1) (IntMap m2)
#if MIN_VERSION_containers(0, 6, 2)
  | IntMap.disjoint m1 m2
#elif MIN_VERSION_containers(0, 5, 8)
  | Const (Any False) <- IntMap.mergeA
    (IntMap.traverseMissing \_ _ -> Const mempty)
    (IntMap.traverseMissing \_ _ -> Const mempty)
    (IntMap.zipWithAMatched \_ _ _ -> Const $ Any True)
    m1
    m2
#else
  | IntMap.null $ IntMapStrict.intersectionWith (\_ _ -> ()) m1 m2
#endif
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | Given two maps proven to have the same keys, for each key apply the
-- function to the associated values, to obtain a new map with the same keys.
zipWith
  :: forall s a b c. (a -> b -> c)
  -> IntMap s a
  -> IntMap s b
  -> IntMap s c
zipWith f (IntMap m1) (IntMap m2) = IntMap
  $ IntMap.mergeWithKey (\_ x y -> Just $ f x y)
    (\m -> if IntMap.null m
      then IntMap.empty
      else error "zipWith: bug: Data.IntMap.Refined has been subverted")
    (\m -> if IntMap.null m
      then IntMap.empty
      else error "zipWith: bug: Data.IntMap.Refined has been subverted")
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
  :: forall s t a. IntMap s a
  -> IntMap t a
  -> SomeIntMapWith (UnionProof 'Int s t) a
union (IntMap m1) (IntMap m2) = SomeIntMapWith (IntMap $ IntMap.union m1 m2)
  $ UnionProof unsafeSubset unsafeSubsetWith2

-- | Remove the keys that appear in the second map from the first map.
difference
  :: forall s t a b. IntMap s a
  -> IntMap t b
  -> SomeIntMapWith (DifferenceProof 'Int s t) a
difference (IntMap m1) (IntMap m2) = SomeIntMapWith
  (IntMap $ IntMap.difference m1 m2)
  $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Return the intersection of two maps, taking values from the first map.
--
-- @
-- 'intersection' = intersectionWith 'const'
-- @
intersection
  :: forall s t a b. IntMap s a
  -> IntMap t b
  -> SomeIntMapWith (IntersectionProof 'Int s t) a
intersection (IntMap m1) (IntMap m2) = SomeIntMapWith
  (IntMap $ IntMap.intersection m1 m2)
  $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Apply a function to all values in a map, together with their corresponding
-- keys, that are proven to be in the map. The set of keys remains the same.
mapWithKey :: forall s a b. (Key s -> a -> b) -> IntMap s a -> IntMap s b
mapWithKey = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.mapWithKey @a @b

-- | Map an 'Applicative' transformation in ascending order of keys, with access
-- to each value's corresponding key and a proof that it is in the map. The set
-- of keys remains unchanged.
traverseWithKey
  :: forall s f a b. Applicative f
  => (Key s -> a -> f b) -> IntMap s a -> f (IntMap s b)
traverseWithKey f (IntMap m) = IntMap <$> IntMap.traverseWithKey (f . unsafeKey) m

-- | Map each key-value pair of a map into a monoid (with proof that the key was
-- in the map), and combine the results using '<>'.
foldMapWithKey
  :: forall s a m. Monoid m => (Key s -> a -> m) -> IntMap s a -> m
foldMapWithKey = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.foldMapWithKey @m @a

-- | Right associative fold with a lazy accumulator.
foldrWithKey :: forall s a b. (Key s -> a -> b -> b) -> b -> IntMap s a -> b
foldrWithKey = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.foldrWithKey @a @b

-- | Left associative fold with a lazy accumulator.
foldlWithKey :: forall s a b. (b -> Key s -> a -> b) -> b -> IntMap s a -> b
foldlWithKey = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.foldlWithKey @b @a

-- | Right associative fold with a strict accumulator.
foldrWithKey' :: forall s a b. (Key s -> a -> b -> b) -> b -> IntMap s a -> b
foldrWithKey' = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.foldrWithKey' @a @b

-- | Left associative fold with a strict accumulator.
foldlWithKey' :: forall s a b. (b -> Key s -> a -> b) -> b -> IntMap s a -> b
foldlWithKey' = gcoerceWith (unsafeCastKey @s) $ coerce
  $ IntMap.foldlWithKey' @b @a

-- | Return the set of keys in the map, with the contents of the set still
-- tracked by the @s@ parameter. See "Data.IntSet.Refined".
keysSet :: forall s a. IntMap s a -> IntSet s
keysSet (IntMap m) = reify (IntMap.keysSet m)
  \(_ :: Proxy s') -> case unsafeCoerce Refl :: s :~: s' of
    Refl -> Dict

-- | Convert to a list of key-value pairs in ascending order of keys.
toList :: forall s a. IntMap s a -> [(Key s, a)]
toList = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.toAscList @a

-- | Convert to a list of key-value pairs in descending order of keys.
toDescList :: forall s a. IntMap s a -> [(Key s, a)]
toDescList = gcoerceWith (unsafeCastKey @s) $ coerce $ IntMap.toDescList @a

-- | Retain only the key-value pairs that satisfy the predicate, returning a
-- potentially smaller map.
filter
  :: forall s a. (a -> Bool)
  -> IntMap s a
  -> SomeIntMapWith (SupersetProof 'Int s) a
filter p (IntMap m) = SomeIntMapWith (IntMap $ IntMap.filter p m)
  $ SupersetProof unsafeSubset

-- | Retain only the key-value pairs that satisfy the predicate, returning a
-- potentially smaller map.
filterKeys
  :: forall s a. (Key s -> Bool)
  -> IntMap s a
  -> SomeIntMapWith (SupersetProof 'Int s) a
filterKeys p (IntMap m) = SomeIntMapWith
#if MIN_VERSION_containers(0, 8, 0)
  (IntMap $ IntMap.filterKeys (p . unsafeKey) m)
#else
  (IntMap $ IntMap.filterWithKey (\k _ -> p (unsafeKey k)) m)
#endif
  $ SupersetProof unsafeSubset

-- | Retain only the key-value pairs that satisfy the predicate, returning a
-- potentially smaller map.
filterWithKey
  :: forall s a. (Key s -> a -> Bool)
  -> IntMap s a
  -> SomeIntMapWith (SupersetProof 'Int s) a
filterWithKey p (IntMap m)
  = SomeIntMapWith (IntMap $ IntMap.filterWithKey (p . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Restrict a map to only those keys that are elements of @t@.
restrictKeys
  :: forall s t a. KnownIntSet t
  => IntMap s a -> SomeIntMapWith (IntersectionProof 'Int s t) a
restrictKeys (IntMap m) = SomeIntMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (IntMap $ IntMap.restrictKeys m $ reflect $ Proxy @t)
#else
  (IntMap $ IntMap.intersectionWith const m
    $ IntMap.fromSet id $ reflect $ Proxy @t)
#endif
  $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Remove all keys that are elements of @t@ from the map.
withoutKeys
  :: forall s t a. KnownIntSet t
  => IntMap s a -> SomeIntMapWith (DifferenceProof 'Int s t) a
withoutKeys (IntMap m) = SomeIntMapWith
#if MIN_VERSION_containers(0, 5, 8)
  (IntMap $ IntMap.withoutKeys m $ reflect $ Proxy @t)
#else
  (IntMap $ IntMap.difference m $ IntMap.fromSet id $ reflect $ Proxy @t)
#endif
  $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Partition a map into two disjoint submaps: those whose values satisfy the
-- predicate, and those whose don't.
partition
  :: forall s a. (a -> Bool)
  -> IntMap s a
  -> Some2IntMapWith (PartitionProof 'Int s Int) a a
partition p (IntMap m) = case IntMap.partition p m of
  (m1, m2) -> Some2IntMapWith (IntMap m1) (IntMap m2) $ PartitionProof
    do \k -> case IntMap.lookup (unrefine k) m of
        Nothing -> error
          "partition: bug: Data.IntMap.Refined has been subverted"
        Just x -> if p x
          then Left $ unsafeKey $ unrefine k
          else Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Partition a map into two disjoint submaps: those whose key-value pairs
-- satisfy the predicate, and those whose don't.
partitionWithKey
  :: forall s a. (Key s -> a -> Bool)
  -> IntMap s a
  -> Some2IntMapWith (PartitionProof 'Int s Int) a a
partitionWithKey p (IntMap m)
  = case IntMap.partitionWithKey (p . unsafeKey) m of
    (m1, m2) -> Some2IntMapWith (IntMap m1) (IntMap m2) $ PartitionProof
      do \k -> case IntMap.lookup (unrefine k) m of
          Nothing -> error
            "partitionWithKey: bug: Data.IntMap.Refined has been subverted"
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
  :: forall s a. (Key s -> Bool)
  -> IntMap s a
  -> SomeIntMapWith (SupersetProof 'Int s) a
takeWhileAntitone p (IntMap m) = SomeIntMapWith
#if MIN_VERSION_containers(0, 6, 7)
  (IntMap $ IntMap.takeWhileAntitone (p . unsafeKey) m)
#else
  (IntMap $ IntMap.fromDistinctAscList
    $ List.takeWhile (p . unsafeKey . fst) $ IntMap.toAscList m)
#endif
  $ SupersetProof unsafeSubset

-- | Take the a submap of keys starting from a point where the predicate stops
-- holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
dropWhileAntitone
  :: forall s a. (Key s -> Bool)
  -> IntMap s a
  -> SomeIntMapWith (SupersetProof 'Int s) a
dropWhileAntitone p (IntMap m) = SomeIntMapWith
#if MIN_VERSION_containers(0, 6, 7)
  (IntMap $ IntMap.dropWhileAntitone (p . unsafeKey) m)
#else
  (IntMap $ IntMap.fromDistinctAscList
    $ List.dropWhile (p . unsafeKey . fst) $ IntMap.toAscList m)
#endif
  $ SupersetProof unsafeSubset

-- | Divide a map into two disjoint submaps at a point where the predicate on
-- the keys stops holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
spanAntitone
  :: forall s a. (Key s -> Bool)
  -> IntMap s a
  -> Some2IntMapWith (PartialPartitionProof 'Int s) a a
spanAntitone p (IntMap m) =
#if MIN_VERSION_containers(0, 6, 7)
  case IntMap.spanAntitone (p . unsafeKey) m of
    (m1, m2)
#else
  case List.span (p . unsafeKey . fst) $ IntMap.toAscList m of
    (xs1, xs2)
      | let m1 = IntMap.fromDistinctAscList xs1
      , let m2 = IntMap.fromDistinctAscList xs2
#endif
      -> Some2IntMapWith (IntMap m1) (IntMap m2) $ PartialPartitionProof
        unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Return two disjoint submaps: those whose keys are less than the given key,
-- and those whose keys are greater than the given key. If the key was in the
-- map, also return the associated value and the proof that it was in the map.
splitLookup
  :: forall s a. Int
  -> IntMap s a
  -> Some2IntMapWith (SplitProof 'Int s (Key s, a)) a a
splitLookup k (IntMap m) = case IntMap.splitLookup k m of
  (m1, v, m2) -> Some2IntMapWith (IntMap m1) (IntMap m2) $ SplitProof
    ((unsafeKey k,) <$> v) unsafeSubset \f g -> unsafeSubsetWith2 f g

-- | Retrieves the key-value pair corresponding to the smallest key of the map,
-- and the map with that pair removed; or a proof that the map was empty.
minViewWithKey
  :: forall s a. IntMap s a
  -> Either
    (EmptyProof 'Int s)
    ((Key s, a), SomeIntMapWith (SupersetProof 'Int s) a)
minViewWithKey (IntMap m) = case IntMap.minViewWithKey m of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (kv, m') -> Right $ (gcoerceWith (unsafeCastKey @s) $ coerce kv,)
    $ SomeIntMapWith (IntMap m') $ SupersetProof unsafeSubset

-- | Retrieves the key-value pair corresponding to the greatest key of the map,
-- and the map with that pair removed; or a proof that the map was empty.
maxViewWithKey
  :: forall s a. IntMap s a
  -> Either
    (EmptyProof 'Int s)
    ((Key s, a), SomeIntMapWith (SupersetProof 'Int s) a)
maxViewWithKey (IntMap m) = case IntMap.maxViewWithKey m of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (kv, m') -> Right $ (gcoerceWith (unsafeCastKey @s) $ coerce kv,)
    $ SomeIntMapWith (IntMap m') $ SupersetProof unsafeSubset

-- | If elements of @s@ can be weakened to elements of @t@ and vice versa, then
-- @s@ and @t@ actually stand for the same set and @'Key' s@ can be safely
-- interconverted with @'Key' t@.
--
-- The requirement that the weakenings are natural transformations ensures that
-- they don't actually alter the keys. To build these you can compose ':->''s
-- from proofs returned by functions in this module, or "Refined" functions like
-- 'andLeft' or 'leftOr'.
castKey
  :: forall s t k.
     (forall x. Refined (InSet 'Int s) x -> Refined (InSet 'Int t) x)
  -> (forall x. Refined (InSet 'Int t) x -> Refined (InSet 'Int s) x)
  -> Coercion (Refined (InSet 'Int s) k) (Refined (InSet 'Int t) k)
castKey = castRefined

-- | If keys can be interconverted (e.g. as proved by 'castKey'), then the maps
-- can be interconverted too. For example, 'Data.IntMap.Refined.zipWithKey' can
-- be implemented via 'Data.IntMap.Refined.intersectionWithKey' by proving that
-- the set of keys remains unchanged:
--
-- @
-- 'Data.IntMap.Refined.zipWithKey'
--   :: forall s a b c. ('Key' s -> a -> b -> c)
--   -> 'IntMap' s a
--   -> 'IntMap' s b
--   -> 'IntMap' s c
-- 'Data.IntMap.Refined.zipWithKey' f m1 m2
--   | v'SomeIntMapWith' @r m proof <- 'Data.IntMap.Refined.intersectionWithKey' (f . 'andLeft') m1 m2
--   , v'IntersectionProof' p1 p2 <- proof
--   , ( v'Coercion' :: t'Coercion' ('IntMap' r c) ('IntMap' s c))
--     <- app $ 'cast' $ 'castKey' ('andLeft' . p1) (p2 'id' 'id')
--   = 'coerce' m
--   where
--     app :: t'Coercion' f g -> t'Coercion' (f x) (g x)
--     app v'Coercion' = v'Coercion'
-- @
cast
  :: forall s t k. (forall x. Coercion
    (Refined (InSet 'Int s) x)
    (Refined (InSet 'Int t) x))
  -> Coercion (IntMap s k) (IntMap t k)
cast Coercion = Coercion

instance FunctorWithIndex (Key s) (IntMap s) where
  imap = mapWithKey

instance FoldableWithIndex (Key s) (IntMap s) where
  ifoldMap = foldMapWithKey

instance TraversableWithIndex (Key s) (IntMap s) where
  itraverse = traverseWithKey

-- | Similar to the instance for functions -- zip corresponding keys. To use
-- '<*>'/'Control.Applicative.liftA2' without 'KnownIntSet' see 'zipWith'.
instance  KnownIntSet s => Applicative (IntMap s) where
  pure x = fromSet \_ -> x
  (<*>) = zipWith id

-- | @'bind' m f@ is a map that for each key @k :: 'Key' s@, contains the
-- value @f (m '!' k) '!' k@, similar to @'>>='@ for functions.
bind :: forall s a b. IntMap s a -> (a -> IntMap s b) -> IntMap s b
bind m f = mapWithKey (\k x -> f x ! k) m

-- | Similar to the instance for functions. To use '>>=' without 'KnownIntSet'
-- see 'bind'.
instance KnownIntSet s => Monad (IntMap s) where
  (>>=) = bind

-- | Similar to the instance for functions. See also
-- 'Data.IntMap.Refined.backpermuteKeys'.
instance KnownIntSet s => MonadReader (Key s) (IntMap s) where
  ask = fromSet id
  local f m = mapWithKey (\k _ -> m ! f k) m

-- | Append the values at the corresponding keys
instance Semigroup a => Semigroup (IntMap s a) where
  (<>) = zipWith (<>)

instance (KnownIntSet s, Monoid a) => Monoid (IntMap s a) where
  mempty = fromSet \_ -> mempty

-- | Similar to the instance for functions
instance KnownIntSet s => Distributive (IntMap s) where
  collect = collectRep
  distribute = distributeRep

-- | Witness isomorphism with functions from @'Key' s@
instance KnownIntSet s => Representable (IntMap s) where
  type Rep (IntMap s) = Key s
  index = (!)
  tabulate = fromSet

#if MIN_VERSION_hashable(1, 3, 4)
#else
instance Hashable.Hashable a => Hashable.Hashable (IntMap s a) where
  hashWithSalt s (IntMap m) = IntMap.foldlWithKey'
    (\s' k v -> Hashable.hashWithSalt (Hashable.hashWithSalt s' k) v)
    (Hashable.hashWithSalt s (IntMap.size m))
    m
#endif
