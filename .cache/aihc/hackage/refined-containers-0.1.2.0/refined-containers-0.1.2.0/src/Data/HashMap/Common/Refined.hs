{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HashMap.Common.Refined where

import           Control.Monad.Reader
import           Control.DeepSeq
import           Data.Bifoldable
import           Data.Coerce
import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Hashable
import           Data.Container.Refined.Proofs
import           Data.Container.Refined.Unsafe
import           Data.Distributive
import           Data.Foldable.WithIndex
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Functor.WithIndex
import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashMap.Strict as HashMapStrict
import qualified Data.HashSet as HashSet
import           Data.Proxy
import           Data.Reflection
import           Data.Traversable.WithIndex
import           Data.Type.Coercion
import           Data.Type.Equality ((:~:)(..))
import           Prelude hiding (zipWith)
import           Refined
import           Refined.Unsafe
import           Unsafe.Coerce

#if MIN_VERSION_unordered_containers(0, 2, 12)
#else
import           Data.Monoid (All(..))
#endif


-- | A wrapper around a regular 'Data.HashMap.HashMap' with a type parameter @s@
-- identifying the set of keys present in the map.
--
-- A key of type @k@ may not be present in the map, but a @'Key' s k@ is
-- guaranteed to be present (if the @s@ parameters match). Thus the map is
-- isomorphic to a (total) function @'Key' s k -> a@, which motivates many of
-- the instances below.
--
-- A 'HashMap' always knows its set of keys, so given @'HashMap' s k a@ we can
-- always derive @'KnownHashSet' s k@ by pattern matching on the 'Dict' returned
-- by 'keysSet'.
newtype HashMap s k a = HashMap (HashMap.HashMap k a)
  deriving newtype
    ( Eq, Eq1, Eq2, Ord, Ord1, Ord2, Show, Show1, Show2
    , Functor, Foldable, Bifoldable, Hashable.Hashable, NFData
    )
  deriving stock (Traversable)
type role HashMap nominal nominal representational

-- | Convert to a regular 'Data.HashMap.HashMap', forgetting its set of keys.
toMap :: forall s k a. HashMap s k a -> HashMap.HashMap k a
toMap (HashMap m) = m

-- | @'Key' s k@ is a key of type @k@ that has been verified to be an element
-- of the set @s@, and thus verified to be present in all @'HashMap' s k@ maps.
--
-- Thus, @'Key' s k@ is a \"refinement\" type of @k@, and this library
-- integrates with an implementation of refimenement types in "Refined", so
-- the machinery from there can be used to manipulate 'Key's (however see
-- 'Data.Set.Refined.revealPredicate').
--
-- The underlying @k@ value can be obtained with 'unrefine'. A @k@ can be
-- validated into an @'Key' s k@ with 'member'.
type Key s = Refined (InSet 'Hashed s)

unsafeCastKey :: forall s k. Coercion k (Key s k)
unsafeCastKey = reallyUnsafeUnderlyingRefined

unsafeKey :: k -> Key s k
unsafeKey = coerceWith unsafeCastKey

-- | An existential wrapper for a 'HashMap' with an as-yet-unknown set of keys.
-- Pattern maching on it gives you a way to refer to the set (the parameter
-- @s@), e.g.
--
-- @
-- case 'fromHashMap' ... of
--   'SomeHashMap' \@s m -> doSomethingWith \@s
--
-- case 'fromHashMap' ... of
--   'SomeHashMap' (m :: 'HashMap' s k a) -> doSomethingWith \@s
-- @
data SomeHashMap k a where
  SomeHashMap :: forall s k a. !(HashMap s k a) -> SomeHashMap k a

-- | Apply a map with an unknown set of keys to a continuation that can accept
-- a map with any set of keys. This gives you a way to refer to the set (the
-- parameter @s@), e.g.:
--
-- @
-- 'withHashMap' ('fromHashMap' ...
--   $ \\(m :: 'HashMap' s k a) -> doSomethingWith \@s
-- @
withHashMap
  :: forall k a r. SomeHashMap k a -> (forall s. HashMap s k a -> r) -> r
withHashMap (SomeHashMap m) k = k m

-- | Construct a map from a regular 'Data.HashMap.Lazy.HashMap'.
fromHashMap :: forall k a. HashMap.HashMap k a -> SomeHashMap k a
fromHashMap m = SomeHashMap (HashMap m)

-- | Given a set of keys @s@ known ahead of time, verify whether a regular
-- 'Data.HashMap.Lazy.HashMap' has exactly that set of keys.
verifyHashMap
  :: forall s k a. (Hashable k, KnownHashSet s k)
  => HashMap.HashMap k a -> Maybe (HashMap s k a)
verifyHashMap m
#if MIN_VERSION_unordered_containers(0, 2, 12)
  | HashMap.isSubmapOfBy (\_ _ -> True) m (HashSet.toMap keys)
  , HashMap.isSubmapOfBy (\_ _ -> True) (HashSet.toMap keys) m
#else
  | All True <- flip HashMap.foldMapWithKey m \k _
    -> All $ k `HashSet.member` keys
  , All True <- flip foldMap keys \k
    -> All $ k `HashMap.member` m
#endif
  = Just (HashMap m)
  | otherwise = Nothing
  where
    keys = reflect $ Proxy @s

-- | An existential wrapper for a 'HashMap' with an as-yet-unknown set of keys,
-- together with a proof of some fact @p@ about the set. Pattern matching on it
-- gives you a way to refer to the set (the parameter @s@). Functions that
-- change the set of keys in a map will return the map in this way, together
-- with a proof that somehow relates the keys set to the function's inputs.
data SomeHashMapWith p k a where
  SomeHashMapWith
    :: forall s k a p. !(HashMap s k a) -> !(p s) -> SomeHashMapWith p k a

-- | Apply a map with proof for an unknown set of keys to a continuation that
-- can accept a map with any set of keys satisfying the proof. This gives you a
-- way to refer to the set (the parameter @s@).
withHashMapWith
  :: forall k a r p. SomeHashMapWith p k a
  -> (forall s. HashMap s k a -> p s -> r)
  -> r
withHashMapWith (SomeHashMapWith m p) k = k m p

-- | An existential wrapper for a pair of maps with as-yet-unknown sets of keys,
-- together with a proof of some fact @p@ relating them.
data Some2HashMapWith p k a b where
  Some2HashMapWith
    :: forall s t k a b p. !(HashMap s k a)
    -> !(HashMap t k b)
    -> !(p s t)
    -> Some2HashMapWith p k a b

-- | Apply a pair of maps with proof for unknown sets of keys to a continuation
-- that can accept any pair of maps with any sets of keys satisfying the proof.
-- This gives you a way to refer to the sets (the parameters @s@ and @t@).
with2HashMapWith
  :: forall k a b r p. Some2HashMapWith p k a b
  -> (forall s t. HashMap s k a -> HashMap t k b -> p s t -> r)
  -> r
with2HashMapWith (Some2HashMapWith m1 m2 p) k = k m1 m2 p

-- | An empty map.
empty :: forall k a. SomeHashMapWith (EmptyProof 'Hashed) k a
empty = SomeHashMapWith (HashMap HashMap.empty) $ EmptyProof unsafeSubset

-- | Create a map from a set of keys, and a function that for each key computes
-- the corresponding value.
fromSet :: forall s k a. KnownHashSet s k => (Key s k -> a) -> HashMap s k a
fromSet f = HashMap $ HashMap.mapWithKey (\k _ -> f $ unsafeKey k)
  $ HashSet.toMap (reflect $ Proxy @s)

-- | Delete a key and its value from the map if present, returning a potentially
-- smaller map.
delete
  :: forall s k a. Hashable k
  => k -> HashMap s k a -> SomeHashMapWith (SupersetProof 'Hashed s) k a
delete k (HashMap m) = SomeHashMapWith (HashMap $ HashMap.delete k m)
  $ SupersetProof unsafeSubset

-- | If the key is in the map, return the proof of this, and the associated
-- value; otherwise return 'Nothing'.
lookup :: forall s k a. Hashable k => k -> HashMap s k a -> Maybe (Key s k, a)
lookup k (HashMap m) = (unsafeKey k,) <$> HashMap.lookup k m

-- | Given a key that is proven to be in the map, return the associated value.
--
-- Unlike 'Data.HashMap.!' from "Data.HashMap.Lazy", this function is total, as
-- it is impossible to obtain a @'Key' s k@ for a key that is not in the map
-- @'HashMap' s k a@.
(!) :: forall s k a. Hashable k => HashMap s k a -> Key s k -> a
(!) (HashMap m) k = case HashMap.lookup (unrefine k) m of
  Nothing -> error "(!): bug: Data.HashMap.Refined has been subverted"
  Just x -> x

-- | If a key is in the map, return the proof that it is.
member :: forall s k a. Hashable k => k -> HashMap s k a -> Maybe (Key s k)
member k (HashMap m)
  | k `HashMap.member` m = Just (unsafeKey k)
  | otherwise = Nothing

-- | If a map is empty, return a proof that it is.
null :: forall s k a. HashMap s k a -> Maybe (EmptyProof 'Hashed s)
null (HashMap m)
  | HashMap.null m = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If all keys of the first map are also present in the second map, and the
-- given function returns 'True' for their associated values, return a proof
-- that the keys form a subset.
isSubmapOfBy
  :: forall s t k a b. Hashable k
  => (a -> b -> Bool)
  -> HashMap s k a
  -> HashMap t k b
  -> Maybe (SubsetProof 'Hashed s t)
isSubmapOfBy f (HashMap m1) (HashMap m2)
#if MIN_VERSION_unordered_containers(0, 2, 12)
  | HashMap.isSubmapOfBy f m1 m2
#else
  | All True <- flip HashMap.foldMapWithKey m1
    \k v1 -> case HashMap.lookup k m2 of
      Just v2 | f v1 v2 -> mempty
      _ -> All False
#endif
  = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If two maps are disjoint (i.e. their intersection is empty), return a proof
-- of that.
disjoint
  :: forall s t k a b. Hashable k
  => HashMap s k a -> HashMap t k b -> Maybe (DisjointProof 'Hashed s t)
disjoint (HashMap m1) (HashMap m2)
  | HashMap.null $ HashMapStrict.intersectionWith (\_ _ -> ()) m1 m2
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | Given two maps proven to have the same keys, for each key apply the
-- function to the associated values, to obtain a new map with the same keys.
zipWith
  :: forall s k a b c. Hashable k
  => (a -> b -> c) -> HashMap s k a -> HashMap s k b -> HashMap s k c
zipWith f (HashMap m1) (HashMap m2) = HashMap $ HashMap.intersectionWith f m1 m2

-- | Return the union of two maps. For keys that exist in both maps, the value
-- is taken from the first map.
--
-- @
-- 'union' = unionWith 'const'
-- @
union
  :: forall s t k a. Hashable k
  => HashMap s k a
  -> HashMap t k a
  -> SomeHashMapWith (UnionProof 'Hashed s t) k a
union (HashMap m1) (HashMap m2) = SomeHashMapWith
  (HashMap $ HashMap.union m1 m2)
  $ UnionProof unsafeSubset unsafeSubsetWith2

-- | Remove the keys that appear in the second map from the first map.
difference
  :: forall s t k a b. Hashable k
  => HashMap s k a
  -> HashMap t k b
  -> SomeHashMapWith (DifferenceProof 'Hashed s t) k a
difference (HashMap m1) (HashMap m2)
  = SomeHashMapWith (HashMap $ HashMap.difference m1 m2)
    $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Return the intersection of two maps, taking values from the first map.
--
-- @
-- 'intersection' = intersectionWith 'const'
-- @
intersection
  :: forall s t k a b. Hashable k
  => HashMap s k a
  -> HashMap t k b
  -> SomeHashMapWith (IntersectionProof 'Hashed s t) k a
intersection (HashMap m1) (HashMap m2) = SomeHashMapWith
  (HashMap $ HashMap.intersection m1 m2)
  $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Apply a function to all values in a map, together with their corresponding
-- keys, that are proven to be in the map. The set of keys remains the same.
mapWithKey
  :: forall s k a b. (Key s k -> a -> b) -> HashMap s k a -> HashMap s k b
mapWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.mapWithKey @k @a @b

-- | Map an 'Applicative' transformation with access to each value's
-- corresponding key and a proof that it is in the map. The set of keys remains
-- unchanged.
traverseWithKey
  :: forall s f k a b. Applicative f
  => (Key s k -> a -> f b) -> HashMap s k a -> f (HashMap s k b)
traverseWithKey f (HashMap m)
  = HashMap <$> HashMap.traverseWithKey (f . unsafeKey) m

-- | Map each key-value pair of a map into a monoid (with proof that the key was
-- in the map), and combine the results using '<>'.
foldMapWithKey
  :: forall s k a m. Monoid m => (Key s k -> a -> m) -> HashMap s k a -> m
foldMapWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.foldMapWithKey @m @k @a

-- | Right associative fold with a lazy accumulator.
foldrWithKey
  :: forall s k a b. (Key s k -> a -> b -> b) -> b -> HashMap s k a -> b
foldrWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.foldrWithKey @k @a @b

-- | Left associative fold with a lazy accumulator.
foldlWithKey
  :: forall s k a b. (b -> Key s k -> a -> b) -> b -> HashMap s k a -> b
foldlWithKey = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.foldlWithKey @b @k @a

-- | Right associative fold with a strict accumulator.
foldrWithKey'
  :: forall s k a b. (Key s k -> a -> b -> b) -> b -> HashMap s k a -> b
foldrWithKey' = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.foldrWithKey' @k @a @b

-- | Left associative fold with a strict accumulator.
foldlWithKey'
  :: forall s k a b. (b -> Key s k -> a -> b) -> b -> HashMap s k a -> b
foldlWithKey' = gcoerceWith (unsafeCastKey @s @k) $ coerce
  $ HashMap.foldlWithKey' @b @k @a

-- | Return the set of keys in the map, with the contents of the set still
-- tracked by the @s@ parameter. See "Data.HashSet.Refined".
keysSet :: forall s k a. HashMap s k a -> HashSet s k
keysSet (HashMap m) = reify (HashMap.keysSet m)
  \(_ :: Proxy s') -> case unsafeCoerce Refl :: s :~: s' of
    Refl -> Dict

-- | Convert to a list of key-value pairs.
toList :: forall s k a. HashMap s k a -> [(Key s k, a)]
toList = gcoerceWith (unsafeCastKey @s @k) $ coerce $ HashMap.toList @k @a

-- | Retain only the values that satisfy the predicate, returning a potentially
-- smaller map.
filter
  :: forall s k a. (a -> Bool)
  -> HashMap s k a
  -> SomeHashMapWith (SupersetProof 'Hashed s) k a
filter p (HashMap m) = SomeHashMapWith (HashMap $ HashMap.filter p m)
  $ SupersetProof unsafeSubset

-- | Retain only the keys that satisfy the predicate, returning a potentially
-- smaller map.
filterKeys
  :: forall s k a. (Key s k -> Bool)
  -> HashMap s k a
  -> SomeHashMapWith (SupersetProof 'Hashed s) k a
filterKeys p (HashMap m) = SomeHashMapWith
  (HashMap $ HashMap.filterWithKey (\k _ -> p (unsafeKey k)) m)
  $ SupersetProof unsafeSubset

-- | Retain only the key-value pairs that satisfy the predicate, returning a
-- potentially smaller map.
filterWithKey
  :: forall s k a. (Key s k -> a -> Bool)
  -> HashMap s k a
  -> SomeHashMapWith (SupersetProof 'Hashed s) k a
filterWithKey p (HashMap m)
  = SomeHashMapWith (HashMap $ HashMap.filterWithKey (p . unsafeKey) m)
    $ SupersetProof unsafeSubset

-- | Restrict a map to only those keys that are elements of @t@.
restrictKeys
  :: forall s t k a. (Hashable k, KnownHashSet t k)
  => HashMap s k a -> SomeHashMapWith (IntersectionProof 'Hashed s t) k a
restrictKeys (HashMap m) = SomeHashMapWith
  (HashMap $ HashMap.intersectionWith const m
    $ HashSet.toMap $ reflect $ Proxy @t)
  $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Remove all keys that are elements of @t@ from the map.
withoutKeys
  :: forall s t k a. (Hashable k, KnownHashSet t k)
  => HashMap s k a -> SomeHashMapWith (DifferenceProof 'Hashed s t) k a
withoutKeys (HashMap m) = SomeHashMapWith
  (HashMap $ HashMap.difference m $ HashSet.toMap $ reflect $ Proxy @t)
  $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Partition a map into two disjoint submaps: those whose key-value pairs
-- satisfy the predicate, and those whose don't.
partition
  :: forall s k a. Hashable k -- TODO: this is only used in the proof
  => (a -> Bool)
  -> HashMap s k a
  -> Some2HashMapWith (PartitionProof 'Hashed s k) k a a
partition p (HashMap m) = Some2HashMapWith
  (HashMap $ HashMap.filter p m)
  (HashMap $ HashMap.filter (not . p) m)
  $ PartitionProof
    do \k -> case HashMap.lookup (unrefine k) m of
        Nothing -> error
          "partition: bug: Data.HashMap.Refined has been subverted"
        Just x -> if p x
          then Left $ unsafeKey $ unrefine k
          else Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Partition a map into two disjoint submaps: those whose key-value pairs
-- satisfy the predicate, and those whose don't.
partitionWithKey
  :: forall s k a. Hashable k -- TODO: this is only used in the proof
  => (Key s k -> a -> Bool)
  -> HashMap s k a
  -> Some2HashMapWith (PartitionProof 'Hashed s k) k a a
partitionWithKey p (HashMap m) = Some2HashMapWith
  (HashMap $ HashMap.filterWithKey (p . unsafeKey) m)
  (HashMap $ HashMap.filterWithKey ((not .) . p . unsafeKey) m)
  $ PartitionProof
    do \k -> case HashMap.lookup (unrefine k) m of
        Nothing -> error
          "partitionWithKey: bug: Data.HashMap.Refined has been subverted"
        Just x -> if p k x
          then Left $ unsafeKey $ unrefine k
          else Right $ unsafeKey $ unrefine k
    unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

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
-- can be interconverted too. For example, 'Data.HashMap.Refined.zipWithKey' can
-- be implemented via 'Data.HashMap.Refined.intersectionWithKey' by proving that
-- the set of keys remains unchanged:
--
-- @
-- 'Data.HashMap.Refined.zipWithKey'
--   :: forall s k a b c. 'Hashable' k
--   => ('Key' s k -> a -> b -> c) -> 'HashMap' s k a -> 'HashMap' s k b -> 'HashMap' s k c
-- 'Data.HashMap.Refined.zipWithKey' f m1 m2
--   | v'SomeHashMapWith' @r m proof <- 'Data.HashMap.Refined.intersectionWithKey' (f . 'andLeft') m1 m2
--   , v'IntersectionProof' p1 p2 <- proof
--   , ( v'Coercion' :: t'Coercion' ('HashMap' r k c) ('HashMap' s k c))
--     <- app $ 'cast' $ 'castKey' ('andLeft' . p1) (p2 'id' 'id')
--   = 'coerce' m
--   where
--     app :: t'Coercion' f g -> t'Coercion' (f x) (g x)
--     app v'Coercion' = v'Coercion'
-- @
cast
  :: forall s t k. (forall x. Coercion (Key s x) (Key t x))
  -> Coercion (HashMap s k) (HashMap t k)
cast Coercion = Coercion

instance FunctorWithIndex (Key s k) (HashMap s k) where
  imap = mapWithKey

instance FoldableWithIndex (Key s k) (HashMap s k) where
  ifoldMap = foldMapWithKey

instance TraversableWithIndex (Key s k) (HashMap s k) where
  itraverse = traverseWithKey

-- | Similar to the instance for functions -- zip corresponding keys. To use
-- '<*>'/'Control.Applicative.liftA2' without 'KnownHashSet' see 'zipWith'.
instance (Hashable k, KnownHashSet s k) => Applicative (HashMap s k) where
  pure x = fromSet \_ -> x
  (<*>) = zipWith id

-- | @'bind' m f@ is a map that for each key @k :: 'Key' s k@, contains the
-- value @f (m '!' k) '!' k@, similar to @'>>='@ for functions.
bind
  :: forall s k a b. Hashable k
  => HashMap s k a -> (a -> HashMap s k b) -> HashMap s k b
bind m f = mapWithKey (\k x -> f x ! k) m

-- | Similar to the instance for functions. To use '>>=' without 'KnownHashSet'
-- see 'bind'.
instance (Hashable k, KnownHashSet s k) => Monad (HashMap s k) where
  (>>=) = bind

-- | Similar to the instance for functions. See also
-- 'Data.HashMap.Refined.backpermuteKeys'.
instance (Hashable k, KnownHashSet s k)
  => MonadReader (Key s k) (HashMap s k) where
  ask = fromSet id
  local f m = mapWithKey (\k _ -> m ! f k) m

-- | Append the values at the corresponding keys
instance (Hashable k, Semigroup a) => Semigroup (HashMap s k a) where
  (<>) = zipWith (<>)

instance (Hashable k, KnownHashSet s k, Monoid a)
  => Monoid (HashMap s k a) where
  mempty = fromSet \_ -> mempty

-- | Similar to the instance for functions
instance (Hashable k, KnownHashSet s k) => Distributive (HashMap s k) where
  collect = collectRep
  distribute = distributeRep

-- | Witness isomorphism with functions from @'Key' s k@
instance (Hashable k, KnownHashSet s k) => Representable (HashMap s k) where
  type Rep (HashMap s k) = Key s k
  index = (!)
  tabulate = fromSet
