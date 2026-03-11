{-# LANGUAGE CPP #-}
-- | This module implements a way of tracking the contents of a
-- 'Data.HashSet.HashSet' at the type level, and provides utilities for
-- manipulating such sets.
--
-- The contents of a set are associated with a type parameter, e.g. @s@, so that
-- whenever you see the same type parameter, you know you are working with the
-- same set. The type @s@ itself has no internal structure, rather it is merely
-- a skolem type variable (rank-2 polymorphism 'Control.Monad.ST.runST' trick)
-- introduced by "Data.Reflection".
--
-- = Warning
-- This module together with "Data.HashSet" rely on 'Eq' and 'Hashable'
-- instances being lawful: that '==' is an equivalence relation, and that
-- 'Data.Hashable.hashWithSalt' is defined on the quotient by this equivalence
-- relation; at least for the subset of values that are actually encountered at
-- runtime. If this assumption is violated, this module may not be able to
-- uphold its invariants and may throw errors. In particular beware of NaN in
-- 'Float' and 'Double', and, if using @hashable < 1.3@, beware of @0@ and @-0@.
module Data.HashSet.Refined
  (
  -- * Set type
    KnownHashSet
  , HashSet
  -- * Refinement type
  , InSet(..)
  , Flavor(Hashed)
  , Element
  , revealPredicate
  -- * Existentials and common proofs
  , SomeHashSet(..)
  , withHashSet
  , SomeHashSetWith(..)
  , withHashSetWith
  , Some2HashSetWith(..)
  , with2HashSetWith
  , (:->)
  , SupersetProof(..)
  , EmptyProof(..)
  -- * Construction
  , empty
  , singleton
  , SingletonProof(..)
  , fromHashSet
  , fromTraversable
  , FromTraversableProof(..)
  -- * Insertion
  , insert
  , InsertProof(..)
  -- * Deletion
  , delete
  -- * Query
  , member
  , null
  , isSubsetOf
  , SubsetProof(..)
  , disjoint
  , DisjointProof(..)
  -- * Combine
  , union
  , UnionProof(..)
  , difference
  , DifferenceProof(..)
  , intersection
  , IntersectionProof(..)
  -- * Filter
  , filter
  , partition
  , PartitionProof(..)
  -- * Map
  , map
  , MapProof(..)
  -- * Folds
  , foldMap
  , foldr
  , foldl
  , foldr'
  , foldl'
  -- * Conversion
  , toList
  , asSet
  , asIntSet
  -- * Casts
  , castElement
  , cast
  , castFlavor
  ) where

import           Data.Coerce
import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Conversion
import           Data.Container.Refined.Hashable
import           Data.Container.Refined.Proofs
import           Data.Container.Refined.Unsafe
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import           Data.Proxy
import           Data.Reflection
import           Data.Traversable
import           Data.Type.Coercion
import           Data.Type.Equality ((:~:)(..))
import           Data.Typeable (Typeable)
import           GHC.Exts (Proxy#, proxy#)
import           Prelude hiding
  ( filter, foldl, foldMap, foldr, map, null
#if MIN_VERSION_base(4, 20, 0)
  , foldl'
#endif
  )
import           Refined
import           Refined.Unsafe
import           Unsafe.Coerce


-- | To use "Refined" machinery that uses the 'Predicate' typeclass you will
-- need to pattern match on this 'Dict'.
--
-- The reason is that in the default /fast/ implementation of reflection, we
-- don't have @'Data.Typeable.Typeable' s@, which "Refined" wants for
-- pretty-printing exceptions. We /can/ provide @'Data.Typeable.Typeable' s@,
-- but at the cost of using the /slow/ implementation of reflection.
revealPredicate
  :: forall s a. (Typeable a, Hashable a, KnownHashSet s a)
  => Dict (Predicate (InSet 'Hashed s) a)
revealPredicate = reifyTypeable (reflect (Proxy @s))
  \(_ :: Proxy s') ->
    reflect (Proxy @s') `seq`
    --  ^ Work around https://github.com/ekmett/reflection/issues/54
      case unsafeCoerce Refl :: s :~: s' of
        Refl -> Dict

-- | @'Element' s a@ is a value of type @a@ that has been verified to be an
-- element of @s@.
--
-- Thus, @'Element' s a@ is a \"refinement\" type of @a@, and this library
-- integrates with an implementation of refimenement types in "Refined", so
-- the machinery from there can be used to manipulate 'Element's (however see
-- 'revealPredicate').
--
-- The underlying @a@ value can be obtained with 'unrefine'. An @a@ can be
-- validated into an @'Element' s a@ with 'member'.
type Element s = Refined (InSet 'Hashed s)

unsafeCastElement :: forall s a. Coercion a (Element s a)
unsafeCastElement = reallyUnsafeUnderlyingRefined

unsafeElement :: a -> Element s a
unsafeElement = coerceWith unsafeCastElement

-- | An existential wrapper for an as-yet-unknown set. Pattern maching on it
-- gives you a way to refer to the set, e.g.
--
-- @
-- case 'fromHashSet' ... of
--   'SomeHashSet' \@s _ -> doSomethingWith \@s
--
-- case 'fromHashSet' ... of
--   'SomeHashSet' (_ :: 'Proxy#' s) -> doSomethingWith \@s
-- @
data SomeHashSet a where
  SomeHashSet :: forall s a. KnownHashSet s a => Proxy# s -> SomeHashSet a

-- | Apply an unknown set to a continuation that can accept any set. This gives
-- you a way to refer to the set (the parameter @s@), e.g.:
--
-- @
-- 'withHashSet' ('fromHashSet' ...) $ \\(_ :: 'Proxy' s) -> doSomethingWith \@s
-- @
withHashSet
  :: forall a r. SomeHashSet a
  -> (forall s. KnownHashSet s a => Proxy s -> r)
  -> r
withHashSet (SomeHashSet (_ :: Proxy# s)) k = k $ Proxy @s

-- | Construct a set from a regular 'Data.HashSet.HashSet'.
fromHashSet :: forall a. HashSet.HashSet a -> SomeHashSet a
fromHashSet s = reify s \(_ :: Proxy s) -> SomeHashSet @s proxy#

-- | An existential wrapper for an as-yet-unknown set, together with a proof of
-- some fact @p@ about the set. Pattern matching on it gives you a way to refer
-- to the set (the parameter @s@). Most functions will return a set in this way,
-- together with a proof that somehow relates the set to the function's inputs.
data SomeHashSetWith p a where
  SomeHashSetWith
    :: forall s a p. KnownHashSet s a => !(p s) -> SomeHashSetWith p a

-- | Apply an unknown set with proof to a continuation that can accept any set
-- satisfying the proof. This gives you a way to refer to the set (the parameter
-- @s@).
withHashSetWith
  :: forall a r p. SomeHashSetWith p a
  -> (forall s. KnownHashSet s a => p s -> r)
  -> r
withHashSetWith (SomeHashSetWith p) k = k p

-- | An existential wrapper for an as-yet-unknown pair of sets, together with
-- a proof of some fact @p@ relating them.
data Some2HashSetWith p a where
  Some2HashSetWith
    :: forall s t a p. (KnownHashSet s a, KnownHashSet t a)
    => !(p s t) -> Some2HashSetWith p a

-- | Apply a pair of unknown sets with proof to a continuation that can accept
-- any pair of sets satisfying the proof. This gives you a way to refer to the
-- sets (the parameters @s@ and @t@).
with2HashSetWith
  :: forall a r p. Some2HashSetWith p a
  -> (forall s t. (KnownHashSet s a, KnownHashSet t a) => p s t -> r)
  -> r
with2HashSetWith (Some2HashSetWith p) k = k p

-- | An empty set.
empty :: forall a. SomeHashSetWith (EmptyProof 'Hashed) a
empty = reify HashSet.empty \(_ :: Proxy r)
  -> SomeHashSetWith @r $ EmptyProof unsafeSubset

-- | Create a set with a single element.
singleton
  :: forall a. Hashable a => a -> SomeHashSetWith (SingletonProof 'Hashed a) a
singleton x = reify (HashSet.singleton x) \(_ :: Proxy r)
  -> SomeHashSetWith @r $ SingletonProof $ unsafeElement x

-- | Create a set from the elements of an arbitrary traversable.
fromTraversable
  :: forall t a. (Traversable t, Hashable a)
  => t a -> SomeHashSetWith (FromTraversableProof 'Hashed t a) a
fromTraversable xs = reify set \(_ :: Proxy r)
  -> SomeHashSetWith @r $ FromTraversableProof
    $ unsafeCoerce @(t (Element _ a)) @(t (Element r a)) proof
  where
    (set, proof) = mapAccumL
      (\s x -> let !s' = HashSet.insert x s in (s', unsafeElement x))
      HashSet.empty
      xs

-- | Insert an element in a set.
insert
  :: forall s a. (Hashable a, KnownHashSet s a)
  => a -> SomeHashSetWith (InsertProof 'Hashed a s) a
insert x = reify (HashSet.insert x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeHashSetWith @r $ InsertProof (unsafeElement x) unsafeSubset

-- | Delete an element from a set.
delete
  :: forall s a. (Hashable a, KnownHashSet s a)
  => a -> SomeHashSetWith (SupersetProof 'Hashed s) a
delete x = reify (HashSet.delete x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeHashSetWith @s $ SupersetProof unsafeSubset

-- | If an element is in the set, return the proof that it is.
member :: forall s a. (Hashable a, KnownHashSet s a) => a -> Maybe (Element s a)
member x
  | x `HashSet.member` reflect (Proxy @s) = Just $ unsafeElement x
  | otherwise = Nothing

-- | If the set is empty, return the proof that it is.
null :: forall s a. KnownHashSet s a => Maybe (EmptyProof 'Hashed s)
null
  | HashSet.null $ reflect $ Proxy @s = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ is a subset of @t@ (or is equal to), return a proof of that.
isSubsetOf
  :: forall s t a. (Hashable a, KnownHashSet s a, KnownHashSet t a)
  => Maybe (SubsetProof 'Hashed s t)
isSubsetOf
#if MIN_VERSION_unordered_containers(0, 2, 12)
  | reflect (Proxy @s) `HashSet.isSubsetOf` reflect (Proxy @t)
#else
  | all (`HashSet.member` reflect (Proxy @t)) (reflect (Proxy @s))
#endif
  = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ and @t@ are disjoint (i.e. their intersection is empty), return a
-- proof of that.
disjoint
  :: forall s t a. (Hashable a, KnownHashSet s a, KnownHashSet t a)
  => Maybe (DisjointProof 'Hashed s t)
disjoint
  | HashSet.null
    $ HashSet.intersection (reflect $ Proxy @s) (reflect $ Proxy @t)
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | The union of two sets.
union
  :: forall s t a. (Hashable a, KnownHashSet s a, KnownHashSet t a)
  => SomeHashSetWith (UnionProof 'Hashed s t) a
union = reify (reflect (Proxy @s) `HashSet.union` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeHashSetWith @r
    $ UnionProof unsafeSubset unsafeSubsetWith2

-- unions :: ?

-- | HashSet with elements of @s@ that are not in @t@.
difference
  :: forall s t a. (Hashable a, KnownHashSet s a, KnownHashSet t a)
  => SomeHashSetWith (DifferenceProof 'Hashed s t) a
difference = reify (reflect (Proxy @s) `HashSet.difference` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeHashSetWith @r
    $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Intersection of two sets.
intersection
  :: forall s t a. (Hashable a, KnownHashSet s a, KnownHashSet t a)
  => SomeHashSetWith (IntersectionProof 'Hashed s t) a
intersection
  = reify (reflect (Proxy @s) `HashSet.intersection` reflect (Proxy @t))
    \(_ :: Proxy r) -> SomeHashSetWith @r
      $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Return a subset of elements that satisfy the given predicate.
filter
  :: forall s a. KnownHashSet s a
  => (Element s a -> Bool) -> SomeHashSetWith (SupersetProof 'Hashed s) a
filter p = reify (HashSet.filter (p . unsafeElement) $ reflect $ Proxy @s)
  \(_ :: Proxy r) -> SomeHashSetWith @r $ SupersetProof unsafeSubset

-- | Partition a set into two disjoint subsets: those that satisfy the
-- predicate, and those that don't.
partition
  :: forall s a. KnownHashSet s a
  => (Element s a -> Bool) -> Some2HashSetWith (PartitionProof 'Hashed s a) a
partition p = reify (HashSet.filter (p . unsafeElement) $ reflect $ Proxy @s)
  \(_ :: Proxy r)
    -> reify (HashSet.filter (not . p . unsafeElement) $ reflect $ Proxy @s)
      \(_ :: Proxy q)
        -> Some2HashSetWith @s @r $ PartitionProof
          do \x -> if p x
              then Left $ unsafeElement $ unrefine x
              else Right $ unsafeElement $ unrefine x
          unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Apply the given function to each element of the set and collect the
-- results. Note that the resulting set can be smaller.
map
  :: forall s a b. (Hashable b, KnownHashSet s a)
  => (Element s a -> b) -> SomeHashSetWith (MapProof 'Hashed s a b) b
map f = reify (HashMap.keysSet m)
  \(_ :: Proxy r) -> SomeHashSetWith @r
    $ MapProof (unsafeElement . f) \y -> case HashMap.lookup (unrefine y) m of
      Nothing -> error "map: bug: Data.HashSet.Refined has been subverted"
      Just x -> x
  where
    !m = HashMap.fromList
      [ (y, unsafeElement x)
      | x <- HashSet.toList $ reflect $ Proxy @s
      , let !y = f $ unsafeElement x
      ]

-- | Map each element of @s@ into a monoid (with proof that it was an element),
-- and combine the results using 'Data.Monoid.<>'.
foldMap :: forall s a m. (KnownHashSet s a, Monoid m) => (Element s a -> m) -> m
foldMap f = Foldable.foldMap (f . unsafeElement) $ reflect $ Proxy @s

-- | Right associative fold with a lazy accumulator.
foldr :: forall s a b. KnownHashSet s a => (Element s a -> b -> b) -> b -> b
foldr f z = HashSet.foldr (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a lazy accumulator.
foldl :: forall s a b. KnownHashSet s a => (b -> Element s a -> b) -> b -> b
foldl f z = Foldable.foldl ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | Right associative fold with a strict accumulator.
foldr' :: forall s a b. KnownHashSet s a => (Element s a -> b -> b) -> b -> b
foldr' f z = Foldable.foldr' (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a strict accumulator.
foldl' :: forall s a b. KnownHashSet s a => (b -> Element s a -> b) -> b -> b
foldl' f z = HashSet.foldl' ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | List of elements in the set.
toList :: forall s a. KnownHashSet s a => [Element s a]
toList = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ HashSet.toList $ reflect $ Proxy @s


-- | Convert an 'IntSet' into a 'Set', retaining its set of elements, which can
-- be converted with 'castFlavor'.
asSet :: forall s a. (Ord a, KnownHashSet s a) => Set s a
asSet = hashSet2Set

-- | Convert an 'IntSet' into a 'HashSet', retaining its set of elements, which
-- can be converted with 'castFlavor'.
asIntSet :: forall s. KnownHashSet s Int => IntSet s
asIntSet = hashSet2IntSet

-- | If elements of @s@ can be weakened to elements of @t@ and vice versa, then
-- @s@ and @t@ actually stand for the same set and @'Element' s@ can be safely
-- interconverted with @'Element' t@.
--
-- The requirement that the weakenings are natural transformations ensures that
-- they don't actually alter the elements. To build these you can compose
-- ':->''s from proofs returned by functions in this module, or "Refined"
-- functions like 'andLeft' or 'leftOr'.
castElement
  :: forall s t a. (forall x. Element s x -> Element t x)
  -> (forall x. Element t x -> Element s x)
  -> Coercion (Element s a) (Element t a)
castElement = castRefined

-- | If elements can be interconverted (e.g. as proved by 'castElement'), then
-- the sets can be interconverted too. For example we can establish that the
-- intersection of a set with itself is interconvertible with that set:
--
-- @
-- castIntersection
--   :: t'IntersectionProof' ''Data.HashSet.Refined.Hashed' s s r
--   -> 'Coercion' ('HashSet' r a) ('HashSet' s a)
-- castIntersection ( v'IntersectionProof' p1 p2)
--   = 'cast' $ 'castElement' ('andLeft' . p1) (p2 'id' 'id')
-- @
cast
  :: forall s t a. (forall x. Coercion (Element s x) (Element t x))
  -> Coercion (HashSet s a) (HashSet t a)
cast Coercion
#if MIN_VERSION_base(4, 15, 0)
  = case unsafeEqualityProof @s @t of UnsafeRefl -> Coercion
#else
  = repr $ unsafeCoerce Refl
#endif
