{-# LANGUAGE CPP #-}
-- | This module implements a way of tracking the contents of an
-- 'Data.IntSet.IntSet' at the type level, and provides utilities for
-- manipulating such sets.
--
-- The contents of a set are associated with a type parameter, e.g. @s@, so that
-- whenever you see the same type parameter, you know you are working with the
-- same set. The type @s@ itself has no internal structure, rather it is merely
-- a skolem type variable (rank-2 polymorphism 'Control.Monad.ST.runST' trick)
-- introduced by "Data.Reflection".
module Data.IntSet.Refined
  (
  -- * Set type
    KnownIntSet
  , IntSet
  -- * Refinement type
  , InSet(..)
  , Flavor(Int)
  , Element
  , revealPredicate
  -- * Existentials and common proofs
  , SomeIntSet(..)
  , withIntSet
  , SomeIntSetWith(..)
  , withIntSetWith
  , Some2IntSetWith(..)
  , with2IntSetWith
  , (:->)
  , SupersetProof(..)
  , EmptyProof(..)
  -- * Construction
  , empty
  , singleton
  , SingletonProof(..)
  , fromIntSet
  , fromTraversable
  , FromTraversableProof(..)
  -- * Insertion
  , insert
  , InsertProof(..)
  -- * Deletion
  , delete
  -- * Query
  , member
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
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
  , spanAntitone
  , PartialPartitionProof(..)
  , splitMember
  , SplitProof(..)
  -- * Map
  , map
  , MapProof(..)
  -- * Folds
  , foldMap
  , foldr
  , foldl
  , foldr'
  , foldl'
  -- * Min/Max
  , minView
  , maxView
  -- * Conversion
  , toList
  , toDescList
  , asSet
  , asHashSet
  -- * Casts
  , castElement
  , cast
  , castFlavor
  ) where

import           Data.Coerce
import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Conversion
import           Data.Container.Refined.Proofs
import           Data.Container.Refined.Unsafe
import qualified Data.Foldable as Foldable
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Proxy
import           Data.Reflection
import           Data.Traversable
import           Data.Type.Coercion
import           Data.Type.Equality ((:~:)(..))
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

#if MIN_VERSION_containers(0, 6, 7)
#else
import qualified Data.List as List
#endif


-- | To use "Refined" machinery that uses the 'Predicate' typeclass you will
-- need to pattern match on this 'Dict'.
--
-- The reason is that in the default /fast/ implementation of reflection, we
-- don't have @'Data.Typeable.Typeable' s@, which "Refined" wants for
-- pretty-printing exceptions. We /can/ provide @'Data.Typeable.Typeable' s@,
-- but at the cost of using the /slow/ implementation of reflection.
revealPredicate
  :: forall s. KnownIntSet s => Dict (Predicate (InSet 'Int s) Int)
revealPredicate = reifyTypeable (reflect (Proxy @s))
  \(_ :: Proxy s') ->
    reflect (Proxy @s') `seq`
    --  ^ Work around https://github.com/ekmett/reflection/issues/54
      case unsafeCoerce Refl :: s :~: s' of
        Refl -> Dict

-- | @'Element' s@ is an t'Int' that has been verified to be an element of @s@.
--
-- Thus, @'Element' s@ is a \"refinement\" type of t'Int', and this library
-- integrates with an implementation of refimenement types in "Refined", so
-- the machinery from there can be used to manipulate 'Element's (however see
-- 'revealPredicate').
--
-- The underlying t'Int' can be obtained with 'unrefine'. An t'Int' can be
-- validated into an @'Element' s@ with 'member'.
type Element s = Refined (InSet 'Int s) Int

unsafeCastElement :: forall s. Coercion Int (Element s)
unsafeCastElement = reallyUnsafeUnderlyingRefined

unsafeElement :: Int -> Element s
unsafeElement = coerceWith unsafeCastElement

-- | An existential wrapper for an as-yet-unknown set. Pattern maching on it
-- gives you a way to refer to the set, e.g.
--
-- @
-- case 'fromIntSet' ... of
--   'SomeIntSet' \@s _ -> doSomethingWith \@s
--
-- case 'fromIntSet' ... of
--   'SomeIntSet' (_ :: 'Proxy#' s) -> doSomethingWith \@s
-- @
data SomeIntSet where
  SomeIntSet :: forall s. KnownIntSet s => Proxy# s -> SomeIntSet

-- | Apply an unknown set to a continuation that can accept any set. This gives
-- you a way to refer to the set (the parameter @s@), e.g.:
--
-- @
-- 'withIntSet' ('fromIntSet' ...) $ \\(_ :: 'Proxy' s) -> doSomethingWith \@s
-- @
withIntSet
  :: forall r. SomeIntSet -> (forall s. KnownIntSet s => Proxy s -> r) -> r
withIntSet (SomeIntSet (_ :: Proxy# s)) k = k $ Proxy @s

-- | Construct a set from a regular 'Data.IntSet.IntSet'.
fromIntSet :: IntSet.IntSet -> SomeIntSet
fromIntSet s = reify s \(_ :: Proxy s) -> SomeIntSet @s proxy#

-- | An existential wrapper for an as-yet-unknown set, together with a proof of
-- some fact @p@ about the set. Pattern matching on it gives you a way to refer
-- to the set (the parameter @s@). Most functions will return a set in this way,
-- together with a proof that somehow relates the set to the function's inputs.
data SomeIntSetWith p where
  SomeIntSetWith :: forall s p. KnownIntSet s => !(p s) -> SomeIntSetWith p

-- | Apply an unknown set with proof to a continuation that can accept any set
-- satisfying the proof. This gives you a way to refer to the set (the parameter
-- @s@).
withIntSetWith
  :: forall r p. SomeIntSetWith p -> (forall s. KnownIntSet s => p s -> r) -> r
withIntSetWith (SomeIntSetWith p) k = k p

-- | An existential wrapper for an as-yet-unknown pair of sets, together with
-- a proof of some fact @p@ relating them.
data Some2IntSetWith p where
  Some2IntSetWith
    :: forall s t p. (KnownIntSet s, KnownIntSet t)
    => !(p s t) -> Some2IntSetWith p

-- | Apply a pair of unknown sets with proof to a continuation that can accept
-- any pair of sets satisfying the proof. This gives you a way to refer to the
-- sets (the parameters @s@ and @t@).
with2IntSetWith
  :: forall r p. Some2IntSetWith p
  -> (forall s t. (KnownIntSet s, KnownIntSet t) => p s t -> r)
  -> r
with2IntSetWith (Some2IntSetWith p) k = k p

-- | An empty set.
empty :: SomeIntSetWith (EmptyProof 'Int)
empty = reify IntSet.empty \(_ :: Proxy r)
  -> SomeIntSetWith @r $ EmptyProof unsafeSubset


-- | Create a set with a single element.
singleton :: Int -> SomeIntSetWith (SingletonProof 'Int Int)
singleton x = reify (IntSet.singleton x) \(_ :: Proxy r)
  -> SomeIntSetWith @r $ SingletonProof $ unsafeElement x

-- | Create a set from the elements of an arbitrary traversable.
fromTraversable
  :: forall t. Traversable t
  => t Int -> SomeIntSetWith (FromTraversableProof 'Int t Int)
fromTraversable xs = reify set \(_ :: Proxy r)
  -> SomeIntSetWith @r $ FromTraversableProof
    $ unsafeCoerce @(t (Element _)) @(t (Element r)) proof
  where
    (set, proof) = mapAccumL
      (\s x -> let !s' = IntSet.insert x s in (s', unsafeElement x))
      IntSet.empty
      xs

-- | Insert an element in a set.
insert :: forall s. KnownIntSet s
  => Int -> SomeIntSetWith (InsertProof 'Int Int s)
insert x = reify (IntSet.insert x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeIntSetWith @r $ InsertProof (unsafeElement x) unsafeSubset

-- | Delete an element from a set.
delete :: forall s. KnownIntSet s
  => Int -> SomeIntSetWith (SupersetProof 'Int s)
delete x = reify (IntSet.delete x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeIntSetWith @s $ SupersetProof unsafeSubset

-- | If an element is in the set, return the proof that it is.
member :: forall s. KnownIntSet s => Int -> Maybe (Element s)
member x
  | x `IntSet.member` reflect (Proxy @s) = Just $ unsafeElement x
  | otherwise = Nothing

-- | Find the largest element smaller than the given one.
lookupLT :: forall s. KnownIntSet s => Int -> Maybe (Element s)
lookupLT x = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.lookupLT x (reflect $ Proxy @s)

-- | Find the smallest element greater than the given one.
lookupGT :: forall s. KnownIntSet s => Int -> Maybe (Element s)
lookupGT x = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.lookupGT x (reflect $ Proxy @s)

-- | Find the largest element smaller or equal to the given one.
lookupLE :: forall s. KnownIntSet s => Int -> Maybe (Element s)
lookupLE x = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.lookupLE x (reflect $ Proxy @s)

-- | Find the smallest element greater or equal to the given one.
lookupGE :: forall s. KnownIntSet s => Int -> Maybe (Element s)
lookupGE x = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.lookupGE x (reflect $ Proxy @s)

-- | If the set is empty, return the proof that it is.
null :: forall s. KnownIntSet s => Maybe (EmptyProof 'Int s)
null
  | IntSet.null $ reflect $ Proxy @s = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ is a subset of @t@ (or is equal to), return a proof of that.
isSubsetOf
  :: forall s t. (KnownIntSet s, KnownIntSet t) => Maybe (SubsetProof 'Int s t)
isSubsetOf
  | reflect (Proxy @s) `IntSet.isSubsetOf` reflect (Proxy @t)
  = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ and @t@ are disjoint (i.e. their intersection is empty), return a
-- proof of that.
disjoint
  :: forall s t. (KnownIntSet s, KnownIntSet t)
  => Maybe (DisjointProof 'Int s t)
disjoint
#if MIN_VERSION_containers(0, 5, 11)
  | IntSet.disjoint (reflect $ Proxy @s) (reflect $ Proxy @t)
#else
  | IntSet.null $ IntSet.intersection (reflect $ Proxy @s) (reflect $ Proxy @t)
#endif
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | The union of two sets.
union
  :: forall s t. (KnownIntSet s, KnownIntSet t)
  => SomeIntSetWith (UnionProof 'Int s t)
union = reify (reflect (Proxy @s) `IntSet.union` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeIntSetWith @r
    $ UnionProof unsafeSubset unsafeSubsetWith2

-- unions :: ?

-- | Set with elements of @s@ that are not in @t@.
difference
  :: forall s t. (KnownIntSet s, KnownIntSet t)
  => SomeIntSetWith (DifferenceProof 'Int s t)
difference = reify (reflect (Proxy @s) `IntSet.difference` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeIntSetWith @r
    $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Intersection of two sets.
intersection
  :: forall s t. (KnownIntSet s, KnownIntSet t)
  => SomeIntSetWith (IntersectionProof 'Int s t)
intersection
  = reify (reflect (Proxy @s) `IntSet.intersection` reflect (Proxy @t))
    \(_ :: Proxy r) -> SomeIntSetWith @r
      $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Return a subset of elements that satisfy the given predicate.
filter
  :: forall s. KnownIntSet s
  => (Element s -> Bool) -> SomeIntSetWith (SupersetProof 'Int s)
filter p = reify (IntSet.filter (p . unsafeElement) $ reflect $ Proxy @s)
  \(_ :: Proxy r) -> SomeIntSetWith @r $ SupersetProof unsafeSubset

-- | Partition a set into two disjoint subsets: those that satisfy the
-- predicate, and those that don't.
partition
  :: forall s. KnownIntSet s
  => (Element s -> Bool) -> Some2IntSetWith (PartitionProof 'Int s Int)
partition p = case IntSet.partition (p . unsafeElement) $ reflect $ Proxy @s of
  (r, q) -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
    -> Some2IntSetWith @s @r $ PartitionProof
      do \x -> if p x
          then Left $ unsafeElement $ unrefine x
          else Right $ unsafeElement $ unrefine x
      unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Divide a set into two disjoint subsets at a point where the predicate stops
-- holding.
--
-- If @p@ is antitone ( \(\forall x y, x < y \implies p(x) \ge p(y)\) ), then
-- this point is uniquely defined. If @p@ is not antitone, a splitting point is
-- chosen in an unspecified way.
spanAntitone
  :: forall s. KnownIntSet s
  => (Element s -> Bool) -> Some2IntSetWith (PartialPartitionProof 'Int s)
spanAntitone p =
#if MIN_VERSION_containers(0, 6, 7)
  case IntSet.spanAntitone (p . unsafeElement) $ reflect $ Proxy @s of
    (r, q)
#else
  case List.span (p . unsafeElement)
    $ IntSet.toAscList $ reflect $ Proxy @s of
    (rs, qs)
      | let r = IntSet.fromDistinctAscList rs
      , let q = IntSet.fromDistinctAscList qs
#endif
      -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
        -> Some2IntSetWith @r @q $ PartialPartitionProof
          unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Return two disjoint subsets: those less than the given element, and those
-- greater than the given element; along with the proof that the given element
-- was in the set, if it was.
splitMember
  :: forall s. KnownIntSet s
  => Int -> Some2IntSetWith (SplitProof 'Int s (Element s))
splitMember x = case IntSet.splitMember x $ reflect $ Proxy @s of
  (r, m, q) -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
    -> Some2IntSetWith @r @q $ SplitProof
      (if m then Just (unsafeElement x) else Nothing)
      unsafeSubset \f g -> unsafeSubsetWith2 f g

-- | Apply the given function to each element of the set and collect the
-- results. Note that the resulting set can be smaller.
map
  :: forall s. KnownIntSet s
  => (Element s -> Int) -> SomeIntSetWith (MapProof 'Int s Int Int)
map f = reify (IntMap.keysSet m) \(_ :: Proxy r) -> SomeIntSetWith @r
  $ MapProof (unsafeElement . f) \y -> case IntMap.lookup (unrefine y) m of
    Nothing -> error "map: bug: Data.IntSet.Refined has been subverted"
    Just x -> x
  where
    !m = IntMap.fromList
      [ (y, unsafeElement x)
      | x <- IntSet.toList $ reflect $ Proxy @s
      , let !y = f $ unsafeElement x
      ]


-- | Map each element of @s@ into a monoid (with proof that it was an element),
-- and combine the results using 'Data.Monoid.<>'.
foldMap :: forall s m. (KnownIntSet s, Monoid m) => (Element s -> m) -> m
foldMap f = go $ reflect $ Proxy @s
  where
    go s = case IntSet.splitRoot s of
      [s'] -> Foldable.foldMap (f . unsafeElement) $ IntSet.toAscList s'
      xs -> Foldable.foldMap go xs

-- | Right associative fold with a lazy accumulator.
foldr :: forall s a. KnownIntSet s => (Element s -> a -> a) -> a -> a
foldr f z = IntSet.foldr (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a lazy accumulator.
foldl :: forall s a. KnownIntSet s => (a -> Element s -> a) -> a -> a
foldl f z = IntSet.foldl ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | Right associative fold with a strict accumulator.
foldr' :: forall s a. KnownIntSet s => (Element s -> a -> a) -> a -> a
foldr' f z = IntSet.foldr' (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a strict accumulator.
foldl' :: forall s a. KnownIntSet s => (a -> Element s -> a) -> a -> a
foldl' f z = IntSet.foldl' ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | Retrieves the smallest element of the set, and the set with that element
-- removed; or a proof that the set was empty.
minView
  :: forall s. KnownIntSet s
  => Either
    (EmptyProof 'Int s)
    (Element s, SomeIntSetWith (SupersetProof 'Int s))
minView = case IntSet.minView $ reflect $ Proxy @s of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (x, xs) -> Right $ (unsafeElement x,) $ reify xs \(_ :: Proxy r)
    -> SomeIntSetWith @r $ SupersetProof unsafeSubset

-- | Retrieves the greatest element of the set, and the set with that element
-- removed; or a proof that the set was empty.
maxView
  :: forall s. KnownIntSet s
  => Either
    (EmptyProof 'Int s)
    (Element s, SomeIntSetWith (SupersetProof 'Int s))
maxView = case IntSet.maxView $ reflect $ Proxy @s of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (x, xs) -> Right $ (unsafeElement x,) $ reify xs \(_ :: Proxy r)
    -> SomeIntSetWith @r $ SupersetProof unsafeSubset

-- | List of elements in the set in ascending order.
toList :: forall s. KnownIntSet s => [Element s]
toList = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.toAscList $ reflect $ Proxy @s

-- | List of elements in the set in descending order.
toDescList :: forall s. KnownIntSet s => [Element s]
toDescList = gcoerceWith (unsafeCastElement @s) $ coerce
  $ IntSet.toDescList $ reflect $ Proxy @s

-- | Convert an 'IntSet' into a 'Set', retaining its set of elements, which can
-- be converted with 'castFlavor'.
asSet :: forall s. KnownIntSet s => Set s Int
asSet = intSet2Set

-- | Convert an 'IntSet' into a 'HashSet', retaining its set of elements, which
-- can be converted with 'castFlavor'.
asHashSet :: forall s. KnownIntSet s => HashSet s Int
asHashSet = intSet2HashSet

-- | If elements of @s@ can be weakened to elements of @t@ and vice versa, then
-- @s@ and @t@ actually stand for the same set and @'Element' s@ can be safely
-- interconverted with @'Element' t@.
--
-- The requirement that the weakenings are natural transformations ensures that
-- they don't actually alter the elements. To build these you can compose
-- ':->''s from proofs returned by functions in this module, or "Refined"
-- functions like 'andLeft' or 'leftOr'.
castElement
  :: forall s t a.
     (forall x. Refined (InSet 'Int s) x -> Refined (InSet 'Int t) x)
  -> (forall x. Refined (InSet 'Int t) x -> Refined (InSet 'Int s) x)
  -> Coercion (Refined (InSet 'Int s) a) (Refined (InSet 'Int t) a)
castElement = castRefined

-- | If elements can be interconverted (e.g. as proved by 'castElement'), then
-- the sets can be interconverted too. For example we can establish that the
-- intersection of a set with itself is interconvertible with that set:
--
-- @
-- castIntersection
--   :: t'IntersectionProof' ''Data.IntSet.Refined.Int' s s r
--   -> 'Coercion' ('IntSet' r) ('IntSet' s)
-- castIntersection ( v'IntersectionProof' p1 p2)
--   = 'cast' $ 'castElement' ('andLeft' . p1) (p2 'id' 'id')
-- @
cast
  :: forall s t. (forall x. Coercion
    (Refined (InSet 'Int s) x)
    (Refined (InSet 'Int t) x))
  -> Coercion (IntSet s) (IntSet t)
cast Coercion
#if MIN_VERSION_base(4, 15, 0)
  = case unsafeEqualityProof @s @t of UnsafeRefl -> Coercion
#else
  = repr $ unsafeCoerce Refl
#endif
