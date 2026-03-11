{-# LANGUAGE CPP #-}
-- | This module implements a way of tracking the contents of a 'Data.Set.Set'
-- at the type level, and provides utilities for manipulating such sets.
--
-- The contents of a set are associated with a type parameter, e.g. @s@, so that
-- whenever you see the same type parameter, you know you are working with the
-- same set. The type @s@ itself has no internal structure, rather it is merely
-- a skolem type variable (rank-2 polymorphism 'Control.Monad.ST.runST' trick)
-- introduced by "Data.Reflection".
--
-- = Warning
-- This module together with "Data.Set" rely on 'Eq' and 'Ord' instances being
-- lawful: that '==' is an equivalence relation, and that the 'Ord' operations
-- define a total order on the quotient defined by this equivalence relation; at
-- least for the subset of values that are actually encountered at runtime. If
-- this assumption is violated, this module may not be able to uphold its
-- invariants and may throw errors. In particular beware of NaN in 'Float' and
-- 'Double'.
module Data.Set.Refined
  (
  -- * Set type
    KnownSet
  , Set
  -- * Refinement type
  , InSet(..)
  , Flavor(Regular)
  , Element
  , revealPredicate
  -- * Existentials and common proofs
  , SomeSet(..)
  , withSet
  , SomeSetWith(..)
  , withSetWith
  , Some2SetWith(..)
  , with2SetWith
  , (:->)
  , SupersetProof(..)
  , EmptyProof(..)
  -- * Construction
  , empty
  , singleton
  , SingletonProof(..)
  , fromSet
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
  , cartesianProduct
  , ProductProof(..)
  , disjointUnion
  , CoproductProof(..)
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
  , asIntSet
  , asHashSet
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
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection
import qualified Data.Set as Set
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

#if MIN_VERSION_containers(0, 5, 8)
#else
import qualified Data.List as List
#endif


-- | To use "Refined" machinery that uses the 'Predicate' typeclass you will
-- need to pattern match on this 'Dict'.
--
-- The reason is that in the default /fast/ implementation of reflection, we
-- don't have @'Typeable' s@, which "Refined" wants for pretty-printing
-- exceptions. We /can/ provide @'Typeable' s@, but at the cost of using the
-- /slow/ implementation of reflection.
revealPredicate
  :: forall s a. (Typeable a, Ord a, KnownSet s a)
  => Dict (Predicate (InSet 'Regular s) a)
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
type Element s = Refined (InSet 'Regular s)

unsafeCastElement :: forall s a. Coercion a (Element s a)
unsafeCastElement = reallyUnsafeUnderlyingRefined

unsafeElement :: a -> Element s a
unsafeElement = coerceWith unsafeCastElement

-- | An existential wrapper for an as-yet-unknown set. Pattern maching on it
-- gives you a way to refer to the set, e.g.
--
-- @
-- case 'fromSet' ... of
--   'SomeSet' \@s _ -> doSomethingWith \@s
--
-- case 'fromSet' ... of
--   'SomeSet' (_ :: 'Proxy#' s) -> doSomethingWith \@s
-- @
data SomeSet a where
  SomeSet :: forall s a. KnownSet s a => Proxy# s -> SomeSet a

-- | Apply an unknown set to a continuation that can accept any set. This gives
-- you a way to refer to the set (the parameter @s@), e.g.:
--
-- @
-- 'withSet' ('fromSet' ...) $ \\(_ :: 'Proxy' s) -> doSomethingWith \@s
-- @
withSet
  :: forall a r. SomeSet a -> (forall s. KnownSet s a => Proxy s -> r) -> r
withSet (SomeSet (_ :: Proxy# s)) k = k $ Proxy @s

-- | Construct a set from a regular 'Data.Set.Set'.
fromSet :: forall a. Set.Set a -> SomeSet a
fromSet s = reify s \(_ :: Proxy s) -> SomeSet @s proxy#

-- | An existential wrapper for an as-yet-unknown set, together with a proof of
-- some fact @p@ about the set. Pattern matching on it gives you a way to refer
-- to the set (the parameter @s@). Most functions will return a set in this way,
-- together with a proof that somehow relates the set to the function's inputs.
data SomeSetWith p a where
  SomeSetWith :: forall s a p. KnownSet s a => !(p s) -> SomeSetWith p a

-- | Apply an unknown set with proof to a continuation that can accept any set
-- satisfying the proof. This gives you a way to refer to the set (the parameter
-- @s@).
withSetWith
  :: forall a r p. SomeSetWith p a -> (forall s. KnownSet s a => p s -> r) -> r
withSetWith (SomeSetWith p) k = k p

-- | An existential wrapper for an as-yet-unknown pair of sets, together with
-- a proof of some fact @p@ relating them.
data Some2SetWith p a where
  Some2SetWith
    :: forall s t a p. (KnownSet s a, KnownSet t a)
    => !(p s t) -> Some2SetWith p a

-- | Apply a pair of unknown sets with proof to a continuation that can accept
-- any pair of sets satisfying the proof. This gives you a way to refer to the
-- sets (the parameters @s@ and @t@).
with2SetWith
  :: forall a r p. Some2SetWith p a
  -> (forall s t. (KnownSet s a, KnownSet t a) => p s t -> r)
  -> r
with2SetWith (Some2SetWith p) k = k p

-- | An empty set.
empty :: forall a. SomeSetWith (EmptyProof 'Regular) a
empty = reify Set.empty \(_ :: Proxy r)
  -> SomeSetWith @r $ EmptyProof unsafeSubset

-- | Create a set with a single element.
singleton :: forall a. a -> SomeSetWith (SingletonProof 'Regular a) a
singleton x = reify (Set.singleton x) \(_ :: Proxy r)
  -> SomeSetWith @r $ SingletonProof $ unsafeElement x

-- | Create a set from the elements of an arbitrary traversable.
fromTraversable
  :: forall t a. (Traversable t, Ord a)
  => t a -> SomeSetWith (FromTraversableProof 'Regular t a) a
fromTraversable xs = reify set \(_ :: Proxy r)
  -> SomeSetWith @r $ FromTraversableProof
    $ unsafeCoerce @(t (Element _ a)) @(t (Element r a)) proof
  where
    (set, proof) = mapAccumL
      (\s x -> let !s' = Set.insert x s in (s', unsafeElement x))
      Set.empty
      xs

-- | Insert an element in a set.
insert
  :: forall s a. (Ord a, KnownSet s a)
  => a -> SomeSetWith (InsertProof 'Regular a s) a
insert x = reify (Set.insert x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeSetWith @r $ InsertProof (unsafeElement x) unsafeSubset

-- | Delete an element from a set.
delete
  :: forall s a. (Ord a, KnownSet s a)
  => a -> SomeSetWith (SupersetProof 'Regular s) a
delete x = reify (Set.delete x $ reflect $ Proxy @s) \(_ :: Proxy r)
  -> SomeSetWith @s $ SupersetProof unsafeSubset

-- | If an element is in the set, return the proof that it is.
member :: forall s a. (Ord a, KnownSet s a) => a -> Maybe (Element s a)
member x
  | x `Set.member` reflect (Proxy @s) = Just $ unsafeElement x
  | otherwise = Nothing

-- | Find the largest element smaller than the given one.
lookupLT :: forall s a. (Ord a, KnownSet s a) => a -> Maybe (Element s a)
lookupLT x = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.lookupLT x (reflect $ Proxy @s)

-- | Find the smallest element greater than the given one.
lookupGT :: forall s a. (Ord a, KnownSet s a) => a -> Maybe (Element s a)
lookupGT x = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.lookupGT x (reflect $ Proxy @s)

-- | Find the largest element smaller or equal to the given one.
lookupLE :: forall s a. (Ord a, KnownSet s a) => a -> Maybe (Element s a)
lookupLE x = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.lookupLE x (reflect $ Proxy @s)

-- | Find the smallest element greater or equal to the given one.
lookupGE :: forall s a. (Ord a, KnownSet s a) => a -> Maybe (Element s a)
lookupGE x = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.lookupGE x (reflect $ Proxy @s)

-- | If the set is empty, return the proof that it is.
null :: forall s a. KnownSet s a => Maybe (EmptyProof 'Regular s)
null
  | Set.null $ reflect $ Proxy @s = Just $ EmptyProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ is a subset of @t@ (or is equal to), return a proof of that.
isSubsetOf
  :: forall s t a. (Ord a, KnownSet s a, KnownSet t a)
  => Maybe (SubsetProof 'Regular s t)
isSubsetOf
  | reflect (Proxy @s) `Set.isSubsetOf` reflect (Proxy @t)
  = Just $ SubsetProof unsafeSubset
  | otherwise = Nothing

-- | If @s@ and @t@ are disjoint (i.e. their intersection is empty), return a
-- proof of that.
disjoint
  :: forall s t a. (Ord a, KnownSet s a, KnownSet t a)
  => Maybe (DisjointProof 'Regular s t)
disjoint
#if MIN_VERSION_containers(0, 5, 11)
  | Set.disjoint (reflect $ Proxy @s) (reflect $ Proxy @t)
#else
  | Set.null $ Set.intersection (reflect $ Proxy @s) (reflect $ Proxy @t)
#endif
  = Just $ DisjointProof \f g -> unsafeSubsetWith2 f g
  | otherwise = Nothing

-- | The union of two sets.
union
  :: forall s t a. (Ord a, KnownSet s a, KnownSet t a)
  => SomeSetWith (UnionProof 'Regular s t) a
union = reify (reflect (Proxy @s) `Set.union` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeSetWith @r $ UnionProof unsafeSubset unsafeSubsetWith2

-- unions :: ?

-- | Set with elements of @s@ that are not in @t@.
difference
  :: forall s t a. (Ord a, KnownSet s a, KnownSet t a)
  => SomeSetWith (DifferenceProof 'Regular s t) a
difference = reify (reflect (Proxy @s) `Set.difference` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeSetWith @r
    $ DifferenceProof unsafeSubset (\f g -> unsafeSubsetWith2 f g) unsafeSubset

-- | Intersection of two sets.
intersection
  :: forall s t a. (Ord a, KnownSet s a, KnownSet t a)
  => SomeSetWith (IntersectionProof 'Regular s t) a
intersection = reify (reflect (Proxy @s) `Set.intersection` reflect (Proxy @t))
  \(_ :: Proxy r) -> SomeSetWith @r
    $ IntersectionProof unsafeSubset unsafeSubsetWith2

-- | Cartesian product of two sets. The elements are all pairs @(x, y)@ for each
-- @x@ from @s@ and for each @y@ from @t@.
cartesianProduct
  :: forall s t a b. (KnownSet s a, KnownSet t b)
  => SomeSetWith (ProductProof 'Regular s t) (a, b)
cartesianProduct = reify
#if MIN_VERSION_containers(0, 5, 11)
  (reflect (Proxy @s) `Set.cartesianProduct` reflect (Proxy @t))
#else
  (Set.fromDistinctAscList $ (,) <$> Set.toAscList (reflect $ Proxy @s)
    <*> Set.toAscList (reflect $ Proxy @t))
#endif
  \(_ :: Proxy r) -> SomeSetWith @r $ ProductProof let
      proof :: forall x y. Coercion
        (Refined (InSet 'Regular s) x, Refined (InSet 'Regular t) y)
        (Refined (InSet 'Regular r) (x, y))
      !proof
        | Coercion <- reallyUnsafeUnderlyingRefined @x @(InSet 'Regular s)
        , Coercion <- reallyUnsafeUnderlyingRefined @y @(InSet 'Regular t)
        = Coercion `trans`
          reallyUnsafeUnderlyingRefined @(x, y) @(InSet 'Regular r)
    in proof

-- | Disjoint union of two sets. Includes @'Left' x@ for each @x@ from @s@, and
-- @'Right' y@ for each @y@ from @t@.
disjointUnion
  :: forall s t a b. (KnownSet s a, KnownSet t b)
  => SomeSetWith (CoproductProof 'Regular s t) (Either a b)
disjointUnion = reify
#if MIN_VERSION_containers(0, 5, 11)
  (reflect (Proxy @s) `Set.disjointUnion` reflect (Proxy @t))
#else
  (Set.fromDistinctAscList $ (Left <$> Set.toAscList (reflect $ Proxy @s))
    ++ (Right <$> Set.toAscList (reflect $ Proxy @t)))
#endif
  \(_ :: Proxy r) -> SomeSetWith @r $ CoproductProof let
      proof :: forall x y. Coercion
        (Either (Refined (InSet 'Regular s) x) (Refined (InSet 'Regular t) y))
        (Refined (InSet 'Regular r) (Either x y))
      !proof
        | Coercion <- reallyUnsafeUnderlyingRefined @x @(InSet 'Regular s)
        , Coercion <- reallyUnsafeUnderlyingRefined @y @(InSet 'Regular t)
        = Coercion `trans`
          reallyUnsafeUnderlyingRefined @(Either x y) @(InSet 'Regular r)
    in proof

-- | Return a subset of elements that satisfy the given predicate.
filter
  :: forall s a. KnownSet s a
  => (Element s a -> Bool) -> SomeSetWith (SupersetProof 'Regular s) a
filter p = reify (Set.filter (p .  unsafeElement) $ reflect $ Proxy @s)
  \(_ :: Proxy r) -> SomeSetWith @r $ SupersetProof unsafeSubset

-- | Partition a set into two disjoint subsets: those that satisfy the
-- predicate, and those that don't.
partition
  :: forall s a. KnownSet s a
  => (Element s a -> Bool) -> Some2SetWith (PartitionProof 'Regular s a) a
partition p = case Set.partition (p . unsafeElement) $ reflect $ Proxy @s of
  (r, q) -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
    -> Some2SetWith @s @r $ PartitionProof
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
  :: forall s a. KnownSet s a
  => (Element s a -> Bool) -> Some2SetWith (PartialPartitionProof 'Regular s) a
spanAntitone p =
#if MIN_VERSION_containers(0, 5, 8)
  case Set.spanAntitone (p . unsafeElement) $ reflect $ Proxy @s of
    (r, q)
#else
  case List.span (p . unsafeElement)
    $ Set.toAscList $ reflect $ Proxy @s of
    (rs, qs)
      | let r = Set.fromDistinctAscList rs
      , let q = Set.fromDistinctAscList qs
#endif
      -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
        -> Some2SetWith @r @q $ PartialPartitionProof
          unsafeSubset unsafeSubsetWith2 \f g -> unsafeSubsetWith2 f g

-- | Return two disjoint subsets: those less than the given element, and those
-- greater than the given element; along with the proof that the given element
-- was in the set, if it was.
splitMember
  :: forall s a. (Ord a, KnownSet s a)
  => a -> Some2SetWith (SplitProof 'Regular s (Element s a)) a
splitMember x = case Set.splitMember x $ reflect $ Proxy @s of
  (r, m, q) -> reify r \(_ :: Proxy r) -> reify q \(_ :: Proxy q)
    -> Some2SetWith @r @q $ SplitProof
      (if m then Just (unsafeElement x) else Nothing)
      unsafeSubset \f g -> unsafeSubsetWith2 f g

-- | Apply the given function to each element of the set and collect the
-- results. Note that the resulting set can be smaller.
map
  :: forall s a b. (Ord b, KnownSet s a)
  => (Element s a -> b) -> SomeSetWith (MapProof 'Regular s a b) b
map f = reify (Map.keysSet m) \(_ :: Proxy r) -> SomeSetWith @r
  $ MapProof (unsafeElement . f) \y -> case Map.lookup (unrefine y) m of
    Nothing -> error "map: bug: Data.Set.Refined has been subverted"
    Just x -> x
  where
    !m = Map.fromList
      [ (y, unsafeElement x)
      | x <- Set.toList $ reflect $ Proxy @s
      , let !y = f $ unsafeElement x
      ]

-- | Map each element of @s@ into a monoid (with proof that it was an element),
-- and combine the results using 'Data.Monoid.<>'.
foldMap :: forall s a m. (KnownSet s a, Monoid m) => (Element s a -> m) -> m
foldMap f = Foldable.foldMap (f . unsafeElement) $ reflect $ Proxy @s

-- | Right associative fold with a lazy accumulator.
foldr :: forall s a b. KnownSet s a => (Element s a -> b -> b) -> b -> b
foldr f z = Set.foldr (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a lazy accumulator.
foldl :: forall s a b. KnownSet s a => (b -> Element s a -> b) -> b -> b
foldl f z = Set.foldl ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | Right associative fold with a strict accumulator.
foldr' :: forall s a b. KnownSet s a => (Element s a -> b -> b) -> b -> b
foldr' f z = Set.foldr' (f . unsafeElement) z $ reflect $ Proxy @s

-- | Left associative fold with a strict accumulator.
foldl' :: forall s a b. KnownSet s a => (b -> Element s a -> b) -> b -> b
foldl' f z = Set.foldl' ((. unsafeElement) . f) z $ reflect $ Proxy @s

-- | Retrieves the smallest element of the set, and the set with that element
-- removed; or a proof that the set was empty.
minView
  :: forall s a. KnownSet s a
  => Either
    (EmptyProof 'Regular s)
    (Element s a, SomeSetWith (SupersetProof 'Regular s) a)
minView = case Set.minView $ reflect $ Proxy @s of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (x, xs) -> Right $ (unsafeElement x,) $ reify xs \(_ :: Proxy r)
    -> SomeSetWith @r $ SupersetProof unsafeSubset

-- | Retrieves the greatest element of the set, and the set with that element
-- removed; or a proof that the set was empty.
maxView
  :: forall s a. KnownSet s a
  => Either
    (EmptyProof 'Regular s)
    (Element s a, SomeSetWith (SupersetProof 'Regular s) a)
maxView = case Set.maxView $ reflect $ Proxy @s of
  Nothing -> Left $ EmptyProof unsafeSubset
  Just (x, xs) -> Right $ (unsafeElement x,) $ reify xs \(_ :: Proxy r)
    -> SomeSetWith @r $ SupersetProof unsafeSubset

-- | List of elements in the set in ascending order.
toList :: forall s a. KnownSet s a => [Element s a]
toList = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.toAscList $ reflect $ Proxy @s

-- | List of elements in the set in descending order.
toDescList :: forall s a. KnownSet s a => [Element s a]
toDescList = gcoerceWith (unsafeCastElement @s @a) $ coerce
  $ Set.toDescList $ reflect $ Proxy @s

-- | Convert a 'Set' into an 'IntSet', retaining its set of elements, which can
-- be converted with 'castFlavor'.
asIntSet :: forall s. KnownSet s Int => IntSet s
asIntSet = set2IntSet

-- | Convert a 'Set' into a 'HashSet', retaining its set of elements, which can
-- be converted with 'castFlavor'.
asHashSet :: forall s a. (Hashable a, KnownSet s a) => HashSet s a
asHashSet = set2HashSet

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
--   :: t'IntersectionProof' ''Regular' s s r
--   -> 'Coercion' ('Set' r a) ('Set' s a)
-- castIntersection ( v'IntersectionProof' p1 p2)
--   = 'cast' $ 'castElement' ('andLeft' . p1) (p2 'id' 'id')
-- @
cast
  :: forall s t a. (forall x. Coercion (Element s x) (Element t x))
  -> Coercion (Set s a) (Set t a)
cast Coercion
#if MIN_VERSION_base(4, 15, 0)
  = case unsafeEqualityProof @s @t of UnsafeRefl -> Coercion
#else
  = repr $ unsafeCoerce Refl
#endif
