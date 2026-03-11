{-# LANGUAGE CPP #-}
#if MIN_VERSION_refined(0, 7, 0)
#else
{-# LANGUAGE UndecidableInstances #-}
#endif
module Data.Container.Refined.Proofs where

import           Data.Constraint (Dict(..))
import           Data.Container.Refined.Hashable
import           Data.Kind
import           Data.Reflection
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import           Data.Type.Coercion
import           Data.Typeable
import           Refined


-- | A constraint evidencing that we know the contents of the set @s@ at
-- runtime. Whenever you see this constraint on a function, there is an actual
-- @'Data.Set.Set' a@ being passed around at runtime.
--
-- Given this constraint, to obtain a regular @'Data.Set.Set' a@ you can use
-- 'reflect'.
type KnownSet s a = Reifies s (Set.Set a)

-- | A 'Data.Set.Set' whose contents are tracked by the type parameter @s@. This
-- is a \"singleton\": for a given @s@ there's only one value of this type.
--
-- Since this is just a 'Dict', you can freely convert between the value ('Set')
-- and the constraint ('KnownSet'). This library prefers to use the constraint.
type Set s a = Dict (KnownSet s a)

-- | A constraint evidencing that we know the contents of the set @s@ at
-- runtime. Whenever you see this constraint on a function, there is an actual
-- 'Data.IntSet.IntSet' being passed around at runtime.
--
-- Given this constraint, to obtain a regular 'Data.IntSet.IntSet' you can use
-- 'reflect'.
type KnownIntSet s = Reifies s IntSet.IntSet

-- | A 'Data.IntSet.IntSet' whose contents are tracked by the type parameter
-- @s@. This is a \"singleton\": for a given @s@ there's only one value of this
-- type.
--
-- Since this is just a 'Dict', you can freely convert between the value
-- ('IntSet') and the constraint ('KnownIntSet'). This library prefers to use
-- the constraint.
type IntSet s = Dict (KnownIntSet s)

-- | A constraint evidencing that we know the contents of the set @s@ at
-- runtime. Whenever you see this constraint on a function, there is an actual
-- @'Data.HashSet.HashSet' a@ being passed around at runtime.
--
-- Given this constraint, to obtain a regular @'Data.HashSet.HashSet' a@ you can
-- use 'reflect'.
type KnownHashSet s a = Reifies s (HashSet.HashSet a)

-- | A 'Data.HashSet.HashSet' whose contents are tracked by the type parameter
-- @s@. This is a \"singleton\": for a given @s@ there's only one value of this
-- type.
--
-- Since this is just a 'Dict', you can freely convert between the value
-- ('HashSet') and the constraint ('KnownHashSet'). This library prefers to use
-- the constraint.
type HashSet s a = Dict (KnownHashSet s a)

-- | Disambiguate the choice of implementation for sets and maps.
data Flavor
  = Regular -- ^ 'Data.Set.Set' and 'Data.Map.Map'
  | Int -- ^ 'Data.IntSet.IntSet' and 'Data.IntMap.IntMap'
  | Hashed -- ^ 'Data.HashSet.HashSet' and 'Data.HashMap.Lazy.HashMap'

-- | A predicate for use with "Refined", verifying that a value is an element of
-- the set @s@.
data InSet (f :: Flavor) (s :: Type) = InSet

-- | See 'Data.Set.Refined.revealPredicate'.
instance (Ord a, Typeable s, KnownSet s a)
  => Predicate (InSet 'Regular s) a where
  validate p x
    | x `Set.member` reflect (Proxy @s) = success
    | otherwise = throwRefineOtherException
      (typeRep p)
      "Value is not in the Set"

-- | See 'Data.IntSet.Refined.revealPredicate'.
instance (a ~ Int, Typeable s, KnownIntSet s)
  => Predicate (InSet 'Int s) a where
  validate p x
    | x `IntSet.member` reflect (Proxy @s) = success
    | otherwise = throwRefineOtherException
      (typeRep p)
      "Value is not in the IntSet"

-- | See 'Data.HashSet.Refined.revealPredicate'.
instance (Hashable a, Typeable s, KnownHashSet s a)
  => Predicate (InSet 'Hashed s) a where
  validate p x
    | x `HashSet.member` reflect (Proxy @s) = success
    | otherwise = throwRefineOtherException
      (typeRep p)
      "Value is not in the HashSet"

-- | A proof that values satisfying @p@ can be cast into values satisfying @q@.
--
-- For example, @'InSet' s ':->' 'InSet' r@ proves that \(s \subseteq r\).
type p :-> q = forall x. Refined p x -> Refined q x
infix 1 :->

-- | Proof that the set @r@ is empty.
newtype EmptyProof f r = EmptyProof
  (forall s. InSet f r :-> InSet f s)
  -- ^ \(\forall s, r \subseteq s\), which is equivalent to
  -- \(r \subseteq \varnothing\)

-- | Proof that @r@ contains an element of type @a@.
newtype SingletonProof f a r = SingletonProof
  (Refined (InSet f r) a) -- ^ The element that is guaranteed to be in @r@

-- | Proof that elements of @t a@ are contained in @r@.
newtype FromTraversableProof f (t :: Type -> Type) a r = FromTraversableProof
  (t (Refined (InSet f r) a))
  -- ^ The original traversable, with all elements refined with a guarantee of
  -- being in @r@

-- | Proof that @r@ is @s@ with @a@ inserted.
data InsertProof f a s r = InsertProof
  (Refined (InSet f r) a)
  -- ^ The element that was inserted and is guaranteed to be in @r@.
  (InSet f s :-> InSet f r) -- ^ \(s \subseteq r \)

-- | Proof that @s@ is a subset of the set @r@.
newtype SubsetProof f s r = SubsetProof
  (InSet f s :-> InSet f r) -- ^ \(s \subseteq r\)

-- | Proof that @s@ is a superset of the set @r@.
newtype SupersetProof f s r = SupersetProof
  (InSet f r :-> InSet f s) -- ^ \(r \subseteq s\)

-- | Proof that @s@ and @r@ are disjoint.
newtype DisjointProof f s r = DisjointProof
  (forall t. InSet f t :-> InSet f s
    -> InSet f t :-> InSet f r
    -> forall u. InSet f t :-> InSet f u)
  -- ^ \(\forall t,(t\subseteq s)\land(t\subseteq r)\implies\forall u,t\subseteq u\),
  -- which is equivalent to \(s \cap r \subseteq \varnothing\)

-- | Proof that unioning @s@ and @t@ gives @r@.
data UnionProof f s t r = UnionProof
  (InSet f s || InSet f t :-> InSet f r) -- ^ \(s \cup t \subseteq r\)
  (forall u. InSet f s :-> InSet f u
    -> InSet f t :-> InSet f u
    -> InSet f r :-> InSet f u)
  -- ^ \(\forall u,(s\subseteq u)\land(t\subseteq u)\implies r\subseteq u\),
  -- which is equivalent to \(r \subseteq s \cup u\)

-- | Proof that if from @s@ you subtract @t@, then you get @r@.
data DifferenceProof f s t r = DifferenceProof
  (InSet f r :-> InSet f s) -- ^ \(r \subseteq s\)
  (forall u. InSet f u :-> InSet f r
    -> InSet f u :-> InSet f t
    -> forall v. InSet f u :-> InSet f v)
  -- ^ \(\forall u,(u\subseteq r)\land(u\subseteq t)\implies\forall v,u\subseteq v\),
  -- which is equivalent to \(r \cap t \subseteq \varnothing\)
  (InSet f s :-> InSet f t || InSet f r) -- ^ \(s \subseteq t \cup r\)

-- | Proof that @r@ is obtained by removing some of @t@'s elements from @s@.
data PartialDifferenceProof f s t r = PartialDifferenceProof
  (InSet f r :-> InSet f s) -- ^ \(r \subseteq s\)
  (InSet f s :-> InSet f t || InSet f r) -- ^ \(s \subseteq t \cup r\)

-- | Proof that intersecting @s@ and @t@ gives @r@.
data IntersectionProof f s t r = IntersectionProof
  (InSet f r :-> InSet f s && InSet f t) -- ^ \(r \subseteq s \cap t\)
  (forall u. InSet f u :-> InSet f s
    -> InSet f u :-> InSet f t
    -> InSet f u :-> InSet f r)
  -- ^ \(\forall u,(u\subseteq s)\land(u\subseteq t)\implies u\subseteq r\),
  -- which is equivalent to \(s \cap t \subseteq r\)

-- | Proof that the cartesian product of @s@ and @t@ is @r@.
newtype ProductProof f s t r = ProductProof
  (forall a b. Coercion
    (Refined (InSet f s) a, Refined (InSet f t) b)
    (Refined (InSet f r) (a, b)))
  -- ^ A pair of elements from @s@ and @t@ respectively can be converted into an
  -- element of @r@ and back. You can use @'coerceWith' co@ and
  -- @'coerceWith' ('sym' co)@.

-- | Proof that the tagged disjoint union of @s@ and @t@ is @r@.
newtype CoproductProof f s t r = CoproductProof
  (forall a b. Coercion
    (Either (Refined (InSet f s) a) (Refined (InSet f t) b))
    (Refined (InSet f r) (Either a b)))
  -- ^ Coproduct of elements of @s@ and @t@ can be converted into an element of
  -- @r@ and back. You can use @'coerceWith' co@ and @'coerceWith' ('sym' co)@.

-- | Proof that @s@ is the union of disjoint subsets @r@ and @q@, together with
-- a procedure that decides which of the two an element belongs to.
data PartitionProof f s a r q = PartitionProof
  (Refined (InSet f s) a
    -> Either (Refined (InSet f r) a) (Refined (InSet f q) a))
  -- ^ Decide whether a given element of @s@ belongs to @r@ or to @q@
  (InSet f r || InSet f q :-> InSet f s) -- ^ \(r \cup q \subseteq s\)
  (forall t. InSet f r :-> InSet f t
    -> InSet f q :-> InSet f t
    -> InSet f s :-> InSet f t)
  -- ^ \(\forall t,(r\subseteq t)\land(q\subseteq t)\implies s\subseteq t\),
  -- which is equivalent to \(s \subseteq r \cup q\)
  (forall t. InSet f t :-> InSet f r
    -> InSet f t :-> InSet f q
    -> forall u. InSet f t :-> InSet f u)
  -- ^ \(\forall t,(t\subseteq r)\land(t\subseteq q)\implies\forall u,t\subseteq u\),
  -- which is equivalent to \(r \cap q \subseteq \varnothing\)

-- | Proof that @s@ is the union of disjoint subsets @r@ and @q@, but without a
-- deciding procedure.
data PartialPartitionProof f s r q = PartialPartitionProof
  (InSet f r || InSet f q :-> InSet f s) -- ^ \(r \cup q \subseteq s\)
  (forall t. InSet f r :-> InSet f t
    -> InSet f q :-> InSet f t
    -> InSet f s :-> InSet f t)
  -- ^ \(\forall t,(r\subseteq t)\land(q\subseteq t)\implies s\subseteq t\),
  -- which is equivalent to \(s \subseteq r \cup q\)
  (forall t. InSet f t :-> InSet f r
    -> InSet f t :-> InSet f q
    -> forall u. InSet f t :-> InSet f u)
  -- ^ \(\forall t,(t\subseteq r)\land(t\subseteq q)\implies\forall u,t\subseteq u\),
  -- which is equivalent to \(r \cap q \subseteq \varnothing\)

-- | Proof that @s@ contains disjoint subsets @r@ and @q@, along with an
-- optional element between them.
data SplitProof f s e r q = SplitProof
  !(Maybe e) -- ^ The element between @r@ and @q@
  (InSet f r || InSet f q :-> InSet f s) -- ^ \(r \cup q \subseteq s\)
  (forall t. InSet f t :-> InSet f r
    -> InSet f t :-> InSet f q
    -> forall u. InSet f t :-> InSet f u)
  -- ^ \(\forall t,(t\subseteq r)\land(t\subseteq q)\implies\forall u,t\subseteq u\),
  -- which is equivalent to \(r \cap q \subseteq \varnothing\)

-- | Proof that @r@ is the direct image of @s@ under some mapping @f :: a -> b@.
data MapProof f s a b r = MapProof
  (Refined (InSet f s) a -> Refined (InSet f r) b)
  -- ^ Compute the image of an element of @s@ (the image is then an element of
  -- @r@)
  (Refined (InSet f r) b -> Refined (InSet f s) a)
  -- ^ For an element of @r@, return an arbitrary preimage from @s@
