{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnicodeSyntax    #-}

-- | Efficient sets over bounded enumerations, using bitwise operations based on
-- [containers](https://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.IntSet.Internal.html)
-- and
-- [EdisonCore](https://hackage.haskell.org/package/EdisonCore-1.3.2.1/docs/src/Data-Edison-Coll-EnumSet.html).
-- In many cases, @EnumSet@s may be optimised away entirely by constant folding
-- at compile-time. For example, in the following code:
--
-- @
-- import Data.Enum.Set as E
--
-- data Foo = A | B | C | D | E | F | G | H deriving (Bounded, Enum, Eq, Ord)
--
-- instance E.AsEnumSet Foo
--
-- addFoos :: E.EnumSet Foo -> E.EnumSet Foo
-- addFoos = E.delete A . E.insert B
--
-- bar :: E.EnumSet Foo
-- bar = addFoos $ E.fromFoldable [A, C, E]
--
-- barHasA :: Bool
-- barHasA = E.member A bar
-- @
--
-- With @-O@ or @-O2@, @bar@ will compile to @GHC.Types.W\# 22\#\#@ and
-- @barHasA@ will compile to @GHC.Types.False@.
--
-- By default, 'Word's are used as the representation. Other representations may
-- be chosen in the class instance:
--
-- @
-- {-\# LANGUAGE TypeFamilies \#-}
--
-- import Data.Enum.Set as E
-- import Data.Word (Word64)
--
-- data Foo = A | B | C | D | E | F | G | H deriving (Bounded, Enum, Eq, Ord, Show)
--
-- instance E.AsEnumSet Foo where
--     type EnumSetRep Foo = Word64
--
-- @
--
-- For type @EnumSet E@, @EnumSetRep E@ should be a @Word@-like type that
-- implements 'Bits' and 'Num', and @E@ should be a type that implements 'Eq'
-- and 'Enum' equivalently and is a bijection to 'Int' over its range.
-- @EnumSet E@ can only store a value of @E@ if the result of applying
-- 'fromEnum' to the value is positive and less than the number of bits in
-- @EnumSetRep E@. For this reason, it is preferable for @E@ to be a type that
-- derives @Eq@ and @Enum@, and for @EnumSetRep E@ to have more bits than the
-- number of constructors of @E@.
--
-- If the highest @fromEnum@ value of @E@ is 29, @EnumSetRep E@ should be
-- 'Word', because it always has at least 30 bits. This is the
-- default implementation.
-- Otherwise, options include 'Data.Word.Word32', 'Data.Word.Word64', and the
-- [wide-word](https://hackage.haskell.org/package/wide-word-0.1.0.8/) package's
-- [Data.WideWord.Word128](https://hackage.haskell.org/package/wide-word-0.1.0.8/docs/Data-WideWord-Word128.html).
-- Foreign types may also be used.
--
-- Note: complexity calculations assume that @EnumSetRep E@ implements 'Bits'
-- with constant-time functions, as is the case with @Word@ etc. Otherwise, the
-- complexity of those operations should be added to the complexity of @EnumSet@
-- functions.
module Data.Enum.Set
  ( AsEnumSet(..)
  -- * Set type
  , EnumSet
    -- * Construction
  , empty
  , singleton
  , fromFoldable

  -- * Insertion
  , insert

  -- * Deletion
  , delete

  -- * Query
  , member
  , notMember
  , null
  , size
  , isSubsetOf

  -- * Combine
  , union
  , difference
  , (\\)
  , symmetricDifference
  , intersection

  -- * Filter
  , filter
  , partition

  -- * Map
  , map

  -- * Folds
  , foldl, foldl', foldr, foldr'
  , foldl1, foldl1', foldr1, foldr1'

  -- ** Special folds
  , foldMap
  , traverse
  , any
  , all

   -- * Min/Max
  , minimum
  , maximum
  , deleteMin
  , deleteMax
  , minView
  , maxView

  -- * Conversion
  , toList
  , fromRaw
  , toRaw
  ) where

import Prelude hiding (Foldable(..), all, any, filter, map, traverse)
import Data.Foldable (Foldable)

import Data.Bits

import qualified Data.Enum.Set.Base as E

class (Enum a, FiniteBits (EnumSetRep a), Num (EnumSetRep a)) => AsEnumSet a where
    type EnumSetRep a
    type EnumSetRep a = Word

type EnumSet a = E.EnumSet (EnumSetRep a) a

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty set.
empty :: ∀ a. AsEnumSet a => EnumSet a
empty = E.empty
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: ∀ a. AsEnumSet a => a -> EnumSet a
singleton = E.singleton
{-# INLINE singleton #-}

-- | /O(n)/. Create a set from a finite foldable data structure.
fromFoldable :: ∀ f a. (Foldable f, AsEnumSet a) => f a -> EnumSet a
fromFoldable = E.fromFoldable
{-# INLINE fromFoldable #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(1)/. Add a value to the set.
insert :: ∀ a. AsEnumSet a => a -> EnumSet a -> EnumSet a
insert = E.insert
{-# INLINE insert #-}

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

-- | /O(1)/. Delete a value in the set.
delete :: ∀ a. AsEnumSet a => a -> EnumSet a -> EnumSet a
delete = E.delete
{-# INLINE delete #-}

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the value a member of the set?
member :: ∀ a. AsEnumSet a => a -> EnumSet a -> Bool
member = E.member
{-# INLINE member #-}

-- | /O(1)/. Is the value not in the set?
notMember :: ∀ a. AsEnumSet a => a -> EnumSet a -> Bool
notMember = E.notMember
{-# INLINE notMember #-}

-- | /O(1)/. Is this the empty set?
null :: ∀ a. AsEnumSet a => EnumSet a -> Bool
null = E.null
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the set.
size :: ∀ a. AsEnumSet a => EnumSet a -> Int
size = E.size
{-# INLINE size #-}

-- | /O(1)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> Bool
isSubsetOf = E.isSubsetOf
{-# INLINE isSubsetOf #-}

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}

-- | /O(1)/. The union of two sets.
union :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> EnumSet a
union = E.union
{-# INLINE union #-}

-- | /O(1)/. Difference between two sets.
difference :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> EnumSet a
difference = E.difference
{-# INLINE difference #-}

-- | /O(1)/. See 'difference'.
(\\) :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> EnumSet a
(\\) = E.difference
infixl 9 \\
{-# INLINE (\\) #-}

-- | /O(1)/. Elements which are in either set, but not both.
symmetricDifference :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> EnumSet a
symmetricDifference = E.symmetricDifference
{-# INLINE symmetricDifference #-}

-- | /O(1)/. The intersection of two sets.
intersection :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a -> EnumSet a
intersection = E.intersection
{-# INLINE intersection #-}

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: ∀ a. AsEnumSet a => (a -> Bool) -> EnumSet a -> EnumSet a
filter = E.filter
{-# INLINE filter #-}

-- | /O(n)/. Partition the set according to some predicate.
-- The first set contains all elements that satisfy the predicate,
-- the second all elements that fail the predicate.
partition :: ∀ a. AsEnumSet a
          => (a -> Bool) -> EnumSet a -> (EnumSet a, EnumSet a)
partition = E.partition
{-# INLINE partition #-}

{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}

-- | /O(n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: ∀ a b. (AsEnumSet a, AsEnumSet b) => (a -> b) -> EnumSet a -> EnumSet b
map = E.map'

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Left fold.
foldl :: ∀ a b. AsEnumSet a => (b -> a -> b) -> b -> EnumSet a -> b
foldl = E.foldl
{-# INLINE foldl #-}

-- | /O(n)/. Left fold with strict accumulator.
foldl' :: ∀ a b. AsEnumSet a => (b -> a -> b) -> b -> EnumSet a -> b
foldl' = E.foldl'
{-# INLINE foldl' #-}

-- | /O(n)/. Right fold.
foldr :: ∀ a b. AsEnumSet a => (a -> b -> b) -> b -> EnumSet a -> b
foldr = E.foldr
{-# INLINE foldr #-}

-- | /O(n)/. Right fold with strict accumulator.
foldr' :: ∀ a b. AsEnumSet a => (a -> b -> b) -> b -> EnumSet a -> b
foldr' = E.foldr'
{-# INLINE foldr' #-}

-- | /O(n)/. Left fold on non-empty sets.
foldl1 :: ∀ a. AsEnumSet a => (a -> a -> a) -> EnumSet a -> a
foldl1 = E.foldl1
{-# INLINE foldl1 #-}

-- | /O(n)/. Left fold on non-empty sets with strict accumulator.
foldl1' :: ∀ a. AsEnumSet a => (a -> a -> a) -> EnumSet a -> a
foldl1' = E.foldl1'
{-# INLINE foldl1' #-}

-- | /O(n)/. Right fold on non-empty sets.
foldr1 :: ∀ a. AsEnumSet a => (a -> a -> a) -> EnumSet a -> a
foldr1 = E.foldr1
{-# INLINE foldr1 #-}

-- | /O(n)/. Right fold on non-empty sets with strict accumulator.
foldr1' :: ∀ a. AsEnumSet a => (a -> a -> a) -> EnumSet a -> a
foldr1' = E.foldr1'
{-# INLINE foldr1'  #-}

-- | /O(n)/. Map each element of the structure to a monoid, and combine the
-- results.
foldMap :: ∀ m a. (Monoid m, AsEnumSet a) => (a -> m) -> EnumSet a -> m
foldMap = E.foldMap
{-# INLINE foldMap #-}

traverse :: ∀ f a. (Applicative f, AsEnumSet a)
         => (a -> f a) -> EnumSet a -> f (EnumSet a)
traverse = E.traverse
{-# INLINE traverse #-}

-- | /O(n)/. Check if all elements satisfy some predicate.
all :: ∀ a. AsEnumSet a => (a -> Bool) -> EnumSet a -> Bool
all = E.all
{-# INLINE all #-}

-- | /O(n)/. Check if any element satisfies some predicate.
any :: ∀ a. AsEnumSet a => (a -> Bool) -> EnumSet a -> Bool
any = E.any
{-# INLINE any #-}

{--------------------------------------------------------------------
  Min/Max
--------------------------------------------------------------------}

-- | /O(1)/. The minimal element of a non-empty set.
minimum :: ∀ a. AsEnumSet a => EnumSet a -> a
minimum = E.minimum
{-# INLINE minimum #-}

-- | /O(1)/. The maximal element of a non-empty set.
maximum :: ∀ a. AsEnumSet a => EnumSet a -> a
maximum = E.maximum
{-# INLINE maximum #-}

-- | /O(1)/. Delete the minimal element.
deleteMin :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a
deleteMin = E.deleteMin
{-# INLINE deleteMin #-}

-- | /O(1)/. Delete the maximal element.
deleteMax :: ∀ a. AsEnumSet a => EnumSet a -> EnumSet a
deleteMax = E.deleteMax
{-# INLINE deleteMax #-}

-- | /O(1)/. Retrieves the minimal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
minView :: ∀ a. AsEnumSet a => EnumSet a -> Maybe (a, EnumSet a)
minView = E.minView
{-# INLINE minView #-}

-- | /O(1)/. Retrieves the maximal element of the set,
-- and the set stripped of that element,
-- or Nothing if passed an empty set.
maxView :: ∀ a. AsEnumSet a => EnumSet a -> Maybe (a, EnumSet a)
maxView = E.maxView
{-# INLINE maxView #-}

{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}

-- | /O(n)/. Convert the set to a list of values.
toList :: ∀ a. AsEnumSet a => EnumSet a -> [a]
toList = E.toList
{-# INLINE toList #-}

-- | /O(1)/. Convert a representation into an @EnumSet@.
-- Intended for use with foreign types.
fromRaw :: ∀ a. AsEnumSet a => EnumSetRep a -> EnumSet a
fromRaw = E.fromRaw
{-# INLINE fromRaw #-}

-- | /O(1)/. Convert an @EnumSet@ into its representation.
-- Intended for use with foreign types.
toRaw :: ∀ a. AsEnumSet a => EnumSet a -> EnumSetRep a
toRaw = E.toRaw
{-# INLINE toRaw #-}
