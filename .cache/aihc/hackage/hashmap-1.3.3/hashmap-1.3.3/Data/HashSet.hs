-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashSet
-- Copyright   :  (c) Milan Straka 2011
-- License     :  BSD-style
-- Maintainer  :  fox@ucw.cz
-- Stability   :  provisional
-- Portability :  portable
--
-- Persistent 'Set' based on hashing, which is defined as
--
-- @
--   data 'Set' e = 'Data.IntMap.IntMap' (Some e)
-- @
--
-- is an 'Data.IntMap.IntMap' indexed by hash values of elements,
-- containing a value of @Some e@. That contains either one 'e'
-- or a @'Data.Set.Set' e@ with elements of the same hash values.
--
-- The interface of a 'Set' is a suitable subset of 'Data.IntSet.IntSet'
-- and can be used as a drop-in replacement of 'Data.Set.Set'.
--
-- The complexity of operations is determined by the complexities of
-- 'Data.IntMap.IntMap' and 'Data.Set.Set' operations. See the sources of
-- 'Set' to see which operations from @containers@ package are used.
-----------------------------------------------------------------------------

module Data.HashSet ( Set
                    , HashSet

                    -- * Operators
                    , (\\)

                    -- * Query
                    , null
                    , size
                    , member
                    , notMember
                    , isSubsetOf
                    , isProperSubsetOf

                    -- * Construction
                    , empty
                    , singleton
                    , insert
                    , delete

                    -- * Combine
                    , union
                    , unions
                    , difference
                    , intersection

                    -- * Filter
                    , filter
                    , partition

                    -- * Map
                    , map

                    -- * Fold
                    , fold

                    -- * Conversion
                    , elems
                    , toList
                    , fromList
                    ) where

import Prelude hiding (lookup,map,filter,null)

import Control.DeepSeq
import Data.Hashable
import Data.List (foldl')
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotentMonoid)
#endif
import Data.Typeable

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..), mkNoRepType)
#endif

import qualified Data.IntMap as I
import qualified Data.Set as S


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | Same as 'difference'.
(\\) :: Ord a => Set a -> Set a -> Set a
s1 \\ s2 = difference s1 s2


{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

data Some a = Only !a | More !(S.Set a) deriving (Eq, Ord)

instance NFData a => NFData (Some a) where
  rnf (Only a) = rnf a
  rnf (More s) = rnf s

-- | The abstract type of a @Set@. Its interface is a suitable
-- subset of 'Data.IntSet.IntSet'.
newtype Set a = Set (I.IntMap (Some a)) deriving (Eq, Ord)

-- | The @HashSet@ is a type synonym for @Set@ for backward compatibility.
-- It is deprecated and will be removed in furture releases.
{-# DEPRECATED HashSet "HashSet is deprecated. Please use Set instead." #-}
type HashSet a = Set a

instance NFData a => NFData (Set a) where
  rnf (Set s) = rnf s

instance Ord a => Monoid (Set a) where
  mempty  = empty
  mconcat = unions
#if !(MIN_VERSION_base(4,9,0))
  mappend = union
#else
  mappend = (<>)

instance Ord a => Semigroup (Set a) where
  (<>)   = union
  stimes = stimesIdempotentMonoid
#endif

instance Show a => Show (Set a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance (Hashable a, Ord a, Read a) => Read (Set a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

#include "hashmap.h"
INSTANCE_TYPEABLE1(Set,setTc,"Set")


#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Hashable a, Ord a, Data a) => Data (Set a) where
  gfoldl f z m = z fromList `f` (toList m)
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.HashSet.Set"
  dataCast1 f  = gcast1 f
#endif

{--------------------------------------------------------------------
  Comparing elements
--------------------------------------------------------------------}

-- For ByteStrings, doing compare is usually faster than doing (==),
-- according to benchmarks. A Set is using compare naturally. We therefore
-- define eq :: Ord a => a -> a -> Bool, which serves as (==).

{-# INLINE eq #-}
eq :: Ord a => a -> a -> Bool
eq x y = x `compare` y == EQ

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | Is the set empty?
null :: Set a -> Bool
null (Set s) = I.null s

-- | Number of elements in the set.
size :: Set a -> Int
size (Set s) = ifoldr ((+) . some_size) 0 s
  where some_size (Only _) = 1
        some_size (More t) = S.size t

-- | Is the element a member of the set?
member :: (Hashable a, Ord a) => a -> Set a -> Bool
member a (Set s) =
  case I.lookup (hash a) s of
    Nothing -> False
    Just (Only a') -> a `eq` a'
    Just (More s') -> S.member a s'

-- | Is the element not a member of the set?
notMember :: (Hashable a, Ord a) => a -> Set a -> Bool
notMember k s = not $ member k s

-- | Is this a subset?
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf (Set s1) (Set s2) =
  I.isSubmapOfBy (some_isSubsetOf) s1 s2
  where some_isSubsetOf (Only a) (Only b) = a `eq` b
        some_isSubsetOf (Only a) (More s) = a `S.member` s
        some_isSubsetOf (More _) (Only _) = False
        some_isSubsetOf (More s) (More t) = s `S.isSubsetOf` t

-- | Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = isSubsetOf s1 s2 && size s1 < size s2


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | The empty set.
empty :: Set a
empty = Set I.empty

-- | A set of one element.
singleton :: Hashable a => a -> Set a
singleton a = Set $
  I.singleton (hash a) $ Only a

-- | Add a value to the set. When the value is already an element of the set,
-- it is replaced by the new one, ie. 'insert' is left-biased.
insert :: (Hashable a, Ord a) => a -> Set a -> Set a
insert a (Set s) = Set $
  I.insertWith some_insert (hash a) (Only a) s
  where some_insert _ v@(Only b) | a `eq` b    = v
                                 | otherwise = More $ S.insert a (S.singleton b)
        some_insert _ (More t) = More $ S.insert a t


some_norm :: S.Set a -> Maybe (Some a)
some_norm s = case S.size s of 0 -> Nothing
                               1 -> Just $ Only $ S.findMin s
                               _ -> Just $ More $ s

some_norm' :: S.Set a -> Some a
some_norm' s = case S.size s of 1 -> Only $ S.findMin s
                                _ -> More $ s

-- | Delete a value in the set. Returns the original set when the value was not
-- present.
delete :: (Hashable a, Ord a) => a -> Set a -> Set a
delete a (Set s) = Set $
  I.update some_delete (hash a) s
  where some_delete v@(Only b) | a `eq` b  = Nothing
                               | otherwise = Just v
        some_delete (More t) = some_norm $ S.delete a t


{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}

-- | The union of two sets.
union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set $ I.unionWith some_union s1 s2
  where some_union v@(Only a) (Only b) | a `eq` b  = v
                                       | otherwise = More (S.singleton a `S.union` S.singleton b)
        some_union (Only a) (More s) = More $ S.singleton a `S.union` s
        some_union (More s) (Only a) = More $ s `S.union` S.singleton a
        some_union (More s) (More t) = More $ s `S.union` t

-- | The union of a list of sets.
unions :: Ord a => [Set a] -> Set a
unions xs = foldl' union empty xs

-- | Difference between two sets.
difference :: Ord a => Set a -> Set a -> Set a
difference (Set s1) (Set s2) = Set $
  I.differenceWith some_diff s1 s2
  where some_diff v@(Only a) (Only b) | a `eq` b  = Nothing
                                      | otherwise = Just v
        some_diff v@(Only a) (More s) | a `S.member` s = Nothing
                                      | otherwise      = Just v
        some_diff (More s) (Only a) = some_norm $ S.delete a s
        some_diff (More s) (More t) = some_norm $ s `S.difference` t

-- The I.intersectionWith does not have type general enough. We need the function
-- given to I.intersectionWith to have type a -> b -> Maybe c, so the elements could
-- be deleted from the IntMap. As it is only a -> b -> c, we allow empty sets to be
-- in the resulting intersection and delete them with a filter afterwards. This is
-- the function performing the deletions.
delete_empty :: I.IntMap (Some a) -> I.IntMap (Some a)
delete_empty = I.filter some_empty
  where some_empty (Only _) = True
        some_empty (More s) = not $ S.null s

-- | The intersection of two sets.
intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set s1) (Set s2) = Set $ delete_empty $
  I.intersectionWith some_intersection s1 s2
  where some_intersection v@(Only a) (Only b) | a `eq` b  = v
                                              | otherwise = More (S.empty)
        some_intersection v@(Only a) (More s) | a `S.member` s = v
                                              | otherwise      = More (S.empty)
        some_intersection (More s) (Only a) | a `S.member` s = Only (S.findMin $ s `S.intersection` (S.singleton a))
                                            | otherwise      = More (S.empty)
        some_intersection (More s) (More t) = some_norm' $ s `S.intersection` t


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | Filter all elements that satisfy some predicate.
filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter p (Set s) = Set $
  I.mapMaybe some_filter s
  where some_filter v@(Only a) | p a       = Just v
                               | otherwise = Nothing
        some_filter (More t) = some_norm (S.filter p t)

-- | Partition the set according to some predicate. The first set contains all
-- elements that satisfy the predicate, the second all elements that fail the
-- predicate.
partition :: Ord a => (a -> Bool) -> Set a -> (Set a, Set a)
partition p s = (filter p s, filter (not . p) s)


{--------------------------------------------------------------------
  Map
--------------------------------------------------------------------}
-- | @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if, for some
-- @(x,y)@, @x /= y && f x == f y@
map :: (Hashable b, Ord b) => (a -> b) -> Set a -> Set b
map f = fromList . fold ((:) . f) []


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | Fold over the elements of a set in an unspecified order.
fold :: (a -> b -> b) -> b -> Set a -> b
fold f z (Set s) = ifoldr some_fold z s
  where some_fold (Only a) x = f a x
        some_fold (More t) x = sfoldr f x t

ifoldr :: (a -> b -> b) -> b -> I.IntMap a -> b
sfoldr :: (a -> b -> b) -> b -> S.Set a -> b
#if MIN_VERSION_containers(0,5,0)
ifoldr = I.foldr
sfoldr = S.foldr
#else
ifoldr = I.fold
sfoldr = S.fold
#endif


{--------------------------------------------------------------------
  Conversions
--------------------------------------------------------------------}
-- | The elements of a set. (For sets, this is equivalent to toList).
elems :: Set a -> [a]
elems = toList

-- | Convert the set to a list of elements.
toList :: Set a -> [a]
toList (Set s) = ifoldr some_append [] s
  where some_append (Only a) acc = a : acc
        some_append (More t) acc = S.toList t ++ acc

-- | Create a set from a list of elements.
fromList :: (Hashable a, Ord a) => [a] -> Set a
fromList xs = foldl' (flip insert) empty xs
