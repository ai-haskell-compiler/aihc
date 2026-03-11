-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap
-- Copyright   :  (c) Milan Straka 2011
-- License     :  BSD-style
-- Maintainer  :  fox@ucw.cz
-- Stability   :  provisional
-- Portability :  portable
--
-- Persistent 'Map' based on hashing, which is defined as
--
-- @
--   data 'Map' k v = 'Data.IntMap.IntMap' (Some k v)
-- @
--
-- is an 'Data.IntMap.IntMap' indexed by hash values of keys,
-- containing a value of @Some e@. That contains either one
-- @('k', 'v')@ pair or a @'Data.Map.Map' k v@ with keys of the same hash values.
--
-- The interface of a 'Map' is a suitable subset of 'Data.IntMap.IntMap'
-- and can be used as a drop-in replacement of 'Data.Map.Map'.
--
-- The complexity of operations is determined by the complexities of
-- 'Data.IntMap.IntMap' and 'Data.Map.Map' operations. See the sources of
-- 'Map' to see which operations from @containers@ package are used.
-----------------------------------------------------------------------------

module Data.HashMap ( Map
                    , HashMap

                    -- * Operators
                    , (!), (\\)

                    -- * Query
                    , null
                    , size
                    , member
                    , notMember
                    , lookup
                    , findWithDefault

                    -- * Construction
                    , empty
                    , singleton

                    -- ** Insertion
                    , insert
                    , insertWith, insertWithKey, insertLookupWithKey

                    -- ** Delete\/Update
                    , delete
                    , adjust
                    , adjustWithKey
                    , update
                    , updateWithKey
                    , updateLookupWithKey
                    , alter

                    -- * Combine

                    -- ** Union
                    , union
                    , unionWith
                    , unionWithKey
                    , unions
                    , unionsWith

                    -- ** Difference
                    , difference
                    , differenceWith
                    , differenceWithKey

                    -- ** Intersection
                    , intersection
                    , intersectionWith
                    , intersectionWithKey

                    -- * Traversal
                    -- ** Map
                    , map
                    , mapWithKey
                    , mapAccum
                    , mapAccumWithKey

                    -- ** Fold
                    , fold
                    , foldWithKey

                    -- * Conversion
                    , elems
                    , keys
                    , keysSet
                    , assocs

                    -- ** Lists
                    , toList
                    , fromList
                    , fromListWith
                    , fromListWithKey

                    -- * Filter
                    , filter
                    , filterWithKey
                    , partition
                    , partitionWithKey

                    , mapMaybe
                    , mapMaybeWithKey
                    , mapEither
                    , mapEitherWithKey

                    -- * Submap
                    , isSubmapOf, isSubmapOfBy
                    , isProperSubmapOf, isProperSubmapOfBy
                    ) where

import Prelude hiding (lookup,map,filter,null)

import Control.Applicative (Applicative(pure,(<*>)))
import Control.DeepSeq
import Data.Hashable
import Data.Foldable (Foldable(foldMap))
import Data.List (foldl')
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotentMonoid)
#endif
import Data.Traversable (Traversable(traverse))
import Data.Typeable

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..), mkNoRepType)
#endif

import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: (Hashable k, Ord k) => Map k a -> k -> a
m ! k = case lookup k m of
          Nothing -> error "HashMap.(!): key not an element of the map"
          Just v -> v

-- | Same as 'difference'.
(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2


{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

data Some k v = Only !k v | More !(M.Map k v) deriving (Eq, Ord)

instance (NFData k, NFData v) => NFData (Some k v) where
  rnf (Only k v) = rnf k `seq` rnf v
  rnf (More m) = rnf m

-- | The abstract type of a @Map@. Its interface is a suitable
-- subset of 'Data.IntMap.IntMap'.
newtype Map k v = Map (I.IntMap (Some k v)) deriving (Eq, Ord)

-- | The @HashMap@ is a type synonym for @Map@ for backward compatibility.
-- It is deprecated and will be removed in furture releases.
{-# DEPRECATED HashMap "HashMap is deprecated. Please use Map instead." #-}
type HashMap k v = Map k v

instance (NFData k, NFData v) => NFData (Map k v) where
  rnf (Map m) = rnf m

instance Functor (Map k) where
  fmap = map

instance Ord k => Monoid (Map k a) where
  mempty  = empty
  mconcat = unions
#if !(MIN_VERSION_base(4,9,0))
  mappend = union
#else
  mappend = (<>)

instance Ord k => Semigroup (Map k a) where
  (<>)   = union
  stimes = stimesIdempotentMonoid
#endif

instance Foldable (Map k) where
  foldMap f (Map m) = foldMap some_fold m
    where some_fold (Only _ x) = f x
          some_fold (More s)   = foldMap f s

instance Traversable (Map k) where
  traverse f (Map m) = pure Map <*> traverse some_traverse m
    where some_traverse (Only k x) = pure (Only k) <*> f x
          some_traverse (More s) = pure More <*> traverse f s

instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance (Read k, Hashable k, Ord k, Read a) => Read (Map k a) where
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
INSTANCE_TYPEABLE2(Map,mapTc,"Map")



#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Data k, Hashable k, Ord k, Data a) => Data (Map k a) where
  gfoldl f z m = z fromList `f` (toList m)
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.HashMap.Map"
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
-- | Is the map empty?
null :: Map k a -> Bool
null (Map m) = I.null m

-- | Number of elements in the map.
size :: Map k a -> Int
size (Map m) = ifoldr ((+) . some_size) 0 m
  where some_size (Only _ _) = 1
        some_size (More s) = M.size s

-- | Is the key a member of the map?
member :: (Hashable k, Ord k) => k -> Map k a -> Bool
member k m = case lookup k m of
               Nothing -> False
               Just _  -> True

-- | Is the key not a member of the map?
notMember :: (Hashable k, Ord k) => k -> Map k a -> Bool
notMember k m = not $ member k m

some_lookup :: Ord k => k -> Some k a -> Maybe a
some_lookup k (Only k' x) | k `eq` k' = Just x
                          | otherwise = Nothing
some_lookup k (More s) = M.lookup k s

-- | Lookup the value at a key in the map.
lookup :: (Hashable k, Ord k) => k -> Map k a -> Maybe a
lookup k (Map m) = I.lookup (hash k) m >>= some_lookup k

-- | The expression @('findWithDefault' def k map)@ returns the value at key
-- @k@ or returns @def@ when the key is not an element of the map.
findWithDefault :: (Hashable k, Ord k) => a -> k -> Map k a -> a
findWithDefault def k m = case lookup k m of
                            Nothing -> def
                            Just x  -> x


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | The empty map.
empty :: Map k a
empty = Map I.empty

-- | A map of one element.
singleton :: Hashable k => k -> a -> Map k a
singleton k x = Map $
  I.singleton (hash k) $ (Only k x)


{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | Insert a new key\/value pair in the map.  If the key is already present in
-- the map, the associated value is replaced with the supplied value, i.e.
-- 'insert' is equivalent to @'insertWith' 'const'@.
insert :: (Hashable k, Ord k)
       => k -> a -> Map k a -> Map k a
insert k x (Map m) = Map $
  I.insertWith some_insert (hash k) (Only k x) m
  where some_insert _ (Only k' x') | k `eq` k' = Only k x
                                   | otherwise = More $ M.insert k x (M.singleton k' x')
        some_insert _ (More s) = More $ M.insert k x s

-- | Insert with a combining function.  @'insertWith' f key value mp@ will
-- insert the pair (key, value) into @mp@ if key does not exist in the map. If
-- the key does exist, the function will insert @f new_value old_value@.
insertWith :: (Hashable k, Ord k)
           => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k x (Map m) = Map $
  I.insertWith some_insert_with (hash k) (Only k x) m
  where some_insert_with _ (Only k' x') | k `eq` k' = Only k (f x x')
                                        | otherwise = More $ M.insert k x (M.singleton k' x')
        some_insert_with _ (More s) = More $ M.insertWith f k x s

-- | Insert with a combining function.  @'insertWithKey' f key value mp@ will
-- insert the pair (key, value) into @mp@ if key does not exist in the map. If
-- the key does exist, the function will insert @f key new_value old_value@.
insertWithKey :: (Hashable k, Ord k)
              => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k x (Map m) = Map $
  I.insertWith some_insert_with_key (hash k) (Only k x) m
  where some_insert_with_key _ (Only k' x') | k `eq` k' = Only k (f k x x')
                                            | otherwise = More $ M.insert k x (M.singleton k' x')
        some_insert_with_key _ (More s) = More $ M.insertWithKey f k x s

-- | The expression (@'insertLookupWithKey' f k x map@) is a pair where the
-- first element is equal to (@'lookup' k map@) and the second element equal to
-- (@'insertWithKey' f k x map@).
insertLookupWithKey :: (Hashable k, Ord k)
                    => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k x (Map m) =
  case I.insertLookupWithKey some_insert_with_key (hash k) (Only k x) m of
    (found, m') -> (found >>= some_lookup k, Map m')
  where some_insert_with_key _ _ (Only k' x') | k `eq` k' = Only k (f k x x')
                                              | otherwise = More $ M.insert k x (M.singleton k' x')
        some_insert_with_key _ _ (More s) = More $ M.insertWithKey f k x s


{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}

some_norm :: M.Map k v -> Maybe (Some k v)
some_norm s = case M.size s of 0 -> Nothing
                               1 -> case M.findMin s of (k, x) -> Just $ Only k x
                               _ -> Just $ More $ s

some_norm' :: M.Map k v -> Some k v
some_norm' s = case M.size s of 1 -> case M.findMin s of (k, x) -> Only k x
                                _ -> More $ s

-- | Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: (Hashable k, Ord k)
       => k -> Map k a -> Map k a
delete k (Map m) = Map $
  I.update some_delete (hash k) m
  where some_delete v@(Only k' _) | k `eq` k'  = Nothing
                                  | otherwise  = Just v
        some_delete (More t) = some_norm $ M.delete k t

-- | Adjust a value at a specific key. When the key is not a member of the map,
-- the original map is returned.
adjust :: (Hashable k, Ord k)
       => (a -> a) -> k -> Map k a -> Map k a
adjust f k (Map m) = Map $
  I.adjust some_adjust (hash k) m
  where some_adjust v@(Only k' x) | k `eq` k'  = Only k (f x)
                                  | otherwise  = v
        some_adjust (More t) = More $ M.adjust f k t

-- | Adjust a value at a specific key. When the key is not a member of the map,
-- the original map is returned.
adjustWithKey :: (Hashable k, Ord k)
              => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k (Map m) = Map $
  I.adjust some_adjust_with_key (hash k) m
  where some_adjust_with_key v@(Only k' x) | k `eq` k'  = Only k (f k x)
                                           | otherwise  = v
        some_adjust_with_key (More t) = More $ M.adjustWithKey f k t

-- | The expression (@'update' f k map@) updates the value @x@ at @k@ (if it is
-- in the map). If (@f x@) is 'Nothing', the element is deleted. If it is
-- (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: (Hashable k, Ord k)
       => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k (Map m) = Map $
  I.update some_update (hash k) m
  where some_update v@(Only k' x) | k `eq` k' = f x >>= return . Only k'
                                  | otherwise = Just v
        some_update (More t) = some_norm $ M.update f k t

-- | The expression (@'update' f k map@) updates the value @x@ at @k@ (if it is
-- in the map). If (@f k x@) is 'Nothing', the element is deleted. If it is
-- (@'Just' y@), the key @k@ is bound to the new value @y@.
updateWithKey :: (Hashable k, Ord k)
              => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k (Map m) = Map $
  I.update some_update_with_key (hash k) m
  where some_update_with_key v@(Only k' x) | k `eq` k' = f k x >>= return . Only k'
                                           | otherwise = Just v
        some_update_with_key (More t) = some_norm $ M.updateWithKey f k t

-- | Lookup and update.  The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.  Returns the
-- original key value if the map entry is deleted.
updateLookupWithKey :: (Hashable k, Ord k)
                    => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k (Map m) =
  case I.updateLookupWithKey some_update_with_key (hash k) m of
    (found, m') -> (found >>= some_lookup k, Map m')
  where some_update_with_key _ v@(Only k' x) | k `eq` k' = f k x >>= return . Only k'
                                             | otherwise = Just v
        some_update_with_key _ (More t) = some_norm $ M.updateWithKey f k t

-- | The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence
-- thereof.  'alter' can be used to insert, delete, or update a value in an
-- 'Map'.
alter :: (Hashable k, Ord k)
      => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k (Map m) = Map $
  I.alter some_alter (hash k) m
  where some_alter Nothing = f Nothing >>= return . Only k
        some_alter (Just v@(Only k' x)) | k `eq` k' = f (Just x) >>= return . Only k'
                                        | otherwise = Just v
        some_alter (Just (More t)) = some_norm $ M.alter f k t


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
unions :: Ord k => [Map k a] -> Map k a
unions xs = foldl' union empty xs

-- | The union of a list of maps, with a combining operation.
unionsWith :: Ord k => (a->a->a) -> [Map k a] -> Map k a
unionsWith f xs = foldl' (unionWith f) empty xs

-- | The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: Ord k => Map k a -> Map k a -> Map k a
union (Map m1) (Map m2) = Map $
  I.unionWith some_union m1 m2
  where some_union v@(Only k x) (Only l y) | k `eq` l  = v
                                           | otherwise = More (M.singleton k x `M.union` M.singleton l y)
        some_union (Only k x) (More t) = More $ M.singleton k x `M.union` t
        some_union (More t) (Only k x) = More $ t `M.union` M.singleton k x
        some_union (More t) (More u) = More $ t `M.union` u

some_union_with_key :: Ord k => (k -> a -> a -> a) -> Some k a -> Some k a -> Some k a
some_union_with_key f (Only k x) (Only l y) | k `eq` l  = Only k (f k x y)
                                            | otherwise = More (M.unionWithKey f (M.singleton k x) (M.singleton l y))
some_union_with_key f (Only k x) (More t) = More $ M.unionWithKey f (M.singleton k x) t
some_union_with_key f (More t) (Only k x) = More $ M.unionWithKey f t (M.singleton k x)
some_union_with_key f (More t) (More u) = More $ M.unionWithKey f t u

-- | The union with a combining function.
unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f (Map m1) (Map m2) = Map $
  I.unionWith (some_union_with_key $ const f) m1 m2

-- | The union with a combining function.
unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f (Map m1) (Map m2) = Map $
  I.unionWith (some_union_with_key f) m1 m2

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | Difference between two maps (based on keys).
difference :: Ord k => Map k a -> Map k b -> Map k a
difference (Map m1) (Map m2) = Map $
  I.differenceWith some_diff m1 m2
  where some_diff v@(Only k _) (Only l _) | k `eq` l  = Nothing
                                          | otherwise = Just v
        some_diff v@(Only k _) (More t) | k `M.member` t = Nothing
                                        | otherwise      = Just v
        some_diff (More t) (Only k _) = some_norm $ M.delete k t
        some_diff (More t) (More u) = some_norm $ t `M.difference` u

some_diff_with_key :: Ord k => (k -> a -> b -> Maybe a) -> Some k a -> Some k b -> Maybe (Some k a)
some_diff_with_key f v@(Only k x) (Only l y) | k `eq` l  = f k x y >>= return . Only k
                                             | otherwise = Just v
some_diff_with_key f (Only k x) (More t) = some_norm $ M.differenceWithKey f (M.singleton k x) t
some_diff_with_key f (More t) (Only k x) = some_norm $ M.differenceWithKey f t (M.singleton k x)
some_diff_with_key f (More t) (More u) = some_norm $ M.differenceWithKey f t u

-- | Difference with a combining function.
differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f (Map m1) (Map m2) = Map $
  I.differenceWith (some_diff_with_key $ const f) m1 m2

-- | Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f (Map m1) (Map m2) = Map $
  I.differenceWith (some_diff_with_key f) m1 m2


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
delete_empty :: I.IntMap (Some k a) -> I.IntMap (Some k a)
delete_empty = I.filter some_empty
  where some_empty (Only _ _) = True
        some_empty (More t) = not $ M.null t

-- | The (left-biased) intersection of two maps (based on keys).
intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection (Map m1) (Map m2) = Map $ delete_empty $
  I.intersectionWith some_intersection m1 m2
  where some_intersection v@(Only k _) (Only l _) | k `eq` l  = v
                                                  | otherwise = More (M.empty)
        some_intersection v@(Only k _) (More t) | k `M.member` t = v
                                                | otherwise      = More (M.empty)
        some_intersection (More t) (Only k x) = some_norm' $ M.intersection t (M.singleton k x)
        some_intersection (More t) (More u) = some_norm' $ M.intersection t u

some_intersection_with_key :: Ord k => (k -> a -> b -> c) -> Some k a -> Some k b -> Some k c
some_intersection_with_key f (Only k x) (Only l y) | k `eq` l  = Only k (f k x y)
                                                   | otherwise = More (M.empty)
some_intersection_with_key f (Only k x) (More t) = some_norm' $ M.intersectionWithKey f (M.singleton k x) t
some_intersection_with_key f (More t) (Only k x) = some_norm' $ M.intersectionWithKey f t (M.singleton k x)
some_intersection_with_key f (More t) (More u) = some_norm' $ M.intersectionWithKey f t u

-- | The intersection with a combining function.
intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f (Map m1) (Map m2) = Map $ delete_empty $
  I.intersectionWith (some_intersection_with_key $ const f) m1 m2

-- | The intersection with a combining function.
intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f (Map m1) (Map m2) = Map $ delete_empty $
  I.intersectionWith (some_intersection_with_key f) m1 m2


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | Is this a proper submap? (ie. a submap but not equal).
isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2 = isSubmapOf m1 m2 && size m1 < size m2

-- | Is this a proper submap? (ie. a submap but not equal).  The expression
-- (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when @m1@ and @m2@ are not
-- equal, all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
-- applied to their respective values.
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f m1 m2 = isSubmapOfBy f m1 m2 && size m1 < size m2

-- | Is this a submap?
isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf (Map m1) (Map m2) =
  I.isSubmapOfBy some_isSubmapOf m1 m2
  where some_isSubmapOf (Only k _) (Only l _) = k `eq` l
        some_isSubmapOf (Only k _) (More t)   = k `M.member` t
        some_isSubmapOf (More _) (Only _ _)   = False
        some_isSubmapOf (More t) (More u)     = t `M.isSubmapOf` u

-- | The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if all keys in
-- @m1@ are in @m2@, and when @f@ returns 'True' when applied to their
-- respective values.
isSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f (Map m1) (Map m2) =
  I.isSubmapOfBy some_isSubmapOfBy m1 m2
  where some_isSubmapOfBy (Only k x) (Only l y) = k `eq` l && x `f` y
        some_isSubmapOfBy (Only k x) (More t) = case M.lookup k t of
                                                  Just y -> f x y
                                                  _      -> False
        some_isSubmapOfBy (More _) (Only _ _) = False
        some_isSubmapOfBy (More t) (More u)   = M.isSubmapOfBy f t u


{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | Map a function over all values in the map.
map :: (a -> b) -> Map k a -> Map k b
map f (Map m) = Map $
  I.map some_map m
  where some_map (Only k x) = Only k $ f x
        some_map (More t)   = More $ M.map f t

-- | Map a function over all values in the map.
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f (Map m) = Map $
  I.map some_map_with_key m
  where some_map_with_key (Only k x) = Only k $ f k x
        some_map_with_key (More t)   = More $ M.mapWithKey f t

-- | The function @'mapAccum'@ threads an accumulating argument through the map
-- in unspecified order of keys.
mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a (Map m) =
  case I.mapAccum some_map_accum a m of
    (acc, m') -> (acc, Map m')
  where some_map_accum acc (Only k x) = case f acc x of (acc', x') -> (acc', Only k x')
        some_map_accum acc (More t)   = case M.mapAccum f acc t of (acc', t') -> (acc', More t')

-- | The function @'mapAccumWithKey'@ threads an accumulating argument through
-- the map in unspecified order of keys.
mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a (Map m) =
  case I.mapAccum some_map_accum_with_key a m of
    (acc, m') -> (acc, Map m')
  where some_map_accum_with_key acc (Only k x) = case f acc k x of (acc', x') -> (acc', Only k x')
        some_map_accum_with_key acc (More t)   = case M.mapAccumWithKey f acc t of (acc', t') -> (acc', More t')


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | Filter all values that satisfy some predicate.
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter p (Map m) = Map $
  I.mapMaybe some_filter m
  where some_filter v@(Only _ x) | p x       = Just v
                                 | otherwise = Nothing
        some_filter (More t) = some_norm $ M.filter p t

-- | Filter all keys\/values that satisfy some predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p (Map m) = Map $
  I.mapMaybe some_filter_with_key m
  where some_filter_with_key v@(Only k x) | p k x     = Just v
                                          | otherwise = Nothing
        some_filter_with_key (More t) = some_norm $ M.filterWithKey p t

-- | Partition the map according to some predicate. The first map contains all
-- elements that satisfy the predicate, the second all elements that fail the
-- predicate.
partition :: Ord k => (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p m = (filter p m, filter (not . p) m)

-- | Partition the map according to some predicate. The first map contains all
-- elements that satisfy the predicate, the second all elements that fail the
-- predicate.
partitionWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey p m = (filterWithKey p m, filterWithKey (\k -> not . p k) m)

-- | Map values and collect the 'Just' results.
mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f (Map m) = Map $
  I.mapMaybe some_map_maybe m
  where some_map_maybe (Only k x) = f x >>= return . Only k
        some_map_maybe (More t) = some_norm $ M.mapMaybe f t

-- | Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f (Map m) = Map $
  I.mapMaybe some_map_maybe_with_key m
  where some_map_maybe_with_key (Only k x) = f k x >>= return . Only k
        some_map_maybe_with_key (More t) = some_norm $ M.mapMaybeWithKey f t

-- | Map values and separate the 'Left' and 'Right' results.
mapEither :: Ord k => (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m = (mapMaybe (maybe_left . f) m, mapMaybe (maybe_right . f) m)

-- | Map keys\/values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f m = (mapMaybeWithKey (\k a -> maybe_left  (f k a)) m
                       ,mapMaybeWithKey (\k a -> maybe_right (f k a)) m)

-- Helper functions for this section
maybe_left :: Either a b -> Maybe a
maybe_left (Left a) = Just a
maybe_left (Right _) = Nothing

maybe_right :: Either a b -> Maybe b
maybe_right (Right b) = Just b
maybe_right (Left _) = Nothing


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | Fold the values in the map, such that @'fold' f z == 'Prelude.foldr'
-- f z . 'elems'@.
fold :: (a -> b -> b) -> b -> Map k a -> b
fold f z (Map m) = ifoldr some_fold z m
  where some_fold (Only _ x) y = f x y
        some_fold (More t) y   = mfoldr f y t

-- | Fold the keys and values in the map, such that @'foldWithKey' f z ==
-- 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey f z (Map m) = ifoldr some_fold_with_key z m
  where some_fold_with_key (Only k x) y = f k x y
        some_fold_with_key (More t) y   = M.foldrWithKey f y t

mfoldr :: (a -> b -> b) -> b -> M.Map k a -> b
ifoldr :: (a -> b -> b) -> b -> I.IntMap a -> b
#if MIN_VERSION_containers(0,5,0)
mfoldr = M.foldr
ifoldr = I.foldr
#else
mfoldr = M.fold
ifoldr = I.fold
#endif

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | Return all elements of the map in arbitrary order of their keys.
elems :: Map k a -> [a]
elems (Map m) = ifoldr some_append_elems [] m
  where some_append_elems (Only _ x) acc = x : acc
        some_append_elems (More t) acc   = M.elems t ++ acc

-- | Return all keys of the map in arbitrary order.
keys  :: Map k a -> [k]
keys (Map m) = ifoldr some_append_keys [] m
  where some_append_keys (Only k _) acc = k : acc
        some_append_keys (More t) acc   = M.keys t ++ acc

-- | The set of all keys of the map.
keysSet :: Ord k => Map k a -> S.Set k
keysSet (Map m) = ifoldr (S.union . some_keys_set) S.empty m
  where some_keys_set (Only k _) = S.singleton k
        some_keys_set (More t)   = M.keysSet t

-- | Return all key\/value pairs in the map in arbitrary key order.
assocs :: Map k a -> [(k,a)]
assocs = toList


{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | Convert the map to a list of key\/value pairs.
toList :: Map k a -> [(k,a)]
toList (Map m) =
  ifoldr some_append [] m
  where some_append (Only k x) acc = (k, x) : acc
        some_append (More t) acc   = M.toList t ++ acc

-- | Create a map from a list of key\/value pairs.
fromList :: (Hashable k, Ord k)
         => [(k,a)] -> Map k a
fromList xs = foldl' (\m (k, x) -> insert k x m) empty xs

-- | Create a map from a list of key\/value pairs with a combining function.
fromListWith :: (Hashable k, Ord k) => (a -> a -> a) -> [(k,a)] -> Map k a
fromListWith f xs = foldl' (\m (k, x) -> insertWith f k x m) empty xs

-- | Build a map from a list of key\/value pairs with a combining function.
fromListWithKey :: (Hashable k, Ord k) => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromListWithKey f xs = foldl' (\m (k, x) -> insertWithKey f k x m) empty xs
