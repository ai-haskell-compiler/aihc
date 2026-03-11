{-# LANGUAGE Haskell2010
    , DeriveDataTypeable
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-name-shadowing
 #-}

-- |
-- Module       : Data.MultiMap
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Portability  : non-portable (DeriveDataTypeable)
-- 
-- A very simple MultiMap, based on 'Data.Map.Map' from the containers package.
module Data.MultiMap (

    -- * MultiMap type
    MultiMap,

    -- * Query
    null,
    size,
    numKeys,
    numValues,

    member,
    notMember,
    lookup,

    -- * Operators
    (!),

    -- * Construction
    empty,
    
    -- ** Insertion
    insert,

    -- ** Delete
    delete,

    -- * Traversal
    map,
    mapKeys,
    mapWithKey,
    
    -- * Folds
    foldr,
    foldl,
    foldrWithKey,
    foldlWithKey,

    -- * Conversion
    elems,
    keys,
    keysSet,
    assocs,

    toMap,
    toMapOfSets,
    toList,
    fromList,
    fromMap,
    
    -- * Min/Max
    findMin,
    findMax,
    findMinWithValues,
    findMaxWithValues

  ) where

import Prelude hiding (lookup, map, null, foldr, foldl)
import qualified Prelude as P

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Word
import Data.Data


-- | A MultiMap with keys @k@ and values @v@.
--
-- A key can have multiple values (but not zero).
-- The same value can be added multiple times (thus no
-- constraints are ever imposed on @v@).
--
-- Internally this is simply a @Map k [v]@.
-- See 'toMap' for accessing the underlying 'Map'.
newtype MultiMap k v = MultiMap (Word32, Word32, Map k [v])
    deriving (Data, Typeable)


null :: MultiMap k a -> Bool
-- ^ /O(1)./ Check whether the multimap is empty or not.
null (MultiMap (_, _, m)) = Map.null m


size :: MultiMap k a -> Int
-- ^ /O(1)./ The number of elements in the multimap.
size (MultiMap (_, size, _)) = fromIntegral size


numKeys :: MultiMap k a -> Word32
-- ^ /O(1)./ The number of keys in the multimap.
-- 
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numKeys (MultiMap (nk, _, _)) = nk


numValues :: MultiMap k a -> Word32
-- ^ /O(1)./ The number of values in the multimap.
--
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numValues (MultiMap (_, nv, _)) = nv


notMember, member :: Ord k => MultiMap k a -> k -> Bool
-- | /O(log n)./ Is the key a member of the multimap?
member (MultiMap (_, _, map)) key = Map.member key map
-- | /O(log n)./ Is the key not a member of the multimap?
notMember key = not . member key


(!) :: Ord k => MultiMap k a -> k -> [a]
-- ^ As @flip lookup@
(!) = flip lookup


lookup :: Ord k => k -> MultiMap k a -> [a]
-- ^ /O(log n)./ Lookup the value at a key in the map.
--
-- The function will return the corrsponding values as a List, or the
-- empty list if no values are associated witht the given key.
lookup key (MultiMap (_, _, map)) = maybe [] id (Map.lookup key map)


empty :: MultiMap k a
-- ^ /O(1)./ The empty multimap.
empty = MultiMap (0, 0, Map.empty)


insert :: Ord k => k -> a -> MultiMap k a -> MultiMap k a
-- ^ /O(log n)./ Insert a new key and value in the map.
insert k v (MultiMap (nk, nv, map))
    | Map.member k map = MultiMap (nk, succ nv, Map.insert k (v : map Map.! k) map)
    | otherwise = MultiMap (succ nk, succ nv, Map.insert k [v] map)

delete :: Ord k => k -> MultiMap k a -> MultiMap k a
-- ^ /O(log n)./ Delete a key and all its values from the map.
delete k m@(MultiMap (nk, nv, map)) = case Map.lookup k map of
    Just v -> MultiMap (pred nk, nv - fromIntegral (length v), Map.delete k map)
    _      -> m


map :: (a -> b) -> MultiMap k a -> MultiMap k b
-- ^ Map a function over all values in the map.
map f (MultiMap (nk, nv, map)) = MultiMap (nk, nv, Map.map (P.map f) map)


mapKeys :: Ord k2 => (k1 -> k2) -> MultiMap k1 a -> MultiMap k2 a
-- ^ mapKeys f s is the multimap obtained by applying f to each key of s.
mapKeys f (MultiMap (nk, nv, map)) = MultiMap (nk, nv, Map.mapKeys f map)


mapWithKey :: (k -> a -> b) -> MultiMap k a -> MultiMap k b
-- ^ Map a function over all key/value pairs in the map.
mapWithKey f (MultiMap (nk, nv, map))
  = MultiMap (nk, nv, Map.mapWithKey (\k -> P.map (f k)) map)


foldr :: (a -> b -> b) -> b -> MultiMap k a -> b
-- ^ Fold the values in the map using the given right-associative binary operator.
foldr f e = P.foldr f e . concat . elems


foldl :: (a -> b -> a) -> a -> MultiMap k b -> a
-- ^  Fold the values in the map using the given left-associative binary operator.
foldl f e = P.foldl f e . concat . elems


foldrWithKey :: (k -> a -> b -> b) -> b -> MultiMap k a -> b
-- ^ /O(n)./ Fold the keys and values in the map using the given right-associative
-- binary operator, taking into account not only the value but also the key.
foldrWithKey f e = P.foldr (uncurry f) e . toList


foldlWithKey :: (a -> k -> b -> a) -> a -> MultiMap k b -> a
-- ^ /O(n)./ Fold the keys and values in the map using the given left-associative
-- binary operator, taking into account not only the value but also the key.
foldlWithKey f e = P.foldl (\a (k,v) -> f a k v) e . toList


elems :: MultiMap k a -> [[a]]
-- ^ /O(n)./ Return all elements of the multimap in the
-- ascending order of their keys.
--
-- A list of lists is returned since a key can have
-- multiple values. Use 'concat' to flatten.
elems (MultiMap (_, _, map)) = Map.elems map


keys :: MultiMap k a -> [k]
-- ^ /O(n)./ Return all keys of the multimap in ascending order.
keys (MultiMap (_, _, map)) = Map.keys map


keysSet :: MultiMap k a -> Set k
-- ^ /O(n)./ The set of all keys of the multimap.
keysSet (MultiMap (_, _, map)) = Map.keysSet map


assocs :: MultiMap k a -> [(k, [a])]
-- ^ /O(n)./ Return all key/value pairs in the multimap
-- in ascending key order.
assocs (MultiMap (_, _, map)) = Map.assocs map


toMap :: MultiMap k a -> Map k [a]
-- ^ /O(1)./ Return the map of lists.
toMap (MultiMap (_, _, theUnderlyingMap)) = theUnderlyingMap


toMapOfSets :: Ord a => MultiMap k a -> Map k (Set a)
-- ^ /O(k*m*log m) where k is the number of keys and m the
-- maximum number of elements associated with a single key/
toMapOfSets (MultiMap (_, _, map)) = Map.map Set.fromList map


toList :: MultiMap k a -> [(k, a)]
-- ^ Return a flattened list of key/value pairs.
toList (MultiMap (_, _, map))
  = concat $ Map.elems $ Map.mapWithKey (\k -> zip (repeat k)) map


fromList :: Ord k => [(k, a)] -> MultiMap k a
-- ^ /O(n*log n)/ Create a multimap from a list of key/value pairs.
--
-- > fromList xs == foldr (uncurry insert) empty
fromList = P.foldr (uncurry insert) empty


fromMap :: Map k [a] -> MultiMap k a
-- ^ Turns a map of lists into a multimap.
fromMap map = MultiMap (numKeys, numValues, map)
  where
    numKeys   = fromIntegral $ Map.size map
    numValues = fromIntegral $ Map.foldr (\v s -> length v + s) 0 map


findMin :: MultiMap k a -> Maybe k
-- ^ /O(log n)/ Find the minimal key of the multimap.
findMin (MultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ fst $ Map.findMin map


findMax :: MultiMap k a -> Maybe k
-- ^ /O(log n)/ Find the maximal key of the multimap.
findMax (MultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ fst $ Map.findMax map


findMinWithValues :: MultiMap k a -> Maybe (k, [a])
-- ^ /O(log n)/ Find the minimal key and the values associated with it.
findMinWithValues (MultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ Map.findMin map


findMaxWithValues :: MultiMap k a -> Maybe (k, [a])
-- ^ /O(log n)/ Find the maximal key and the values associated with it.
findMaxWithValues (MultiMap (_, _, map))
    | Map.null map = Nothing
    | otherwise    = Just $ Map.findMax map




