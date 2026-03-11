{-# LANGUAGE Haskell2010
    , DeriveDataTypeable
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-name-shadowing
 #-}

-- Module       : Data.SetMap
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Portability  : non-portable (DeriveDataTypeable)
--
-- A SetMap allows the association of multiple values with a single key,
-- but there are no duplicates per key.
module Data.SetMap (

    -- * SetMap type
    SetMap,

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

    -- ** Deletion
    delete,

    -- * Traversal
    map,

    -- * Conversion
    elems,
    keys,

    toMap,

  ) where


import Prelude hiding (lookup, map, null, foldr, foldl)
import qualified Prelude as P

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Word
import Data.Data


-- | A SetMap with keys @k@ and values @v@.
newtype SetMap k v = SetMap (Word32, Word32, Map k (Set v))
    deriving (Data, Typeable)


null :: SetMap k a -> Bool
-- ^ /O(1)./ Check whether the multimap is empty or not.
null (SetMap (_, _, m)) = Map.null m


size :: SetMap k a -> Int
-- ^ /O(1)./ The number of elements in the multimap.
size (SetMap (_, size, _)) = fromIntegral size


numKeys :: SetMap k a -> Word32
-- ^ /O(1)./ The number of keys in the multimap.
-- 
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numKeys (SetMap (nk, _, _)) = nk


numValues :: SetMap k a -> Word32
-- ^ /O(1)./ The number of values in the multimap.
--
-- As this is a multimap, the number of keys is not
-- necessarily equal to the number of values.
numValues (SetMap (_, nv, _)) = nv


notMember, member :: Ord k => SetMap k a -> k -> Bool
-- | /O(log n)./ Is the key a member of the multimap?
member (SetMap (_, _, map)) key = Map.member key map
-- | /O(log n)./ Is the key not a member of the multimap?
notMember key = not . member key


(!) :: Ord k => SetMap k a -> k -> Set a
-- ^ As @flip lookup@
(!) = flip lookup


lookup :: Ord k => k -> SetMap k a -> Set a
-- ^ /O(log n)./ Lookup the value at a key in the map.
--
-- The function will return the corrsponding values as a List, or the
-- empty list if no values are associated witht the given key.
lookup key (SetMap (_, _, map)) = maybe Set.empty id (Map.lookup key map)


empty :: SetMap k a
-- ^ /O(1)./ The empty multimap.
empty = SetMap (0, 0, Map.empty)


insert :: (Ord k, Ord a) => k -> a -> SetMap k a -> SetMap k a
-- ^ Insert a new key and value in the map.
insert k v (SetMap (nk, nv, map))
    | Map.member k map =
        let oldSet = map Map.! k
            (nv', newSet) = if v `Set.member` oldSet
                then (nv, oldSet) else (succ nv, v `Set.insert` oldSet)
        in  SetMap (nk, nv', Map.insert k newSet map)
    | otherwise = SetMap (succ nk, succ nv, Map.insert k (Set.singleton v) map)


delete :: Ord k => k -> SetMap k a -> SetMap k a
-- ^ Delete a key and all its values from the map.
delete k m@(SetMap (nk, nv, map)) = case Map.lookup k map of
    Just v -> SetMap (pred nk, nv - fromIntegral (Set.size v), Map.delete k map)
    _      -> m


map :: (Ord a, Ord b) => (a -> b) -> SetMap k a -> SetMap k b
-- ^ Map a function over all values in the map.
map f (SetMap (nk, nv, map)) = SetMap (nk, nv, Map.map (Set.map f) map)


elems :: SetMap k a -> [[a]]
-- ^ Return all elements of the multimap in the
-- ascending order of their keys.
--
-- A list of lists is returned since a key can have
-- multiple values. Use 'concat' to flatten.
elems (SetMap (_, _, map)) = P.map (Set.elems) $ Map.elems map


keys :: SetMap k a -> [k]
-- ^ /O(n)./ Return all keys of the multimap in ascending order.
keys (SetMap (_, _, map)) = Map.keys map


toMap :: SetMap k a -> Map k (Set a)
-- ^ /O(1)./ Return the map of sets.
toMap (SetMap (_, _, theUnderlyingMap)) = theUnderlyingMap


