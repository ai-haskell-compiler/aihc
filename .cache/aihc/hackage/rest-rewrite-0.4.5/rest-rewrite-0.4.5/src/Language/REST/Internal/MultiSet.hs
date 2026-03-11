{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.Internal.MultiSet
  ( MultiSet
  , delete
  , deleteMany
  , distinctElems
  , empty
  , filter
  , insert
  , member
  , null
  , toList
  , toOccurList
  , singleton
  , fromList
  , toSet
  ) where

import Prelude hiding (null, filter)

import GHC.Generics
import Data.Hashable
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

newtype MultiSet a = MultiSet (M.HashMap a Int) deriving (Eq, Generic, Hashable, Ord)

instance Show a => Show (MultiSet a) where
  show ms = "{" ++ L.intercalate ", " (map show $ toList ms) ++ "}"

-- | @delete k m@ removes a single instance of @k@ from the multiset @m.
--   If @k is not in the multiset, the original multiset is returned
delete :: (Hashable a, Eq a) => a -> MultiSet a -> MultiSet a
delete k = deleteMany k 1

-- | @delete k n m@ removes @n@ instances of @k@ from the multiset @m@.
--   If there are less than @n@ instances of @k@ in the multiset, all
--   instances are removed.
deleteMany :: (Hashable a, Eq a) => a -> Int -> MultiSet a -> MultiSet a
deleteMany k v (MultiSet ms) | Just c <- M.lookup k ms
                             , c > v = MultiSet $ M.insert k (c - v) ms
deleteMany k _ (MultiSet ms)  = MultiSet $ M.delete k ms

distinctElems :: MultiSet a -> [a]
distinctElems (MultiSet ms) = M.keys ms

empty :: MultiSet a
empty = MultiSet M.empty

toOccurList :: MultiSet a -> [(a, Int)]
toOccurList (MultiSet ms) = M.toList ms

filter :: (a -> Bool) -> MultiSet a -> MultiSet a
filter f (MultiSet ms) = MultiSet $ M.filterWithKey f' ms
  where
    f' k _ = f k

null :: MultiSet a -> Bool
null (MultiSet ms) = M.null ms

-- | @member k m@ returns @true@ iff there is at least one instance of @k@
--   in @m@
member :: (Eq a, Hashable a) => a -> MultiSet a -> Bool
member k (MultiSet ms) = M.member k ms

toList :: MultiSet a -> [a]
toList ms = concatMap go (toOccurList ms)
  where
    go (k, num) = replicate num k

insert :: (Eq a, Hashable a) => a -> MultiSet a -> MultiSet a
insert k (MultiSet ms) | Just c <- M.lookup k ms
                       = MultiSet $ M.insert k (c + 1) ms
insert k (MultiSet ms)
                       = MultiSet $ M.insert k 1 ms

singleton :: (Eq a, Hashable a) => a -> MultiSet a
singleton k = MultiSet (M.singleton k 1)

fromList  :: (Eq a, Hashable a) => [a] -> MultiSet a
fromList = foldl (flip insert) empty

toSet :: MultiSet a -> S.HashSet a
toSet (MultiSet ms) = M.keysSet ms
