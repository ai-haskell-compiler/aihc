module Test.MockCat.AssociationList 
 (AssociationList, empty, insert, lookup, member, (!?), update) where

import Prelude hiding (lookup)
import Data.Maybe (isJust)

type AssociationList k a = [(k, a)]

empty :: AssociationList k a
empty = []

insert :: Eq k => k -> a -> AssociationList k a -> AssociationList k a
insert key value [] = [(key, value)]
insert key value ((k, v) : xs)
  | key == k  = (key, value) : xs
  | otherwise = (k, v) : insert key value xs

lookup :: Eq k => k -> AssociationList k a -> Maybe a
lookup _ [] = Nothing
lookup key ((k, a) : xs)
  | key == k  = Just a
  | otherwise = lookup key xs

member :: Eq k => k -> AssociationList k a -> Bool
member k list = isJust (lookup k list)

(!?) :: Eq k => AssociationList k a -> k -> Maybe a
(!?) = flip lookup

update :: Eq k => (a -> a) -> k -> AssociationList k a -> AssociationList k a
update f key list =
  case list !? key of
    Just value -> insert key (f value) list
    Nothing    -> list
