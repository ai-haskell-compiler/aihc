{- |
This module is only needed for DeBruijn sequence generation.
-}
module Reactive.Banana.MIDI.Trie where

import qualified Data.List as List
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, )

import Prelude hiding (null, lookup)


data Trie a b = Leaf b | Branch [(a, Trie a b)]
   deriving (Show)

full :: b -> [a] -> Int -> Trie a b
full b _ 0 = Leaf b
full b as n =
   Branch $
   map (\a -> (a, full b as (n-1))) as

null :: Trie a [b] -> Bool
null (Branch []) = True
null (Leaf []) = True
null _ = False

delete :: (Eq a, Eq b) => b -> [a] -> Trie a [b] -> Trie a [b]
delete b [] (Leaf bs) = Leaf (List.delete b bs)
delete b (a:as) (Branch subTries) =
   Branch $ mapMaybe
      (\(key,trie) ->
         fmap ((,) key) $
            if key==a
              then let delTrie = delete b as trie
                   in  toMaybe (not (null delTrie)) delTrie
              else Just trie)
      subTries
delete _ _ _ = error "Trie.delete: key and trie depth mismatch"

lookup :: (Eq a) => [a] -> Trie a b -> Maybe b
lookup [] (Leaf b) = Just b
lookup (a:as) (Branch subTries) =
   List.lookup a subTries >>= lookup as
lookup _ _ = error "Trie.lookup: key and trie depth mismatch"
