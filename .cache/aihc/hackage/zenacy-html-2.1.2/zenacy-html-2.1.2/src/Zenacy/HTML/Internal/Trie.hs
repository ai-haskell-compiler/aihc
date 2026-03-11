-- | A basic trie over byte strings.
module Zenacy.HTML.Internal.Trie
  ( Trie
  , empty
  , fromList
  , insert
  , insertWords
  , match
  ) where

import Zenacy.HTML.Internal.BS
import Data.Map
  ( Map
  )
import qualified Data.Map as Map
  ( empty
  , fromList
  , insert
  , lookup
  )
import Data.Word
  ( Word8
  )

-- | Defines the tree.
data Trie a = Trie (Maybe a) (Map Word8 (Trie a)) deriving (Show)

-- | Creates an empty trie.
empty :: Trie a
empty = Trie Nothing Map.empty

-- | Creates a trie from a list of tuples containing key and value.
fromList :: [(BS,a)] -> Trie a
fromList = foldl (\t (x,y) -> insert x y t) empty

-- | Finds the longest prefix with a value in the trie and returns
-- the prefix, the value, and the remaining string.
match :: Trie a -> BS -> Maybe (BS, a, BS)
match t s =
  case go Nothing 0 t s of
    Nothing -> Nothing
    Just (n, v) -> Just (bsTake n s, v, bsDrop n s)
  where
    go a n (Trie v m) s =
      case bsUncons s of
        Nothing -> a
        Just (w, t) ->
          case Map.lookup w m of
            Nothing -> a
            Just b@(Trie (Just v2) _) ->
              go (Just (n + 1, v2)) (n + 1) b t
            Just b ->
              go a (n + 1) b t

-- | Inserts a value into a trie.
insert :: BS -> a -> Trie a -> Trie a
insert x = insertWords $ bsUnpack x

-- | Inserts a value into a trie.
insertWords :: [Word8] -> a -> Trie a -> Trie a
insertWords x y = go x
  where
    go [] (Trie v m) =
      Trie (Just y) m
    go (w:ws) (Trie v m) =
      case Map.lookup w m of
        Nothing ->
          Trie v $ Map.insert w (go ws empty) m
        Just b ->
          Trie v $ Map.insert w (go ws b) m
