-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   This module defines a simple key\/value list ordered by keys
--   which both faster and more suitable for bencode dictionaries than
--   just [(k,v)].
--

{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.BEncode.BDict
       ( BKey
       , BDictMap (..)

         -- * Construction
       , Data.BEncode.BDict.empty
       , Data.BEncode.BDict.singleton

         -- * Query
       , Data.BEncode.BDict.null
       , Data.BEncode.BDict.member
       , Data.BEncode.BDict.lookup

         -- * Combine
       , Data.BEncode.BDict.union

         -- * Maps
       , Data.BEncode.BDict.map
       , Data.BEncode.BDict.mapWithKey

         -- * Folds
       , Data.BEncode.BDict.foldMapWithKey
       , Data.BEncode.BDict.bifoldMap

         -- * Conversion
       , Data.BEncode.BDict.fromAscList
       , Data.BEncode.BDict.toAscList
       ) where

import Control.DeepSeq
import Data.ByteString as BS
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid (Monoid (mappend, mempty))
#endif
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup ((<>)))
#endif
import GHC.Generics (Generic)

type BKey = ByteString

-- STRICTNESS NOTE: the BKey is always evaluated since we either use a
-- literal or compare before insert to the dict
--
-- LAYOUT NOTE: we don't use [StrictPair BKey a] since it introduce
-- one more constructor per cell
--

-- | BDictMap is an ascending list of key\/value pairs sorted by keys.
data BDictMap a
  = Cons !BKey a !(BDictMap a)
  | Nil
    deriving (Show, Read, Eq, Ord, Generic)

instance NFData a => NFData (BDictMap a) where
  rnf  Nil         = ()
  rnf (Cons _ v xs)= rnf v `seq` rnf xs

instance Functor BDictMap where
  fmap = Data.BEncode.BDict.map
  {-# INLINE fmap #-}

instance Foldable BDictMap where
  foldMap f = go
    where
      go  Nil          = mempty
      go (Cons _ v xs) = f v `mappend` go xs
  {-# INLINE foldMap #-}

instance Semigroup (BDictMap a) where
  (<>) = Data.BEncode.BDict.union

instance Monoid (BDictMap a) where
  mempty  = Data.BEncode.BDict.empty
  mappend = (<>)

-- | /O(1)/. The empty dicionary.
empty :: BDictMap a
empty = Nil
{-# INLINE empty #-}

-- | /O(1)/. Dictionary of one key-value pair.
singleton :: BKey -> a -> BDictMap a
singleton k v = Cons k v Nil
{-# INLINE singleton #-}

-- | /O(1)/. Is the dictionary empty?
null :: BDictMap a -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | /O(n)/. Is the key a member of the dictionary?
member :: BKey -> BDictMap a -> Bool
member key = go
  where
    go  Nil          = False
    go (Cons k _ xs)
      | k == key  = True
      | otherwise = go xs

-- | /O(n)/. Lookup the value at a key in the dictionary.
lookup :: BKey -> BDictMap a -> Maybe a
lookup x = go
  where
    go Nil = Nothing
    go (Cons k v xs)
      |   k == x  = Just v
      | otherwise = go xs
{-# INLINE lookup #-}

-- | /O(n + m)/. Merge two dictionaries by taking pair from both given
-- dictionaries. Dublicated keys are /not/ filtered.
--
union :: BDictMap a -> BDictMap a -> BDictMap a
union Nil xs  = xs
union xs  Nil = xs
union bd@(Cons k v xs) bd'@(Cons k' v' xs')
  |   k < k'  = Cons k  v  (union xs bd')
  | otherwise = Cons k' v' (union bd xs')

-- | /O(n)./ Map a function over all values in the dictionary.
map :: (a -> b) -> BDictMap a -> BDictMap b
map f = go
  where
    go Nil = Nil
    go (Cons k v xs) = Cons k (f v) (go xs)
{-# INLINE map #-}

-- | /O(n)./ Map a function over all keys\/value pairs in the dictionary.
mapWithKey :: (BKey -> a -> b) -> BDictMap a -> BDictMap b
mapWithKey f = go
  where
    go Nil = Nil
    go (Cons k v xs) = Cons k (f k v) (go xs)
{-# INLINE mapWithKey #-}

-- | /O(n)/. Map each key\/value pair to a monoid and fold resulting
-- sequnce using 'mappend'.
--
foldMapWithKey :: Monoid m => (BKey -> a -> m) -> BDictMap a -> m
foldMapWithKey f = go
  where
    go  Nil          = mempty
    go (Cons k v xs) = f k v `mappend` go xs
{-# INLINE foldMapWithKey #-}

{-# DEPRECATED bifoldMap "Use foldMapWithKey instead" #-}
-- | /O(n)/. Map each key\/value pair to a monoid and fold resulting
-- sequnce using 'mappend'.
--
bifoldMap :: Monoid m => (BKey -> a -> m) -> BDictMap a -> m
bifoldMap = foldMapWithKey
{-# INLINE bifoldMap #-}

-- | /O(n)/. Build a dictionary from a list of key\/value pairs where
-- the keys are in ascending order.
--
fromAscList :: [(BKey, a)] -> BDictMap a
fromAscList [] = Nil
fromAscList ((k, v) : xs) = Cons k v (fromAscList xs)

-- | /O(n)/. Convert the dictionary to a list of key\/value pairs
-- where the keys are in ascending order.
--
toAscList :: BDictMap a -> [(BKey, a)]
toAscList Nil = []
toAscList (Cons k v xs) = (k, v) : toAscList xs
