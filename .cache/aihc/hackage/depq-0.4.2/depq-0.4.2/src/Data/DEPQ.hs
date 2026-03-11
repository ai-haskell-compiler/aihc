{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.DEPQ
-- Copyright   :  (c) Marco Zocca 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  @ocramz
-- Stability   :  experimental
-- Portability :  portable
--
-- Double-ended priority queue (DEPQ)
--
-- Allows for efficiently finding and removing both the minimum and maximum priority elements, due to the min-heap invariant property of the underlying representation.
--
-- See https://en.wikipedia.org/wiki/Double-ended_priority_queue for definitions; the current implementation is based on the "dual structure" method outlined in the wikipedia page.
--
-- Based on `P.IntPSQ` : https://hackage.haskell.org/package/psqueues-0.2.7.2/docs/Data-IntPSQ.html
--
-- = Usage
--
-- Populate a DEPQ (either from a `Foldable` collection such as a list or array or by `insert`ing incrementally) and query either of its extremes (with `findMin`, `findMax`, `popMin`, `popMax`, `topK`, `bottomK`).
--
-- = Note
--
-- Import this module qualified (e.g. @import qualified Data.DEPQ as DQ@ or similar), as some of the function names are pretty common (e.g. `lookup`, `empty`), and might collide with similar functions imported from other libraries.
------------------------------------------------------------------------
module Data.DEPQ (
   DEPQ, 
   -- * Creation
   empty,
   -- * Conversion from/to lists
   fromList, toList,
   -- * Predicates
   null,
   valid,
   -- * Properties
   size,
   -- * Modification
   insert, delete, deleteMin, deleteMax, popMin, popMax,
   -- * Lookup
   lookup, findMin, findMax,
   -- ** Top-K lookup
   topK, bottomK
  ) where

import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))

-- containers
import qualified Data.Sequence as S (Seq, empty, (|>))
-- deepseq
import Control.DeepSeq     (NFData (rnf))
-- psqueues
import qualified Data.IntPSQ as P (IntPSQ, empty, null, size, insert, delete, toList, findMin, delete, deleteMin, valid, lookup)

import Prelude hiding (null, lookup)

import Test.QuickCheck (Arbitrary(..), Gen)

-- | A double-ended priority queue
data DEPQ p a = DEPQ {
    minHeap :: P.IntPSQ p a
  , maxHeap :: P.IntPSQ (Down p) a
                     } deriving (Eq, Show)

instance Foldable (DEPQ p) where
  foldr f z (DEPQ mi _) = foldr f z mi

instance (NFData p, NFData a) => NFData (DEPQ p a) where
  rnf (DEPQ mi ma) = rnf mi `seq` rnf ma

instance (Ord p, Arbitrary p, Arbitrary a) => Arbitrary (DEPQ p a) where
  arbitrary = fromList <$> (arbitrary :: Gen [(Int, p, a)])
  -- Convert given DEPQ into list, shrink it, then convert it back
  shrink depq = map fromList $ shrink $ toList depq


-- | Insert an element
insert :: (Ord p) =>
          Int -- ^ key
       -> p -- ^ priority
       -> a -- ^ value
       -> DEPQ p a -> DEPQ p a
insert k p v (DEPQ mi ma) = DEPQ mi' ma'
  where
    mi' = P.insert k p  v mi
    ma' = P.insert k (Down p) v ma
{-# INLINE insert #-}

-- | The empty DEPQ
empty :: DEPQ p a
empty = DEPQ P.empty P.empty

-- | Number of elements in the DEPQ
size :: DEPQ p a -> Int
size (DEPQ p _) = P.size p

-- | Lookup a key
lookup :: Int -- ^ lookup key
       -> DEPQ p v
       -> Maybe (p, v)
lookup k (DEPQ p _) = P.lookup k p
{-# inline lookup #-}

-- | Populate a DEPQ from a 'Foldable' container (e.g. a list)
fromList :: (Foldable t, Ord p) =>
            t (Int, p, a) -- ^ (key, priority, value)
         -> DEPQ p a
fromList = foldl insf empty where
  insf acc (k,p,v) = insert k p v acc
{-# inline fromList #-}

-- | Produce a list of (key, priority, value) triples with the entries of the DEPQ
--
-- Note : the order of the output list is undefined
toList :: DEPQ p v -> [(Int, p, v)]
toList (DEPQ p _) = P.toList p
{-# inline toList #-}

-- | Is the DEPQ empty ?
null :: DEPQ p v -> Bool
null (DEPQ mi ma) = P.null mi && P.null ma

-- | Is the DEPQ valid ?
valid :: (Ord p) => DEPQ p v -> Bool
valid (DEPQ mi ma) = P.valid mi && P.valid ma

-- | Delete a (key, priority, value) triple from the queue. When
-- the key is not a member of the queue, the original queue is returned.
delete :: Ord p => Int -- ^ key of the triple to be deleted
       -> DEPQ p a -> DEPQ p a
delete k (DEPQ mi ma) = DEPQ mi' ma'
  where
    mi' = P.delete k mi
    ma' = P.delete k ma

-- | Delete the minimum-priority element in the DEPQ
deleteMin :: Ord p => DEPQ p a -> DEPQ p a
deleteMin de@(DEPQ mi ma) = case P.findMin mi of
  Nothing -> de
  Just (imin, _, _) -> DEPQ mi' ma' where
    mi' = P.deleteMin mi
    ma' = P.delete imin ma
{-# INLINE deleteMin #-}

-- | Delete the maximum-priority element in the DEPQ
deleteMax :: Ord p => DEPQ p a -> DEPQ p a
deleteMax de@(DEPQ mi ma) = case P.findMin ma of
  Nothing -> de
  Just (imax, _, _) -> DEPQ mi' ma' where
    ma' = P.deleteMin ma
    mi' = P.delete imax mi
{-# INLINE deleteMax #-}

-- | /O(1)/ Find the minimum-priority element in the DEPQ
findMin :: Ord p => DEPQ p v -> Maybe (Int, p, v)
findMin (DEPQ mi _) = P.findMin mi
{-# inline findMin #-}

-- | /O(1)/ Find the maximum-priority element in the DEPQ
findMax :: Ord p => DEPQ p v -> Maybe (Int, p, v)
findMax (DEPQ _ ma) = f <$> P.findMin ma
  where
    f (i, Down p, v) = (i, p, v)
{-# inline findMax #-}


-- | Return the minimum along with a new DEPQ without that element
popMin :: Ord p => DEPQ p v -> Maybe ((Int, p, v), DEPQ p v)
popMin q = do
  x <- findMin q
  let q' = deleteMin q
  pure (x, q')

-- | Return the maximum along with a new DEPQ without that element
popMax :: Ord p => DEPQ p v -> Maybe ((Int, p, v), DEPQ p v)
popMax q = do
  x <- findMax q
  let q' = deleteMax q
  pure (x, q')

-- | K highest-scoring entries in the DEPQ
--
-- NB : this returns an empty sequence if there are fewer than K elements in the DEPQ
topK :: Ord p => Int -> DEPQ p v -> S.Seq (Int, p, v)
topK = popK popMax

-- | K lowest-scoring entries in the DEPQ
--
-- NB : this returns an empty sequence if there are fewer than K elements in the DEPQ
bottomK :: Ord p => Int -> DEPQ p v -> S.Seq (Int, p, v)
bottomK = popK popMin

popK :: (q -> Maybe (a, q))
     -> Int
     -> q
     -> S.Seq a
popK pop kk qq = fromMaybe S.empty $ go qq kk S.empty where
  go _ 0 acc = pure acc
  go q k acc = do
    (x, q') <- pop q
    go q' (k - 1) (acc S.|> x)
