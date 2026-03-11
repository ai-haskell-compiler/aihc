{-|
Module      : Data.MedianStream
Description : Main module
Copyright   : (c) Joseph Canero, 2016
License     : BSD-3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE GADTs #-}

module Data.MedianStream
(
  MedianStream
, (+>)
, (<+)
, empty
, insert
, median
, size
, fromList
, insertList
) where

import Control.Applicative ((<$>), (<*>))
import Data.Heap (MaxHeap, MinHeap, Heap)
import qualified Data.Heap as Heap hiding (MaxHeap, MinHeap, Heap)
import Data.List (foldl')
import Data.Maybe (fromJust)

type Left a  = MaxHeap a
type Right a = MinHeap a

-- | A MedianStream is a data type that can be inserted into and queried
-- to get a median of a stream of numeric values.
data MedianStream a where
  MedianStream :: (Real a, Eq a) => Left a -> Right a -> MedianStream a

-- | Infix wrapper around insert with the MedianStream on the left.
-- Complexity: O(lgn)
(+>) :: MedianStream a -> a -> MedianStream a
(+>) ms a = insert a ms

-- | Infix wrapper around insert with the MedianStream on the right.
-- Complexity: O(lgn)
(<+) :: a -> MedianStream a -> MedianStream a
(<+) = insert

-- | Create an empty MedianStream with no values.
-- Complexity: O(1)
empty :: (Real a, Eq a) => MedianStream a
empty = MedianStream Heap.empty Heap.empty

-- | Insert a new numeric value into the median stream.
-- Complexity: O(lgn)
insert :: a -> MedianStream a -> MedianStream a
insert a ms@(MedianStream lh rh)
  | even $ size ms = oddMedianStream
  | otherwise      = evenMedianStream
    where
      oddMedianStream
        | maybe False (a >=) $ Heap.viewHead rh =
          uncurry MedianStream $ popAndSwap lh rh a
        | otherwise = MedianStream (Heap.insert a lh) rh
      evenMedianStream
        | maybe False (a < ) $ Heap.viewHead lh =
          uncurry (flip MedianStream) $ popAndSwap rh lh a
        | otherwise = MedianStream lh (Heap.insert a rh)

-- | Query the MedianStream for the median of the stream of numbers
-- inserted so far.
-- Complexity: O(1)
median :: MedianStream a -> Maybe Double
median ms@(MedianStream lh rh)
  | even $ size ms = average <$> Heap.viewHead lh <*> Heap.viewHead rh
  | otherwise      = (fromRational . toRational) <$> Heap.viewHead lh

-- | Returns the number of elements in the MedianStream.
-- Complexity: O(1)
size :: MedianStream a -> Int
size (MedianStream lh rh) = Heap.size lh + Heap.size rh

-- | Creates a MedianStream from a list of input elements.
-- Complexity: O(nlgn)
fromList :: (Real a, Eq a) => [a] -> MedianStream a
fromList = insertList empty

-- | Adds a list of input elements to an existing MedianStream
-- Complexity: O(nlgn)
insertList :: (Real a, Eq a) => MedianStream a -> [a] -> MedianStream a
insertList = foldl' (+>)

popAndSwap :: (Heap.HeapItem pol1 a,
               Heap.HeapItem pol2 a)
           => Heap pol1 a
           -> Heap pol2 a
           -> a
           -> (Heap pol1 a, Heap pol2 a)
popAndSwap lh rh a =
  let
    ([top], srh) = Heap.splitAt 1 rh
    nlh          = Heap.insert top lh
    nrh          = Heap.insert a   srh
    in (nlh, nrh)

average :: (Real a) => a -> a -> Double
average x l = (/2 ) . fromRational $ toRational (x + l)
