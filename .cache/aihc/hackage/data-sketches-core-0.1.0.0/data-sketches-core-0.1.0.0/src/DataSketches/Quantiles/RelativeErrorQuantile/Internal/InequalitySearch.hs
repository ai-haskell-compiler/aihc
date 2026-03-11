module DataSketches.Quantiles.RelativeErrorQuantile.Internal.InequalitySearch where

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

data (:<) = (:<)
data (:<=) = (:<=)
data (:>) = (:>)
data (:>=) = (:>=)
-- JavaDoc copypasta
-- 
-- This provides efficient, unique and unambiguous binary searching for inequality comparison criteria
--  for ordered arrays of values that may include duplicate values. The inequality criteria include
--  <, >, ==, >=, <=. All the inequality criteria use the same search algorithm.
--  (Although == is not an inequality, it is included for convenience.)

--  In order to make the searching unique and unambiguous, we modified the traditional binary
--  search algorithm to search for adjacent pairs of values <i>{A, B}</i> in the values array
--  instead of just a single value, where <i>A</i> and <i>B</i> are the array indicies of two
--  adjacent values in the array. For all the search criteria, if the algorithm reaches the ends of
--  the search range, the algorithm calls the <i>resolve()</i> method to determine what to
--   return to the caller. If the key value cannot be resolved, it returns a -1 to the caller.

--  Given an array of values <i>arr[]</i> and the search key value <i>v</i>, the algorithms for
--  the searching criteria are as follows:</p>
-- 
--  <li><b>LT:</b> Find the highest ranked adjacent pair <i>{A, B}</i> such that:<br>
--  <i>arr[A] &lt; v &le; arr[B]</i>. The normal return is the index <i>A</i>.
--  </li>
--  <li><b>LE:</b>  Find the highest ranked adjacent pair <i>{A, B}</i> such that:<br>
--  <i>arr[A] &le; v &lt; arr[B]</i>. The normal return is the index <i>A</i>.
--  </li>
--  <li><b>EQ:</b>  Find the adjacent pair <i>{A, B}</i> such that:<br>
--  <i>arr[A] &le; v &le; arr[B]</i>. The normal return is the index <i>A</i> or <i>B</i> whichever
--  equals <i>v</i>, otherwise it returns -1.
--  </li>
--  <li><b>GE:</b>  Find the lowest ranked adjacent pair <i>{A, B}</i> such that:<br>
--  <i>arr[A] &lt; v &le; arr[B]</i>. The normal return is the index <i>B</i>.
--  </li>
--  <li><b>GT:</b>  Find the lowest ranked adjacent pair <i>{A, B}</i> such that:<br>
--  <i>arr[A] &le; v &lt; arr[B]</i>. The normal return is the index <i>B</i>.
--  </li>
--  </ul>
class InequalitySearch s where
  inequalityCompare :: Ord a
    => s
    -> a 
    -- ^ V
    -> a
    -- ^ A
    -> a
    -- ^ B
    -> Ordering
    -- ^ 'GT' means we must search higher in the array, 'LT' means we must
    -- search lower in the array, or `EQ`, which means we have found 
    -- the correct bounding pair.
  getIndex 
    :: (PrimMonad m, MVector v a, Ord a) 
    => s 
    -> v (PrimState m) a 
    -> Int 
    -> Int 
    -> a 
    -> m Int
  resolve 
    :: s 
    -> Int -- Vector length
    -> (Int, Int) 
    -- ^ Final low index, high index (lo, hi)
    -> (Int, Int) 
    -- ^ Initial search region (low, high)
    -> Int
    -- ^ A thing

instance InequalitySearch (:<) where
  inequalityCompare _ v a b 
    | v <= a = LT
    | b < v = GT
    | otherwise = EQ
  getIndex _ _ a _ _ = pure a
  resolve _ vl (lo, hi) (low, high) = if lo >= high then high else vl

instance InequalitySearch (:<=) where
  inequalityCompare _ v a b
    | v < a = LT
    | b <= v = GT
    | otherwise = EQ
  getIndex _ _ a _ _ = pure a
  resolve _ vl (lo, hi) (low, high) = if lo >= high then high else vl 

instance InequalitySearch (:>) where
  inequalityCompare _ v a b
    | v < a = LT
    | b <= v = GT
    | otherwise = EQ
  getIndex _ _ _ b _ = pure b
  resolve _ vl (lo, hi) (low, high) = if hi <= low then low else vl

instance InequalitySearch (:>=) where
  inequalityCompare _ v a b
    | v <= a = LT
    | b < v = GT
    | otherwise = EQ
  getIndex _ _ _ b _ = pure b
  resolve _ vl (lo, hi) (low, high) = if hi <= low then low else vl

find :: (InequalitySearch s, PrimMonad m, MVector v a, Ord a) => s -> v (PrimState m) a -> Int -> Int -> a -> m Int
find strat v low high x = go low (high - 1)
  where
    go lo hi 
      | lo <= hi && lo < high = do
          let mid = lo + ((hi - lo) `div` 2)
          midV <- MV.read v mid
          midV' <- MV.read v (mid + 1)
          case inequalityCompare strat x midV midV' of
            LT -> go lo (mid - 1)
            EQ -> getIndex strat v mid (mid + 1) x
            GT -> go (mid + 1) hi
      | otherwise = pure $! resolve strat (MV.length v) (lo, hi) (low, high)
{-# INLINE find #-}