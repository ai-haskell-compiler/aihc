-- |
-- Module      :  Data.MinMax1
-- Copyright   :  (c) OleksandrZhabenko 2020-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Functions to find both minimum and maximum elements of the finite 'F.Foldable' structure of the 'Ord'ered elements.

{-# LANGUAGE NoImplicitPrelude, MultiWayIf #-}

module Data.MinMax1 where

import GHC.Base
import qualified Data.Foldable as F

-- | Returns a pair where the first element is the minimum element from the two given ones and the second one is the maximum. If the arguments are
-- equal then the tuple contains equal elements.
minmaxP :: (Ord a) => a -> a -> (a,a)
minmaxP = minmaxPBy compare
{-# INLINE minmaxP #-}

-- | A variant of the 'minmaxP' where you can specify your own comparison function.
minmaxPBy :: (Ord a) => (a -> a -> Ordering) -> a -> a -> (a,a)
minmaxPBy g x y
 | g x y == LT = (x,y)
 | otherwise = (y,x)

-- | A ternary predicate to check whether the third argument lies between the first two unequal ones or whether they are all equal.
betweenNX :: (Ord a) => a -> a -> a -> Bool
betweenNX = betweenNXBy compare
{-# INLINE betweenNX #-}

-- | A variant of the 'betweenNX' where you can specify your own comparison function.
betweenNXBy :: (Ord a) => (a -> a -> Ordering) -> a -> a -> a -> Bool
betweenNXBy g x y z
 | x == y = x == z
 | g z k == LT && g z t == GT = True
 | otherwise = False
      where (t,k) = minmaxPBy g x y

-- | Finds out the minimum and maximum values of the finite structure that has not less than two elements. Otherwise returns 'Nothing'.
minMax11 :: (Ord a, F.Foldable t) => t a -> Maybe (a, a)
minMax11 = minMax11By compare
{-# INLINE minMax11 #-}
{-# SPECIALIZE minMax11 :: (Ord a) => [a] -> Maybe (a, a) #-}

-- | A generalized variant of the 'minMax11' where you can specify your own comparison function.
minMax11By :: (Ord a, F.Foldable t) => (a -> a -> Ordering) -> t a -> Maybe (a, a)
minMax11By g xs 
 | n < 2 = Nothing
 | otherwise = Just (r1, r2) 
      where f (x,y,k) z
              | k >= 2 = if
                 | g z x == LT -> (z,y,2)
                 | g z y == GT -> (x,z,2)
                 | otherwise -> (x,y,2)                
              | k == 1 = if
                 | g z y == LT -> (z,y,2) 
                 | otherwise -> (y,z,2) 
              | otherwise = (undefined,z,1)
            (r1,r2,n) = F.foldl' f (undefined,undefined,0) xs
{-# SPECIALIZE minMax11By :: (Ord a) => (a -> a -> Ordering) -> [a] -> Maybe (a, a) #-}

-- | Given a finite structure with at least 3 elements returns a tuple with the two most minimum elements
-- (the first one is less than the second one) and the maximum element. If the structure has less elements, returns 'Nothing'.
-- Uses just one pass through the structure, so may be more efficient than some other approaches.
minMax21 :: (Ord a, F.Foldable t) => t a -> Maybe (a, a, a)
minMax21 = minMax21By compare
{-# INLINE minMax21 #-}
{-# SPECIALIZE minMax21 :: (Ord a) => [a] -> Maybe (a,a,a) #-}

-- | A variant of the 'minMax21' where you can specify your own comparison function.
minMax21By :: (Ord a, F.Foldable t) => (a -> a -> Ordering) -> t a -> Maybe (a, a, a)
minMax21By g xs
 | n < 3 = Nothing
 | otherwise = Just (r1, r2, r3) 
      where f (x,y,t,k) z
              | k >= 3 = if
                 | g z x == LT -> (z,x,t,3)
                 | g z y == LT -> (x,z,t,3)
                 | g z t == GT -> (x,y,z,3)
                 | otherwise -> (x,y,t,3)                
              | k == 2 = if
                 | g z t == GT -> (y,t,z,3)
                 | g z y == LT -> (z,y,t,3) 
                 | otherwise -> (y,z,t,3) 
              | k == 1 = if
                 | g z t == GT -> (undefined,t,z,2)
                 | otherwise -> (undefined,z,t,2)
              | otherwise = (undefined,undefined,z,1)
            (r1,r2,r3,n) = F.foldl' f (undefined,undefined,undefined,0) xs
{-# SPECIALIZE minMax21By :: (Ord a) => (a -> a -> Ordering) -> [a] -> Maybe (a, a, a) #-}

-- | Given a finite structure with at least 3 elements returns a tuple with the minimum element
-- and two maximum elements (the first one is less than the second one). If the structure has less elements, returns 'Nothing'.
-- Uses just one pass through the structure, so may be more efficient than some other approaches.
minMax12 :: (Ord a, F.Foldable t) => t a -> Maybe (a,a,a)
minMax12 = minMax12By compare
{-# INLINE minMax12 #-}
{-# SPECIALIZE minMax12 :: (Ord a) => [a] -> Maybe (a, a, a) #-}

-- | A variant of the 'minMax12' where you can specify your own comparison function.
minMax12By :: (Ord a, F.Foldable t) => (a -> a -> Ordering) -> t a -> Maybe (a,a,a)
minMax12By g xs
 | n < 3 = Nothing
 | otherwise = Just (r1, r2, r3) 
      where f (x,y,t,k) z
              | k >= 3 = if
                 | g z x == LT -> (z,y,t,3)
                 | g z t == GT -> (x,t,z,3)
                 | g z y == GT -> (x,z,t,3)
                 | otherwise -> (x,y,t,3)                
              | k == 2 = if
                 | g z y == LT -> (z,y,t,3)
                 | g z t == GT -> (y,t,z,3) 
                 | otherwise -> (y,z,t,3) 
              | k == 1 = if
                 | g z t == GT -> (undefined,t,z,2)
                 | otherwise -> (undefined,z,t,2)
              | otherwise = (undefined,undefined,z,1)
            (r1,r2,r3,n) = F.foldl' f (undefined,undefined,undefined,0) xs
{-# SPECIALIZE  minMax12By :: (Ord a) => (a -> a -> Ordering) -> [a] -> Maybe (a,a,a) #-}

-- | Given a finite structure with at least 4 elements returns a tuple with two minimum elements
-- and two maximum elements. If the structure has less elements, returns 'Nothing'.
-- Uses just one pass through the structure, so may be more efficient than some other approaches.
minMax22 :: (Ord a, F.Foldable t) => t a -> Maybe (a,a, a,a)
minMax22 = minMax22By compare
{-# INLINE minMax22 #-}
{-# SPECIALIZE  minMax22 :: (Ord a) => [a] -> Maybe (a,a,a,a) #-}

-- | A variant of the 'minMax22' where you can specify your own comparison function.
minMax22By :: (Ord a, F.Foldable t) => (a -> a -> Ordering) -> t a -> Maybe (a,a, a,a)
minMax22By g xs
 | n < 4 = Nothing
 | otherwise = Just (r1, r2, r3, r4) 
      where f (x,y,t,u,k) z
              | k >= 4 = if
                 | g z u == GT -> (x,y,u,z,4)
                 | g z t == GT -> (x,y,z,u,4)
                 | g z x == LT -> (z,x,t,u,4)
                 | g z y == LT -> (x,z,t,u,4)
                 | otherwise -> (x,y,t,u,4)
             | k == 3 = if
                 | g z u == GT -> (y,t,u,z,4)
                 | g z t == GT -> (y,t,z,u,4)
                 | g z y == GT -> (y,z,t,u,4)
                 | otherwise -> (z,y,t,u,4)                
              | k == 2 = if
                 | g z u == GT -> (undefined,t,u,z,3)
                 | g z t == GT -> (undefined,t,z,u,3) 
                 | otherwise -> (undefined,z,t,u,3) 
              | k == 1 = if
                 | g z u == GT -> (undefined,undefined,u,z,2)
                 | otherwise -> (undefined,undefined,z,u,2)
              | otherwise = (undefined,undefined,undefined,z,1)
            (r1,r2,r3,r4,n) = F.foldl' f (undefined,undefined,undefined,undefined,0) xs
{-# SPECIALIZE  minMax22By :: (Ord a) => (a -> a -> Ordering) -> [a] -> Maybe (a,a,a,a) #-}

