-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.Median
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for computing the median on a list
--
-- Reference: Ross, NRiC
--
-----------------------------------------------------------------------------

module Numeric.Statistics.Median (median, medianFast) where

import Data.List (sort)

-- | Compute the median of a list

median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x


{- |
Compute the center of the list in a more lazy manner
and thus halves memory requirement.
-}

medianFast :: (Ord a, Fractional a) => [a] -> a
medianFast [] = error "medianFast: empty list has no median"
medianFast zs =
   let recurse (x0:_)    (_:[])   = x0
       recurse (x0:x1:_) (_:_:[]) = (x0+x1)/2
       recurse (_:xs)    (_:_:ys) = recurse xs ys
       recurse _ _  =
          error "median: this error cannot occur in the way 'recurse' is called"
   in  recurse zs zs
