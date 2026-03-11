{-# LANGUAGE FlexibleContexts #-}

{- |
Module      :  Statistics.Pava.Common
Description :  Auxiliary functions
Copyright   :  2021 Dominik Schrempf
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon Jun  8 11:03:12 2020.

-}

module Statistics.Pava.Common
  ( slope
  , strictlyOrdered
  , smooth
  , unsafeSmooth
  , reverse3
  )
where

import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as M
import           Data.Vector.Generic            ( Vector )

-- | Calculate the slope between to points.
slope :: (Real a, Real b) => a -> a -> b -> b -> Double
slope x0 x1 y0 y1 = realToFrac (y1 - y0) / realToFrac (x1 - x0)
{-# SPECIALIZE slope :: Int -> Int -> Double -> Double -> Double #-}
{-# SPECIALIZE slope :: Double -> Double -> Double -> Double -> Double #-}
{-# INLINE slope #-}

-- -- Differences between values in vector.
-- diff :: (Num a, Vector v a) => v a -> v a
-- diff v = V.zipWith (-) (V.tail v) v
-- {-# SPECIALIZE diff :: (Vector v Double) => v Double -> v Double #-}

-- | Check if vector is ordered strictly (<).
strictlyOrdered :: (Ord a, Vector v a, Vector v Bool) => v a -> Bool
strictlyOrdered xs | V.length xs <= 1 = True
                   | otherwise        = V.and $ V.zipWith (<) xs (V.tail xs)

-- | Fill in missing values of an indexed vector.
--
-- @
--  smooth [-2, 2, 4, 5] [0.0, 4.0, 10.0, 88.0] = [0.0, 1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 88.0]
-- @
smooth
  :: (Vector v Bool, Vector v Double, Vector v Int)
  => v Int
  -> v Double
  -> v Double
smooth xs ys
  | V.length xs /= V.length ys = error
    "smooth: Index and value vector have different length."
  | not (strictlyOrdered xs) = error
    "smooth: Index vector is not strictly ordered."
  | otherwise = unsafeSmooth xs ys

-- | See 'smooth'.
--
-- Assume that:
-- - the lengths of the provided vectors are equal;
-- - the predictors are ordered.
unsafeSmooth
  :: (Vector v Bool, Vector v Double, Vector v Int)
  => v Int
  -> v Double
  -> v Double
unsafeSmooth xs ys
  | l == 0 = V.empty
  | l == 1 = V.take 1 ys
  | otherwise = V.create
    (do
      zs <- M.new m
      go zs 0 1 (bounds 1)
      return zs
    )
 where
  l = V.length xs
  a = V.head xs
  b = V.last xs
  m = b - a + 1
  -- 0 <= i < m; index traversing resulting vector
  -- 0 <= j < l; index traversing given vectors
  bounds i = (xs V.! (i - 1), xs V.! i, ys V.! (i - 1), ys V.! i)
  go zs i j (il, ir, yl, yr)
    | i >= m = return ()
    | a + i >= ir = do
      M.write zs i yr
      go zs (i + 1) (j + 1) (bounds $ j + 1)
    | otherwise = do
      M.write zs i (yl + dy)
      go zs (i + 1) j (il, ir, yl, yr)
   where
    dx = a + i - il
    dy = fromIntegral dx * slope il ir yl yr

-- | Reverse lists in a three-tuple.
reverse3 :: ([a], [b], [c]) -> ([a], [b], [c])
reverse3 (xs, ys, zs) = (reverse xs, reverse ys, reverse zs)
