-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Multirate.Polyphase
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Polyphase interpolators and decimators
--
-- Reference: C&R
--
-----------------------------------------------------------------------------

module DSP.Multirate.Polyphase (poly_interp) where

import Data.List (transpose)
import Data.Array

import DSP.Filter.FIR.FIR

-- mkpoly turns a single filter into a list of l subfilters

mkpoly :: Num a => Array Int a -> Int -> Int -> Array Int a
mkpoly h l k = listArray (0,m) [ h!(k+n*l) | n <- [0..m] ]
    where m = ((snd $ bounds h) + 1) `div` l - 1

-- | Polyphase interpolator

poly_interp :: (Num a, Eq a) => Int -- ^ L
	    -> Array Int a -- ^ h[n]
	    -> [a] -- ^ x[n]
	    -> [a] -- ^ y[n]

poly_interp l h x = concat $ transpose y
    where g = map (fir . mkpoly h l) [0..(l-1)]
	  y = map ($ x) g

{-

gZipWith :: Eq a => (a -> a -> a) -> [[a]] -> [a]
gZipWith f xs | any (== []) xs = []
	      | otherwise = foldl1 f (map head xs) : gZipWith f (map tail xs)

poly_decim :: Num a => Int -> Array Int a -> [a] -> [a]

poly_decim l h x = gZipWith (+) g
    where g = map (fir . mkpoly h l) [0..(l-1)]

Test

> h :: Array Int Double
> h = listArray (0,15) [1..16]

> x :: [Double]
> x =  [ 1, 0, 0, 0 ]

-}
