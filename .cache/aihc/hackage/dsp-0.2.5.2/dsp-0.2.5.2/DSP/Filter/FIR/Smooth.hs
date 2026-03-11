-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.Smooth
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Herrmann type smooth FIR filters, from Hamming, Chapter 7, also
-- known as maximally flat FIR filters
--
-- If x is the -3 dB point, then p\/q = -(x+1)\/(x-1)
--
-----------------------------------------------------------------------------

-- TODO: function for rational fraction approximation

-- TODO: input parameters in the style of sect53.f

module DSP.Filter.FIR.Smooth (smoothfir) where

import Data.Array

import Polynomial.Basic

-- Normalize is the step to set g(1) = 1 (pg 123)

normalize :: Fractional a => [a] -> [a]
normalize x = map (/ a) x
    where a = sum x

-- Expand performs the algorithm in Sect 7.3

expand :: Fractional a => [a] -> [a]
expand (x1:x2:[]) = [ x1, x2 ]
expand (x:xs) = expand' x $ expand xs

expand' :: Fractional a => a -> [a] -> [a]
expand' x ys0 = zipWith (+) (x : m1 ys0) (p1 ys0)
    where m1 (y:ys) = y : map (0.5*) ys
	  p1 (_:ys) = map (0.5*) ys ++ [ 0, 0 ]

-- Reflect makes the filter symmetric (not sure where this is stated)

reflect :: Fractional a => [a] -> [a]
reflect (x:xs) = (map (0.5*) $ reverse xs) ++ x : (map (0.5*) xs)

-- The actual function.  Note that we use (1+t)^p * (1-t)^q directly
-- since we have a polynomial library.

-- | designs smooth FIR filters

smoothfir :: (Ix a, Integral a, Fractional b) => a -- ^ p
	  -> a -- ^ q
	  -> Array a b -- ^ h[n]

smoothfir p q = listArray (0,n-1) $ reflect $ expand $ b
    where b' = polymult (polypow [ 1, 1 ] p) (polypow [ 1, -1 ] q)
          b1 = polyinteg b' 0
	  c = -polyeval b1 (-1)
	  b = normalize $ c : tail b1
	  n = 2 * (p+1 + q+1) - 1

-- Test

-- map (256*) $ elems $ smoothfir 3 1 == [ -1, -5, -5, 20, 70, 98, 70, 20, -5, -5, -1 ]
