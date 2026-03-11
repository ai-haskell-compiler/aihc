-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.DFT
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Not so naive implementation of a Discrete Fourier Transform.
--
-----------------------------------------------------------------------------

{-
We cheat in three ways from a direct translation of the DFT equation:

     X(k) = sum(n=0..N-1) x(n) * e^(-2*j*pi*n*k/N)

1.  We precompute all values of W_N, and exploit the periodicity.
This is just to cut down on the number of sin/cos calls.

2.  We calculate X(0) seperately to prevent multiplication by 1

3.  We factor out x(0) to prevent multiplication by 1
-}

module Numeric.Transform.Fourier.DFT (dft) where

import Data.Array
import Data.Complex

-- We use a helper function here because we may want to have special
-- cases for small DFT's and we want to precompute the suspension all of
-- the twiddle factors.

{-# specialize dft :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize dft :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

dft :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
    -> Array a (Complex b) -- ^ X[k]
dft a = dft' a w n
    where w = listArray (0,n-1) [ cis (-2 * pi * fromIntegral i / fromIntegral n) | i <- [0..(n-0)] ]
	  n = snd (bounds a) + 1

{-# specialize dft' :: Array Int (Complex Float) -> Array Int (Complex Float) -> Int -> Array Int (Complex Float) #-}
{-# specialize dft' :: Array Int (Complex Double) -> Array Int (Complex Double) -> Int -> Array Int (Complex Double) #-}

dft' :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -> Array a (Complex b) -> a -> Array a (Complex b)
dft' a _ 1 = a
dft' a w n = listArray (0,n-1) (sum [ a!k | k <- [0..(n-1)] ] : [ a!0 + sum [ a!k * wik i k | k <- [1..(n-1)] ] | i <- [1..(n-1)] ])
    where wik 0 _ = 1
          wik _ 0 = 1
          wik i k = w!(i*k `mod` n)
