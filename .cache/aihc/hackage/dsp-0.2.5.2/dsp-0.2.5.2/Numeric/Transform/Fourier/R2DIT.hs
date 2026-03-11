-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.R2DIT
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Radix-2 Decimation in Time FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.R2DIT (fft_r2dit) where

import Data.Array
import Data.Complex

-------------------------------------------------------------------------------

-- This a recursive implementation of a FFT.  I believe this is
-- equivalent to a radix-2 decimation-in-time (DIT) FFT, which is a
-- special case of the Cooley-Tukey algorithm for N=2^v.

-- This algorithm was taken from Cormen, Leiserson, and Rivest's
-- Introduction to Algorithms.

-- | Radix-2 Decimation in Time FFT

{-# specialize fft_r2dit :: Array Int (Complex Float) -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_r2dit :: Array Int (Complex Double) -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_r2dit :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	  -> a -- ^ N
	  -> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	  -> Array a (Complex b) -- ^ X[k]

fft_r2dit a n fft = y
    where wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
	  a0 = listArray (0,n2-1) [ a!k | k <- [0..(n-1)], even k ]
	  a1 = listArray (0,n2-1) [ a!k | k <- [0..(n-1)], odd k  ]
	  y0 = fft a0
	  y1 = fft a1
 	  y  = array (0,n-1) ([ (k, y0!k + w!k * y1!k) | k <- [0..(n2-1)] ] ++ [ (k + n2, y0!k - w!k * y1!k) | k <- [0..(n2-1)] ])
          n2 = n `div` 2
