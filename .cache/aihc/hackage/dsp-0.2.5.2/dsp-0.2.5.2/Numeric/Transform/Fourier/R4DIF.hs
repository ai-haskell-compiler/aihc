-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.R4DIF
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Radix-4 Decimation in Frequency FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.R4DIF (fft_r4dif) where

import DSP.Basic (interleave)
import Data.Array
import Data.Complex

-------------------------------------------------------------------------------

-- | Radix-4 Decimation in Frequency FFT

{-# specialize fft_r4dif :: Array Int (Complex Float) -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_r4dif :: Array Int (Complex Double) -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_r4dif :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	  -> a -- ^ N
	  -> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	  -> Array a (Complex b) -- ^ X[k]

fft_r4dif x n fft = listArray (0,n-1) $ c
    where c4k0 = elems $ fft $ listArray (0,n4-1) x4k0
	  c4k1 = elems $ fft $ listArray (0,n4-1) x4k1
	  c4k2 = elems $ fft $ listArray (0,n4-1) x4k2
	  c4k3 = elems $ fft $ listArray (0,n4-1) x4k3
	  c    = interleave (interleave c4k0 c4k2) (interleave c4k1 c4k3)
	  x4k0 = [  x!i + x!(i+n2) +      x!(i+n4) + x!(i+n34)             | i <- [0..n4-1] ]
	  x4k1 = [ (x!i - x!(i+n2) - j * (x!(i+n4) - x!(i+n34))) * w!i     | i <- [0..n4-1] ]
	  x4k2 = [ (x!i + x!(i+n2) -      x!(i+n4) - x!(i+n34))  * w!(2*i) | i <- [0..n4-1] ]
	  x4k3 = [ (x!i - x!(i+n2) + j * (x!(i+n4) - x!(i+n34))) * w!(3*i) | i <- [0..n4-1] ]
	  j = 0 :+ 1
	  wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
	  n2  = n `div` 2
	  n4  = n `div` 4
	  n34 = 3 * n4
