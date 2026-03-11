-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.R2DIF
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Radix-2 Decimation in Frequency FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.R2DIF (fft_r2dif) where

import DSP.Basic (interleave)
import Data.Array
import Data.Complex

-------------------------------------------------------------------------------

-- | Radix-2 Decimation in Frequency FFT

{-# specialize fft_r2dif :: Array Int (Complex Float) -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_r2dif :: Array Int (Complex Double) -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_r2dif :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	  -> a -- ^ N
	  -> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	  -> Array a (Complex b) -- ^ X[k]

fft_r2dif a n fft = y
    where wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
	  ae = listArray (0,n2-1) [  a!k + a!(k+n2)        | k <- [0..(n2-1)] ]
	  ao = listArray (0,n2-1) [ (a!k - a!(k+n2)) * w!k | k <- [0..(n2-1)] ]
	  ye = fft ae
	  yo = fft ao
 	  y  = listArray (0,n-1) (interleave (elems ye) (elems yo))
	  n2 = n `div` 2
