-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.Rader
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Rader's Algorithm for computing prime length FFT's
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.Rader (fft_rader1, fft_rader2) where

import Data.Array
import Data.Complex

-------------------------------------------------------------------------------

-- Rader's Algorithm.  We define this two ways: using direct circular
-- convolution, and FFT circular convolution.  The algorithms and
-- implementations, are esentially the same, except for how hg is
-- computed.

-- | Rader's Algorithm using direct convolution

fft_rader1 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	  -> a -- ^ N
	  -> Array a (Complex b) -- ^ X[k]

fft_rader1 f n = f'
    where h = listArray (0,n-2) [ f!(a ^* (n-(1+n'))) | n' <- [0..(n-2)] ]
          g = listArray (0,n-2) [ w!(a ^* n') | n' <- [0..(n-2)] ]
          hg = listArray (0,n-2) [ sum [ h!j * g!((i-j)`mod`(n-1)) | j <- [0..(n-2)] ] | i <- [0..(n-2)] ]
          f' = array (0,n-1) ((0, sum [ f!i | i <- [0..(n-1)] ]) : [ (a ^* i, f!0 + hg!i) | i <- [0..(n-2)] ])
	  wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
          _ ^* 0 = 1
	  i ^* j = (i * (i ^* (j-1))) `mod` n
	  a = generator n

-- | Rader's Algorithm using FFT convolution

{-# specialize fft_rader2 :: Array Int (Complex Float) -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_rader2 :: Array Int (Complex Double) -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_rader2 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	  -> a -- ^ N
	  -> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	  -> Array a (Complex b) -- ^ X[k]

fft_rader2 f n fft = f'
     where h = listArray (0,n-2) [ f!(a ^* (n-(1+n'))) | n' <- [0..(n-2)] ]
           g = listArray (0,n-2) [ w!(a ^* n') | n' <- [0..(n-2)] ]
	   h' = fft h
	   g' = fft g
           hg' = listArray (0,n-2) [ h'!i * g'!i | i <- [0..(n-2)] ]
           hg = ifft hg'
	   f' = array (0,n-1) ((0, sum [ f!i | i <- [0..(n-1)] ]) : [ (a ^* i, f!0 + hg!i) | i <- [0..(n-2)] ])
	   wn = cis (-2 * pi / fromIntegral n)
	   w = listArray (0,n-1) $ iterate (* wn) 1
           _ ^* 0 = 1
           i ^* j = (i * (i ^* (j-1))) `mod` n
	   a = generator n
           ifft b = fmap (/ fromIntegral (n-1)) $ fmap swap $ fft $ fmap swap b
           swap (x:+y) = (y:+x)

-- Haskell translation of find_generator from FFTW

{-# specialize generator :: Int -> Int #-}

generator :: (Integral a) => a -> a
generator p = findgen 1
    where findgen 0 = error "rader: generator: no primitive root?"
	  findgen x | (period x x) == (p - 1) = x
		    | otherwise               = findgen ((x + 1) `mod` p)
	  period _ 1    = 1
          period x prod = 1 + (period x (prod * x `mod` p))
