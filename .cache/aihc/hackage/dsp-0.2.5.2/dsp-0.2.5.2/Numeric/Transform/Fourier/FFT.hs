-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.FFT
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- FFT driver functions
--
-----------------------------------------------------------------------------

-- TODO: unify the notation and methods in this file

module Numeric.Transform.Fourier.FFT (fft, ifft, rfft, irfft, r2fft) where

import Data.Array
import Data.Complex

import Numeric.Transform.Fourier.FFTHard
import Numeric.Transform.Fourier.R2DIF
-- import Numeric.Transform.Fourier.R2DIT
import Numeric.Transform.Fourier.R4DIF
-- import Numeric.Transform.Fourier.SRDIF
import Numeric.Transform.Fourier.CT
import Numeric.Transform.Fourier.PFA
import Numeric.Transform.Fourier.Rader

import DSP.Basic (uninterleave)


-------------------------------------------------------------------------------

-- | This is the driver routine for calculating FFT's.  All of the
-- recursion in the various algorithms are defined in terms of 'fft'.

-- The logic is based on FFTW.

{-# specialize fft :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize fft :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

fft :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
    -> Array a (Complex b) -- ^ X[k]
fft a | n == 1            = a
      | n == 2            = fft'2 a
      | n == 3            = fft'3 a
      | n == 4            = fft'4 a
      | l == 1 && n <= 11 = fft_rader1 a n
      | l == 1 && n >  11 = fft_rader2 a n fft
      | gcd l m == 1      = fft_pfa a l m fft
      | n `mod` 4 == 0    = fft_r4dif a n fft
      | n `mod` 2 == 0    = fft_r2dif a n fft
      | otherwise         = fft_ct1 a l m fft
    where l = choose_factor n
          m = n `div` l
          n = snd (bounds a) + 1

-- choose_factor is borrowed from FFTW

{-# specialize choose1 :: Int -> Int #-}

choose1 :: (Integral a) => a -> a
choose1 n = loop1 1 1
    where loop1 i f | i * i > n = f
	            | (n `mod` i) == 0 && gcd i (n `div` i) == 1 = loop1 (i+1) i
	            | otherwise = loop1 (i+1) f

{-# specialize choose2 :: Int -> Int #-}

choose2 :: (Integral a) => a -> a
choose2 n = loop2 1 1
    where loop2 i f | i * i > n = f
                    | n `mod` i == 0 = loop2 (i+1) i
		    | otherwise = loop2 (i+1) f

{-# specialize choose_factor :: Int -> Int #-}

choose_factor :: (Integral a) => a -> a
choose_factor n | i > 1 = i
		| otherwise = choose2 n
    where i = choose1 n

-------------------------------------------------------------------------------

-- We want to define the inverse and real valued FFT's based on the
-- forward complex Numeric.Transform.Fourier.  This way, if we implement a speedup, we only
-- have to do it in one place.  Personally, I don't like adding a sign
-- argument to the FFT for signify forward and inverse.

-- x(n) = 1/N * ~(fft ~X(k))
--   where X(k) = fft(x(n))
--         x    = conjugate x
--         N    = length x

-- P&M and Rick Lyon's books have the derivation.

-- ifft a = fmap (/ fromIntegral n) $ fmap conjugate $ fft $ fmap conjugate a
--   where n = snd (bounds a) + 1

-- We can also replace complex conjugation by swapping the real and
-- imaginary parts and get the same result.  Rick Lyon's book has the
-- derivation.

{-# specialize ifft :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize ifft :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

-- | Inverse FFT, including scaling factor, defined in terms of 'fft'

ifft :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
     -> Array a (Complex b) -- ^ x[n]
ifft a = fmap (/ fromIntegral n) $ fmap swap $ fft $ fmap swap a
    where swap (x:+y) = (y:+x)
	  n = snd (bounds a) + 1

-------------------------------------------------------------------------------

-- | This is the algorithm for computing 2N-point real FFT with an N-point
-- complex FFT, defined in terms of 'fft'

--  This formulation is from Rick's book.

{-# specialize rfft :: Array Int Float -> Array Int (Complex Float) #-}
{-# specialize rfft :: Array Int Double -> Array Int (Complex Double) #-}

rfft :: (Ix a, Integral a, RealFloat b) => Array a b -- ^ x[n]
     -> Array a (Complex b) -- ^ X[k]

rfft a = listArray (0,n-1) $ [ xa1 m | m <- [0..(n2-1)] ] ++ [ xa2 m | m <- [0..(n2-1)] ]
    where x   = fft $ listArray (0,n2-1) $ rfft_unzip (elems a)
	  xpr = listArray (0,n2-1) (xr!0 : [ (xr!m + xr!(n2-m)) / 2 | m <- [1..(n2-1)] ])
	  xmr = listArray (0,n2-1) (0 :    [ (xr!m - xr!(n2-m)) / 2 | m <- [1..(n2-1)] ])
	  xpi = listArray (0,n2-1) (xi!0 : [ (xi!m + xi!(n2-m)) / 2 | m <- [1..(n2-1)] ])
	  xmi = listArray (0,n2-1) (0 :    [ (xi!m - xi!(n2-m)) / 2 | m <- [1..(n2-1)] ])
	  xr = fmap realPart x
          xi = fmap imagPart x
          xa1 m = (xpr!m + cos w * xpi!m - sin w * xmr!m) :+
		  (xmi!m - sin w * xpi!m - cos w * xmr!m)
	      where w = pi * fromIntegral m / fromIntegral n2
          xa2 m = (xpr!m - cos w * xpi!m + sin w * xmr!m) :+
		  (xmi!m + sin w * xpi!m + cos w * xmr!m)
	      where w = pi * fromIntegral m / fromIntegral n2
	  rfft_unzip = uncurry (zipWith (:+)) . uninterleave
	  n = (snd (bounds a) + 1)
	  n2 = n `div` 2

-------------------------------------------------------------------------------

-- | This is the algorithm for computing a 2N-point real inverse FFT with an
-- N-point complex FFT, defined in terms of 'ifft'

{-# specialize irfft :: Array Int (Complex Float) -> Array Int Float #-}
{-# specialize irfft :: Array Int (Complex Double) -> Array Int Double #-}

irfft :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
      -> Array a b -- ^ x[n]

irfft f = listArray (0,n-1) $ irfft_unzip $ elems $ ifft $ z
    where fe = listArray (0,n2-1) [ 0.5 * (f!k + f!(n2+k))       | k <- [0..n2-1] ]
	  fo = listArray (0,n2-1) [ 0.5 * (f!k - f!(n2+k)) * w k | k <- [0..n2-1] ]
	  w k = cis $ 2 * pi * fromIntegral k / fromIntegral n
	  z = listArray (0,n2-1) [ fe!k + j * fo!k | k <- [0..n2-1] ]
	  j = 0 :+ 1
	  n = snd (bounds f) + 1
	  n2 = n `div` 2
	  irfft_unzip []         = []
	  irfft_unzip ((xr:+xi):xs) = xr : xi : irfft_unzip xs

-------------------------------------------------------------------------------

-- | Algorithm for 2 N-point real FFT's computed with N-point complex
-- FFT, defined in terms of 'fft'

{-# specialize r2fft :: Array Int Float -> Array Int Float -> (Array Int (Complex Float),Array Int (Complex Float)) #-}
{-# specialize r2fft :: Array Int Double -> Array Int Double -> (Array Int (Complex Double),Array Int (Complex Double)) #-}

r2fft :: (Ix a, Integral a, RealFloat b) => Array a b -- ^ x1[n]
      -> Array a b -- ^ x2[n]
      -> (Array a (Complex b), Array a (Complex b)) -- ^ (X1[k],X2[k])

r2fft x1 x2 = (x1',x2')
    where x = listArray (0,n-1) $ zipWith (:+) (elems x1) (elems x2)
          x' = fft x
          x1' = listArray (0,n-1) (x1'0 : [ (0.5 :+ 0.0) *  (x'!k + conjugate (x'!(n-k))) | k <- [1..(n-1)] ])
          x2' = listArray (0,n-1) (x2'0 : [ (0.0 :+ (-0.5)) * (x'!k - conjugate (x'!(n-k))) | k <- [1..(n-1)] ])
          x1'0 = realPart (x'!0) :+ 0
	  x2'0 = imagPart (x'!0) :+ 0
	  n = snd (bounds x1) + 1
