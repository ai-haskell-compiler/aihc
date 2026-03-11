-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.SlidingFFT
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Sliding FFT Algorithm
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.SlidingFFT (sfft) where

import Data.Array
import Data.Complex

import Numeric.Transform.Fourier.FFT

-- Sliding FFT algorithm.  We assume that the head of the list is the
-- oldest sample, and the last element is the newest sample.  This is why
-- we need the reverse.  By doing this we can abstract things like A/D
-- converters as infinite lists.

-- The only published reference I have seen for this is the TI TMS320C3x
-- General-Purpose Applications (SPRU194).  You can also check out
-- comp.dsp.  The author, Keith Larson, hangs out there.

-- The type of (!!) forces the type signatures to use Int instead of
-- (Integral a)

{-# specialize sfft :: Int -> [Complex Float] -> [Array Int (Complex Float)] #-}
{-# specialize sfft :: Int -> [Complex Double] -> [Array Int (Complex Double)] #-}

-- | Sliding FFT

sfft :: RealFloat a => Int -- ^ N
     -> [Complex a] -- ^ x[n]
     -> [Array Int (Complex a)] -- ^ [X[k]]

sfft _ [] = error "sfft: input must have at least on value"
sfft n (x:xs) = x' : sfft' n x xs x'
    where x' = fft $ listArray (0,n-1) $ reverse $ take n (x:xs)

{-# specialize sfft' :: Int -> Complex Float -> [Complex Float] -> Array Int (Complex Float) -> [Array Int (Complex Float)] #-}
{-# specialize sfft' :: Int -> Complex Double -> [Complex Double] -> Array Int (Complex Double) -> [Array Int (Complex Double)] #-}

sfft' :: RealFloat a => Int
     -> Complex a
     -> [Complex a]
     -> Array Int (Complex a)
     -> [Array Int (Complex a)]
sfft' n xn (x:xs)  x' | enough n (x:xs) = x'' : sfft' n x xs x''
		      | otherwise       = []
    where x'' = listArray (0,n-1) [ x0 - xn + x'!i * w i | i <- [0..(n-1)] ]
          x0  = xs !! (n-2)
	  w i = cis $ -2 * pi * fromIntegral i / fromIntegral n
sfft' _ _ [] _ = error "sfft': input must have at least on value"

-- We can't use Prelude.length because we may be operating on infinite,
-- or ginormous lists.  So enough will return True is there is enough
-- data to perform the next FFT update, or False if there is not enough.

enough :: Int -> [a] -> Bool
enough n xs  =  n<=0 || not (null (drop (n-1) xs))

{-
alternative:

enough 0 _      = True
enough _ []     = False
enough n (_:xs) = enough (n-1) xs
-}
