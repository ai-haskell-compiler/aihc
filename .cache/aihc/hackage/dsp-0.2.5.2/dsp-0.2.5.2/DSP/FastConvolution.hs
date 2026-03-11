-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.FastConvolution
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module to perform fast linear convolution of two sequences
--
-----------------------------------------------------------------------------

module DSP.FastConvolution (fast_conv) where

import Data.Array
import Data.Complex

import Numeric.Transform.Fourier.FFT

-- * Functions

-- | @fast_conv@ convolves two finite sequences using DFT relationships

fast_conv :: (RealFloat b) => Array Int (Complex b) -> Array Int (Complex b) -> Array Int (Complex b)
fast_conv h1 h2 = h3
    where m1  = snd $ bounds h1
	  m2  = snd $ bounds h2
	  m3  = m1 + m2
	  h1' = fft $ listArray (0,m3) $ elems h1 ++ replicate m2 0
          h2' = fft $ listArray (0,m3) $ elems h2 ++ replicate m1 0
          h3' = listArray (0,m3) $ zipWith (*) (elems h1') (elems h2')
          h3  = ifft h3'
