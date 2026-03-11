-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.FFTHard
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Hard-coded FFT transforms
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.FFTHard where

import Data.Array
import Data.Complex

-- These are the hard coded DFT's borrowed from FFTW

{-# specialize fft'2 :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize fft'2 :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

-- | Length 2 FFT

fft'2 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
      -> Array a (Complex b) -- ^ X[k]

fft'2 a = array (0,1) [ (0, ((tmp1 + tmp2) :+ (tmp3 + tmp4))),
			(1, ((tmp1 - tmp2) :+ (tmp3 - tmp4) )) ]
    where tmp1 = realPart (a!0)
	  tmp3 = imagPart (a!0)
	  tmp2 = realPart (a!1)
	  tmp4 = imagPart (a!1)

{-# specialize fft'3 :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize fft'3 :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

-- | Length 3 FFT

fft'3 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
      -> Array a (Complex b) -- ^ X[k]

fft'3 a = array (0,2) [ (0, ((tmp1 + tmp4) :+ (tmp10 + tmp11))),
		        (1, ((tmp5 + tmp8) :+ (tmp9 + tmp12))),
		        (2, ((tmp5 - tmp8) :+ (tmp12 - tmp9))) ]
    where k866025403 = sqrt 3 / 2
	  k500000000 = 0.5
	  tmp1  = realPart (a!0)
	  tmp10 = imagPart (a!0)
	  tmp2  = realPart (a!1)
	  tmp6  = imagPart (a!1)
	  tmp3  = realPart (a!2)
	  tmp7  = imagPart (a!2)
	  tmp4  = tmp2 + tmp3
	  tmp9  = k866025403 * (tmp3 - tmp2)
	  tmp8  = k866025403 * (tmp6 - tmp7)
	  tmp11 = tmp6 + tmp7
	  tmp5  = tmp1 - (k500000000 * tmp4)
	  tmp12 = tmp10 - (k500000000 * tmp11)

{-# specialize fft'4 :: Array Int (Complex Float) -> Array Int (Complex Float) #-}
{-# specialize fft'4 :: Array Int (Complex Double) -> Array Int (Complex Double) #-}

-- | Length 4 FFT

fft'4 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
      -> Array a (Complex b) -- ^ X[k]

fft'4 a = array (0,3) [ (0, (tmp3 + tmp6) :+ (tmp15 + tmp16)),
		        (1, (tmp11 + tmp14) :+ (tmp9 - tmp10)),
		        (2, (tmp3 - tmp6) :+ (tmp15 - tmp16)),
		        (3, (tmp11 - tmp14) :+ (tmp10 + tmp9)) ]
    where tmp1  = realPart (a!0)
	  tmp7  = imagPart (a!0)
	  tmp4  = realPart (a!1)
	  tmp12 = imagPart (a!1)
	  tmp2  = realPart (a!2)
	  tmp8  = imagPart (a!2)
	  tmp5  = realPart (a!3)
	  tmp13 = imagPart (a!3)
	  tmp3  = tmp1 + tmp2
	  tmp11 = tmp1 - tmp2
	  tmp9  = tmp7 - tmp8
	  tmp15 = tmp7 + tmp8
	  tmp6  = tmp4 + tmp5
	  tmp10 = tmp4 - tmp5
	  tmp14 = tmp12 - tmp13
	  tmp16 = tmp12 + tmp13

-------------------------------------------------------------------------------

