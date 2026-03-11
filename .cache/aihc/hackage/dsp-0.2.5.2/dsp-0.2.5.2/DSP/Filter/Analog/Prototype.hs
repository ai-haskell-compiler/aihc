-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.Analog.Prototype
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for generating analog filter prototypes
--
-----------------------------------------------------------------------------

-- Notes (mainly for self):

-- The gain of an analog filter is

--    gain = abs $ realPart $ product zeros / product poles
--         = abs $ b_m / a_n

-- For a Butterworth filter, the product of the poles is one, so we don't
-- have to worry about any gain.

-- For a Chebyshev 1 filter, the product of the poles is a_n, which is
-- the head of the polynomial.  We make this b_0 to set the gain in the
-- passband.

-- For a Chebyshev 2 filter, we use the full gain formula because we want
-- to set the gain to unity at DC.

-- TODO: Do we want to include Bessel filters?

module DSP.Filter.Analog.Prototype where

import Data.Complex (Complex((:+)), realPart)

import Polynomial.Basic (roots2poly)

-- | Generates Butterworth filter prototype

butterworth :: Int -- ^ N
	    -> ([Double],[Double]) -- ^ (b,a)

butterworth n = (num, den)
    where poles = [ (-u k) :+ (w k) | k <- [0..(n-1)] ]
	  u k = sin (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  w k = cos (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  num = [ 1 ]
	  den = map realPart $ roots2poly $ poles

-- | Generates Chebyshev filter prototype

chebyshev1 :: Double -- ^ epsilon
	   -> Int -- ^ N
	   -> ([Double],[Double]) -- ^ (b,a)

chebyshev1 eps n = (num, den)
    where poles = [ (-u k) :+ (w k) | k <- [0..(n-1)] ]
	  u k = sinh v0 * sin (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  w k = cosh v0 * cos (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  num = [ gain ]
	  den = map realPart $ roots2poly $ poles
	  v0 = asinh (1/eps) / fromIntegral n
	  gain =
             if even n
               then abs $ head den / sqrt (1 + eps^(2::Int))
	       else abs $ head den

-- | Generates Inverse Chebyshev filter prototype

chebyshev2 :: Double -- ^ epsilon
	   -> Int -- ^ N
	   -> ([Double],[Double]) -- ^ (b,a)

chebyshev2 eps n = (num, den)
    where zeros = [ 0 :+ 1 / wz k | k <- [0..(n-1)], 2*k+1 /= n ]
	  poles = [ 1 / ((-u k) :+ (w k)) | k <- [0..(n-1)] ]
	  wz k = cos (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  u k = sinh v0 * sin (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  w k = cosh v0 * cos (fromIntegral (2*k+1) * pi / fromIntegral (2*n))
	  num = map (*gain) $ map realPart $ roots2poly $ zeros
	  den =               map realPart $ roots2poly $ poles
	  v0 = asinh (1/eps) / fromIntegral n
	  gain = abs $ realPart $ product poles / product zeros
