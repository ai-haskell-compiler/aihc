-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.Goertzel
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of Goertzel's algorithm, which computes one
-- bin of a DFT.  A description can be found in Oppenheim and Schafer's
-- /Discrete Time Signal Processing/, pp 585-587.
--
-----------------------------------------------------------------------------

-- TODO: do the cipherin' to figure out the best simplification for the
-- cgoertzel_power case

-- TODO: Bonzanigo's phase correction

module Numeric.Transform.Fourier.Goertzel where

import Data.Array
import Data.Complex

-- | Goertzel's algorithm for complex inputs

cgoertzel :: (RealFloat a, Ix b, Integral b) => Array b (Complex a) -- ^ x[n]
	  -> b -- ^ k
	  -> Complex a -- ^ X[k]

cgoertzel x0 k = g (elems x0) 0 0
    where w = 2 * pi * fromIntegral k / fromIntegral n
          a = 2 * cos w
	  g []     x1 x2 = x1 * cis w - x2
	  g (x:xs) x1@(x1r:+x1i) x2 = g xs (x + (a*x1r:+a*x1i) - x2) x1
	  n = (snd $ bounds x0) - 1

-- | Power via Goertzel's algorithm for complex inputs

cgoertzel_power :: (RealFloat a, Ix b, Integral b) => Array b (Complex a) -- ^ x[n]
		-> b -- ^ k
		-> a -- ^ |X[k]|^2

cgoertzel_power x k = (magnitude $ cgoertzel x k)^(2::Int)

-- | Goertzel's algorithm for real inputs

rgoertzel :: (RealFloat a, Ix b, Integral b) => Array b a -- ^ x[n]
	  -> b -- ^ k
	  -> Complex a -- ^ X[k]

rgoertzel x0 k = g (elems x0) 0 0
    where w = 2 * pi * fromIntegral k / fromIntegral n
          a = 2 * cos w
	  g []     x1 x2 = ((x1 - cos w * x2) :+ x2 * sin w)
	  g (x:xs) x1 x2 = g xs (x + a * x1 - x2) x1
	  n = (snd $ bounds x0) - 1

-- | Power via Goertzel's algorithm for real inputs

rgoertzel_power :: (RealFloat a, Ix b, Integral b) => Array b a -- ^ x[n]
		-> b -- ^ k
		-> a -- ^ |X[k]|^2

rgoertzel_power x0 k = g (elems x0) 0 0
    where w = 2 * pi * fromIntegral k / fromIntegral n
          a = 2 * cos w
	  g []     x1 x2 = x1^(2::Int) + x2^(2::Int) - a * x1 * x2
	  g (x:xs) x1 x2 = g xs (x + a * x1 - x2) x1
	  n = (snd $ bounds x0) - 1
