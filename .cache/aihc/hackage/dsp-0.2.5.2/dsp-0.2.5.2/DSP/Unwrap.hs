-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Unwrap
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple phase unwrapping algorithm
--
-----------------------------------------------------------------------------

-- O&S, pg 790

module DSP.Unwrap (unwrap) where

import Data.Array

-- * Functions

-- | This is the simple phase unwrapping algorithm from Oppenheim and
-- Schafer.

unwrap :: (Ix a, Integral a, Ord b, Floating b) => b         -- ^ epsilon
                                                -> Array a b -- ^ ARG
						-> Array a b -- ^ arg

unwrap eps phi = listArray b [ phi!i + 2 * pi * r!i | i <- range b ]
    where r = listArray b [ ri i | i <- range b ]
          ri 0 = 0
	  ri i | phi!i - phi!(i-1) >  (2*pi-eps) = r!(i-1) - 1
	       | phi!i - phi!(i-1) < -(2*pi-eps) = r!(i-1) + 1
	       | otherwise                       = r!(i-1)
	  b = bounds phi
