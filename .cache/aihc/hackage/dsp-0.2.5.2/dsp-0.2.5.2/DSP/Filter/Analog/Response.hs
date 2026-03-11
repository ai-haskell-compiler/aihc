-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.Analog.Response
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for generating analog filter responses
--
-- Formulas are from Oppenheim and Schafer, Appendix B
--
-----------------------------------------------------------------------------

module DSP.Filter.Analog.Response where

import DSP.Basic ((^!))
import Polynomial.Basic
import Polynomial.Chebyshev

-- | Butterworth filter response function

butterworth_H :: Int    -- ^ N
	      -> Double -- ^ w_c
	      -> Double -- ^ w
	      -> Double -- ^ |H_c(w)|^2

butterworth_H n wc w = 1 / (1 + (w/wc)^(2*n))

-- | Chebyshev filter response function

chebyshev1_H :: Int    -- ^ N
	     -> Double -- ^ epsilon
	     -> Double -- ^ w_c
	     -> Double -- ^ w
	     -> Double -- ^ |H_c(w)|^2

chebyshev1_H n eps wc w = 1 / (1 + eps^!2 * vn(w/wc)^!2)
    where vn = polyeval (cheby n)

-- | Inverse Chebyshev filter response function
--
-- Note that @w_c@ is a property of the stopband for this filter

chebyshev2_H :: Int    -- ^ N
	     -> Double -- ^ epsilon
	     -> Double -- ^ w_c
	     -> Double -- ^ w
	     -> Double -- ^ |H_c(w)|^2

chebyshev2_H n eps wc w = 1 / (1 + (eps^!2 * vn(wc/w)^!2)**(-1))
    where vn = polyeval (cheby n)
