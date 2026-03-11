-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.Sharpen
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module to sharpen FIR filters
--
-- Reference: Hamming, Sect 6.6
--
-- @H'(z) = 3 * H(z)^2 - s * H(z)^3@
-- @      = H(z)^2 * (3 - 2 * H(z))@
--
-- Procedure:
--
-- (1)  Filter the signal once with H(z)
--
-- 2.  Double this
--
-- 3.  Subtract this from 3x
--
-- 4.  Filter this twice by H(z) or once by H(z)^2
--
-----------------------------------------------------------------------------

module DSP.Filter.FIR.Sharpen where

import Data.Array

import qualified DSP.Basic as Basic
import DSP.Filter.FIR.FIR

-- | Filter shaprening routine

sharpen :: (Num a, Eq a) => Array Int a -- ^ h[n]
	-> ([a] -> [a]) -- ^ function that implements the sharpened filter

sharpen h x = step4
    where step1 = fir h x
	  step2 = map (2*) step1
	  step3 = zipWith (-) (map (3*) (Basic.delay delay x)) step2
	  step4 = fir h $ fir h $ step3
	  -- step4 = fir $ conv h h $ step3
	  m = snd $ bounds h
	  delay = m `div` 2
