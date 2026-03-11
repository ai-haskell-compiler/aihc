-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Multirate.Halfband
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Halfband interpolators and decimators
--
-- Reference: C&R
--
-----------------------------------------------------------------------------

module DSP.Multirate.Halfband (hb_interp, hb_decim) where

import Data.Array

import DSP.Basic (delay, uninterleave, interleave)
import DSP.Filter.FIR.FIR

mkhalfband :: Num a => Array Int a -> Array Int a
mkhalfband h = listArray (0,m `div` 2) [ h!n | n <- [0,2..m] ]
    where m = snd $ bounds h

-- | Halfband interpolator

hb_interp :: (Num a, Eq a) => Array Int a -- ^ h[n]
	  -> [a] -- ^ x[n]
	  -> [a] -- ^ y[n]

hb_interp h x = interleave y1 y2
    where (x1,x2) = uninterleave x
	  y1 = fir (mkhalfband h) x1
	  y2 = map (h!m2 *) $ delay m2 $ x2
	  m2 = (snd $ bounds h) `div` 2

-- | Halfband decimator

hb_decim :: (Num a, Eq a) => Array Int a -- ^ h[n]
	 -> [a] -- ^ x[n]
	 -> [a] -- ^ y[n]

hb_decim h x = zipWith (+) y1 y2
    where (x1,x2) = uninterleave x
	  y1 = fir (mkhalfband h) x1
	  y2 = map (h!m2 *) $ delay m2 $ x2
	  m2 = (snd $ bounds h) `div` 2
