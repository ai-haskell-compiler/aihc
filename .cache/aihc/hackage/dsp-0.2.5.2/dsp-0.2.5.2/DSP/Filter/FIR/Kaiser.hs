-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.Kaiser
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the Kaiser Window Method for designing FIR
-- filters.
--
-----------------------------------------------------------------------------

-- Reference:
--
-- @Book{dsp,
--   author = 	 "Alan V. Oppenheim and Ronald W. Schafer",
--   title = 	 "Discrete-Time Signal Processing",
--   publisher = 	 "Prentice-Hall",
--   year = 	 1989,
--   address =	 "Englewood Cliffs",
--   series =       {Prentice-Hall Signal Processing Series}
-- }

module DSP.Filter.FIR.Kaiser (kaiser_lpf, kaiser_hpf) where

import Data.Array

import DSP.Window
import DSP.Filter.FIR.Taps

-- Set the cutoff frequency to the middle of the transition band.  This
-- equation isn't numbered.

calc_wc :: Fractional a => a -> a -> a
calc_wc wp ws = (wp + ws) / 2

-- Equation 7.90

calc_dw :: Num a => a -> a -> a
calc_dw wp ws = abs (ws - wp)

-- Equation 7.91

calc_A :: (Floating a, Ord a) => a -> a -> a
calc_A d1 d2 = -20 * logBase 10 (min d1 d2)

-- xEquation 7.92

calc_beta :: (Ord a, Floating a) => a -> a
calc_beta a | a > 50    = 0.1102 * (a - 8.7)
            | a >= 21   = 0.5842 * ((a-21) ** 0.4) + 0.07886 * (a-21)
            | otherwise = 0.0

-- Equation 7.93

calc_M :: (Integral b, RealFrac a) => a -> a -> b
calc_M a dw = ceiling ((a - 8) / (2.285 * dw))

-- Procedure on pg 455.  We should really check the peak approximation
-- error and then increase M if necessary.

-- | Designs a lowpass Kaiser filter

kaiser_lpf :: Double -- ^ wp
	   -> Double -- ^ ws
	   -> Double -- ^ dp
	   -> Double -- ^ ds
	   -> Array Int Double -- ^ h[n]

kaiser_lpf wp ws d1 d2 = window (kaiser beta m) (lpf wc m)
    where wc = calc_wc wp ws
          dw = calc_dw wp ws
          a = calc_A d1 d2
          beta = calc_beta a
          m = calc_M a dw

-- The weird case for m below is because highpass (or bandstop) filters
-- should only be Type I.  Linear phase forces a null at w=pi for Type II
-- filters, which doesn't fit well with these kinds of filters.  Again,
-- we should really check the peak approximation error and then increase
-- M (by two) if necessary.

-- | Designs a highpass Kaiser filter

kaiser_hpf :: Double -- ^ wp
	   -> Double -- ^ ws
	   -> Double -- ^ dp
	   -> Double -- ^ ds
	   -> Array Int Double -- ^ h[n]

kaiser_hpf wp ws d1 d2 = window (kaiser beta m) (hpf wc m)
    where wc = calc_wc wp ws
          dw = calc_dw wp ws
          a = calc_A d1 d2
          beta = calc_beta a
          m = ceilingEven (calc_M a dw)

ceilingEven :: Integral b => b -> b
ceilingEven x = x + mod (-x) 2
