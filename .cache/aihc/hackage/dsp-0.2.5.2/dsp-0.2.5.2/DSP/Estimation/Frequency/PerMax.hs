-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Frequency.PerMax
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements an algorithm to maximize the peak value of a
-- DFT\/FFT.  It is based off an aticle by Mark Sullivan from Personal
-- Engineering Magazine.
--
-- Maximizes
--
-- @S(w) = 1\/N * sum(k=0,N-1) |x[k] * e^(-jwk)|^2@
--
-- which is equivalent to solving
--
-- @S'(w) = Im{X(w) * ~Y(w)} = 0@
--
-- where
--
-- @X(w) =         sum(k=0,N-1) (x[k] * e^(-jwk))@
-- @Y(w) = X'(w) = sum(k=0,N-1) (k * x[k] * e^(-jwk))@
--
-- This algorithm used the bisection method for finding the zero of a
-- function.  The search area is +- half a bin width.
--
-- Regula falsi requires an additional (x,f(x)) pair which is expensive
-- in this case.  Newton's method could be used but requires S''(w),
-- which takes twice as long to caculate as S'(w).  Brent's method may be
-- best here, but it also requires three (x,f(x)) pairs
--
-----------------------------------------------------------------------------

module DSP.Estimation.Frequency.PerMax (permax) where

import Data.Array
import Data.Complex

-- TODO: could we use sinc interpolation instead of calc_x,calc_y for
-- the off-bin values?

-- TODO: the twiddle factor in calc_x,calc_y can be computed
-- recursively

-- TODO: the twiddle factor in calc_x,calc_y can be shared


-- calc_x x w = sum [ x!k * cis (-w * fromIntegral k) | k <- [0..(n-1)] ]
--      where n = snd (bounds x) + 1

calc_x :: (RealFloat a, Ix i) =>
          Array i (Complex a) -> a -> Complex a
calc_x x w = sum $ zipWith (*) (elems x) (iterate (cis (-w) *) 1)

calc_y :: (RealFloat b, Ix i, Integral i) =>
          Array i (Complex b) -> b -> Complex b
calc_y x w = sum [ fromIntegral k * x!k * cis (-w * fromIntegral k) | k <- [0..(n-1)] ]
    where n = snd (bounds x) + 1

-- | Discrete frequency periodigram maximizer

permax :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ X[k]
       -> a -- ^ k
       -> b -- ^ w

permax x k = permax' x (w-d) (w+d)
    where w = 2 * pi * fromIntegral k / fromIntegral n
          d = 1 / fromIntegral (2*n) -- half a bin width
          n = snd (bounds x) + 1

permax' :: (RealFloat b, Ix i, Integral i) =>
           Array i (Complex b) -> b -> b -> b
permax' x w0 w1 | w1-w0 < eps = wmid
                | otherwise   = if signum t0 == signum tm
                                then permax' x wmid w1
                                else permax' x w0   wmid
    where t0 = imagPart ((calc_x x w0)   * (conjugate (calc_y x w0)))
          tm = imagPart ((calc_x x wmid) * (conjugate (calc_y x wmid)))
--        t1 = imagPart ((calc_x x w1)   * (conjugate (calc_y x w1)))
          wmid = (w0 + w1) / 2 -- bisection method
--          wmid = w1 - t1 * (w1 - w0) / (t1 - t0) -- regula falsi
          eps = 1.0e-6
--        n = snd (bounds x) + 1
