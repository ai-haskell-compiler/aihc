-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Frequency.QuinnFernandes
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of the Quinn-Fernandes algorithm for
-- estimating the frequency of a real sinusoid in noise.
--
-----------------------------------------------------------------------------

module DSP.Estimation.Frequency.QuinnFernandes (qf) where

import Data.Array

-- | The Quinn-Fernandes algorithm

qf :: (Ix a, Integral a, RealFloat b) => Array a b -- ^ y
      -> b -- ^ initial w estimate
      -> b -- ^ w

qf y w = qf' y (2 * cos w)

qf' :: (Ix a, Integral a, RealFloat b) =>
       Array a b
    -> b
    -> b
qf' y a | abs (a-b) < eps = acos(0.5 * b)
	| otherwise       = qf' y b
    where z = array (-2,n-1) ([ (-2, 0), (-1, 0) ] ++ [ (i, y!i + a * z!(i-1) - z!(i-2)) | i <- [0..(n-1)] ])
	  b = sum [ (z!i + z!(i-2)) * z!(i-1) | i <- [0..(n-1)] ] / sum [ (z!(i-1))^(2::Int) | i <- [0..(n-1)] ]
	  eps = 1.0e-6
	  n = snd (bounds y) + 1
