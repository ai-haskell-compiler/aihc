-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Pisarenko
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains an implementation of Pisarenko Harmonic
-- Decomposition for a single real sinusoid.  For this case, eigenvalues
-- do not need to be computed.
--
-----------------------------------------------------------------------------

-- This implmentation is based off of a Matlab version by Peter
-- Kootsookos (p.kootsookos@ieee.org).

module DSP.Estimation.Frequency.Pisarenko (pisarenko) where

import DSP.Basic((^!))
import Data.Array

rss :: (Ix a, Integral a, Num b) =>
             Array a b
	  -> a
	  -> b

rss x k = sum [ x!(i+k) * x!i | i <- [0..(n-1-k)] ]
    where n = snd (bounds x) + 1

-- | Pisarenko's method for a single sinusoid

pisarenko :: (Ix a, Integral a, Floating b) => Array a b -- ^ x
	  -> b -- ^ w

pisarenko x = acos (alpha / 2)
    where alpha = (rss2 + sqrt (rss2^!2 + 8*rss1^!2)) / (2*(rss1 + eps))
	  rss1 = rss x 1
	  rss2 = rss x 2
	  eps = 1.0e-15
