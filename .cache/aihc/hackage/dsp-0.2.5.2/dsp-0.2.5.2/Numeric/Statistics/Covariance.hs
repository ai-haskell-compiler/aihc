-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.Covariance
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- UNTESTED
--
-- Simple module for computing the covariance of two lists
--
-- @ Cov(X1,X2) = 1\/(N-1) * sum (i=1..N) ((x1_i - mu1)(x2_i - mu2)) @
--
-- Reference: Ross, NRiC
--
-----------------------------------------------------------------------------

module Numeric.Statistics.Covariance (cov) where

import Numeric.Statistics.Moment

cov :: (Fractional a) => [a] -> [a] -> a
cov x1 x2 = Prelude.sum (zipWith (*) (map f1 x1) (map f2 x2)) / (n - 1)
    where mu1 = mean x1
	  mu2 = mean x2
	  n = fromIntegral $ length $ x1
	  f1 = \x -> (x - mu1)^(2::Int)
	  f2 = \x -> (x - mu2)^(2::Int)
