-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Statistics.TTest
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- UNTESTED: DO NOT USE
--
-- Student's t-test functions
--
-- Reference: NRiC
--
-----------------------------------------------------------------------------

module Numeric.Statistics.TTest (ttest, tutest, tptest) where

import Numeric.Statistics.Covariance
import Numeric.Statistics.Moment

ttest :: [Double] -- ^ X1
      -> [Double] -- ^ X2
      -> Double   -- ^ t

ttest x1 x2 = t
    where t = (mu1 - mu2) / s_d
	  mu1 = Prelude.sum x1 / n1
	  mu2 = Prelude.sum x2 / n2
	  v1  = Prelude.sum (map (\x -> (x - mu1)^(2::Int)) x1)
	  v2  = Prelude.sum (map (\x -> (x - mu2)^(2::Int)) x2)
	  n1  = fromIntegral $ length $ x1
	  n2  = fromIntegral $ length $ x2
	  s_d = sqrt (((v1 + v2) / (n1+n2-2)) * (1/n1 + 1/n2))

tutest :: [Double] -- ^ X1
       -> [Double] -- ^ X2
       -> Double   -- ^ t

tutest x1 x2 = t
    where t = (mu1 - mu2) / sqrt (var1 / n1 + var2 / n2)
	  mu1 = mean x1
	  mu2 = mean x2
	  var1 = var x1
	  var2 = var x2
	  n1  = fromIntegral $ length $ x1
	  n2  = fromIntegral $ length $ x2

tptest :: [Double] -- ^ X1
       -> [Double] -- ^ X2
       -> Double   -- ^ t

tptest x1 x2 = t
    where t = (mu1 - mu2) / s_d
	  mu1 = mean x1
	  mu2 = mean x2
	  var1 = var x1
	  var2 = var x2
	  s_d = sqrt ((var1 + var2 - 2 * cov x1 x2) / n)
	  n  = fromIntegral $ length $ x1
