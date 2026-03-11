-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Matchedz
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Matched-z transform
--
-- References: Proakis and Manolakis, Rabiner and Gold
--
-----------------------------------------------------------------------------


module DSP.Filter.IIR.Matchedz (matchedz) where

import Polynomial.Basic
import Polynomial.Roots

import Data.Complex

-- | Performs the matched-z transform

matchedz :: Double -- ^ T_s
	 -> ([Double],[Double]) -- ^ (b,a)
	 -> ([Double],[Double]) -- ^ (b',a')

matchedz ts (num,den) = (num',den')
    where zeros  = roots 1.0e-12 1000 $ map (:+ 0) $ num
	  poles  = roots 1.0e-12 1000 $ map (:+ 0) $ den
	  zeros' = map exp $ map (* (ts :+ 0)) $ zeros
	  poles' = map exp $ map (* (ts :+ 0)) $ poles
	  num'   = map realPart $ roots2poly zeros'
	  den'   = map realPart $ roots2poly poles'
