-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Spectrum.White
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Function for white noise
--
-- This is pretty useless, but it is here to be comprehensive
--
-----------------------------------------------------------------------------

module Numeric.Random.Spectrum.White (white) where

white :: [Double] -- ^ noise
      -> [Double] -- ^ white noise

white = id
