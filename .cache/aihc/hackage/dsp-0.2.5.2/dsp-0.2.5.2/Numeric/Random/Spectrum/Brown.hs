-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Spectrum.Brown
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Function for brown noise, which is integrated white noise
--
-----------------------------------------------------------------------------

module Numeric.Random.Spectrum.Brown (brown) where

brown :: [Double] -- ^ noise
      -> [Double] -- ^ brown noise

brown = scanl1 (+)

