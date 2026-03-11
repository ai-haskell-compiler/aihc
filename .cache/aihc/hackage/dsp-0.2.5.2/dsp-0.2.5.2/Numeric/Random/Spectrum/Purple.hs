-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Spectrum.Purple
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Function for purple noise, which is differentiated white noise
--
-- This currently just does a simple first-order difference.  This is
-- equivalent to filtering the white noise with @ h[n] = [1,-1] @
-- A better solution would be to use a proper FIR differentiator.
--
-----------------------------------------------------------------------------

module Numeric.Random.Spectrum.Purple (purple) where

purple :: [Double] -- ^ noise
       -> [Double] -- ^ purple noise

purple xs = zipWith (-) xs (0:xs)
