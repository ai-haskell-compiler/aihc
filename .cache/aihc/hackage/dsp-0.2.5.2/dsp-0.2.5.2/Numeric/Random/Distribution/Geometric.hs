-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Geometric
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- UNTESTED
--
-- Module for transforming a list of uniform random variables into a
-- list of geometric random variables.
--
-- @ P{X=n} = (1-p)^(n-1)*p @
--
-- Reference: Ross
--
----------------------------------------------------------------------------

module Numeric.Random.Distribution.Geometric (geometric) where

-- * Functions

-- | Generates a list of geometric random variables from a list
-- of uniforms

geometric :: Double    -- ^ p
	  -> [Double]  -- ^ U
	  -> [Double]  -- ^ X

geometric p us = map (\u -> 1 + log u / log (1 - p)) us

