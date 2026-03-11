-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Gamma
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
-- list of gamma random variables.
--
-- @ f(x) = lambda * exp(-lambda*x) * (lambda * x)^(t-1) \/ Gamma(t) @
--
-- Reference: Ross
--
----------------------------------------------------------------------------

module Numeric.Random.Distribution.Gamma (gamma) where

-- * Functions

-- | Generates a list of gamma random variables from a list
-- of uniforms via the inverse transformation method

gamma :: Int       -- ^ n
      -> Double    -- ^ lambda
      -> [Double]  -- ^ U
      -> [Double]  -- ^ X

gamma n lambda u = x : gamma n lambda u'
    where x = -log (product (take n u)) / lambda
	  u' = drop n u
