-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Exponential
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
-- list of exponential random variables.
--
-- @ f(x) = lambda * exp(-lambda*x) @
--
-- @ F(x) = 1 - exp(-lambda*x) @
--
-- @ lambda = 1 \/ mu @
--
-- Reference: Ross
--
----------------------------------------------------------------------------

-- TODO: Marsaglia's ziggurat method

module Numeric.Random.Distribution.Exponential (exponential_inv) where

-- * Functions

-- | Generates a list of exponential random variables from a list
-- of uniforms via the inverse transformation method
--
-- @ F(x) = 1 - exp(-lambda*x) @
--
-- @ F^-1(x) = -log(1 - x) \/ lambda@

exponential_inv ::  Double   -- ^ lambda
		-> [Double]  -- ^ U
		-> [Double]  -- ^ X

exponential_inv lambda us = map (\u -> -log (1 - u) / lambda) us
