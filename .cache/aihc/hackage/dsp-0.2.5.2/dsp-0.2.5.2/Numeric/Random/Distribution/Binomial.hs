-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Binomial
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
-- list of binomial random variables.
--
-- Reference: Ross
--
----------------------------------------------------------------------------

module Numeric.Random.Distribution.Binomial (binomial) where

-- * Functions

-- | Generates a list of binomial random variables from a list
-- of uniforms

binomial :: Int       -- ^ n
	 -> Double    -- ^ p
	 -> [Double]  -- ^ U
	 -> [Double]  -- ^ X

binomial n p us = sum xi : binomial n p (drop n us)
    where xi = map (\u -> if u < p then 1 else 0) (take n us)


