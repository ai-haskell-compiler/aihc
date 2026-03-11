-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Spectrum.Pink
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for pinking noise
--
-- <http://www.firstpr.com.au/dsp/pink-noise/>
--
-----------------------------------------------------------------------------

module Numeric.Random.Spectrum.Pink (kellet,
				     voss) where

import DSP.Basic (downsample, upsampleAndHold)
import Data.List (tails)

-------------------------------------------------------------------------------

-- rb-j filter

-- pole            zero
-- ----            ----
-- 0.99572754      0.98443604
-- 0.94790649      0.83392334
-- 0.53567505      0.07568359

-------------------------------------------------------------------------------

-- | Kellet's filter

-- b0 = 0.99886 * b0 + white * 0.0555179;
-- b1 = 0.99332 * b1 + white * 0.0750759;
-- b2 = 0.96900 * b2 + white * 0.1538520;
-- b3 = 0.86650 * b3 + white * 0.3104856;
-- b4 = 0.55000 * b4 + white * 0.5329522;
-- b5 = -0.7616 * b5 - white * 0.0168980;
-- pink = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
-- b6 = white * 0.115926;

kellet :: [Double] -- ^ noise
       -> [Double] -- ^ pinked noise

kellet w = kellet' w 0 0 0 0 0 0 0
    where kellet' []         _  _  _  _  _  _  _  = []
          kellet' (white:ws) b0 b1 b2 b3 b4 b5 b6 = pink : kellet' ws b0' b1' b2' b3' b4' b5' b6'
	      where b0' = 0.99886 * b0 + white * 0.0555179
		    b1' = 0.99332 * b1 + white * 0.0750759
		    b2' = 0.96900 * b2 + white * 0.1538520
		    b3' = 0.86650 * b3 + white * 0.3104856
		    b4' = 0.55000 * b4 + white * 0.5329522
		    b5' = -0.7616 * b5 - white * 0.0168980
		    pink = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362
		    b6' = white * 0.115926

-------------------------------------------------------------------------------

-- voss algorithm

add :: Num a => [[a]] -> [a]
add = foldl1 (zipWith (+))

split :: Int -> [a] -> [[a]]
split n = take n . map (downsample n) . (++repeat []) . tails


mkOctaves :: [[a]] -> [[a]]
mkOctaves = mkOctaves' 1
    where mkOctaves' _ []       = []
	  mkOctaves' n (xs:xss) = upsampleAndHold n xs : mkOctaves' (2*n) xss

-- | Voss's algorithm
--
-- UNTESTED, but the algorithm looks like it is working based on my hand
-- tests.

-- TODO: Since the input noise is consumed in different speed for the octaves
-- the function will certainly leak memory.

voss :: Int      -- ^ number of octaves to sum
     -> [Double] -- ^ noise
     -> [Double] -- ^ pinked noise

voss n w = add $ mkOctaves $ split n w

-------------------------------------------------------------------------------

-- voss-mccartney algorithm

-------------------------------------------------------------------------------

-- vm w =
