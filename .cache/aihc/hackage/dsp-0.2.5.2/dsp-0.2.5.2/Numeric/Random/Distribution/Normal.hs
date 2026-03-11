-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Normal
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for transforming a list of uniform random variables into a
-- list of normal random variables.
--
-----------------------------------------------------------------------------

-- TODO: The speedup from Ross for the A-R method

-- TODO: Marsaglia's ziggurat method

-- TODO: Leva' method

-- TODO: Ahrens-Dieter method

module Numeric.Random.Distribution.Normal (normal_clt, normal_bm,
                                           normal_ar, normal_r) where

import DSP.Basic (interleave, uninterleave, norm2sqr, toMaybe)
import Data.Maybe (mapMaybe)

-- * Functions

-- adjust takes a unit normal random variable and sets the mean and
-- variance to whatever is needed.

adjust :: (Double,Double) -> Double -> Double
adjust (mu,sigma) x = mu + sigma * x

-- | Normal random variables via the Central Limit Theorm (not explicity
-- given, but see Ross)
--
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-n/2,n/2]

normal_clt :: Int             -- ^ Number of uniforms to sum
           -> (Double,Double) -- ^ (mu,sigma)
           -> [Double]        -- ^ U
           -> [Double]        -- ^ X

normal_clt n muSigma u = map (adjust muSigma) $ normal' u
    where normal' us = var_adj * ((sum $ take n us) - mean_adj) : (normal' $ drop n us)
          var_adj  = sqrt $ 12 / fromIntegral n
          mean_adj = fromIntegral n / 2

-- | Normal random variables via the Box-Mueller Polar Method (Ross, pp
-- 450--452)
--
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-8.57,8.57] assuing that the uniform RNG is really giving full
-- precision for doubles.

normal_bm :: (Double,Double) -- ^ (mu,sigma)
          -> [Double]        -- ^ U
          -> [Double]        -- ^ X

normal_bm muSigma =
   map (adjust muSigma) .
   uncurry interleave . unzip . mapMaybe normalDist .
   uncurry zip . uninterleave . map (\u -> 2*u-1)

normalDist :: (Floating a, Ord a) => (a,a) -> Maybe (a,a)
normalDist z@(x,y) =
   let norm2 = norm2sqr z
       p = sqrt (-2 * log norm2) / norm2
   in  toMaybe (norm2<=1) (p*x, p*y)


-- | Acceptance-Rejection Method (Ross, pp 448--450)
--
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-36.74,36.74] assuming that the uniform RNG is really giving full
-- precision for doubles.

normal_ar :: (Double,Double) -- ^ (mu,sigma)
          -> [Double]        -- ^ U
          -> [Double]        -- ^ X

normal_ar muSigma u = map (adjust muSigma) $ normal' u
    where normal' (u1:u2:u3:us) | y > 0     = z : normal' us
                                | otherwise = normal' (u3:us)
              where y1 = -log u1
                    y2 = -log u2
                    y  = y2 - (y1 - 1)^(2::Int) / 2
                    z = if u3 <= 0.5 then y1 else -y1
          normal' _ = error "normal_ar: infinite list of random variables expected"

-- | Ratio Method (Kinderman-Monahan) (Knuth, v2, 2ed, pp 125--127)
--
-- If mu=0 and sigma=1, then this will generate numbers in the range
-- [-1e15,1e15] (?) assuming that the uniform RNG is really giving full
-- precision for doubles.

normal_r :: (Double,Double) -- ^ (mu,sigma)
         -> [Double]        -- ^ U
         -> [Double]        -- ^ X

normal_r muSigma = map (adjust muSigma) . normal'
    where normal' (u:v:us) | x^(2::Int) <= -4 * log u = x : normal' us
                           | otherwise                = normal' us
              where x = a * (v - 0.5) / u
                    a = sqrt $ 8 / exp 1 -- 1.71552776992141359295
          normal' _ = error "normal_r: infinite list of random variables expected"
