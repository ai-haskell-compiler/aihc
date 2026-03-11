-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Approximation.Chebyshev
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Function approximation using Chebyshev polynomials
--
-- @ f(x) = ( sum (k=0..N-1) c_k * T_k(x) ) - 0.5 * c_0 @
--
-- over the interval @ [a,b] @
--
-- Reference: NRiC
--
-----------------------------------------------------------------------------

module Numeric.Approximation.Chebyshev (cheby_approx,
					cheby_eval) where

import Data.Array

-- | Calculates the Chebyshev approximation to @f(x)@ over @[a,b]@

cheby_approx :: (Double -> Double) -- ^ f(x)
	     -> Double             -- ^ a
	     -> Double             -- ^ b
	     -> Int                -- ^ N
	     -> [Double]           -- ^ c_n

cheby_approx f a b n = f''
    where a' = 0.5 * (b - a)
	  b' = 0.5 * (b + a)
	  y = [ a' * cos (pi * (fromIntegral k + 0.5) / fromIntegral n) + b' | k <- [0..n-1] ]
	  f' = map f y
	  f'' = [ 2 * sum (zipWith (*) f' [ cos (pi * fromIntegral j * (fromIntegral k + 0.5) / fromIntegral n) | k <- [0..n-1] ]) / fromIntegral n | j <- [0..n-1] ]

-- | Evaluates the Chebyshev approximation to @f(x)@ over @[a,b]@ at @x@

cheby_eval :: [Double] -- ^ c_n
	   -> Double   -- ^ a
	   -> Double   -- ^ b
	   -> Double   -- ^ x
	   -> Double   -- ^ f(x)

cheby_eval f a b x = y * d!1 - d!2 + 0.5 * c!0
    where y = (2 * x - a - b) / (b - a)
          c = listArray (0,n) f
	  d = array (1,n+2) ((n+2,0) : (n+1,0) : [ (j,2*y*d!(j+1) - d!(j+2) + c!j) | j <- [1..n] ])
	  n = length f - 1
