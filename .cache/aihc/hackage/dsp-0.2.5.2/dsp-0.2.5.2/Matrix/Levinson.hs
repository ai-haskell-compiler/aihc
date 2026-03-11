-----------------------------------------------------------------------------
-- |
-- Module      :  Matrix.Cholesky
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains an implementation of Levinson-Durbin recursion.
--
-----------------------------------------------------------------------------

module Matrix.Levinson (levinson) where

import Data.Array
import Data.Complex

-- * Functions

-- Section 6.3.3 in Kay, formulas 6.46--6.48

-- TODO: rho is typing as complex, but it is real
-- TODO: add stepdown function
-- TODO: some applications may want all model estimations from [1..p]

-- | levinson takes an array, r, of autocorrelation values, and a
-- model order, p, and returns an array, a, of the model estimate and
-- rho, the noise power.

levinson :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ r
	                                    -> a                   -- ^ p
					    -> (Array a (Complex b),b) -- ^ (a,rho)

levinson r p = (array (1,p) [ (k, a!(p,k)) | k <- [1..p] ], realPart (rho!p))
    where a   = array ((1,1),(p,p)) [ ((k,i), ak k i) | k <- [1..p], i <- [1..k] ]
	  rho = array (1,p) [ (k, rhok k) | k <- [1..p] ]
	  ak 1 1             = -r!1 / r!0
	  ak k i | k==i      = -(r!k + sum [ a!(k-1,l) * r!(k-l) | l <- [1..(k-1)] ]) / rho!(k-1)
		 | otherwise = a!(k-1,i) + a!(k,k) * (conjugate (a!(k-1,k-i)))
	  rhok 1 = (1 - (abs (a!(1,1)))^(2::Int)) * r!0
	  rhok k = (1 - (abs (a!(k,k)))^(2::Int)) * rho!(k-1)

-- r = array (0,2) [ (0, (2.0 :+ 0.0)), (1, ((-1.0) :+ 1.0)), (2, (0.0 :+ 0.0)) ]
-- a = fst (levinson r 2)

-- verify = a == array (1,2) [(1,1.0 :+ (-1.0)),(2,0.0 :+ (-1.0))]
