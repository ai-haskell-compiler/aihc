-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Spectral.ARMA
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a few algorithms for ARMA parameter estimation.
-- Algorithms are taken from Steven M. Kay, _Modern Spectral Estimation:
-- Theory and Application_, which is one of the standard texts on the
-- subject.  When possible, variable conventions are the same in the code
-- as they are found in the text.
--
-- BROKEN: DO NOT USE
--
-----------------------------------------------------------------------------

module DSP.Estimation.Spectral.ARMA (arma_mywe) where

import Data.Array
import Data.Complex

import DSP.Correlation
-- import DSP.Estimation.Spectral.MA

-- import Matrix.LU


-- * Functions

-- | THIS DOES NOT WORK

arma_mywe ::
   (RealFloat b, Integral i, Ix i) =>
   Array i (Complex b) -> i -> i -> Array i (Complex b)
arma_mywe x p q = a'
    where r = array (q-2*p+1,q+p) [ (k, rxx_u x k) | k <- [(q-2*p+1)..(q+p)] ]
 	  a' = array (1,p) [ (k, a!(p,k)) | k <- [1..p] ]
 	  a = array ((1,1),(p,p)) [ ((k,i), ak k i) | k <- [1..p], i <- [1..k] ]
 	  b = array ((1,1),(p-1,p-1)) [ ((k,i), bk k i) | k <- [1..(p-1)], i <- [1..k] ]
 	  rho = array (1,p-1) [ (k, rhok k) | k <- [1..(p-1)] ]
 	  ak 1 1             = -r!(q+1) / r!q
	  ak k i | i==k      = -(r!(q+k) + sum [ a!(k-1,l) * r!(q+k-l) | l <- [1..(k-1)] ] ) / rho!(k-1)
 		 | otherwise = a!(k-1,i) + a!(k,k) * b!(k-1,k-i)
 	  bk 1 1             = -r!(q-1) / r!q
 	  bk k i | i==k      = -(r!(q-k) + sum [ b!(k-1,l) * r!(q-k-l) | l <- [1..(k-1)] ] ) / rho!(k-1)
 		 | otherwise = b!(k-1,i) + b!(k,k) * a!(k-1,k-i)
 	  rhok 1 = (1 - a!(1,1) * b!(1,1)) * r!q
 	  rhok k = (1 - a!(k,k) * b!(k,k)) * rho!(k-1)
