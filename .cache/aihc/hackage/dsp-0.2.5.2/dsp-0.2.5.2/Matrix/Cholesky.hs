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
-- This module contains a routine that solves the system Ax=b, where A
-- is positive definite, using Cholesky decomposition.
--
-----------------------------------------------------------------------------


module Matrix.Cholesky (cholesky) where

import Data.Array
import Data.Complex

-- * Functions

-- Formulas 2.53--2.55 in Kay

cholesky :: (Ix a, Integral a, RealFloat b) => Array (a,a) (Complex b) -- ^ A
	                                    -> Array a (Complex b)     -- ^ b
					    -> Array a (Complex b)     -- ^ x
cholesky a b = x
    where y = array (1,n) ((1,b!1) : [ (k, b!k - sum [ l!(k,j) * y!j | j <- [1..(k-1)] ] ) | k <- [2..n] ])
	  x = array (1,n) ((n, y!n / d!n) : [ (k, y!k / d!k - sum [ (conjugate (l!(j,k))) * x!j | j <- [(k+1)..n] ] ) | k <- (reverse [1..(n-1)]) ])
	  l = array ((1,1),(n,n)) [ ((i,j), lij i j) | i <- [2..n], j <- [1..(i-1)] ]
	  lij i j | j==1      = a!(i,1) / d!1
		    | otherwise = a!(i,j) / d!j - sum [ l!(i,k) * d!k * (conjugate (l!(j,k))) / d!j | k <- [1..(j-1)] ]
	  d = array (1,n) ((1, a!(1,1)) : [ (i, a!(i,i) - sum [ d!k * ((abs (l!(i,k)))^(2::Int)) | k <- [1..(i-1)] ] ) | i <- [2..n]])
	  ((_,_),(n,_)) = bounds a
