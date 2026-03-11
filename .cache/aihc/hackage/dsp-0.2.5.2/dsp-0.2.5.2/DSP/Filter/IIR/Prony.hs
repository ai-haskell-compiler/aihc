-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Prony
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General case of Prony's Method where K > p+q
--
-- References: L&I, Sect 8.1; P&B, Sect 7.5; P&M, Sect 8.5.2
--
-- Notation follows L&I
--
-----------------------------------------------------------------------------

-- TODO: Handle rank deficiencies of G3 gracefully.  Can/should we
-- generate a (K/2+1) by (K/2+1) G2, and set p=q=rank(G2)?  Need SVD to
-- compute rank, though.

module DSP.Filter.IIR.Prony (prony) where

import Data.Array

import Matrix.Matrix
import Matrix.LU

{------------------------------------------------------------------------------

Case 1: K=p+q

a = array (0,p)
b = array (0,q)

g1 : q+1 by p+1
g2 : p   by p+1
g3 : p   by p

We do not define G1 and G2, but

mg2 = array ((1,1),(p,p+1)) [ ((i,j), g!(p+i+1-j)) | j <- [1..p+1], i <- [1..p] ]

prony p q g = (a,b)
    where mg3 = array ((1,1),(p,p)) [ ((i,j), g!(p+i-j)) | j <- [1..p], i <- [1..p] ]
          g1  = array (1,p) [ (i, g!(p+i)) | i <- [1..p] ]
          a'  = solve mg3 (fmap negate g1)
          a   = array (0,p) $ (0,1) : [ (i,a'!i) | i <- [1..p] ]
          b   = listArray (0,q) [ sum [ a!j * g!(i-j) | j <- [0..(min i p)] ] | i <- [0..q] ]

Test case, pg 422

g = listArray (0,6) [ 1, 18, 9, 2, 1, 2/9, 1/9 ] :: Array Int Double

------------------------------------------------------------------------------}

-- Case 2: K>p+q

-- a = array (0,p)
-- b = array (0,q)

-- g1 : q+1 by p+1
-- g2 : K-q by p+1
-- g3 : K-q by p

-- We need gi for the q<p cases because these generate zero elements in
-- G3, and this is the easiest way to take care of that.

-- mg1 = array ((1,1),(q+1,p+1)) [ ((i,j), gi (i-j)) | j <- [1..p+1], i <- [1..q+1] ]
-- mg2 = array ((1,1),(k-q,p+1)) [ ((i,j), gi (q+i-j+1)) | j <- [1..p+1], i <- [1..k-q] ]

-- | Implementation of Prony's method

prony :: Int -- ^ p
      -> Int -- ^ q
      -> Array Int Double -- ^ g[n]
      -> (Array Int Double, Array Int Double) -- ^ (b,a)

prony p q g = (b,a)
    where k   = snd $ bounds g
	  gi i | i < 0     = 0
	       | i > k     = 0
	    | otherwise = g!i
	  mg3 = array ((1,1),(k-q,p)) [ ((i,j), gi (q+i-j)) | j <- [1..p], i <- [1..k-q] ]
	  g1  = array (1,k-q) [ (i, gi (q+i)) | i <- [1..k-q] ]
	  a'  = solve (mm_mult (m_trans mg3) mg3) (fmap negate (mv_mult (m_trans mg3) g1))
          a   = array (0,p) $ (0,1) : [ (i,a'!i) | i <- [1..p] ]
	  b   = listArray (0,q) [ sum [ a!j * gi (i-j) | j <- [0..(min i p)] ] | i <- [0..q] ]
