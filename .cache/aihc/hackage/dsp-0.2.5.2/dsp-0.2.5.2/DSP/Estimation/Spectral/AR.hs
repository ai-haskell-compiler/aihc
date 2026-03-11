-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Spectral.AR
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a few algorithms for AR parameter estimation.
-- Algorithms are taken from Steven M. Kay, /Modern Spectral Estimation:
-- Theory and Application/, which is one of the standard texts on the
-- subject.  When possible, variable conventions are the same in the code
-- as they are found in the text.
--
-----------------------------------------------------------------------------

module DSP.Estimation.Spectral.AR where

import DSP.Correlation
import Matrix.Levinson
import Matrix.Cholesky

import DSP.Basic((^!))
import Data.Array
import Data.Complex

-- * Functions

-------------------------------------------------------------------------------
-- ar_yw x p
-------------------------------------------------------------------------------

-- Section 7.3 in Kay

-- | Computes an AR(p) model estimate from x using the Yule-Walker method

ar_yw :: (Ix a, Integral a, RealFloat b) => Array a (Complex b)      -- ^ x
      -> a                        -- ^ p
      -> (Array a (Complex b), b) -- ^ (a,rho)

ar_yw x p = levinson r p
    where r = array (0,p) [ (k, rxx_b x k) | k <- [0..p] ]

-------------------------------------------------------------------------------
-- ar_cov x p
-------------------------------------------------------------------------------

-- Section 7.4 in Kay, but I factored out the 1/(N-p) term, and only
-- generate the lower triangle of cxx

-- TODO: use modified Prony method instead of matrix solver

-- | Computes an AR(p) model estimate from x using the covariance method

ar_cov :: (Ix a, Integral a, RealFloat b) => Array a (Complex b)      -- ^ x
       -> a                        -- ^ p
       -> (Array a (Complex b), b) -- ^ (a,rho)

ar_cov x p = (a, sig2  / (fromIntegral (n-p)))
    where a = cholesky m v
 	  sig2 = realPart ((cxx 0 0) + sum [ a!k * (cxx 0 k) | k <- [1..p] ])
	  m = array ((1,1),(p,p)) [ ((j,k), cxx j k) | j <- [1..p], k <- [1..j] ]
	  v = array (1,p) [ (j, -(cxx j 0)) | j <- [1..p] ]
	  cxx j k = sum [ (conjugate (x!(i-j))) * x!(i-k) | i <- [p..(n-1)] ]
	  n = snd (bounds x) + 1

-------------------------------------------------------------------------------
-- ar_mcov x p
-------------------------------------------------------------------------------

-- Section 7.5 in Kay, but I factored out the 1/(2(N-p)) term, and only
-- generate the lower triangle of cxx

-- | Computes an AR(p) model estimate from x using the modified covariance method

ar_mcov :: (Ix a, Integral a, RealFloat b) => Array a (Complex b)      -- ^ x
        -> a                        -- ^ p
        -> (Array a (Complex b), b) -- ^ (a,rho)

ar_mcov x p = (a, sig2  / (fromIntegral (2*(n-p))))
    where a = cholesky m v
	  sig2 = realPart ((cxx 0 0) + sum [ a!k * (cxx 0 k) | k <- [1..p] ])
	  m = array ((1,1),(p,p)) [ ((j,k), cxx j k) | j <- [1..p], k <- [1..j] ]
	  v = array (1,p) [ (j, -(cxx j 0)) | j <- [1..p] ]
	  cxx j k = (sum [ (conjugate (x!(i-j))) * x!(i-k) | i <- [p..(n-1)] ] + sum [ x!(i+j) * (conjugate (x!(i+k))) | i <- [0..(n-1-p)] ])
	  n = snd (bounds x) + 1

-------------------------------------------------------------------------------
-- ar_burg x p
-------------------------------------------------------------------------------

-- Section 7.6 in Kay

-- TODO: rho doesn't need to be an array
-- TODO: kk doesn't need to be an array
-- TODO: ef and eb don't need to be 2-D arrays

-- | Computes an AR(p) model estimate from x using the Burg' method

ar_burg :: (Ix a, Integral a, RealFloat b) => Array a (Complex b)      -- ^ x
        -> a                        -- ^ p
        -> (Array a (Complex b), b) -- ^ (a,rho)

ar_burg x p = (array (1,p) [ (k, a!(p,k)) | k <- [1..p] ], realPart (rho!p))
    where a = array ((1,1),(p,p)) [ ((k,i), ak k i) | k <- [1..p], i <- [1..k] ]
	  ak k i | i==k      = kk!k
		 | otherwise = a!(k-1,i) + kk!k * (conjugate (a!(k-1,k-i)))
	  kk = array (1,p) [ (k, -2 * sum [ ef!((k-1),i) * (conjugate (eb!(k-1,i-1))) | i <- [k..(n-1)] ] / sum [ abs (ef!(k-1,i)) ^! 2 + abs (eb!(k-1,i-1)) ^! 2 | i <- [k..(n-1)] ]) | k <- [1..p] ]
	  rho = array (0,p) ((0, rxx_b x 0) : [ (k, (1 - abs (kk!k) ^! 2) * rho!(k-1)) | k <- [1..p] ])
	  ef = array ((0,1),(p,n-1)) [ ((k,i), efki k i) | k <- [0..p], i <- [(k+1)..(n-1)] ]
	  eb = array ((0,0),(p,n-2)) [ ((k,i), ebki k i) | k <- [0..p], i <- [k..(n-2)] ]
	  efki 0 i = x!i
	  efki k i = ef!(k-1,i) + kk!k * eb!(k-1,i-1)
	  ebki 0 i = x!i
	  ebki k i = eb!(k-1,i-1) + (conjugate (kk!k)) * ef!(k-1,i)
	  n = snd (bounds x) + 1

-------------------------------------------------------------------------------
-- ar_rmle x p
-------------------------------------------------------------------------------

