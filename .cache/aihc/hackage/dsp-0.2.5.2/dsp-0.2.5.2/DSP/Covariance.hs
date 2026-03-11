-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Covariance
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains routines to perform cross- and auto-covariance
-- These formulas can be found in most DSP textbooks.
--
-- In the following routines, x and y are assumed to be of the same
-- length.
--
-----------------------------------------------------------------------------


-- TODO: fix these routines to handle the case were x and y are different
-- lengths.

-- TODO: Cxx(X) = Var(X), but I'm not sure how the lag works into that

module DSP.Covariance (cxy, cxy_b, cxy_u, cxx, cxx_b, cxx_u) where

import Data.Array
import Data.Complex

import DSP.Correlation
import Numeric.Statistics.Moment

-- | raw cross-covariance
--
-- We define covariance in terms of correlation.
--
-- Cxy(X,Y) = E[(X - E[X])(Y - E[Y])]
--          = E[XY] - E[X]E[Y]
--          = Rxy(X,Y) - E[X]E[Y]

-- cxy x y k | k >= 0 = sum [ (x!(i+k) - xm) * ((conjugate (y!i)) - ym) | i <- [0..(n-1-k)] ]

cxy :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> Array a (Complex b) -- ^ y
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xy[k]

cxy x y k =
   if k >= 0
     then rxy x y k - xm * ym
     else conjugate (cxy y x (-k))
    where xm = mean (elems x)
          ym = mean (map conjugate (elems y))

-- | raw auto-covariance
--
-- Cxx(X,X) = E[(X - E[X])(X - E[X])]
--          = E[XX] - E[X]E[X]
--          = Rxy(X,X) - E[X]^2

-- We define this explicitly to prevent the mean from being calculated
-- twice.

-- cxx x k | k >= 0 = sum [ (x!(i+k) - xm) * (conjugate (x!i - xm)) | i <- [0..(n-1-k)] ]

cxx :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xx[k]

cxx x k =
   if k >= 0
     then rxx x k - mean (elems x) ^ (2::Int)
     else conjugate (cxx x (-k))

-- Define the biased and unbiased versions in terms of the above.

-- | biased cross-covariance

cxy_b :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> Array a (Complex b) -- ^ y
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xy[k] \/ N

cxy_b x y k = cxy x y k / fromIntegral n
    where n = snd (bounds x) + 1

-- | unbiased cross-covariance

cxy_u :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> Array a (Complex b) -- ^ y
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xy[k] \/ (N-k)

cxy_u x y k = cxy x y k / fromIntegral (n - abs k)
    where n = snd (bounds x) + 1

-- | biased auto-covariance

cxx_b :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xx[k] \/ N

cxx_b x k = cxx x k / fromIntegral n
    where n = snd (bounds x) + 1

-- | unbiased auto-covariance

cxx_u :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ C_xx[k] \/ (N-k)

cxx_u x k = cxx x k / fromIntegral (n - abs k)
    where n = snd (bounds x) + 1
