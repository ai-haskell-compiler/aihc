-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Correlation
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains routines to perform cross- and auto-correlation.
-- These formulas can be found in most DSP textbooks.
--
-- In the following routines, x and y are assumed to be of the same
-- length.
--
-----------------------------------------------------------------------------

module DSP.Correlation (rxy, rxy_b, rxy_u, rxx, rxx_b, rxx_u, test) where

import Data.Array
import Data.Complex

-- * Functions

-- TODO: fix these routines to handle the case were x and y are different
-- lengths.

-- | raw cross-correllation

rxy :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> Array a (Complex b) -- ^ y
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ R_xy[k]

rxy x y k =
   if k >= 0
     then sum [ x!(i+k) * conjugate (y!i) | i <- [0..(n-1-k)] ]
     else conjugate (rxy y x (-k))
    where n = snd (bounds x) + 1

-- | biased cross-correllation

rxy_b :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                   -> Array a (Complex b) -- ^ y
                                   -> a                   -- ^ k
                                   -> Complex b           -- ^ R_xy[k] \/ N

rxy_b x y k = rxy x y k / fromIntegral n
    where n = snd (bounds x) + 1

-- | unbiased cross-correllation

rxy_u :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                   -> Array a (Complex b) -- ^ y
                                   -> a                   -- ^ k
                                   -> Complex b           -- ^ R_xy[k] \/ (N-k)

rxy_u x y k = rxy x y k / fromIntegral (n - abs k)
    where n = snd (bounds x) + 1

-- autocorrellation

-- We define autocorrelation in terms of the cross correlation routines.

-- | raw auto-correllation

rxx :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                 -> a                   -- ^ k
                                 -> Complex b           -- ^ R_xx[k]

rxx   x k = rxy   x x k

-- | biased auto-correllation

rxx_b :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                   -> a                   -- ^ k
                                   -> Complex b           -- ^ R_xx[k] \/ N

rxx_b x k = rxy_b x x k

-- | unbiased auto-correllation

rxx_u :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x
                                   -> a                   -- ^ k
                                   -> Complex b           -- ^ R_xx[k] \/ (N-k)

rxx_u x k = rxy_u x x k

----------------------------------------------------------------------------
-- test routines
----------------------------------------------------------------------------

xt, yt :: Array Int (Complex Double)
xt = array (0,4)
   [ (0, 1 :+ 0),
     (1, 0 :+ 1),
     (2, (-1) :+  0),
     (3, 0 :+ (-1)),
     (4, 1 :+ 0) ]

yt = array (0,4)
   [ (0, 1 :+ 0),
     (1, (-1) :+ 0),
     (2, 1 :+ 0),
     (3, (-1) :+ 0),
     (4, 1 :+ 0) ]

rt :: [Complex Double]
rt = map (rxy_b xt yt) [ 0, 1, 2 ]

test :: Bool
test  =  rt == [ (0.2 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.2) ]
