-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.Bilinear
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The module contains a function for performing the bilinear transform.
--
-- The input is a rational polynomial representation of the s-domain
-- function to be transformed.
--
-- In the bilinear transform, we substitute
--
-- @       2    1 - z^-1@
--
-- @s \<--  -- * --------@
--
-- @       ts   1 + z^-1@
--
-- into the rational polynomial, where ts is the sampling period.  To get
-- a rational polynomial back, we use the following method:
--
-- (1) Substitute s^n with (2\/ts * (1-z^-1))^n == [ -2\/ts, 2\/ts ]^n
--
-- 2.  Multiply the results by (1+z^-1)^n == [ 1, 1 ]^n
--
-- 3.  Add up all of the common terms
--
-- 4.  Normalize all of the coeficients by a0
--
-- where n is the maximum order of the numerator and denominator
--
-----------------------------------------------------------------------------

-- TODO: Rework to replace roots2poly using the fact that most poles
-- and\/or zeros are either complex conjugate pairs, or real only.

-- TODO: Do we want to include prewarping?

module DSP.Filter.IIR.Bilinear {- (bilinear, prewarp) -} where

import Polynomial.Basic

-- Computes (2\/ts * (1-z^-1))^n == [ -2\/ts, 2\/ts ]^n

zm :: (Integral b, Fractional a) => a -> b -> [a]
zm ts n = polypow [ -2/ts, 2/ts ] n

-- Computes (1+z^-1)^n == [ 1, 1 ]^n

zp :: (Integral b, Num a) => b -> [a]
zp n = polypow [ 1, 1 ] n

-- Step 1: Substitute s^n with (2\/ts * (1-z^-1))^n == [ -2\/ts, 2\/ts ]^n
-- in num and den

step1 :: Fractional a => a -> [a] -> [[a]]
step1 ts = step1' (0::Int)
    where step1' _ []     = []
          step1' n (x:xs) = polyscale x (zm ts n) : step1' (n+1) xs

-- Step 2: Multiply the num and den by (1+z^-1)^n == [ 1, 1 ]^n

step2 :: (Num a, Integral b) => b -> [[a]] -> [[a]]
step2 _ []     = []
step2 n (x:xs) = polymult (zp n) x : step2 (n-1) xs

-- Step 3: Add up all of the common terms

step3 :: Num a => [[a]] -> [a]
step3 x = foldr polyadd [0] x

-- Step 4: Normalize all of the coeficients by a0

step4 :: Fractional a => a -> [a] -> [a]
step4 a0 x = map (/a0) x

-- Glue it all together

-- | Performs the bilinear transform

bilinear :: Double -- ^ T_s
	 -> ([Double],[Double]) -- ^ (b,a)
	 -> ([Double],[Double]) -- ^ (b',a')

bilinear ts (num,den) = (num'', den'')
    where n = max (length num - 1) (length den - 1)
	  num' = step3 $ step2 n $ step1 ts $ num
          den' = step3 $ step2 n $ step1 ts $ den
          a0 = last den'
	  num'' = step4 a0 num'
	  den'' = step4 a0 den'

-- | Function for frequency prewarping

prewarp :: Double -- ^ w_c
	-> Double -- ^ T_s
	-> Double -- ^ W_c

prewarp wc ts = 2/ts * tan (wc / 2)

{-

-- Test, section 6.5.1 from Lyon's book

num1 = [ 17410.145 ]
den1 = [ 17410.145, 137.94536, 1 ]

(num1',den1') = bilinear 0.01 (num1,den1)

-- Test, from O&S, p 421

num2 = [ 0.202238 ]
den2 = polymult (polymult [ 0.5871, 0.3996, 1 ] [ 0.5871, 1.0836, 1 ] ) [ 0.5871, 1.4802, 1 ]

(num2',den2') = bilinear 1 (num2,den2)

bilinear ([0, 0, 0, 0, 1], reverse [ 1, 158881.5000000000000000000000, 6734684542.320000000000000000, 33433292062222.63200000000000, 26749649944094120.95199999999, 5301498365227355432.219999999, 308666240537082938598.7999999 ]) 48000

> num3 = [ 0, 0, 0, 0, 72687672654.5 ]
> den3 = reverse [ 1, 158881.5000000000000000000000, 6734684542.320000000000000000, 33433292062222.63200000000000, 26749649944094120.95199999999, 5301498365227355432.219999999, 308666240537082938598.7999999 ]

num31 = [ 0.0, 519.2365 ]
den31 = polypow [ 129.4, 1.0 ] 2

num32 = [ 0.0, 519.2365 ]
den32 = [ 676.7, 1.0 ]

num33 = [ 0.0, 519.2365 ]
den33 = [ 4636.0, 1.0 ]

num34 = [ 0.0, 519.2365 ]
den34 = polypow [ 76655.0, 1.0 ] 2

-}
