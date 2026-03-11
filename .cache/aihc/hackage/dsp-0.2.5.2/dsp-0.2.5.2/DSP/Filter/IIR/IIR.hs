-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.IIR.IIR
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- IIR functions
--
-- IMPORTANT NOTE:
--
-- Except in integrator, we use the convention that
--
-- @y[n] = sum(k=0..M) b_k*x[n-k] - sum(k=1..N) a_k*y[n-k]@
--
--
--
-- @         sum(k=0..M) b_k*z^-1@
--
-- @H(z) = ------------------------@
--
-- @       1 + sum(k=1..N) a_k*z^-1@
--
-----------------------------------------------------------------------------

-- TODO: Should these use Arrays for a and b?  Tuples?

{-

Reference:

@Book{dsp,
  author = 	 "Alan V. Oppenheim and Ronald W. Schafer",
  title = 	 "Discrete-Time Signal Processing",
  publisher = 	 "Prentice-Hall",
  year = 	 1989,
  address =	 "Englewood Cliffs",
  series =       {Prentice-Hall Signal Processing Series}
}

However, we differ in the convention of the sign of the poles, as
noted in the module header.

-}

module DSP.Filter.IIR.IIR (integrator,
    fos_df1, fos_df2, fos_df2t,
    biquad_df1, biquad_df2, biquad_df2t,
    iir_df1, iir_df2,
    -- for testing
    xt, yt, f1, f2, f3, f4, f5,
    ) where

import Data.Array

import DSP.Filter.FIR.FIR

-- | This is an integrator when a==1, and a leaky integrator when @0 \< a \< 1@.
--
--  @y[n] = a * y[n-1] + x[n]@

{-# specialize integrator :: Float -> [Float] -> [Float] #-}
{-# specialize integrator :: Double -> [Double] -> [Double] #-}

integrator :: Num a => a -- ^ a
	   -> [a] -- ^ x[n]
	   -> [a] -- ^ y[n]

integrator a x = integrator' a 0 x


integrator' :: Num a => a -> a-> [a] -> [a]
integrator' _ _  []     = []
integrator' a y1 (x:xs) = y : integrator' a y xs
    where y = a * y1 + x

-- | First order section, DF1
--
--	@v[n] = b0 * x[n] + b1 * x[n-1]@
--
--	@y[n] = v[n] - a1 * y[n-1]@

{-# specialize fos_df1 :: Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize fos_df1 :: Double -> Double -> Double -> [Double] -> [Double] #-}

fos_df1 :: Num a => a -- ^ a_1
	-> a -- ^ b_0
	-> a -- ^ b_1
	-> [a] -- ^ x[n]
	-> [a] -- ^ y[n]

fos_df1 a1 b0 b1 x = fos_df1' a1 b0 b1 0 0 x

fos_df1' :: Num a => a -> a -> a -> a -> a -> [a] -> [a]
fos_df1' _  _  _  _  _  []     = []
fos_df1' a1 b0 b1 x1 y1 (x:xs) = y : fos_df1' a1 b0 b1 x y xs
    where v = b0 * x + b1 * x1
	  y = v      - a1 * y1


-- | First order section, DF2
--
--	@w[n] = -a1 * w[n-1] + x[n]@
--
--	@y[n] = b0 * w[n] + b1 * w[n-1]@

{-# specialize fos_df2 :: Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize fos_df2 :: Double -> Double -> Double -> [Double] -> [Double] #-}

fos_df2 :: Num a => a -- ^ a_1
	-> a -- ^ b_0
	-> a -- ^ b_1
	-> [a] -- ^ x[n]
	-> [a] -- ^ y[n]

fos_df2 a1 b0 b1 x = fos_df2' a1 b0 b1 0 x

fos_df2' :: Num a => a -> a -> a -> a -> [a] -> [a]
fos_df2' _  _  _  _  []     = []
fos_df2' a1 b0 b1 w1 (x:xs) = y : fos_df2' a1 b0 b1 w xs
    where w = x - a1 * w1
          y = b0 * w + b1 * w1

-- | First order section, DF2T
--
--	@v0[n] = b0 * x[n] + v1[n-1]@
--
--	@y[n] = v0[n]@
--
--	@v1[n] = -a1 * y[n] + b1 * x[n]@

{-# specialize fos_df2t :: Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize fos_df2t :: Double -> Double -> Double -> [Double] -> [Double] #-}

fos_df2t :: Num a => a -- ^ a_1
	    -> a -- ^ b_0
	    -> a -- ^ b_1
	    -> [a] -- ^ x[n]
	    -> [a] -- ^ y[n]

fos_df2t a1 b0 b1 x = fos_df2t' a1 b0 b1 0 x

fos_df2t' :: Num a => a -> a -> a -> a -> [a] -> [a]
fos_df2t' _  _  _  _   []     = []
fos_df2t' a1 b0 b1 v11 (x:xs) = y : fos_df2t' a1 b0 b1 v1 xs
    where v0 = b0 * x + v11
          y  = v0
	  v1 = -a1 * y + b1 * x

-- | Direct Form I for a second order section
--
--	@v[n] = b0 * x[n] + b1 * x[n-1] + b2 * x[n-2]@
--
--	@y[n] = v[n] - a1 * y[n-1] - a2 * y[n-2]@

{-# specialize biquad_df1 :: Float -> Float -> Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize biquad_df1 :: Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] #-}

biquad_df1 :: Num a => a -- ^ a_1
	   -> a -- ^ a_2
	   -> a -- ^ b_0
	   -> a -- ^ b_1
	   -> a -- ^ b_2
	   -> [a] -- ^ x[n]
	   -> [a] -- ^ y[n]

biquad_df1 a1 a2 b0 b1 b2 x = df1 a1 a2 b0 b1 b2 0 0 0 0 x

df1 :: Num a => a -> a -> a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df1 _  _  _  _  _  _  _  _  _  []     = []
df1 a1 a2 b0 b1 b2 x1 x2 y1 y2 (x:xs) = y : df1 a1 a2 b0 b1 b2 x x1 y y1 xs
    where v = b0 * x + b1 * x1 + b2 * x2
	  y = v      - a1 * y1 - a2 * y2


-- | Direct Form II for a second order section (biquad)
--
--	@w[n] = -a1 * w[n-1] - a2 * w[n-2] + x[n]@
--
--	@y[n] = b0 * w[n] + b1 * w[n-1] + b2 * w[n-2]@

{-# specialize biquad_df2 :: Float -> Float -> Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize biquad_df2 :: Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] #-}

biquad_df2 :: Num a => a -- ^ a_1
	   -> a -- ^ a_2
	   -> a -- ^ b_0
	   -> a -- ^ b_1
	   -> a -- ^ b_2
	   -> [a] -- ^ x[n]
	   -> [a] -- ^ y[n]

biquad_df2 a1 a2 b0 b1 b2 x = df2 a1 a2 b0 b1 b2 0 0 x

df2 :: Num a => a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df2 _  _  _  _  _  _  _  []     = []
df2 a1 a2 b0 b1 b2 w1 w2 (x:xs) = y : df2 a1 a2 b0 b1 b2 w w1 xs
    where w = x - a1 * w1 - a2 * w2
          y = b0 * w + b1 * w1 + b2 * w2

-- | Transposed Direct Form II for a second order section
--
--	@v0[n] = b0 * x[n] + v1[n-1]@
--
--	@y[n] = v0[n]@
--
--	@v1[n] = -a1 * y[n] + b1 * x[n] + v2[n-1]@
--
--	@v2[n] = -a2 * y[n] + b2 * x[n]@

{-# specialize biquad_df2t :: Float -> Float -> Float -> Float -> Float -> [Float] -> [Float] #-}
{-# specialize biquad_df2t :: Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] #-}

biquad_df2t :: Num a => a -- ^ a_1
	    -> a -- ^ a_2
	    -> a -- ^ b_0
	    -> a -- ^ b_1
	    -> a -- ^ b_2
	    -> [a] -- ^ x[n]
	    -> [a] -- ^ y[n]

biquad_df2t a1 a2 b0 b1 b2 x = df2t a1 a2 b0 b1 b2 0 0 x

df2t :: Num a => a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df2t _  _  _  _  _  _   _   []     = []
df2t a1 a2 b0 b1 b2 v11 v21 (x:xs) = y : df2t a1 a2 b0 b1 b2 v1 v2 xs
    where v0 = b0 * x + v11
          y = v0
	  v1 = -a1 * y + b1 * x + v21
	  v2 = -a2 * y + b2 * x

-- | Direct Form I IIR
--
-- @v[n] = sum(k=0..M) b_k*x[n-k]@
--
-- @y[n] = v[n] - sum(k=1..N) a_k*y[n-k]@
--
-- @v[n]@ is calculated with 'fir'

{- specialize iir_df1 :: (Array Int Float, Array Int Float) -> [Float] -> [Float] -}
{- specialize iir_df1 :: (Array Int Double, Array Int Double) -> [Double] -> [Double] -}

iir_df1 :: (Num a, Eq a) => (Array Int a, Array Int a) -- ^ (b,a)
	-> [a] -- ^ x[n]
	-> [a] -- ^ y[n]

iir_df1 (b,a) x = y
    where v = fir b x
	  y = iir'df1 a w v
	  w = listArray (1,n) $ repeat 0
	  n = snd $ bounds a

{- specialize iir'df1 :: Array Int Float -> Array Int Float -> [Float] -> [Float] -}
{- specialize iir'df1 :: Array Int Double -> Array Int Double -> [Double] -> [Double] -}

iir'df1 :: (Num a) => Array Int a -> Array Int a -> [a] -> [a]
iir'df1 _ _ []  = []
iir'df1 a w (v:vs) = y : iir'df1 a w' vs
    where y  = v - sum [ a!i * w!i | i <- [1..n] ]
          w' = listArray (1,n) $ y : elems w
	  n  = snd $ bounds a

-- | Direct Form II IIR
--
-- @w[n] = x[n] - sum(k=1..N) a_k*w[n-k]@
--
-- @y[n] = sum(k=0..M) b_k*w[n-k]@

{- specialize iir_df2 :: (Array Int Float, Array Int Float) -> [Float] -> [Float] -}
{- specialize iir_df2 :: (Array Int Double, Array Int Double) -> [Double] -> [Double] -}

iir_df2 :: (Num a) => (Array Int a, Array Int a) -- ^ (b,a)
	-> [a] -- ^ x[n]
	-> [a] -- ^ y[n]

iir_df2 (b,a) x = y
    where y = iir'df2 (b,a) w x
	  w = listArray (0,mn) $ repeat 0
	  m = snd $ bounds b
	  n = snd $ bounds a
	  mn = max m n

{- specialize iir'df2 :: Array Int Float -> Array Int Float -> [Float] -> [Float] -}
{- specialize iir'df2 :: Array Int Double -> Array Int Double -> [Double] -> [Double] -}

iir'df2 :: (Num a) => (Array Int a,Array Int a) -> Array Int a -> [a] -> [a]
iir'df2 _     _ []     = []
iir'df2 (b,a) w (x:xs) = y : iir'df2 (b,a) w' xs
    where y  = sum [ b!i * w'!i | i <- [0..m] ]
          w0 = x - sum [ a!i * w'!i | i <- [1..m] ]
	  w' = listArray (0,mn) $ w0 : elems w
	  m  = snd $ bounds b
	  mn = snd $ bounds w

---------

-- test

xt :: [Double]
xt = [ 1, 0, 0, 0, 0, 0, 0, 0 ] :: [Double]

yt :: [Double]
yt = integrator 0.5 xt

f1 :: Fractional a => [a] -> [a]
f1 x = biquad_df1  (-0.4) 0.3 0.5 0.4 (-0.3) x

f2 :: Fractional a => [a] -> [a]
f2 x = biquad_df2  (-0.4) 0.3 0.5 0.4 (-0.3) x

f3 :: Fractional a => [a] -> [a]
f3 x = biquad_df2t (-0.4) 0.3 0.5 0.4 (-0.3) x

at :: Array Int Double
at = listArray (1,2) [ -0.4, 0.3 ]

bt :: Array Int Double
bt = listArray (0,2) [ 0.5, 0.4, -0.3 ]

f4 :: [Double] -> [Double]
f4 x = iir_df1 (bt,at) x

f5 :: [Double] -> [Double]
f5 x = iir_df2 (bt,at) x
