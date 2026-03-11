-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.FIR
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Finite Impuse Response filtering functions
--
-----------------------------------------------------------------------------

module DSP.Filter.FIR.FIR (fir, test) where

import Data.Array

-- | Implements the following function, which is a FIR filter
--
-- @y[n] = sum(k=0,M) h[k]*x[n-k]@
--
-- We implement the fir function with five helper functions, depending on
-- the type of the filter.  In the following functions, we use the O&S
-- convention that m is the order of the filter, which is equal to the
-- number of taps minus one.

{-# specialize fir :: Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir :: Array Int Double -> [Double] -> [Double] #-}

fir :: (Num a, Eq a) => Array Int a -- ^ h[n]
    -> [a] -- ^ x[n]
    -> [a] -- ^ y[n]

fir _ [] = []
fir h (x:xs) | isFIRType1 h = fir'1 h w xs
             | isFIRType2 h = fir'2 h w xs
             | isFIRType3 h = fir'3 h w xs
             | isFIRType4 h = fir'4 h w xs
             | otherwise    = fir'0 h w xs
    where w = listArray (0,m) $ x : replicate m 0
	  m = snd $ bounds h

-- This is for testing the symmetric helpers.

fir0 :: Num a => Array Int a -> [a] -> [a]
fir0 _ []     = []
fir0 h (x:xs) = fir'0 h w xs
    where w = listArray (0,m) $ x : replicate m 0
	  m = snd $ bounds h

-- Asymmetric FIR

{-# specialize fir'0 :: Array Int Float ->  Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir'0 :: Array Int Double -> Array Int Double -> [Double] -> [Double] #-}

fir'0 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'0 h w []     = y : []
    where y  = sum [ h!i * w!i | i <- [0..m] ]
	  m  = snd $ bounds h
fir'0 h w (x:xs) = y : fir'0 h w' xs
    where y  = sum [ h!i * w!i | i <- [0..m] ]
          w' = listArray (0,m) $ x : elems w
	  m  = snd $ bounds h

-- Type 1: symmetric FIR, even order / odd length

{-# specialize fir'1 :: Array Int Float ->  Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir'1 :: Array Int Double -> Array Int Double -> [Double] -> [Double] #-}

fir'1 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'1 h w []     = y : []
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2-1] ]
	  m  = snd $ bounds h
	  m2 = m `div` 2
fir'1 h w (x:xs) = y : fir'1 h w' xs
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2-1] ]
          w' = listArray (0,m) $ x : elems w
	  m  = snd $ bounds h
	  m2 = m `div` 2

-- Type 2: symmetric FIR, odd order / even length

{-# specialize fir'2 :: Array Int Float ->  Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir'2 :: Array Int Double -> Array Int Double -> [Double] -> [Double] #-}

fir'2 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'2 h w []     = y : []
    where y  = sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2] ]
	  m  = snd $ bounds h
	  m2 = m `div` 2
fir'2 h w (x:xs) = y : fir'2 h w' xs
    where y  = sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2] ]
          w' = listArray (0,m) $ x : elems w
	  m  = snd $ bounds h
	  m2 = m `div` 2

-- Type 3: anti-symmetric FIR, even order / odd length

{-# specialize fir'3 :: Array Int Float ->  Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir'3 :: Array Int Double -> Array Int Double -> [Double] -> [Double] #-}

fir'3 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'3 h w []     = y : []
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2-1] ]
	  m  = snd $ bounds h
	  m2 = m `div` 2
fir'3 h w (x:xs) = y : fir'3 h w' xs
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2-1] ]
          w' = listArray (0,m) $ x : elems w
	  m  = snd $ bounds h
	  m2 = m `div` 2

-- Type 4: anti-symmetric FIR, off order / even length

{-# specialize fir'4 :: Array Int Float ->  Array Int Float ->  [Float]  -> [Float]  #-}
{-# specialize fir'4 :: Array Int Double -> Array Int Double -> [Double] -> [Double] #-}

fir'4 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'4 h w []     = y : []
    where y  = sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2] ]
	  m  = snd $ bounds h
	  m2 = m `div` 2
fir'4 h w (x:xs) = y : fir'4 h w' xs
    where y  = sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2] ]
          w' = listArray (0,m) $ x : elems w
	  m  = snd $ bounds h
	  m2 = m `div` 2

-- Aux functions.  Note that the tap numbers go from [0..m], so if m is
-- even, then the filter has odd length, and vice versa.

{-# specialize isFIRType1 :: Array Int Float ->  Bool  #-}
{-# specialize isFIRType1 :: Array Int Double -> Bool #-}

isFIRType1 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType1 h = even m && (h' == (reverse h'))
    where m = snd $ bounds h
	  h' = elems h

{-# specialize isFIRType2 :: Array Int Float ->  Bool  #-}
{-# specialize isFIRType2 :: Array Int Double -> Bool #-}

isFIRType2 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType2 h = odd m && (h' == (reverse h'))
    where m = snd $ bounds h
	  h' = elems h

{-# specialize isFIRType3 :: Array Int Float ->  Bool  #-}
{-# specialize isFIRType3 :: Array Int Double -> Bool #-}

isFIRType3 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType3 h = even m && ha == reverse hb
    where m = snd $ bounds h
	  h' = elems h
	  ha = take n h'
          hb = map negate (drop (n+1) h')
          n = m `div` 2

{-# specialize isFIRType4 :: Array Int Float ->  Bool #-}
{-# specialize isFIRType4 :: Array Int Double -> Bool #-}

isFIRType4 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType4 h = odd m && ha == reverse hb
    where m = snd $ bounds h
	  ha = elems h
	  hb = fmap negate $ ha

-- Test routines

-- This tests out fir'0

ht :: Array Int Double
ht = listArray (0,4) [ 1, 2, 0, -1, 1 ]

xt :: [Double]
xt = [1, 3, -1, -2, 0, 0, 0, 0 ]

yt :: [Double]
yt = [1, 5, 5, -5, -6, 4, 1, -2]

yt' :: [Double]
yt' = fir ht xt

-- This checks the symmetric routines against fir'0

h1 :: Array Int Double
h1 = listArray (0,4) [ 1, 2, 3, 2, 1 ]
h2 :: Array Int Double
h2 = listArray (0,5) [ 1, 2, 3, 3, 2, 1 ]
h3 :: Array Int Double
h3 = listArray (0,4) [ 1, 2, 3, -2, -1 ]
h4 :: Array Int Double
h4 = listArray (0,5) [ 1, 2, 3, -3, -2, -1 ]

y1, y2, y3, y4 :: [Double]
y1 = fir0 h1 xt
y2 = fir0 h2 xt
y3 = fir0 h3 xt
y4 = fir0 h4 xt

y1', y2', y3', y4' :: [Double]
y1' = fir h1 xt
y2' = fir h2 xt
y3' = fir h3 xt
y4' = fir h4 xt

-- If everything works, then test == True

test :: Bool
test = and [ yt == yt', y1 == y1', y2 == y2', y3 == y3', y4 == y4' ]
