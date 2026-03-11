-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.CT
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Cooley-Tukey algorithm for computing the FFT
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.CT (fft_ct1, fft_ct2) where

import Data.List (transpose)
import Data.Array
import Data.Complex

-- | Cooley-Tukey algorithm doing row FFT's then column FFT's

{-# specialize fft_ct1 :: Array Int (Complex Float) -> Int -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_ct1 :: Array Int (Complex Double) -> Int -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_ct1 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	-> a -- ^ nrows
	-> a -- ^ ncols
	-> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	-> Array a (Complex b) -- ^ X[k]

fft_ct1 a l m fft = array (0,n-1) $ zip ks (elems x')
    where x = listArray ((0,0),(l-1,m-1)) [ a!i | i <- xs ]
	  f = listArray ((0,0),(l-1,m-1)) (flatten_rows $ map fft $ rows x)
	  g = listArray ((0,0),(l-1,m-1)) [ f!(i,j) * w!(i*j) | i <- [0..(l-1)], j <- [0..(m-1)] ]
	  x' = listArray ((0,0),(l-1,m-1)) (flatten_cols $ map fft $ cols g)
	  wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
	  (xs,ks) = ct_index_map1 l m
	  n = l * m

-- | Cooley-Tukey algorithm doing column FFT's then row FFT's

{-# specialize fft_ct2 :: Array Int (Complex Float) -> Int -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_ct2 :: Array Int (Complex Double) -> Int -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

fft_ct2 :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	-> a -- ^ nrows
	-> a -- ^ ncols
	-> (Array a (Complex b) -> Array a (Complex b)) -- ^ fft function
	-> Array a (Complex b) -- ^ X[k]

fft_ct2 a l m fft = array (0,n-1) $ zip ks (elems x')
    where x = listArray ((0,0),(l-1,m-1)) [ a!i | i <- xs ]
	  f = listArray ((0,0),(l-1,m-1)) (flatten_cols $ map fft $ cols x)
	  g = listArray ((0,0),(l-1,m-1)) [ f!(i,j) * w!(i*j) | i <- [0..(l-1)], j <- [0..(m-1)] ]
	  x' = listArray ((0,0),(l-1,m-1)) (flatten_rows $ map fft $ rows g)
	  wn = cis (-2 * pi / fromIntegral n)
	  w = listArray (0,n-1) $ iterate (* wn) 1
	  (xs,ks) = ct_index_map2 l m
	  n = l * m

-- Index maps

{-# specialize ct_index_map1 :: Int -> Int -> ([Int],[Int]) #-}

ct_index_map1 :: (Integral a) => a -> a -> ([a],[a])
ct_index_map1 l m = (n,k)
    where n = [ n1 + l * n2 | n1 <- [0..(l-1)], n2 <- [0..(m-1)] ]
          k = [ m * k1 + k2 | k1 <- [0..(l-1)], k2 <- [0..(m-1)] ]

{-# specialize ct_index_map2 :: Int -> Int -> ([Int],[Int]) #-}

ct_index_map2 :: (Integral a) => a -> a -> ([a],[a])
ct_index_map2 l m = (n,k)
    where n = [ m * n1 + n2 | n1 <- [0..(l-1)], n2 <- [0..(m-1)] ]
          k = [ k1 + l * k2 | k1 <- [0..(l-1)], k2 <- [0..(m-1)] ]

-- Auxilary functions (also used for PFA)

{-# specialize rows :: Array (Int,Int) (Complex Float) -> [Array Int (Complex Float)] #-}
{-# specialize rows :: Array (Int,Int) (Complex Double) -> [Array Int (Complex Double)] #-}

rows :: (Ix a, Integral a, RealFloat b) => Array (a,a) (Complex b) -> [Array a (Complex b)]
rows x = [ listArray (0,m) [ x!(i,j) | j <- [0..m] ] | i <- [0..l] ]
    where ((_,_),(l,m)) = bounds x

{-# specialize cols :: Array (Int,Int) (Complex Float) -> [Array Int (Complex Float)] #-}
{-# specialize cols :: Array (Int,Int) (Complex Double) -> [Array Int (Complex Double)] #-}

cols :: (Ix a, Integral a, RealFloat b) => Array (a,a) (Complex b) -> [Array a (Complex b)]
cols x = [ listArray (0,l) [ x!(i,j) | i <- [0..l] ] | j <- [0..m] ]
    where ((_,_),(l,m)) = bounds x

{-# specialize flatten_rows :: [Array Int (Complex Float)] -> [(Complex Float)] #-}
{-# specialize flatten_rows :: [Array Int (Complex Double)] -> [(Complex Double)] #-}

flatten_rows :: (Ix a, Integral a, RealFloat b) => [Array a (Complex b)] -> [(Complex b)]
flatten_rows a = foldr (++) [] $ map elems a

{-# specialize flatten_cols :: [Array Int (Complex Float)] -> [(Complex Float)] #-}
{-# specialize flatten_cols :: [Array Int (Complex Double)] -> [(Complex Double)] #-}

flatten_cols :: (Ix a, Integral a, RealFloat b) => [Array a (Complex b)] -> [(Complex b)]
flatten_cols a = foldr (++) [] $ transpose $ map elems a
