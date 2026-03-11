-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Transform.Fourier.PFA
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Prime Factor Algorithm
--
-----------------------------------------------------------------------------

module Numeric.Transform.Fourier.PFA (fft_pfa) where

import Data.List (transpose)
import Data.Array
import Data.Complex

{-# specialize fft_pfa :: Array Int (Complex Float) -> Int -> Int -> (Array Int (Complex Float) -> Array Int (Complex Float)) -> Array Int (Complex Float) #-}
{-# specialize fft_pfa :: Array Int (Complex Double) -> Int -> Int -> (Array Int (Complex Double) -> Array Int (Complex Double)) -> Array Int (Complex Double) #-}

-- | Prime Factor Algorithm doing row FFT's then column FFT's

fft_pfa :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ x[n]
	-> a -- ^ nrows
	-> a -- ^ ncols
	-> (Array a (Complex b) -> Array a (Complex b)) -- ^ FFT function
	-> Array a (Complex b) -- ^ X[k]

fft_pfa a l m fft = array (0,n-1) $ zip ks (elems x')
    where x = listArray ((0,0),(l-1,m-1)) [ a!i | i <- xs ]
	  f = listArray ((0,0),(l-1,m-1)) (flatten_rows $ map fft $ rows x)
	  x' = listArray ((0,0),(l-1,m-1)) (flatten_cols $ map fft $ cols f)
          (xs,ks) = pfa_index_map l m
	  n = l * m

{-# specialize pfa_index_map :: Int -> Int -> ([Int],[Int]) #-}

pfa_index_map :: (Integral a) => a -> a -> ([a],[a])
pfa_index_map l m = (ns,ks)
    where ns = [ (m * n1 + l * n2) `mod` n | n1 <- [0..(l-1)], n2 <- [0..(m-1)] ]
          ks = [ (c * m * k1 + d * l * k2) `mod` n | k1 <- [0..(l-1)], k2 <- [0..(m-1)] ]
	  c = find_inverse m l
	  d = find_inverse l m
	  n = l * m

{-# specialize find_inverse :: Int -> Int -> Int #-}

find_inverse :: (Integral a) => a -> a -> a
find_inverse a0 n0 = find_inverse' a0 n0 1
    where find_inverse' a n a' | (a*a') `mod` n == 1 = a'
		               | otherwise = find_inverse' a n (a'+1)

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
