-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Convolution
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module to perform the linear convolution of two sequences
--
-----------------------------------------------------------------------------

module DSP.Convolution (conv, test) where

import Data.Array

-- * Functions

-- | @conv@ convolves two finite sequences

conv :: (Ix a, Integral a, Num b) => Array a b -> Array a b -> Array a b
conv x1 x2 = x3
    where m1 = snd $ bounds x1
          m2 = snd $ bounds x2
	  m3 = m1 + m2
	  x3 = listArray (0,m3) [
                    sum [ x1!k * x2!(n-k) | k <- [max 0 (n-m2)..min n m1] ]
                       | n <- [0..m3] ]

-- Test vectors.  Linear convolution is also equivalent to polynomial
-- multiplication.

h1, h2, h3 :: Array Int Integer
h1 = listArray (0,3) [ 1, 2, 3, 4 ]
h2 = listArray (0,4) [ 1, 2, 3, 4, 5 ]
h3 = listArray (0,7) [ 1, 4, 10, 20, 30, 34, 31, 20 ]

test :: Bool
test  =  conv h1 h2 == h3
