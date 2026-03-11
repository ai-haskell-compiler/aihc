-----------------------------------------------------------------------------
-- |
-- Module      :  Polynomial.Chebyshev
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for generating Chebyshev polynomials
--
-- @T_0(x) = 1@
--
-- @T_1(x) = x@
--
-- @T_N+1(x) = 2x T_N(x) - T_N-1(x)@
--
-----------------------------------------------------------------------------

module Polynomial.Chebyshev (cheby) where

import Polynomial.Basic

-- | generates Chebyshev polynomials

{-# specialize cheby :: Int -> [Int]    #-}
{-# specialize cheby :: Int -> [Double] #-}

cheby :: (Integral a, Num b) => a -- ^ N
      -> [b] -- ^ T_N(x)

-- the cases for n=2.. aren't needed for the recursion, but I added
-- them anyway

cheby 0 = [ 1 ]
cheby 1 = [ 0, 1 ]
cheby 2 = [ -1, 0, 2 ]
cheby 3 = [ 0, -3, 0, 4 ]
cheby 4 = [ 1, 0, -8, 0, 8 ]
cheby 5 = [ 0, 5, 0, -20, 0, 16]
cheby 6 = [ -1, 0, 18, 0, -48, 0, 32 ]
cheby n = polysub (polymult [ 0, 2 ] (cheby (n-1))) (cheby (n-2))
