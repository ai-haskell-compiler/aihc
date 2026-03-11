-----------------------------------------------------------------------------
-- |
-- Module      :  Polynomial.Maclaurin
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for generating Maclaurin series representation of a few
-- functions:
--
-- @f(x) = sum [ a_i * x^i | i \<- [0..] ]@
--
-- The @Int@ parameter for all functions is the /order/ of the polynomial,
-- eg:
--
-- @[ a_i | i \<- [0..N] ]@
--
-- and not the number of non-zero terms
--
-----------------------------------------------------------------------------

module Polynomial.Maclaurin (polyexp, polyln1,
			     polycos, polysin, polyatan,
			     polycosh, polysinh, polyatanh) where

-- A few utility lists

ifacs :: [Double]
ifacs = map (1/) $ scanl (*) 1 [1..]

inverses :: [Double]
inverses = map (1/) $ 1:[1..]

-- Exponential and logarithm

-- | e^x

polyexp :: Int -> [Double]
polyexp n = take (n+1) ifacs

-- | ln (1+x), 0 \<= x \<= 1

polyln1 :: Int -> [Double]
polyln1 n = 0 : (take n $ zipWith (*) i $ map (1/) [1..])
    where i = [ 1, -1 ] ++ i

-- Trig functions

-- | cos x

polycos :: Int -> [Double]
polycos n = take (n+1) $ zipWith (*) i ifacs
    where i = [ 1, 0, -1, 0 ] ++ i

-- | sin x

polysin :: Int -> [Double]
polysin n = take (n+1) $ zipWith (*) i ifacs
    where i = [ 0, 1, 0, -1 ] ++ i

-- | atan x, -1 \< x \< 1

polyatan :: Int -> [Double]
polyatan n = take (n+1) $ zipWith (*) i inverses
    where i = [ 0, 1, 0, -1 ] ++ i

-- Hyperbolic functions

-- | cosh x

polycosh :: Int -> [Double]
polycosh n = take (n+1) $ zipWith (*) i ifacs
    where i = [ 1, 0 ] ++ i

-- | sinh x

polysinh :: Int -> [Double]
polysinh n = take (n+1) $ zipWith (*) i ifacs
    where i = [ 0, 1 ] ++ i

-- | atanh x

polyatanh :: Int -> [Double]
polyatanh n = take (n+1) $ zipWith (*) i inverses
    where i = [ 0, 1 ] ++ i
