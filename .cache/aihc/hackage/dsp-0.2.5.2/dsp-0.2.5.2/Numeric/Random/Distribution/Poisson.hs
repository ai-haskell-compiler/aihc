-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Poisson
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- UNTESTED
--
-- Module for transforming a list of uniform random variables
-- into a list of Poisson random variables.
--
-- Reference: Ross
--     Donald E. Knuth (1969). Seminumerical Algorithms, The Art of Computer Programming, Volume 2
--
----------------------------------------------------------------------------

module Numeric.Random.Distribution.Poisson (poisson, test, testHead) where

import Numeric.Statistics.Moment (mean)

import Data.List (mapAccumL)
import System.Random (randomRs, mkStdGen)


-- * Functions

{- |
Generates a list of poisson random variables from a list of uniforms.
-}

poisson :: Double    -- ^ lambda - expectation value, should be non-negative.
	-> [Double]  -- ^ uniformly distributed values from the interval [0,1]
	-> [Int]     -- ^ Poisson distributed outputs

poisson lambda (u:us) =
   let e = exp (-lambda)
       {- 'group' cannot replace segmentAfter here,
          because it merges adjacent False values. -}
   in  map (length . tail) . segmentAfter not . snd $
       mapAccumL
          (\p ui ->
             let b = p >= e
             in  (if b then p*ui else ui, b))
          u us
poisson _ [] =
   error "poisson: list of uniformly distributed values must not be empty"


{- |
Split after every element that satisfies the predicate.

A candidate for a Utility module.
-}
segmentAfter :: (a -> Bool) -> [a] -> [[a]]
segmentAfter p =
   foldr (\ x ~yt@(y:ys) -> if p x then [x]:yt else (x:y):ys) [[]]



{-
The expectation value,
and thus the mean of a sequence of Poisson distributed values,
approximates lambda.
-}

test :: Int -> Double -> Double
test n lambda =
   mean $ map fromIntegral $
   take n $ poisson lambda $
   randomRs (0,1) $ mkStdGen 1

{-
Only test the leading number of several Poisson lists.
-}
testHead :: Int -> Double -> Double
testHead n lambda =
   mean $ map fromIntegral $
   map
      (head . poisson lambda .
       randomRs (0,1) . mkStdGen)
      [1..n]
