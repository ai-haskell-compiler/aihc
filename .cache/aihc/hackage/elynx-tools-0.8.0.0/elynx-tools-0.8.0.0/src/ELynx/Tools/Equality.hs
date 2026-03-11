-- |
-- Module      :  ELynx.Tools.Equality
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:27:05 2019.
--
-- Equality tests.
module ELynx.Tools.Equality
  ( -- * Equality
    allEqual,
    allNearlyEqualWith,
    allNearlyEqual,
    nearlyEqWith,
    eps,
    nearlyEq,
    (=~=),
    nearlyEqListWith,
    nearlyEqList,
    nearlyEqVecWith,
    nearlyEqVec,
    nearlyEqMatWith,
    nearlyEqMat,
  )
where

import ELynx.Tools.Definitions
import Numeric.LinearAlgebra

-- | Test if all elements of a list are equal; returns True for empty list.
allEqual :: (Eq a) => [a] -> Bool
-- Well, maybe it should be False, but then, it is True that all elements are
-- equal :).
allEqual [] = True
allEqual xs = all (== head xs) (tail xs)

-- | Test if all elements of a list are nearly equal; returns True for empty list.
allNearlyEqualWith :: Double -> [Double] -> Bool
allNearlyEqualWith _ [] = True
allNearlyEqualWith tol xs = all (nearlyEqWith tol $ head xs) (tail xs)

-- | Test if all elements of a list are nearly equal; returns True for empty list.
allNearlyEqual :: [Double] -> Bool
allNearlyEqual = allNearlyEqualWith eps

-- | Test for equality with given tolerance (needed because of machine precision).
nearlyEqWith :: Double -> Double -> Double -> Bool
nearlyEqWith tol a b = tol > abs (a - b)

-- | Test for equality with predefined tolerance 'eps' (needed because of
-- machine precision).
nearlyEq :: Double -> Double -> Bool
nearlyEq = nearlyEqWith eps

-- | Infix synonym for 'nearlyEq'.
(=~=) :: Double -> Double -> Bool
(=~=) = nearlyEq

-- Test if the given number is nearly equal to all elements of a list.
nearlyEqValListWith :: Double -> Double -> [Double] -> Bool
nearlyEqValListWith tol a = all (nearlyEqWith tol a)

-- | Test if two lists are nearly equal.
nearlyEqListWith :: Double -> [Double] -> [Double] -> Bool
nearlyEqListWith tol xs ys = nearlyEqValListWith tol 0 (zipWith (-) xs ys)

-- | Test if two lists are nearly equal; use tolerance 'eps'.
nearlyEqList :: [Double] -> [Double] -> Bool
nearlyEqList = nearlyEqListWith eps

-- | Test if two vectors are nearly equal.
nearlyEqVecWith :: Double -> Vector R -> Vector R -> Bool
nearlyEqVecWith tol a b = nearlyEqValListWith tol 0 (toList $ a - b)

-- | Test if two vectors are nearly equal; use tolerance 'eps'.
nearlyEqVec :: Vector R -> Vector R -> Bool
nearlyEqVec = nearlyEqVecWith eps

-- | Test if two vectors are nearly equal.
nearlyEqMatWith :: Double -> Matrix R -> Matrix R -> Bool
nearlyEqMatWith tol a b = nearlyEqValListWith tol 0 (concat . toLists $ a - b)

-- | Test if two vectors are nearly equal; use tolerance 'eps'.
nearlyEqMat :: Matrix R -> Matrix R -> Bool
nearlyEqMat = nearlyEqMatWith eps
