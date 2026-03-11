{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Aftovolio.General.Distance where

import Data.List (replicate)
import GHC.Base
import GHC.Float (Floating (..), sqrt)
import GHC.Int
import GHC.List
import GHC.Num (Integer, abs, subtract, (*), (-))
import GHC.Real (
    Fractional (..),
    Integral,
    Real (..),
    fromIntegral,
    gcd,
    quot,
    toInteger,
    (/),
 )
import GHC.Word

{- | 'toEqLength' changes two given lists into two lists of equal
minimal lengths and also returs its new length and initial lengths of the lists given.
-}
toEqLength :: [a] -> [a] -> ([a], [a], Int, Int, Int)
toEqLength xs ys
    | null xs = ([], [], 0, 0, 0)
    | null ys = ([], [], 0, 0, 0)
    | otherwise = (ts, vs, lx * ly `quot` dc, lx, ly)
  where
    lx = length xs
    ly = length ys
    dc = gcd lx ly
    ts = concatMap (replicate (ly `quot` dc)) $ xs
    vs = concatMap (replicate (lx `quot` dc)) $ ys

{- | 'toEqLengthL' changes two given lists into two lists of equal
minimal lengths and also returs its new length and initial lengths of the lists given. Is
intended to be used when the length of the lists are known and given as the first and the second parameters
here respectively.
-}
toEqLengthL :: Int -> Int -> [a] -> [a] -> ([a], [a], Int, Int, Int)
toEqLengthL lx ly xs ys
    | lx == 0 = ([], [], 0, 0, 0)
    | ly == 0 = ([], [], 0, 0, 0)
    | otherwise = (ts, vs, lx * ly `quot` dc, lx, ly)
  where
    dc = gcd lx ly
    ts = concatMap (replicate (ly `quot` dc)) $ xs
    vs = concatMap (replicate (lx `quot` dc)) $ ys

-- | Is also a simplified distance between the lists. Intended to be used with 'Word8'.
sumAbsDistNorm :: (Integral a, Ord a) => [a] -> [a] -> Integer
sumAbsDistNorm xs ys
    | lc == 0 = 0
    | otherwise =
        sum . zipWith (\x y -> toInteger (if x > y then x - y else y - x)) ts $ vs
  where
    (ts, vs, lc, lx, ly) = toEqLength xs ys

-- | Intended to be used with 'Compards' of the same constructor in both arguments of the function. Otherwise returns -1.
sumAbsDistNormComp :: Compards -> Compards -> Integer
sumAbsDistNormComp x1s@(C1 xs) y1s@(C1 ys)
    | lc == 0 = 0
    | otherwise =
        sum . zipWith (\x y -> toInteger (if x > y then x - y else y - x)) ts $ vs
  where
    (ts, vs, lc, lx, ly) = toEqLength xs ys
sumAbsDistNormComp x1s@(C2 xs) y1s@(C2 ys)
    | lc == 0 = 0
    | otherwise =
        sum . zipWith (\x y -> toInteger (if x > y then x - y else y - x)) ts $ vs
  where
    (ts, vs, lc, lx, ly) = toEqLength xs ys
sumAbsDistNormComp _ _ = -1

sumSqrDistNorm :: (Real a, Fractional a) => [a] -> [a] -> a
sumSqrDistNorm xs ys
    | lc == 0 = 0
    | otherwise = sum (zipWith (\x y -> (x - y) * (x - y)) ts vs) / fromIntegral lc
  where
    (ts, vs, lc, lx, ly) = toEqLength xs ys

{- | 'distanceSqr' is applied on two lists of non-negative 'Real' numbers (preferably, of type
'Double') and returns a special kind of distance that is similar to the statistical distance used
in the regression analysis. Is intended to be used e. g. for the AFTOVolio approach. The less
is the resulting number, the more \'similar\' are the two lists of non-negative numbers in their
distributions. Here, in contrast to the more general 'distanceSqrG', the numbers must be normed
to 1.0, so that the largest ones in both listn must be 1.0.
-}
distanceSqr :: (Real a, Floating a, Fractional a) => [a] -> [a] -> a
distanceSqr xs ys = sqrt s
  where
    s = sumSqrDistNorm xs ys
{-# INLINE distanceSqr #-}

{- | 'distanceSqrG' is applied on two lists of non-negative 'Real' numbers (preferably, of type
'Double') and returns a special kind of distance that is similar to the statistical distance used
in the regression analysis. Is intended to be used e. g. for the AFTOVolio approach. The less
is the resulting number, the more \'similar\' are the two lists of non-negative numbers in their
distributions.
-}
distanceSqrG :: (Real a, Floating a, Fractional a) => [a] -> [a] -> a
distanceSqrG xs ys = distanceSqr qs rs
  where
    mx = maximum xs
    my = maximum ys
    qs = map (/ mx) xs
    rs = map (/ my) ys
{-# INLINE distanceSqrG #-}

{- | 'distanceSqrG2' is an partially optimized variant of the 'distanceSqrG' if length of the least
common multiplier of the two lists is known and provided as the first argument, besides if it is
equal to the length of the second argument, and if maximum element of the second argument here is
equal to 1.0.
-}
distanceSqrG2 :: (Real a, Floating a, Fractional a) => Int -> [a] -> [a] -> a
distanceSqrG2 lc xs ys = sqrt (sum (zipWith (\x y -> (x - y) * (x - y)) xs qs) / fromIntegral lc)
  where
    my = maximum ys
    rs = map (/ my) ys
    lr = length rs
    dc = lc `quot` lr
    qs = concatMap (replicate dc) rs
{-# INLINE distanceSqrG2 #-}

data Compards = C1 [Word8] | C2 [Int8] deriving (Eq)

isWord8Based :: Compards -> Bool
isWord8Based (C1 _) = True
isWord8Based _ = False

isInt8Based :: Compards -> Bool
isInt8Based (C2 _) = True
isInt8Based _ = False

-- | The elements in  the first argument must not be greater than 127 though it is not checked.
fromSmallWord8toInt8Diff :: [Word8] -> [Int8]
fromSmallWord8toInt8Diff xs@(_ : ys) = zipWith (\t u -> fromIntegral u - fromIntegral t) xs ys
fromSmallWord8toInt8Diff [] = []
{-# INLINE fromSmallWord8toInt8Diff #-}

class DoubleFunc a b c d where
    doubleFunc :: (a -> c) -> (b -> c) -> d -> c

instance (DoubleFunc [Word8] [Int8] Int) Compards where
    doubleFunc f g (C1 xs) = f xs
    doubleFunc f g (C2 ys) = g ys

instance (DoubleFunc [Word8] [Int8] Compards) Compards where
    doubleFunc f g (C1 xs) = f xs
    doubleFunc f g (C2 ys) = g ys

instance (DoubleFunc [Word8] [Int8] Bool) Compards where
    doubleFunc f g (C1 xs) = f xs
    doubleFunc f g (C2 ys) = g ys

instance (DoubleFunc [Word8] [Int8] Integer) Compards where
    doubleFunc f g (C1 xs) = f xs
    doubleFunc f g (C2 ys) = g ys
