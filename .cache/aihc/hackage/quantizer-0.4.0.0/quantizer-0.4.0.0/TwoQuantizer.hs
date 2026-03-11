-- |
-- Module      :  TwoQuantizer
-- Copyright   :  (c) OleksandrZhabenko 2022-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A module to provide the simple version of the obtaining from the list of values the list of other
-- values, the pre-defined ones. Provides both pure functions and monadic versions. Contrary to
-- ListQuantizer module, the results  in every function  here depend on the two values, 
-- which the point is located in between. Defined for just positive real numbers of 'Double' type.

{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module TwoQuantizer where

import GHC.Base
import GHC.Num
import Data.Maybe
import Numeric.Stats (meanD)
import GHC.Float
import GHC.Real
import GHC.List
import Data.List (partition) 

round2 
  :: Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is when the square of the third paremeter is equal to  the product of the second one and the fourth one. 
  -> Double 
  -> Double -- ^ This one should lie between the other two similar parameters — the one before and the one after it.
  -> Double 
  -> Maybe Double -- ^ The numeric value (in 'Just' case) can be equal just to the one of the two first arguments.
round2 bool x y z 
 | x <= 0 || y <= 0 || z <= 0 = Nothing
 | (x - z) * (y - z) <= 0 = Just (case compare (z*z) (x*y) of { GT -> max x y; LT -> min x y; EQ -> (if bool then max else min) x y })
 | otherwise = Nothing

round2L 
 :: Bool  -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. 
 -> [Double] 
 -> Double 
 -> Double
round2L ctrl ts x 
 | null ts = x
 | null ks = y
 | null us = y0
 | x < y = fromJust . round2 ctrl y0 y $ x
 | otherwise = y
  where (ks, us) = partition (<x) ts
        y = minimum us
        y0 = maximum ks

twoQuantizer 
 :: Bool  -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. 
 -> [Double] 
 -> [Double] 
 -> [Double]
twoQuantizer ctrl needs xs = map ((round2L ctrl needs) . (*k)) xs
  where !k 
           | meanD xs == 0 = error "TwoQuantizer.twoQuantizer: division by zero!"
           | otherwise = meanD needs / meanD xs

round2G 
 :: (Ord a) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> Ordering) 
 -> a 
 -> a 
 -> a 
 -> Maybe a -- ^ The @a@ value (in 'Just' case) can be equal just to the one of the two first @a@ arguments.
round2G bool f x y z 
 | z == x = Just x
 | z == y = Just y
 | (x < z && y > z) || (x > z && y < z) = Just (case f x y z of { GT -> max x y; LT -> min x y; EQ -> (if bool then max else min) x y })
 | otherwise = Nothing

round2GL 
 :: (Ord a) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> Ordering) 
 -> [a] 
 -> a 
 -> a
round2GL ctrl f ts x 
 | null ts = x
 | null ks = y
 | null us = y0
 | x < y = fromJust . round2G ctrl f y0 y $ x
 | otherwise = y
  where (ks, us) = partition (<x) ts
        y = minimum us
        y0 = maximum ks

twoQuantizerG 
 :: (Ord a, Floating a) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> Ordering) 
 -> [a] 
 -> [a] 
 -> [a]
twoQuantizerG ctrl f needs xs = map ((round2GL ctrl f needs) . (*k)) xs
  where !k 
           | meanF2 xs 0 0 == 0 = error "TwoQuantizer.twoQuantizerG: division by zero!"
           | otherwise = meanF2 needs 0 0 / meanF2 xs 0 0

round2GM 
 :: (Ord a, Monad m) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> m Ordering) 
 -> a 
 -> a 
 -> a 
 -> m (Maybe a)
round2GM bool f x y z 
 | z == x = return . Just $ x
 | z == y = return . Just $ y
 | (x < z && y > z) || (x > z && y < z) = do
     t <- f x y z
     case t of { GT -> return . Just . max x $ y; LT -> return . Just . min x $ y; EQ -> return. Just $ (if bool then max else min) x y }
 | otherwise = return Nothing

round2GLM 
 :: (Ord a, Monad m) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> m Ordering) 
 -> [a] 
 -> a 
 -> m a
round2GLM ctrl f ts x 
 | null ts = return x
 | null ks = return y
 | null us = return y0
 | x < y = fmap fromJust . round2GM ctrl f y0 y $ x 
 | otherwise = return y
  where (ks, us) = partition (<x) ts
        y = minimum us
        y0 = maximum ks

-- | Simple arithmetic mean. Is vulnerable to floating point rounding error so if possible use just
-- for double-precision values.
meanF2 
 :: (Floating a) => [a] 
 -> a 
 -> a 
 -> a
meanF2 (!t:ts) !s !l = meanF2 ts (s + t) (l + 1) 
meanF2 _ !s !l = s / l

twoQuantizerGM 
 :: (Ord a, Floating a, Monad m) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (a -> a -> a -> m Ordering) 
 -> [a] 
 -> [a] 
 -> m [a]
twoQuantizerGM ctrl f needs xs = mapM ((round2GLM ctrl f needs) . (*k)) xs
  where !k 
           | meanF2 xs 0 0 == 0 = error "TwoQuantizer.twoQuantizerGM: division by zero!"
           | otherwise = meanF2 needs 0 0  / meanF2 xs 0 0

