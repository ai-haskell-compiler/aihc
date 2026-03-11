-- |
-- Module      :  FoldableQuantizer
-- Copyright   :  (c) OleksandrZhabenko 2022-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A module to provide the extended variants to convert a 'S.InsertLeft' instance structure with
-- some values to another one with the values from the pre-defined structure. Similar to 
-- the measurement of the quantum state observables with the discrete spectrum.
-- For performance reasons it is better to use module ListQuantizer whenever possible (especially if the
-- given 'F.Foldable' and 'S.InsertLeft' instances are just lists). Contrary to
-- TwoQuantizer module, the results  in every function  here depend not just on the two values, 
-- which the point is located in between, but on the whole structure. Defined for just positive real numbers of 'Double' type.

{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module FoldableQuantizer where

import GHC.Base
import GHC.List
import GHC.Real
import GHC.Float
import GHC.Num
import Data.Maybe
import qualified Data.Foldable as F
import qualified TwoQuantizer as Q (meanF2)
import Data.MinMax1 (minMax11)
import qualified Data.InsertLeft as IL

round2G 
 :: (Ord a, IL.InsertLeft t a, Monoid (t a)) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (t a -> a -> Ordering) 
 -> t a 
 -> a 
 -> Maybe a -- ^ The @a@ value (in 'Just' case) can be equal just to the one of the two first @a@ arguments.
round2G bool f xs z 
 | z `F.elem` xs = Just z
 | F.length xs < 2 = Nothing
 | z < x || z > y = Nothing
 | F.null ts = Just u
 | F.null us = Just t
 | otherwise = Just (case f xs z of { GT -> u; LT -> t; EQ -> if bool then u else t })
   where (x, y) = fromJust . minMax11 $ xs
         (ts,us) = IL.partitionG (<z) xs
         t = F.maximum ts
         u = F.minimum us

foldableQuantizerG 
 :: (Ord a, Floating a, IL.InsertLeft t1 a, Monoid (t1 a), F.Foldable t2) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (t1 a -> a -> Ordering) 
 -> t1 a 
 -> t2 a
 -> [a]
foldableQuantizerG ctrl f needs xs = map (fromJust . round2G ctrl f needs) ys
  where !k 
          | y == 0 = error "FoldableQuantizer.foldableQuantizerG: division by zero!"
          | otherwise = Q.meanF2 (F.toList needs) 0 0 / y
                where !y = Q.meanF2 (F.toList xs) 0 0
        ys = F.foldr (\t ts -> t * k : ts) [] xs

round2GM 
 :: (Ord a, Monad m, IL.InsertLeft t1 a, Monoid (t1 a)) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (t1 a -> a -> m Ordering) 
 -> t1 a 
 -> a 
 -> m (Maybe a)
round2GM bool f xs z 
 | z `F.elem` xs = return . Just $ z
 | F.length xs < 2 = return Nothing
 | z < x || z > y = return Nothing
 | F.null ts = return . Just $ u
 | F.null us = return . Just $ t
 | otherwise = do
     q <- f xs z
     case q of { GT -> return . Just $ u; LT -> return . Just $ t; EQ -> return . Just $ if bool then u else t}
   where (x, y) = fromJust . minMax11 $ xs
         (ts,us) = IL.partitionG (<z) xs
         t = F.maximum ts 
         u = F.minimum us

foldableQuantizerGM 
 :: (Ord a, Floating a, Monad m, IL.InsertLeft t1 a, Monoid (t1 a), F.Foldable t2) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> (t1 a -> a -> m Ordering) 
 -> t1 a 
 -> t2 a 
 -> m [a]
foldableQuantizerGM ctrl f needs xs = mapM (fmap fromJust . round2GM ctrl f needs) ys
  where !k 
           | y == 0 = error "FoldableQuantizer.foldableQuantizerGM: division by zero!"
           | otherwise = Q.meanF2 (F.toList needs) 0 0  / y
               where !y = Q.meanF2 (F.toList xs) 0 0 
        ys = F.foldr (\u us -> u * k : us) [] xs

