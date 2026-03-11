-- |
-- Module      :  ListQuantizer
-- Copyright   :  (c) OleksandrZhabenko 2023-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A module to provide the extended variants to convert a list with
-- some values to another one with the values from the pre-defined another list. Similar to 
-- the measurement of the quantum state observables with the discrete spectrum. Contrary to
-- TwoQuantizer module, the results  in every function  here depend not just on the two values, 
-- which the point is located in between, but on the whole list. Defined for just positive real numbers of 'Double' type.


{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module ListQuantizer where

import GHC.Base
import GHC.List
import GHC.Real
import GHC.Float
import GHC.Num
import Data.Maybe
import qualified TwoQuantizer as Q (meanF2)
import Data.MinMax1 (minMax11)
import Data.List (partition) 

-- | A better suited variant for 'FoldableQuantizer.round2G' for lists. 
round2GL
 :: (Ord a) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> ([a] -> a -> Ordering) 
 -> [a] 
 -> a 
 -> Maybe a -- ^ The @a@ value (in 'Just' case) can be equal just to the one of the two first @a@ arguments.
round2GL bool f xs@(w:_:_) z 
 | z `elem` xs = Just z
 | length xs < 2 = Nothing
 | z < x || z > y = Nothing
 | null ts = Just u
 | null us = Just t
 | otherwise = Just (case f xs z of { GT -> u; LT -> t; EQ -> if bool then u else t })
     where (x, y) = fromJust . minMax11 $ xs
           (ts,us) = partition (<z) xs
           t = maximum ts
           u = minimum us

foldableQuantizerGL 
 :: (Ord a, Floating a) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> ([a] -> a -> Ordering) 
 -> [a] 
 -> [a]
 -> [a]
foldableQuantizerGL ctrl f needs xs = map (fromJust . round2GL ctrl f needs . (*k)) xs
  where !k 
           | Q.meanF2 xs 0 0 == 0 = error "ListQuantizer.foldableQuantizerGL: division by zero!"
           | otherwise = Q.meanF2 needs 0 0 / Q.meanF2 xs 0 0
        ys = foldr (\t ts -> t * k : ts) [] xs

round2GML 
 :: (Ord a, Monad m) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> ([a] -> a -> m Ordering) 
 -> [a] 
 -> a 
 -> m (Maybe a)
round2GML bool f xs z 
 | z `elem` xs = return . Just $ z
 | length xs < 2 = return Nothing
 | z < x || z > y = return Nothing
 | null ts = return u
 | null us = return t
 | otherwise = do
     q <- f xs z
     case q of { GT -> return u; LT -> return t; EQ -> return (if bool then u else t)}
   where (x, y) = fromJust . minMax11 $ xs
         (ts,us) = partition (<z) xs
         t = if null ts then Nothing else Just . maximum $ ts
         u = if null us then Nothing else Just . minimum $ us

foldableQuantizerGML 
 :: (Ord a, Floating a, Monad m) => Bool -- ^ If 'True' then the function rounds the result in the ambiguous situation to the greater value. The ambigous situation is defined by the second argument.
 -> ([a] -> a -> m Ordering) 
 -> [a] 
 -> [a] 
 -> m [a]
foldableQuantizerGML ctrl f needs xs = mapM (fmap fromJust . round2GML ctrl f needs . (*k)) xs
  where !k 
           | Q.meanF2 xs 0 0 == 0 = error "ListQuantizer.foldableQuantizerGML: division by zero!"
           | otherwise = Q.meanF2 needs 0 0  / Q.meanF2 xs 0 0
        ys = foldr (\u us -> u * k : us) [] xs

