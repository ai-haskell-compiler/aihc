{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  CLI.Arguments.Get
-- Copyright   :  (c) OleksandrZhabenko 2022-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A library to process command line arguments in some more convenient way.

module CLI.Arguments.Get where

import GHC.Base
import GHC.List (filter, null, elem)
import Data.Maybe (fromJust)
import qualified Data.Foldable as F
import CLI.Arguments

oneA
 :: (F.Foldable t) => String -> t Arguments -> Bool
oneA xs ys = F.any (\(A ts) -> ts == xs) ys

oneB
 :: (F.Foldable t) => String -> t Arguments -> Bool
oneB xs ys = F.any (\(B _ zs _) -> zs == xs) ys

oneC
 :: (F.Foldable t) => String -> t Arguments -> Bool
oneC xs ys = F.any (\(C zs _) -> zs == xs) ys

listA
 :: (F.Foldable t) => [String] -> t Arguments -> Bool
listA xss ys
  | null xss = False
  | otherwise = F.any (\(A ts) -> ts `elem` xss) ys

listB
 :: (F.Foldable t) => [String] -> t Arguments -> Bool
listB xss ys
  | null xss = False
  | otherwise = F.any (\(B _ zs _) -> zs `elem` xss) ys

listC
 :: (F.Foldable t) => [String] -> t Arguments -> Bool
listC xss ys
  | null xss = False
  | otherwise = F.any (\(C zs _) -> zs `elem` xss) ys

getA
 :: (F.Foldable t) => String -> t Arguments -> String
getA xs ys
  | oneA xs ys = (\(A ts) -> ts) . fromJust . F.find (\(A rs) -> rs == xs) $ ys
  | otherwise = []

getB
 :: (F.Foldable t) => String -> t Arguments -> [String]
getB xs ys
  | oneB xs ys =  (\(B _ _ yss) -> yss) . fromJust . F.find (\(B _ zs _) -> zs == xs) $ ys
  | otherwise = []

getC
 :: (F.Foldable t) => String -> t Arguments -> [String]
getC xs ys
  | oneC xs ys = (\(C _ yss) -> yss) . fromJust . F.find (\(C zs _) -> zs == xs) $ ys
  | otherwise = []

getLstA
 :: (F.Foldable t) => [String] -> t Arguments -> [String]
getLstA xss ys
  | listA xss ys = filter (not . null) . map (\xs -> getA xs ys) $ xss
  | otherwise = []

getLstB
 :: (F.Foldable t) => [String] -> t Arguments -> [[String]]
getLstB xss ys
  | listB xss ys = filter (not . null) . map (\xs -> getB xs ys) $ xss
  | otherwise = []

getLstC
 :: (F.Foldable t) => [String] -> t Arguments -> [[String]]
getLstC xss ys
  | listC xss ys = filter (not . null) . map (\xs -> getC xs ys) $ xss
  | otherwise = []

