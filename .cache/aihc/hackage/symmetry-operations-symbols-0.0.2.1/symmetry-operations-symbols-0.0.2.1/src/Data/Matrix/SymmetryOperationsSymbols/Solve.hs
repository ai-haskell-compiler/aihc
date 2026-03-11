{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.Solve
Description : Solving linear equation part
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.Solve (
  solve,
  ) where

import Data.Ratio
import Data.Matrix

forsub :: Num a => [a] -> [a] -> [a] -> [a]
forsub b lower y = y ++ [(b !! length y) - sum (zipWith (*) y lower)]

forward :: Num a => Matrix a -> [a] -> [a]
forward lower b = (foldl1 (.) $ reverse $ map (forsub b) $ toLists lower) []

backsub :: (Eq a, Fractional a) => [a] -> [a] -> [a] -> [a]
backsub ys zs xs = xs ++ [g]
  where
    d = reverse zs !! length xs
    e = reverse ys !! length xs
    c = sum $ zipWith (*) xs (reverse zs)
    g = if d == 0 then 0 else (e - c) / d

backward :: (Fractional a, Eq a) => Matrix a -> [a] -> [a]
backward upper y = (foldl1 (.) $ map (backsub y) $ toLists upper) []

solve' :: (Integral a) =>
          Matrix (Ratio a)
       -> [Ratio a]
       -> Maybe [Ratio a]
solve' mat vec = do
      (u,l,p,q,_,_) <- luDecomp' mat
      return $ permutation q . reverse . backward u . forward l . permutation p $ vec
  where
    permutation n v = toList (multStd n (fromList (length v) 1 v))

solve :: (Integral a) =>
         Matrix (Ratio a) -- ^ jpn) 正方行列
      -> Matrix (Ratio a) -- ^ jpn) ベクトル
      -> Maybe (Matrix (Ratio a))
solve mat vec = fromList (nrows vec) (ncols vec) <$> solve' mat (toList vec)
