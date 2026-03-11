{- |
Module      : Data.Matrix.AsXYZ.Common
Copyright   : (c) Jun Narumi 2020
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?
-}
module Data.Matrix.AsXYZ.Common (
  Sign(..),
  Var(..),
  Val(..),
  rowVars,
  ) where

import Data.Ratio
import Numeric

data Sign a
  = P a
  | N a
  | Zero
  deriving (Show,Eq)

data Var a
  = X a
  | Y a
  | Z a
  | W a
  deriving (Show,Eq)

-- 数値の型情報
data Val a
  -- 整数
  = I a
  -- 浮動小数
  | F a
  -- 分数
  | R a
  deriving Show

instance Functor Var where
  fmap f (X a) = X (f a)
  fmap f (Y a) = Y (f a)
  fmap f (Z a) = Z (f a)
  fmap f (W a) = W (f a)

instance Functor Val where
  fmap f (I a) = I (f a)
  fmap f (F a) = F (f a)
  fmap f (R a) = R (f a)

rowVars :: Integral a => [Ratio a] -> [Sign (Var (Ratio a))]
rowVars = reduceVars . sortVars . toVars
    
toVars :: Integral a => [Ratio a] -> [Sign (Var (Ratio a))]
toVars = zipWith (\a b -> hoge a b) [X,Y,Z,W]
      where
        hoge f r | r < 0 = N $ f (r * (-1))
                 | otherwise = P $ f r

sortVars :: Eq a => [Sign (Var a)] -> [Sign (Var a)]
sortVars parts | null hh = parts
               | otherwise = h: filter (/= h) parts
      where
        hh = filter isPrimary parts
        h = head hh

-- 正の係数がついた変数である
isPrimary :: Sign (Var a) -> Bool
isPrimary Zero = False
isPrimary (N _) = False
isPrimary (P (W _)) = False
isPrimary _ = True

reduceVars :: (Eq a, Num a) => [Sign (Var a)] -> [Sign (Var a)]
reduceVars rr = if null a then [Zero] else a
      where
        a = filter (not . isZero) rr

isZero :: (Eq a, Num a) => Sign (Var a) -> Bool
isZero (N v) = isZero' v
isZero (P v) = isZero' v
isZero Zero = True

isZero' :: (Eq a, Num a) => Var a -> Bool
isZero' (X n) = n == 0
isZero' (Y n) = n == 0
isZero' (Z n) = n == 0
isZero' (W n) = n == 0

