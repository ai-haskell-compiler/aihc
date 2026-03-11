{- |
Module      : Data.Matrix.AsXYZ.ParseXYZ
Copyright   : (c) Jun Narumi 2020
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Jones-Faithful notation plain text printer.

-}
module Data.Matrix.AsXYZ.Plain (
  showAs,
  showAs',
  xyzLabel,
  abcLabel,
)where

import Data.Ratio (Ratio(..),numerator,denominator)
import Data.List (intercalate)
import Data.Matrix (Matrix(..),toLists)
import Numeric (showInt)
import Data.Matrix.AsXYZ.Common(Sign(..),Var(..),rowVars)

num :: Integral a => Ratio a -> String
num r | d == 1 = int n
      | otherwise = frac n d
  where
    nn = numerator r
    d = denominator r
    n = if nn >= 0 then nn else nn * (-1)
    showInt' = flip showInt "" -- Constraint Show class を回避するため
    int n = showInt' n
    frac n d = showInt' n ++ "/" ++ showInt' d

rowStr label (x:xs) = texP' label x ++ concatMap (texP label) xs

texP label (P var) = "+" ++ texV label var
texP label (N var) = "-" ++ texV label var
texP _ Zero = "0"

texP' label (P var) = texV label var
texP' label (N var) = "-" ++ texV label var
texP' _ Zero = "0"

texV _      (W n) = num $ n
texV labels var   | 1 == n = label
                  | otherwise = (++ label) . num $ n 
  where
    get (X n) = (0,n)
    get (Y n) = (1,n)
    get (Z n) = (2,n)
    t = get var
    n = snd t
    label = labels !! fst t:[]

showAs' :: (Integral a) => String -> Matrix (Ratio a) -> String
showAs' labels = intercalate "," . map (rowStr labels . rowVars . hoge) . take 2 . toLists
  where
    hoge (x:y:z) = x:y:0:z

showAs :: (Integral a) => String -> Matrix (Ratio a) -> String
showAs labels = intercalate "," . map (rowStr labels . rowVars) . take 3 . toLists

xyzLabel :: String
xyzLabel = "xyz"

abcLabel :: String
abcLabel = "abc"


