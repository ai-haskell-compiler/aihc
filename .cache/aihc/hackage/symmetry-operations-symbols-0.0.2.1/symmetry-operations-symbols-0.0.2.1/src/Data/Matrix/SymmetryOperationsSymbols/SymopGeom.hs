{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.SymopGeom
Description : Type of geometric representation
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.SymopGeom where

import Data.Ratio

data ABC = A | B | C deriving (Show,Read,Eq)
data DGN = D | G | N deriving (Show,Read,Eq)
data NFold = ThreeFold | FourFold | SixFold deriving (Show,Read,Eq)
data Sense = Positive | Negative deriving (Show,Read,Eq)

data SymopGeom a
  = Identity
  | Translation     { vector :: (Ratio a,Ratio a,Ratio a) }
  | Reflection      { plane :: [[Ratio a]] }
  | GlideABC        { abc :: ABC, plane :: [[Ratio a]] }
  | GlideDGN        { dgn :: DGN, plane :: [[Ratio a]], glide :: (Ratio a,Ratio a,Ratio a) }
  | TwoFoldRotation { axis :: [[Ratio a]] }
  | TwoFoldScrew    { axis :: [[Ratio a]], vector :: (Ratio a,Ratio a,Ratio a) }
  | NFoldRotation   { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]] }
  | NFoldScrew      { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]], vector :: (Ratio a,Ratio a,Ratio a) }
  | Inversion       { centre :: (Ratio a,Ratio a,Ratio a) }
  | RotInversion    { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]], point :: (Ratio a,Ratio a,Ratio a) }
  deriving (Read,Show,Eq)

showSymbol :: NFold -> String
showSymbol ThreeFold = "3"
showSymbol FourFold  = "4"
showSymbol SixFold   = "6"

showABC :: ABC -> String
showABC A = "a"
showABC B = "b"
showABC C = "c"

showDGN :: DGN -> String
showDGN D = "d"
showDGN G = "g"
showDGN N = "n"
