{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.PlainText
Description : Output as plain text
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.PlainText (
  showAsPlainText,
) where

import Data.List
import Data.Ratio
import Data.Ratio.Slash
import Data.Matrix
import Data.Matrix.AsXYZ

import Data.Matrix.SymmetryOperationsSymbols.SymopGeom

-- |
-- >>> triplet (1%2,3%4,5%6)
-- "1/2,3/4,5/6"
triplet :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
triplet (a,b,c) = intercalate "," . map (show . Slash) $ [a,b,c]

-- |
-- >>> triplet (1%2,3%4,5%6)
-- "(1/2,3/4,5/6)"
tripletParen :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
tripletParen = ("(" ++) . (++ ")") . triplet

showSense :: Sense -> String
showSense Positive = "+"
showSense Negative = "-"

showPlane :: Integral a => SymopGeom a -> String
showPlane = prettyXYZ . fromLists . plane

showAxis :: Integral a => SymopGeom a -> String
showAxis = prettyXYZ . fromLists . axis

showAsPlainText :: Integral a => SymopGeom a -> String
showAsPlainText Identity = " 1 "
showAsPlainText val@Translation {}
  = concat [" t ",tripletParen $ vector val," "]
showAsPlainText val@Reflection {}
  = concat [" m  ", showPlane val]
showAsPlainText val@GlideABC {}
  = concat [" ",showABC $ abc val,"  ", showPlane val]
showAsPlainText val@GlideDGN {}
  = concat [" ",showDGN $ dgn val," ",tripletParen $ glide val," ", showPlane val]
showAsPlainText val@TwoFoldRotation {}
  = concat [" 2  ",showAxis val]
showAsPlainText val@TwoFoldScrew {}
  = concat [" 2 ",tripletParen $ vector val," ",showAxis val]
showAsPlainText val@NFoldRotation {}
  = concat [" ",showSymbol (nFold val),showSense (sense val)," ",showAxis val]
showAsPlainText val@NFoldScrew {}
  = concat [" ",showSymbol (nFold val),showSense (sense val),tripletParen $ vector val," ",showAxis val]
showAsPlainText val@Inversion {}
  = concat ["-1 ",triplet $ centre val]
showAsPlainText val@RotInversion {}
  = concat ["-",showSymbol (nFold val),showSense (sense val)," ",showAxis val,"; ",triplet $ point val]

