{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase
Description : Part of matrix reader (Translate and Identity)
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}

module Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase (
  unitMatrixCase,
  -- unitMatrixCase2,
  ) where

import Data.Ratio (Ratio)    
import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom

-- | Case (i) The matrix W is the unit matrix:
unitMatrixCase w
    | all (== 0) w' = return Identity
    | otherwise     = return $ Translation { vector = (a,b,c) }
    where
      w'@[a,b,c] = toList . transPart $ w

