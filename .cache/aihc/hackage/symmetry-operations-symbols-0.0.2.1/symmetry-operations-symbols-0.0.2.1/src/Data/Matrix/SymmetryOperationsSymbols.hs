{-# LANGUAGE FlexibleInstances, CPP #-}
{-|
Module      : Data.Matrix.SymmetryOperationsSymbols
Description : Read and Display Geometric representation of symmetry operation
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

Haskell Derivation of symbols and coordinate triplets Library

[References]

1. W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

2. Wondratschek, H. & Neubu ̈ser, J. (1967). Determination of the symmetry elements of a space group from the ‘general positions’ listed in International Tables for X-ray Crystallography, Vol. I. Acta Cryst. 23, 349–352.

-}
module Data.Matrix.SymmetryOperationsSymbols (
  fromMatrix,
  fromMatrix',
  readMatrix,
  readMatrix',
  toMatrix,
  toMatrixHex,
  notHexagonal,
  hexagonal,
  liftError,
  ) where

import Data.Ratio (Ratio)    
import Data.Matrix (Matrix,detLU,trace,identity)
import Text.ParserCombinators.Parsec (ParseError,parse)

import Data.Matrix.SymmetryOperationsSymbols.Common

import Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase
import Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase
import Data.Matrix.SymmetryOperationsSymbols.RotationCase
import Data.Matrix.SymmetryOperationsSymbols.RotInversionCase

import Data.Matrix.SymmetryOperationsSymbols.Parser
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom
import Data.Matrix.SymmetryOperationsSymbols.Calc

import Data.Matrix.SymmetryOperationsSymbols.PlainText

-- for doctest
import Data.Matrix.AsXYZ (fromXYZ,prettyXYZ)

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
instance MonadFail (Either String) where
  fail = Left
#endif

-- | Derivation of geometric representation of symmetry operations from given matrix of symmetry operations
--
-- jpn) 与えられた対称操作の行列から、対称操作の幾何的表現を導出
--
-- >>> fromMatrix . fromXYZ $ "x,y,z"
-- Right " 1 "
--
-- >>> fromMatrix . fromXYZ $ "-y,x,-z"
-- Right "-4- 0,0,z; 0,0,0"
--
fromMatrix :: Integral a =>
              Matrix (Ratio a) -- ^ 3x4 or 4x4 Matrix
           -> Either String String
fromMatrix = fromMatrix'

-- | Derivation of geometric representation of symmetry operations from given matrix of symmetry operations
--
-- jpn) 与えられた対称操作の行列から、対称操作の幾何的表現を導出
--
#if MIN_VERSION_base(4,13,0)
fromMatrix' :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f String
#else
fromMatrix' :: (Monad m, Integral a) => Matrix (Ratio a) -> m String
#endif
fromMatrix' m = showAsPlainText <$> readMatrix' m

readMatrix :: Integral a =>
              Matrix (Ratio a) -- ^ 3x4 or 4x4 Matrix
           -> Maybe (SymopGeom a)
readMatrix = readMatrix'

-- | Derivation of geometric representation of symmetry operations from given matrix of symmetry operations
--
-- jpn) 与えられた対称操作の行列から、対称操作の幾何的表現を導出
--
#if MIN_VERSION_base(4,13,0)
readMatrix' :: (MonadFail m, Integral a) => Matrix (Ratio a) -> m (SymopGeom a)
#else
readMatrix' :: (Monad m, Integral a) => Matrix (Ratio a) -> m (SymopGeom a)
#endif
readMatrix' m
  -- (i)
  | rotPart m == identity 3             = unitMatrixCase m
  -- (ii) (a)
  | correpondToRotInversion tr det      = rotInversionCase m
  -- -- (ii) (b)
  | correpondToNFoldRotation tr det     = nFoldRotationCase m
  -- -- (ii) (c)
  | correpondToGlideOrReflection tr det = glideOrReflectionCase m
  -- --
  | otherwise = fail $ "coordinate triplet '" ++ prettyXYZ m ++ "' is not symmetry operation."
  where
  tr  = trace (rotPart m)
  det = detLU (rotPart m)

-- | Derivation of matrix representation from a string of geometric representations of symmetric operations
-- for cubic, tetragonal, orthorhombic, monoclinic, triclinic or rhombohedral.
--
-- jpn) 対称操作の幾何的表現の文字列から行列表現の導出
--
-- >>> prettyXYZ <$> toMatrixHex "-4- 0,0,z; 0,0,0"
-- Right "-y,x,-z"
--
toMatrix :: Integral a =>
            String -- ^ like " -1 0,0,0"
         -> Either ParseError (Matrix (Ratio a)) -- ^ 3x4 Matrix
toMatrix st = parse notHexagonal st st

-- | Derivation of matrix representation from a string of geometric representations of symmetric operations
-- for hexagonal.
--
-- jpn) 対称操作の幾何的表現の文字列から行列表現の導出(六方晶用)
--
-- >>> prettyXYZ <$> toMatrixHex "-3+ 0,0,z; 0,0,0"
-- Right "y,y-x,-z"
--
toMatrixHex :: Integral a =>
               String -- ^ like " -1 0,0,0"
            -> Either ParseError (Matrix (Ratio a)) -- ^ 3x4 Matrix
toMatrixHex st = parse hexagonal st st

#if MIN_VERSION_base(4,13,0)
liftError :: MonadFail m => Either ParseError a -> m a
#else
liftError :: Monad m => Either ParseError a -> m a
#endif
liftError (Left s) = fail ""
liftError (Right m) = return m
