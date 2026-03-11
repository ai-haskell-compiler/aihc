{-# LANGUAGE CPP #-}
{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.RotInversionCase
Description : Part of matrix reader (Inversion)
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.RotInversionCase (
  rotInversionCase,
  ) where

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ
--import Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

-- | Case (ii) (b) W corresponds to an n-fold rotation
#if MIN_VERSION_base(4,13,0)
rotInversionCase :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f (SymopGeom a)
#else
rotInversionCase :: (Monad m, Integral a) => Matrix (Ratio a) -> m (SymopGeom a)
#endif
rotInversionCase m = arrange m <$> calc m

#if MIN_VERSION_base(4,13,0)
calc :: (MonadFail m, Integral a) => Matrix (Ratio a) -> m ([Ratio a], Matrix (Ratio a))
#else
calc :: (Monad m, Integral a) => Matrix (Ratio a) -> m ([Ratio a], Matrix (Ratio a))
#endif
calc m = (,) <$> solvingEquation2 m <*> solvingEquation1 m

arrange :: Integral a => Matrix (Ratio a) -> ([Ratio a], Matrix (Ratio a)) -> SymopGeom a
arrange m ([],b) = Inversion { centre = (i,j,k) }
  where
    [i,j,k] = toList b
arrange m (a,b) = RotInversion { nFold = n, sense = sen, axis = loc, point = (i,j,k) }
  where
    [i,j,k] = toList b
    rt = rotationType m
    loc = toLists $ locationOf m <|> fromList 3 1 a
    n | rt == -3 = ThreeFold
      | rt == -4 = FourFold
      | rt == -6 = SixFold
    sen = case senseOf m of
      "+" -> Positive
      "-" -> Negative
      _ -> error ""

------------

-- for repl check
-- "-z,-x+1/2,y"
-- " 3+(-1/6,1/6,1/6) x+1/6,-x+1/6,-x"
testMat = fromList 3 4 [ 0,0,1,0, 0,-1,0,1%2, -1,0,0,1%2 ]

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
rotationType :: (Num a,Eq a,Integral b) => Matrix a -> b
rotationType m
  | tr == (-3) = -1
  | tr == (-2) = -6
  | tr == (-1) = -4
  | tr ==   0  = -3
  where
    tr = trace $ rotPart m

wg :: (Fractional b, Eq b) => Matrix b -> Matrix b
wg mat = multStd (rotPart mat) (transPart mat)

wl :: (Fractional b, Eq b) => Matrix b -> Matrix b
wl mat = elementwise (+) (wg mat) (transPart mat)

-- (ii) (a) solving equation (W,w)x = x
#if MIN_VERSION_base(4,13,0)
solvingEquation1 :: (MonadFail m, Integral a) => Matrix (Ratio a) -> m (Matrix (Ratio a))
#else
solvingEquation1 :: (Monad m, Integral a) => Matrix (Ratio a) -> m (Matrix (Ratio a))
#endif
solvingEquation1 mat = case solvingEquation1' mat of
  Nothing -> fail "<RotInv> when calculate equation (W,w)x = x"
  Just a -> return a

solvingEquation1' :: Integral a => Matrix (Ratio a) -> Maybe (Matrix (Ratio a))
solvingEquation1' mat = solve (iw mat) (transPart mat)

-- (ii) (a) solving equation (W,w)^2 x = x
solvingEquation2' :: Integral a => Matrix (Ratio a) -> Maybe (Matrix (Ratio a))
solvingEquation2' mat = solve (iw w2) (wl mat)
  where
    pow2 m = multStd m m
    w2 = pow2 (rotPart mat)

#if MIN_VERSION_base(4,13,0)
solvingEquation2 :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f [Ratio a]
#else
solvingEquation2 :: (Monad m, Integral a) => Matrix (Ratio a) -> m [Ratio a]
#endif
solvingEquation2 mat = case result of
    Nothing -> fail "<RotInv> when calculate equation (W,w)^2 x = x."
    Just a -> return a
  where
    result = do
      sol <- solvingEquation2' mat
      adjustAnswerOnAxis mat (toList sol)


-- 検算。repl用
check mat sol = if multStd (iw2 mat) (vec sol) == wl mat
                then Right sol
                else Left "General solution error."
    where
      vec l = fromList (length l) 1 l
      iw2 m = iw $ multStd _W _W
      _W = rotPart mat
