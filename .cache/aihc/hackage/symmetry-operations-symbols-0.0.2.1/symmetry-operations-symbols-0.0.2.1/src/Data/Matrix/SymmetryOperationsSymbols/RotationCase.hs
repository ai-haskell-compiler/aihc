{-# LANGUAGE CPP #-}

{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.RotationCase
Description : Part of matrix reader (Screw and Rotation)
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.RotationCase (
  nFoldRotationCase,
  ) where

import Data.Ratio
import Data.List (transpose)
import Data.Matrix hiding (transpose)
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ
--import Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

-- | Case (ii) (a) W corresponds to a rotoinversion
#if MIN_VERSION_base(4,13,0)
nFoldRotationCase :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f (SymopGeom a)
#else
nFoldRotationCase :: (Monad m, Integral a) => Matrix (Ratio a) -> m (SymopGeom a)
#endif
nFoldRotationCase m = arrange m <$> solvingEquation m

arrange :: Integral a => Matrix (Ratio a) -> [Ratio a] -> SymopGeom a
arrange m ans 
  | rt == 2 && noScrew = TwoFoldRotation { axis = location }
  | rt == 2            = TwoFoldScrew { axis = location, vector = (sa,sb,sc) }
  | noScrew            = NFoldRotation { nFold = n, sense = sen, axis = location }
  | otherwise          = NFoldScrew { nFold = n, sense = sen, axis = location, vector = (sa,sb,sc) }
      where
        noScrew = all (==0) screwPart 
        rt = rotationType m
        n | rt == 3 = ThreeFold
          | rt == 4 = FourFold
          | rt == 6 = SixFold
        sen = case senseOf m of
          "+" -> Positive
          "-" -> Negative
          _ -> error ""
        s = if null . senseOf $ m then " " else senseOf m
        sym = " " ++ show rt ++ s
        screwPart@[sa,sb,sc] = toList (wg m)
        location = toLists $ locationOf m <|> fromList 3 1 ans
    
--

-- for repl check
-- "-z,-x+1/2,y"
-- " 3+(-1/6,1/6,1/6) x+1/6,-x+1/6,-x"
testMat = fromList 3 4 [ 0,0,-1,0, -1,0,0,1%2, 0,1,0,0 ]

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
rotationType :: (Num a,Eq a,Integral b) => Matrix a -> b
rotationType m
  | tr == (-1) = 2
  | tr ==   0  = 3
  | tr ==   1  = 4
  | tr ==   2  = 6
  where tr = trace $ rotPart m

pow :: (Num a, Integral b) => Matrix a -> b -> Matrix a
pow m 0 = identity 3
pow m n = foldl1 multStd $ replicate (fromIntegral n) (rotPart m)

-- (W,w)^(n-1) = t
t :: (Num b, Eq b) => Matrix b -> Matrix b
t mat = f [ pow mat n | n <- [0..(pred r)] ]
  where
    r = rotationType mat
    w = transPart mat
    g n m = multStd m n
    f l = foldl1 (elementwise (+)) $ map (g w) l

-- | (W,w)^n
wg :: (Fractional b, Eq b) => Matrix b -> Matrix b
wg mat = fmap (/ fromIntegral r) (t mat)
  where
    r = rotationType mat

wl :: (Fractional b, Eq b) => Matrix b -> Matrix b
wl mat = elementwise (-) (transPart mat) (wg mat)

solvingEquation' :: Integral a => Matrix (Ratio a) -> Maybe (Matrix (Ratio a))
solvingEquation' mat = solve (iw mat) (wl mat)

solvingEquation'' :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation'' mat = do
  sol <- solvingEquation' mat
  adjustAnswerOnAxis mat (toList sol)

#if MIN_VERSION_base(4,13,0)
solvingEquation :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f [Ratio a]
#else
solvingEquation :: (Monad m, Integral a) => Matrix (Ratio a) -> m [Ratio a]
#endif
solvingEquation mat = case solvingEquation'' mat of
  Nothing -> fail "<Rot> when calculate equation."
  Just a -> return a

-- 検算。repl用
check mat sol = if toList (multStd (iw mat) (vec sol)) == toList (wl mat)
                then sol
                else error "General solution error."
    where
      vec l = fromList (length l) 1 l
