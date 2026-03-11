{-# LANGUAGE CPP #-}

{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase
Description : Part of matrix reader (Glide and Reflection)
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase (
  glideOrReflectionCase,
  ) where

import Control.Monad

import Data.Ratio
import Data.List (transpose)
import Data.Matrix hiding (transpose)
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ
-- import qualified Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation as S
import qualified Data.Matrix.SymmetryOperationsSymbols.SymopGeom as S

#if MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

-- | Case (ii) (c) W corresponds to a (glide) reflection
#if MIN_VERSION_base(4,13,0)
glideOrReflectionCase :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f (S.SymopGeom a)
#else
glideOrReflectionCase :: (Monad m, Integral a) => Matrix (Ratio a) -> m (S.SymopGeom a)
#endif
glideOrReflectionCase m = arrange m <$> solvingEquation m

arrange :: Integral a => Matrix (Ratio a) -> [Ratio a] -> S.SymopGeom a
arrange m ans
      | sym == M           = S.Reflection { S.plane = location }
      | sym `elem` [A,B,C] = S.GlideABC { S.abc = abc, S.plane = location }
      | otherwise          = S.GlideDGN { S.dgn = dgn, S.glide = (a,b,c), S.plane = location }
      where
        glidePart@[a,b,c] = toList (wg m)
        sym = millerSymbol (orientationOf m) glidePart
        abc | sym == A = S.A
            | sym == B = S.B
            | sym == C = S.C
        dgn | sym == D = S.D
            | sym == G = S.G
            | sym == N = S.N
        location = toLists $ locationOf m <|> fromList 3 1 ans

        
-- for repl check
-- "-y+1/2,-x,z+3/4"
-- " d (1/4,-1/4,3/4) x+1/4,-x,z"
testMat = fromList 3 4 [0,-1,0,1%2, -1,0,0,0, 0,0,1,3%4]

-- Ww + w = t
t :: (Fractional b, Eq b) => Matrix b -> Matrix b
t mat = let (_W,_w,_,_) = splitBlocks 3 3 mat
        in elementwise (+) (_W `multStd` _w) _w

-- glide part
wg :: (Fractional b, Eq b) => Matrix b -> Matrix b
wg mat = fmap (/2) (t mat)

wl :: (Fractional b, Eq b) => Matrix b -> Matrix b
wl mat = elementwise (-) (transPart mat) (wg mat)

-- solving equation (W,wl)x = x
solvingEquation' :: Integral a => Matrix (Ratio a) -> Maybe (Matrix (Ratio a))
solvingEquation' mat = solve (iw mat) (wl mat)

-------------------

solvingEquation'' :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation'' mat = adjustAnswerOnPlane mat . toList =<< solvingEquation' mat

#if MIN_VERSION_base(4,13,0)
solvingEquation :: (Integral a, MonadFail f) => Matrix (Ratio a) -> f [Ratio a]
#else
solvingEquation :: (Integral a, Monad m) => Matrix (Ratio a) -> m [Ratio a]
#endif
solvingEquation mat = case solvingEquation'' mat of
  Nothing -> fail "<GlideOrReflection> when calculate equation solve."
  Just a -> return a

normalOf :: (Integral a,Num b) => Matrix (Ratio a) -> [b]
normalOf = axisOf

adjustAnswerOnPlane :: Integral a => Matrix (Ratio a) -> [Ratio a] -> Maybe [Ratio a]
adjustAnswerOnPlane mat s = intersectSegmentAndPlane (answerAdjustSegment mat) (plane (normalOf mat) s)

type Point a = [a]
type Vector a = [a]
type Plane a = (Vector a,a)
type Segment a = (Point a,Point a)

dot :: (Num a) => Vector a -> Vector a -> a
dot vecA vecB = sum $ zipWith (*) vecA vecB

plane :: (Num a) => Vector a -> Point a -> Plane a
plane normal point = (normal,d)
  where
    d = dot normal point

intersectSegmentAndPlane' :: (Ord c, Fractional c) => Segment c -> Plane c -> Point c
intersectSegmentAndPlane' (pointA,pointB) (planeN,planeD) = zipWith (+) pointA (map (*t) ab)
  where
    ab = zipWith (-) pointB pointA
    t = (planeD - dot planeN pointA) / dot planeN ab

-- 検算。repl用
check :: (Ord c, Fractional c) => Segment c -> Plane c -> Bool
check (pointA,pointB) (planeN,planeD) = t' /= 0 && t >= 0 && t <= 1.0
  where
    ab = zipWith (-) pointB pointA
    t' = dot planeN ab
    t = (planeD - dot planeN pointA) / dot planeN ab

intersectSegmentAndPlane :: (Ord c, Fractional c) => Segment c -> Plane c -> Maybe [c]
intersectSegmentAndPlane s@(pointA,pointB) p@(planeN,planeD)
  = if check s p
    then Just $ intersectSegmentAndPlane' s p
    else Nothing

answerAdjustSegment :: (Integral a, Num b) => Matrix (Ratio a) -> Segment b
answerAdjustSegment mat = (base, fmap negate base)
  where
    base = case axisOf mat of
      [ 0, 0, 1] -> [0,0,1]
      [ 0, 1, 0] -> [0,1,0]
      [ 1, 0, 0] -> [1,0,0]
      [ 1, 1, 0] -> [1,0,0]
      [ 1, 0, 1] -> [1,0,0]
      [ 0, 1, 1] -> [0,1,0]
      [ 1,-1, 0] -> [1,0,0]
      [-1, 0, 1] -> [1,0,0]
      [ 0, 1,-1] -> [0,1,0]
      [ 1, 2, 0] -> [1,0,0]
      [ 2, 1, 0] -> [1,0,0]
      --
      [ 2,-1, 0] -> [1,0,0]
      [-1, 2, 0] -> [1,0,0]
      a -> error $ "unknown normal of glide plane" ++ show a

--------------

data GlideType = M | A | B | C | N | D | G deriving (Eq,Show)

type Orientation a = [Ratio a]
type GlideVector a = [Ratio a]

millerSymbol :: (Integral a) => Orientation a -> GlideVector a -> GlideType
millerSymbol o v

      | v == [   0,   0,   0] = M

      | v == [ 1%2,   0,   0] = A
      | v == [   0, 1%2,   0] = B
      | v == [   0,   0, 1%2] = C
      
      | (-1) `notElem` o && vv [_12all,_12all,_12all] = N
      | (-1) `elem` o    && vv [_12   ,_12   ,_12   ] = N

      | o == [ 0, 0, 1] &&  vv [_14w,_14w,_0  ] = D
      | o == [ 1, 0, 0] &&  vv [_0  ,_14w,_14w] = D
      | o == [ 0, 1, 0] &&  vv [_14w,_0  ,_14w] = D
      | o == [ 1, 1, 0] &&  vv [_14v,_14v,_14w] = D
      | o == [ 0, 1, 1] &&  vv [_14w,_14v,_14v] = D
      | o == [ 1, 0, 1] &&  vv [_14v,_14w,_14v] = D
      | o == [ 1,-1, 0] && (vv [_14 ,_14 ,_14w] || vv [_34,_34,_34]) = D
      | o == [ 0, 1,-1] && (vv [_14w,_14 ,_14 ] || vv [_34,_34,_34]) = D
      | o == [-1, 0, 1] && (vv [_14 ,_14w,_14 ] || vv [_34,_34,_34]) = D

      | otherwise = G
      
      where
        _0 n = n == 0
        _12 n = n == 1%2
        _12all n = n `elem` [-1%2,0,1%2]
        _14 n = n == 1%4
        _14v n = abs n == 1%4
        _14w n = n == 1%4 || n == 3%4
        _34 n = n == 3%4
        vv p = and $ zipWith ($) p v

