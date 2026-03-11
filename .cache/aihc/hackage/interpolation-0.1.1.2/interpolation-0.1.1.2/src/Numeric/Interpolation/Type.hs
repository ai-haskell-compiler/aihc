module Numeric.Interpolation.Type (
   T(..),
   linear,
   hermite1,
   cubicLinear,
   cubicParabola,
   ) where

import qualified Numeric.Interpolation.NodeList as Nodes
import qualified Numeric.Interpolation.Piece as Piece
import qualified Numeric.Interpolation.Basis as Basis
import qualified Numeric.Interpolation.Sample as Sample
import Numeric.Interpolation.Private.Basis (hermite1Split)


{- $setup
>>> import qualified Numeric.Interpolation.Type as Type
>>>
>>> checkOverlap :: Type.T Double y ny -> [Double] -> Double -> Bool
>>> checkOverlap typ xs xi =
>>>    let samples = map fst $ Type.sampleBasisFunctions typ xs xi
>>>    in  all (< minimum samples + Type.basisOverlap typ) samples
>>>
>>> checkOverlapNotTotal :: Type.T Double y ny -> [Double] -> Double -> Bool
>>> checkOverlapNotTotal typ xs xi =
>>>    let samples = map fst $ Type.sampleBasisFunctions typ xs xi
>>>    in  maximum samples - minimum samples < Type.basisOverlap typ
-}


data T x y ny =
   Cons {
      ssvFromNodes :: [x] -> [y] -> String,
      interpolatePiece :: Piece.T x y ny,
      basisOverlap :: Int
         {- ^
         maximum difference of indices of basis functions that overlap plus one
         -},
      basisFunctions :: [x] -> [Nodes.T x ny],
      sampleBasisFunctions :: [x] -> x -> [(Int, y)],
      coefficientsToInterpolator :: [x] -> [y] -> Nodes.T x ny,
      valueFromNode :: ny -> y
   }

{- |
prop> checkOverlap Type.linear
-}
linear :: (Fractional a, Ord a, Show a) => T a a a
linear =
   Cons {
      ssvFromNodes =
         \xs ys -> unlines $ zipWith (\x y -> show x ++ " " ++ show y) xs ys,
      interpolatePiece = Piece.linear,
      basisOverlap = 2,
      basisFunctions = Basis.linear,
      sampleBasisFunctions = Sample.linear,
      coefficientsToInterpolator = Basis.coefficientsToLinear,
      valueFromNode = id
   }

{- |
prop> checkOverlap Type.hermite1
-}
hermite1 :: (Fractional a, Ord a, Show a) => T a a (a, a)
hermite1 =
   Cons {
      ssvFromNodes =
         \xs ys ->
            unlines .
            zipWith (\x (y,dy) -> show x ++ " " ++ show y ++ " " ++ show dy) xs $
            hermite1Split xs ys,
      interpolatePiece = Piece.hermite1,
      basisOverlap = 4,
      basisFunctions = Basis.hermite1,
      sampleBasisFunctions = Sample.hermite1,
      coefficientsToInterpolator = Basis.coefficientsToHermite1,
      valueFromNode = fst
   }

{- |
prop> checkOverlap Type.cubicLinear
-}
cubicLinear :: (Fractional a, Ord a, Show a) => T a a (a, a)
cubicLinear =
   Cons {
      ssvFromNodes =
         \xs ys -> unlines $ zipWith (\x y -> show x ++ " " ++ show y) xs ys,
      interpolatePiece = Piece.hermite1,
      basisOverlap = 4,
      basisFunctions = Basis.cubicLinear,
      sampleBasisFunctions = Sample.cubicLinear,
      coefficientsToInterpolator = Basis.coefficientsToCubicLinear,
      valueFromNode = fst
   }

{- |
prop> checkOverlap Type.cubicParabola
-}
cubicParabola :: (Fractional a, Ord a, Show a) => T a a (a, a)
cubicParabola =
   Cons {
      ssvFromNodes =
         \xs ys -> unlines $ zipWith (\x y -> show x ++ " " ++ show y) xs ys,
      interpolatePiece = Piece.hermite1,
      basisOverlap = 4,
      basisFunctions = Basis.cubicParabola,
      sampleBasisFunctions = Sample.cubicParabola,
      coefficientsToInterpolator = Basis.coefficientsToCubicParabola,
      valueFromNode = fst
   }


_cubicMean :: (Fractional a, Ord a, Show a) => T a a (a, a)
_cubicMean =
   Cons {
      ssvFromNodes =
         \xs ys -> unlines $ zipWith (\x y -> show x ++ " " ++ show y) xs ys,
      interpolatePiece = Piece.hermite1,
      basisOverlap = 4,
      basisFunctions = Basis.cubicParabola, -- Basis.cubicMean,
      sampleBasisFunctions = Sample.cubicParabola, -- Sample.cubicMean,
      coefficientsToInterpolator = Basis.coefficientsToCubicParabola, -- not correct
      valueFromNode = fst
   }
