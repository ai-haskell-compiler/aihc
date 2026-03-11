{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.Calc
Description : Translate SymopGeom to matrix
Copyright   : (c) Jun Narumi, 2018-2020
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.Calc (
    deriveSymmetryOperation
    , correpondToRotInversion
    , correpondToNFoldRotation
    , correpondToGlideOrReflection
  ) where

import Data.Ratio
import Data.Matrix
import Data.Fixed
import Data.Maybe
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.Symbol

-- (<|>)の衝突を避けたくてParse.hsと分けてある

deriveSymmetryOperation :: (Monad m, Integral a)
                        => (SymbolSenseVectorOrientation -> m String)
                        -> SymbolSenseVectorOrientation
                        -> m (Matrix (Ratio a))
deriveSymmetryOperation lookupFunc elements = calcMatrix elements <$> lookupFunc elements

calcMatrix :: Integral a => SymbolSenseVectorOrientation -> String -> Matrix (Ratio a)
calcMatrix (symbol,sense,vector,orientation) = calc vector' orientation
    where
      vector' = fromMaybe vector . fromSymbol $ symbol
      fromSymbol A = Just "1/2,0,0"
      fromSymbol B = Just "0,1/2,0"
      fromSymbol C = Just "0,0,1/2"
      fromSymbol _   = Nothing

-- 対称操作の幾何表現から行列表現へ変換する際の計算部分
calc :: Integral a => String -> String -> String -> Matrix (Ratio a)
calc vector orientation transCoord
   -- matrixWの回転操作部分と生成したwを連結します
   = totalPart matrixW vector' orientation'
    where
      -- 文字列から各種行列（ベクター）を生成します
      matrixW = fromXYZ'' transCoord
      vector' = fromVec vector
      orientation' = fromVec orientation
      -- ベクトル表記から3x1行列を生成する
      fromVec = transPart . fromXYZ''

totalPart :: Integral a => Matrix (Ratio a) -> Matrix (Ratio a) -> Matrix (Ratio a) -> Matrix (Ratio a)
totalPart matrixW vector orientation
   -- matrixWの回転操作部分と生成したwを連結します
   = rotPart matrixW <|> transPart' matrixW vector orientation

transPart' :: Integral a => Matrix (Ratio a) -> Matrix (Ratio a) -> Matrix (Ratio a) -> Matrix (Ratio a)
transPart' matrixW vector orientation
   = elementwiseMod1 w
    where
      -- 0 <= x < 1 となるように全要素の剰余をとる
      elementwiseMod1 m = flip mod' 1 <$> m
      -- wl = (I-W)x の計算をします
      wl = multStd matrixIW orientation
      wg = vector
      -- w = wl + wgの計算をします
      -- inversionの場合、参照論文に記載がないが、別の計算が必要となり、w関数の最初の行
      -- 試行錯誤でみつけたもので根拠となる論文をまだみつけていないが、
      -- (ii) (a)の逆算でこうなった
      w | isRotInversion matrixW = multStd matrixIW vector
        | otherwise              = elementwise (+) wl wg
      -- 行列 I-W
      matrixIW = iw matrixW

isRotInversion :: Integral a => Matrix (Ratio a) -> Bool
isRotInversion matrix
    = correpondToRotInversion tr det
    where
      tr  = trace (rotPart matrix)
      det = detLU (rotPart matrix)
    
type Trace a = Ratio a
type Determinant a = Ratio a

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
correpondToRotInversion :: Integral a => Trace a -> Determinant a -> Bool
correpondToRotInversion (-3) (-1) = True -- -1
correpondToRotInversion (-2) (-1) = True -- -6
correpondToRotInversion (-1) (-1) = True -- -4
correpondToRotInversion   0  (-1) = True -- -3
correpondToRotInversion   _    _  = False

correpondToNFoldRotation :: Integral a => Trace a -> Determinant a -> Bool
correpondToNFoldRotation (-1) 1 = True -- 2
correpondToNFoldRotation   0  1 = True -- 3
correpondToNFoldRotation   1  1 = True -- 4
correpondToNFoldRotation   2  1 = True -- 6
correpondToNFoldRotation   _  _ = False

correpondToGlideOrReflection :: Integral a => Trace a -> Determinant a -> Bool
correpondToGlideOrReflection 1 (-1) = True -- m
correpondToGlideOrReflection _   _  = False
