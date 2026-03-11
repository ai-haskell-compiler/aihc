{-# LANGUAGE CPP #-}

{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.Common
Description : Utilities and Matrix table
Copyright   : (c) Jun Narumi, 2018
License     : MIT
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.Common (
  ErrorMessage,
  SymbolSenseVectorOrientation,
  -- maybeToEither,
  rotPart,
  transPart,
  iw,
--  triplet,
--  tripletParen,
  adjustAnswerOnAxis,
  axisOf,
  senseOf,
  locationOf,
  orientationOf,
  properMatrixW,
  hexagonalMatrixW,
  fromXYZ'',
  MatrixForPointGroupCorrespondingSymmetryElement(..),
  properMatricesForPointGroup,
  hexagonalMatricesForPointGroup,
  matricesForPointGroupCorrespondingSymmetryElements,
  ) where

import Data.List
import Data.Ratio
import Data.Matrix
import Data.Maybe
import Data.Matrix.AsXYZ
import Data.Ratio.Slash

import Data.Matrix.SymmetryOperationsSymbols.Symbol

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif

type ErrorMessage = String
type SymbolSenseVectorOrientation = (Symbol,String,String,String)

-- | calculate (I-W)
iw :: Num c => Matrix c -> Matrix c
iw = elementwise (-) (identity 3) . rotPart

-- | 3x3 rotation part of matrix
rotPart :: Matrix a -> Matrix a
rotPart = submatrix 1 3 1 3

-- | 3x1 translation part of matrix
transPart :: Matrix a -> Matrix a
transPart = submatrix 1 3 4 4

-- | jpn) 解を解直線上で補正
adjustAnswerOnAxis :: (Eq b, Fractional b, Integral a) =>  Matrix (Ratio a) -> [b] -> Maybe [b]
adjustAnswerOnAxis mat ans = do
  let basis = axisOf mat
  adjustRate <- rate ans basis
  -- 算出した補正量と解の積を返す
  return $ zipWith (+) ans (fmap (*adjustRate) basis)

-- 計算対象となる要素の補正値
-- 解の直線を算出することができなかったので、データを参照することで代替している
rate :: (Eq a, Fractional a) => [a] -> [a] -> Maybe a
rate ans [] = Just 1
rate ans basis
  = listToMaybe
    . map snd . filter fst . zip select
    $ zipWith rating ans basis
  where
    rating ans basis = if basis == 0 then 0 else -ans / basis
    select = case basis of
        [ 0, 0, 1] -> [False,False, True] -- OK
        [ 0, 1, 0] -> [False, True,False] -- OK
        [ 1, 0, 0] -> [ True,False,False] -- OK
        [ 1, 1, 1] -> [False,False, True] -- ??
        [ 1,-1,-1] -> [False,False, True] -- OK
        [-1,-1, 1] -> [False,False, True] -- OK
        [-1, 1,-1] -> [False,False, True] -- OK
        [ 1, 1, 0] -> [ True,False,False] -- OK
        [ 1, 0, 1] -> [False,False, True] -- OK
        [ 0, 1, 1] -> [False,False, True] -- OK
        [ 1,-1, 0] -> [ True,False,False] -- OK
        [-1, 0, 1] -> [False,False, True] -- OK
        [ 0, 1,-1] -> [False,False, True] -- OK
        [ 1, 2, 0] -> [False, True,False] -- ??
        [ 2, 1, 0] -> [ True,False,False] -- ??
        _ -> error ""

----------------------

axisOf :: (Integral a,Num b) => Matrix (Ratio a) -> [b]
axisOf mat = fromJust $ fmap fromIntegral . axis <$> searchByRotationPart mat

senseOf :: (Integral a) => Matrix (Ratio a) -> String
senseOf mat = fromJust $ sense <$> searchByRotationPart mat

locationOf :: (Integral a) => Matrix (Ratio a) -> Matrix (Ratio a)
locationOf mat = fromJust $ location <$> searchByRotationPart mat

orientationOf :: (Integral a) => Matrix (Ratio a) -> [Ratio a]
orientationOf mat = fromJust $ orientation <$> searchByRotationPart mat

-- テーブルのレコードからほしい要素を取り出す関数たち
axis (a,s,b,c,d,e,f,g) = if null g then e else g
isHex (a,s,b,c,d,e,f,g) = a == Hexagonal
sense (_,_,_,s,_,_,_,_) = s
location (_,_,_,_,o,_,_,_) = rotPart . fromXYZ'' $ o
orientation (a,s,b,c,d,e,f,g) = fmap fromIntegral e

-- | jpn) 入力文字列が空だった場合に、4x4の0行列を返す
fromXYZ'' s = fromMaybe (zero 4 4) (fromXYZ' s)

-- | jpn) 入力の行列に該当するレコードを返却する
-- jpn) 行列の内容に対して一意なので、hexagonalを区別する必要がない
searchByRotationPart m = lookup (rotPart m) d
  where
    d = map (\i@(a,s,b,c,d,e,f,g)->(rotPart . fromXYZ'' $ f,i)) tbl

type MatrixLookupRecord a = ((Symbol,Sense,Matrix (Ratio a)),TransformedCoordinate)

lookupSSVO :: (Integral a) => (Symbol,String,String) -> [MatrixLookupRecord a] -> Maybe TransformedCoordinate
lookupSSVO (sym, sen, axis) d = lookup (sym, sen, rotPart . fromXYZ'' $ axis) d

#if MIN_VERSION_base(4,11,0)
lookupMatrixM :: (Monad m,MonadFail m) => Integral a => String -> [MatrixLookupRecord a] -> SymbolSenseVectorOrientation -> m TransformedCoordinate
properMatrixW :: (Monad m,MonadFail m) => SymbolSenseVectorOrientation -> m TransformedCoordinate
hexagonalMatrixW :: (Monad m,MonadFail m) => SymbolSenseVectorOrientation -> m TransformedCoordinate
#else
lookupMatrixM :: (Monad m) => Integral a => String -> [MatrixLookupRecord a] -> SymbolSenseVectorOrientation -> m TransformedCoordinate
properMatrixW :: (Monad m) => SymbolSenseVectorOrientation -> m TransformedCoordinate
hexagonalMatrixW :: (Monad m) => SymbolSenseVectorOrientation -> m TransformedCoordinate
#endif

lookupMatrixM reason dataTable (sy,se,_,el)
   = case lookupSSVO (primeSymbol sy, se, el) dataTable of
      Nothing -> fail reason
      Just c  -> return c

properMatrixW = lookupMatrixM "matrix W not found (proper)." (fromTbl properMatricesForPointGroup)

hexagonalMatrixW = lookupMatrixM "matrix W not found (hexagonal)." (fromTbl hexagonalMatricesForPointGroup)

fromTbl :: (Integral a) => [Tbl a] -> [MatrixLookupRecord a]
fromTbl = map tblToMLR

tblToMLR :: (Integral a) => Tbl a -> MatrixLookupRecord a
tblToMLR (a,s,b,c,d,e,f,g) = ((s,c,rotPart . fromXYZ'' $ d),f)

properMatricesForPointGroup :: Integral a => [Tbl a]
properMatricesForPointGroup = filter (not . isHex) tbl

hexagonalMatricesForPointGroup :: Integral a => [Tbl a]
hexagonalMatricesForPointGroup = filter isHex tbl ++ filter (not . isHex) tbl

primeSymbol :: Symbol -> Symbol
primeSymbol T = Id
primeSymbol A = M
primeSymbol B = M
primeSymbol C = M
primeSymbol D = M
primeSymbol N = M
primeSymbol G = M
primeSymbol otherSymbol = otherSymbol

data TableType
   = Hexagonal -- ^ for hexagonal and trigonal crystal systems.
   | Others    -- ^ for cubic, tetragonal, orthorhombic, monoclinic and triclinic crystal systems.
   deriving (Eq)

type SymbolLabel = String
type Sense = String
type SymmetryElement = String
type Orientation a = [a] -- orientation or location
type TransformedCoordinate = String
type AxisOrNormal a = [a]

type Tbl a = MatrixForPointGroupCorrespondingSymmetryElement a

type MatrixForPointGroupCorrespondingSymmetryElement a
  = ( TableType, Symbol, SymbolLabel, Sense, SymmetryElement, Orientation a, TransformedCoordinate, AxisOrNormal a )

tbl :: Integral a => [MatrixForPointGroupCorrespondingSymmetryElement a]
tbl = matricesForPointGroupCorrespondingSymmetryElements

-- | [Reference]
--
-- W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
--
-- listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.
--
-- Table 11.2.2.1. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a cubic, tetragonal, orthorhombic,
-- monoclinic, triclinic or rhombohedral coordinate system
--
-- Table 11.2.2.2. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a hexagonal coordinate system
--
matricesForPointGroupCorrespondingSymmetryElements ::
  Integral a => 
  [MatrixForPointGroupCorrespondingSymmetryElement a]
matricesForPointGroupCorrespondingSymmetryElements = [
-- Table 11.2.2.1. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a cubic, tetragonal, orthorhombic,
-- monoclinic, triclinic or rhombohedral coordinate system
  (    Others,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  (    Others,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  (    Others,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0],   "-x,y,-z", []),
  (    Others,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0],   "x,-y,-z", []),
  (    Others,  R3,  "3", "+",   "x,x,x", [ 1, 1, 1],     "z,x,y", []),
  (    Others,  R3,  "3", "+", "x,-x,-x", [ 1,-1,-1],   "-z,-x,y", []),
  (    Others,  R3,  "3", "+", "-x,x,-x", [-1, 1,-1],   "z,-x,-y", []),
  (    Others,  R3,  "3", "+", "-x,-x,x", [-1,-1, 1],   "-z,x,-y", []),
  (    Others,  R3,  "3", "-",   "x,x,x", [ 1, 1, 1],     "y,z,x", []),
  (    Others,  R3,  "3", "-", "x,-x,-x", [ 1,-1,-1],   "-y,z,-x", []),
  (    Others,  R3,  "3", "-", "-x,x,-x", [-1, 1,-1],   "-y,-z,x", []),
  (    Others,  R3,  "3", "-", "-x,-x,x", [-1,-1, 1],   "y,-z,-x", []),
  (    Others,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  (    Others,  R2,  "2",  "",   "x,0,x", [ 1, 0, 1],    "z,-y,x", []),
  (    Others,  R2,  "2",  "",   "0,y,y", [ 0, 1, 1],    "-x,z,y", []),
  (    Others,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
  (    Others,  R2,  "2",  "",  "-x,0,x", [-1, 0, 1],  "-z,-y,-x", []),
  (    Others,  R2,  "2",  "",  "0,y,-y", [ 0, 1,-1],  "-x,-z,-y", []),
  (    Others,  R4,  "4", "+",   "0,0,z", [ 0, 0, 1],    "-y,x,z", []),
  (    Others,  R4,  "4", "+",   "0,y,0", [ 0, 1, 0],    "z,y,-x", []),
  (    Others,  R4,  "4", "+",   "x,0,0", [ 1, 0, 0],    "x,-z,y", []),
  (    Others,  R4,  "4", "-",   "0,0,z", [ 0, 0, 1],    "y,-x,z", []),
  (    Others,  R4,  "4", "-",   "0,y,0", [ 0, 1, 0],    "-z,y,x", []),
  (    Others,  R4,  "4", "-",   "x,0,0", [ 1, 0, 0],    "x,z,-y", []),
----
  (    Others, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  (    Others,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  (    Others,   M,  "m",  "",   "x,0,z", [ 0, 1, 0],    "x,-y,z", []),
  (    Others,   M,  "m",  "",   "0,y,z", [ 1, 0, 0],    "-x,y,z", []),
  (    Others, RI3, "-3", "+",   "x,x,x", [ 1, 1, 1],  "-z,-x,-y", []),
  (    Others, RI3, "-3", "+", "x,-x,-x", [ 1,-1,-1],    "z,x,-y", []),
  (    Others, RI3, "-3", "+", "-x,x,-x", [-1, 1,-1],    "-z,x,y", []),
  (    Others, RI3, "-3", "+", "-x,-x,x", [-1,-1, 1],    "z,-x,y", []),
  (    Others, RI3, "-3", "-",   "x,x,x", [ 1, 1, 1],  "-y,-z,-x", []),
  (    Others, RI3, "-3", "-", "x,-x,-x", [ 1,-1,-1],    "y,-z,x", []),
  (    Others, RI3, "-3", "-", "-x,x,-x", [-1, 1,-1],    "y,z,-x", []),
  (    Others, RI3, "-3", "-", "-x,-x,x", [-1,-1, 1],    "-y,z,x", []),
  (    Others,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
  (    Others,   M,  "m",  "",  "-x,y,x", [ 1, 0, 1],   "-z,y,-x", []),
  (    Others,   M,  "m",  "",  "x,y,-y", [ 0, 1, 1],   "x,-z,-y", []),
  (    Others,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  (    Others,   M,  "m",  "",   "x,y,x", [-1, 0, 1],     "z,y,x", []),
  (    Others,   M,  "m",  "",   "x,y,y", [ 0, 1,-1],     "x,z,y", []),
  (    Others, RI4, "-4", "+",   "0,0,z", [ 0, 0, 1],   "y,-x,-z", []),
  (    Others, RI4, "-4", "+",   "0,y,0", [ 0, 1, 0],   "-z,-y,x", []),
  (    Others, RI4, "-4", "+",   "x,0,0", [ 1, 0, 0],   "-x,z,-y", []),
  (    Others, RI4, "-4", "-",   "0,0,z", [ 0, 0, 1],   "-y,x,-z", []),
  (    Others, RI4, "-4", "-",   "0,y,0", [ 0, 1, 0],   "z,-y,-x", []),
  (    Others, RI4, "-4", "-",   "x,0,0", [ 1, 0, 0],   "-x,-z,y", []),
-- Table 11.2.2.2. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a hexagonal coordinate system
  ( Hexagonal,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  ( Hexagonal,  R3,  "3", "+",   "0,0,z", [ 0, 0, 1],  "-y,x-y,z", []),
  ( Hexagonal,  R3,  "3", "-",   "0,0,z", [ 0, 0, 1],  "y-x,-x,z", []),
  ( Hexagonal,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  ( Hexagonal,  R6,  "6", "+",   "0,0,z", [ 0, 0, 1],   "x-y,x,z", []),
  ( Hexagonal,  R6,  "6", "-",   "0,0,z", [ 0, 0, 1],   "y,y-x,z", []),
  ( Hexagonal,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  ( Hexagonal,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0], "x-y,-y,-z", []),
  ( Hexagonal,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0], "-x,y-x,-z", []),
  ( Hexagonal,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
--
  ( Hexagonal,  R2,  "2",  "",  "x,2x,0", [ 1, 2, 0],  "y-x,y,-z", []),
  ( Hexagonal,  R2,  "2",  "",  "2x,x,0", [ 2, 1, 0],  "x,x-y,-z", []),
-- 
  ( Hexagonal, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  ( Hexagonal, RI3, "-3", "+",   "0,0,z", [ 0, 0, 1],  "y,y-x,-z", []),
  ( Hexagonal, RI3, "-3", "-",   "0,0,z", [ 0, 0, 1],  "x-y,x,-z", []),
  ( Hexagonal,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  ( Hexagonal, RI6, "-6", "+",   "0,0,z", [ 0, 0, 1], "y-x,-x,-z", []),
  ( Hexagonal, RI6, "-6", "-",   "0,0,z", [ 0, 0, 1], "-y,x-y,-z", []),
  ( Hexagonal,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
-- 以下二つが、Orientationを利用して解の補正をすることができなかったため、代替値を用意している
-- 行列を解いた場合の解平面とorientationが一致していない可能性（2017の試行錯誤)
-- そもそも勘違いの可能性もまだあるので、のちのち再確認する。
-- hackageに登録するに至らない理由の一つ
-- 正規化された値として正しいが、正規化の結果、復元に必要な情報が欠落してしまった可能性(2020リファクタリング時の見解)
-- どうしてこうなっているのか、やっぱりわからない。
  ( Hexagonal,   M,  "m",  "",  "x,2x,z", [ 1, 0, 0],   "y-x,y,z", [ 2,-1, 0]),
  ( Hexagonal,   M,  "m",  "",  "2x,x,z", [ 0, 1, 0],   "x,x-y,z", [-1, 2, 0]),

  (    Hexagonal,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  (    Hexagonal,   M,  "m",  "",   "x,0,z", [ 1, 2, 0],  "x-y,-y,z", []),
  (    Hexagonal,   M,  "m",  "",   "0,y,z", [ 2, 1, 0],  "-x,y-x,z", [])
-- Notice
-- Hexagonal用のテーブルが、HexagonalのITで出現する対称操作全てをカバーしているわけではないことに注意
-- hexagonalでのlookup時には、hexagonal部分が優先となるよう、順番をいれかえている
-- それ以外のケースでは除外している
  ]
