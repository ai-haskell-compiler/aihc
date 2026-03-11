{- |
Module      : Data.Matrix.AsXYZ
Copyright   : (c) Jun Narumi 2017-2020
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Read and Display Jones-Faithful notation for spacegroup (e.g. 'x,y,z') and planegroup (e.g. 'x,y')

-}
module Data.Matrix.AsXYZ (
  fromXYZ,
  fromXYZ',
  fromABC,
  prettyXYZ,
  prettyABC,
  fromXY,
  fromXY',
  fromAB,
  prettyXY,
  prettyAB,
  ) where

import Control.Monad (join)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio,(%))
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->),submatrix)
import Text.ParserCombinators.Parsec (parse,ParseError)

import Data.Ratio.Slash (getRatio,Slash(..))
import qualified Data.Matrix.AsXYZ.ParseXYZ as XYZ(equivalentPositions,transformPpABC,ratio)
import qualified Data.Matrix.AsXYZ.ParseXY as XY (equivalentPositions,transformPpAB)
import qualified Data.Matrix.AsXYZ.ParseXYZ as XY(ratio)
import qualified Data.Matrix.AsXYZ.Plain as Plain (showAs,showAs',xyzLabel,abcLabel)

-- | Create a matirx from xyz coordinate string of general equivalent position
--
-- >                                      ( 1 % 1 0 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 1 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 0 % 1 1 % 1 0 % 1 )
-- > fromXYZ "x,y,z" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
-- >
-- >                                                  ( 1 % 1 0 % 1 0 % 1 1 % 2 )
-- >                                                  ( 0 % 1 1 % 1 0 % 1 1 % 3 )
-- >                                                  ( 0 % 1 0 % 1 1 % 1 1 % 4 )
-- > fromXYZ "x+1/2,y+1/3,z+1/4" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
-- >
-- >                                                              (  1  2  3  4 )
-- >                                                              (  5  6  7  8 )
-- >                                                              (  9 10 11 12 )
-- > fromXYZ "x+2y+3z+4,5x+6y+7z+8,9x+10y+11z+12" :: Matrix Int = (  0  0  0  1 )
fromXYZ :: Integral a => String -> Matrix (Ratio a)
fromXYZ input = unsafeGet $ makeMatrixS <$> parse (XYZ.equivalentPositions XYZ.ratio) input input

-- | Maybe version
fromXYZ' :: Integral a => String -> Maybe (Matrix (Ratio a))
fromXYZ' input = get $ makeMatrixS <$> parse (XYZ.equivalentPositions XYZ.ratio) input input

-- | It's uses abc instead of xyz
--
-- >                                      ( 1 % 1 0 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 1 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 0 % 1 1 % 1 0 % 1 )
-- > fromXYZ "a,b,c" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
fromABC :: Integral a => String -> Matrix (Ratio a)
fromABC input = unsafeGet $ makeMatrixS <$> parse (XYZ.transformPpABC XYZ.ratio) input input

makeMatrixS :: Num a => [[a]] -> Matrix a
makeMatrixS m = (submatrix 1 3 1 4 . fromLists) m <-> fromLists [[0,0,0,1]]

unsafeGet :: Either ParseError a -> a
unsafeGet e = case e of
  Left s -> error $ show s
  Right m -> m

get :: Either ParseError a -> Maybe a
get e = case e of
  Left s -> Nothing
  Right m -> Just m

----------------------------------

-- | Get the xyz string of matrix
--
-- >>> prettyXYZ (identity 4 :: Matrix Rational)
-- "x,y,z"
--
-- >           ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >           ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >           ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > prettyXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
prettyXYZ :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyXYZ = Plain.showAs Plain.xyzLabel

-- | It's uses abc instead of xyz as text format
--
-- >>> prettyABC (identity 4 :: Matrix Rational)
-- "a,b,c"
prettyABC :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyABC = Plain.showAs Plain.abcLabel

-- | Create a matirx from xyz coordinate string of general equivalent position
--

-- >>> toLists . fromXY $ "x,y"
-- [[1 % 1,0 % 1,0 % 1],[0 % 1,1 % 1,0 % 1],[0 % 1,0 % 1,1 % 1]]
fromXY :: Integral a =>
          String
       -> Matrix (Ratio a)
fromXY input = unsafeGet $ makeMatrixP <$> parse (XY.equivalentPositions XY.ratio) input input

-- | Maybe version
--

-- >>> toLists <$> fromXY' "x,y"
-- Just [[1 % 1,0 % 1,0 % 1],[0 % 1,1 % 1,0 % 1],[0 % 1,0 % 1,1 % 1]]
fromXY' :: Integral a =>
           String
        -> Maybe (Matrix (Ratio a))
fromXY' input = get $ makeMatrixP <$> parse (XY.equivalentPositions XY.ratio) input input

-- | It's uses abc instead of xyz
--

-- >>> toLists . fromAB $ "a,b"
-- [[1 % 1,0 % 1,0 % 1],[0 % 1,1 % 1,0 % 1],[0 % 1,0 % 1,1 % 1]]
fromAB :: Integral a => 
          String
       -> Matrix (Ratio a)
fromAB input = unsafeGet $ makeMatrixP <$> parse (XY.transformPpAB XY.ratio) input input

makeMatrixP :: Num a => [[a]] -> Matrix a
makeMatrixP m = (submatrix 1 2 1 3 . fromLists) m <-> fromLists [[0,0,1]]

-- | Get the xyz string of matrix
--

-- >>> prettyXY (identity 4 :: Matrix Rational)
-- "x,y"
prettyXY :: (Integral a) =>
             Matrix (Ratio a) -- ^ 2x2, 2x3 or 3x3 matrix
          -> String
prettyXY = Plain.showAs' Plain.xyzLabel

-- | It's uses abc instead of xyz as text format
--
-- >>> prettyAB (identity 4 :: Matrix Rational)
-- "a,b"
prettyAB :: (Integral a) =>
             Matrix (Ratio a) -- ^ 2x2, 2x3 or 3x3 matrix
          -> String
prettyAB = Plain.showAs' Plain.abcLabel