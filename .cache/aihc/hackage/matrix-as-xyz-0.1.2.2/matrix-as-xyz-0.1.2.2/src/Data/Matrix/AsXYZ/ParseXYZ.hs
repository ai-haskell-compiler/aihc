{- |
Module      : Data.Matrix.AsXYZ.ParseXYZ
Copyright   : (c) Jun Narumi 2018-2020
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Jones-Faithful notation parser for spacegroup.

-}
module Data.Matrix.AsXYZ.ParseXYZ (
  ReadNum(..),
  equivalentPositions,
  transformPpABC,
  transformQqXYZ,
  ratio,
  floating,
  integral,
  sign,
  minus,
  overlap,
  elementBody,
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec

import Data.Ratio
import Data.Ratio.Slash

import Data.Ratio.ParseFloat (readFloatingPoint)

import Data.Matrix (fromList,fromLists,Matrix(..),joinBlocks,(<->))

import Data.Matrix.AsXYZ.Common (Var(..),Val(..))

-- | Jones-Faithful notation parser
--
-- >>> parse (equivalentPositions integral) "" "x+1,y+2,z+3"
-- Right [[1,0,0,1],[0,1,0,2],[0,0,1,3]]
equivalentPositions :: Num a =>
　　　　　　　　　　　　　　ReadNum a -- ^ ratio or floating or integral
　　　　　　　　　　　　 -> CharParser () [[a]]
equivalentPositions = components xyz

-- | Same as equivalentPositions but uses abc instead of xyz
--
-- >>> parse (transformPpABC integral) "" "a+1,b+2,c+3"
-- Right [[1,0,0,1],[0,1,0,2],[0,0,1,3]]
transformPpABC :: Num a => ReadNum a -> CharParser () [[a]]
transformPpABC = components abc

-- | Alias of equivalentPositions
--
-- >>> parse (transformQqXYZ integral) "" "x+1,y+2,z+3"
-- Right [[1,0,0,1],[0,1,0,2],[0,0,1,3]]
transformQqXYZ :: Num a => ReadNum a -> CharParser () [[a]]
transformQqXYZ = components xyz

-- | Converter of 3 kind of number (int,float,ratio) string to rational
--
-- >>> parse (equivalentPositions ratio) "" "x+1,y+2,z+3"
-- Right [[1 % 1,0 % 1,0 % 1,1 % 1],[0 % 1,1 % 1,0 % 1,2 % 1],[0 % 1,0 % 1,1 % 1,3 % 1]]
ratio :: Integral a => ReadNum (Ratio a)
ratio (I s) = Right $ getRatio . read $ s
ratio (R s) = Right $ getRatio . read $ s
ratio (F s) = Right $ readFloatingPoint s

-- | Converter of integral number description to integral
--
-- This can not read ratio and floating string (e.g. '1/2', '0.1')
--
-- >>> parse (equivalentPositions integral) "" "x+1,y+2,z+3"
-- Right [[1,0,0,1],[0,1,0,2],[0,0,1,3]]
integral :: Integral a => ReadNum a
integral (I s) = Right $ fromIntegral (read s :: Integer)
integral (R s) = Left  $ "cannot convert to integer from " ++ s ++ "."
integral (F s) = Left  $ "cannot convert to integer from " ++ s ++ "."

-- | Converter of 3 kind of number description to floating point
--
-- >>> parse (equivalentPositions floating) "" "x+1,y+2,z+3"
-- Right [[1.0,0.0,0.0,1.0],[0.0,1.0,0.0,2.0],[0.0,0.0,1.0,3.0]]
floating :: Floating a => ReadNum a
floating v = fromRational <$> ratio v

-- | Type of numeric type information generated in the middle
type Value = Val String

v c = f $ toLower <$> c
  where
    f (Just 'x') = X
    f (Just 'a') = X
    f (Just 'y') = Y
    f (Just 'b') = Y
    f (Just 'z') = Z
    f (Just 'c') = Z
    f Nothing = W

sign :: CharParser () Char
sign = oneOf "-+"

zero :: CharParser () String
zero = do
  char '0'
  return "0"

num :: CharParser () String
num = do
  x <- oneOf "123456789"
  xs <- many digit
  return $ x : xs

int :: CharParser () String
int = zero <|> num

integer :: CharParser () Value
integer = do
  i <- int
  return (I i)

float :: CharParser () Value
float = do
  i <- option "" int
  char '.'
  f <- many digit
  return (F $ i ++ "." ++ f )

fract :: CharParser () Value
fract = do
  n <- many1 digit
  option () spaces
  char '/'
  option () spaces
  d <- many1 digit
  return (R $ n ++ "/" ++ d)

number' :: CharParser () Value
number'
  =   try fract
  <|> try float
  <|> integer

type ReadNum b = Value -> Either String b

number :: ReadNum b -> CharParser () b
number numRead = do
  n <- number'
  case numRead n of
    Left s -> fail s
    Right nn -> return nn

elementBody :: CharParser () Char -> ReadNum a -> CharParser () (Maybe a, Maybe Char)
elementBody var conv = do
  n <- optionMaybe (number conv)
  option () spaces
  v <- optionMaybe var
  option () spaces
  guard (isJust n || isJust v)
  return (n,v)

minus :: Num a => Maybe Char -> (a -> a)
minus (Just '-') = negate
minus (Just '+') = id
minus Nothing    = id

one :: Num a => CharParser () Char -> ReadNum a -> CharParser () (Var a)
one var numRead = do
  s <- optionMaybe sign
  option () spaces
  (n,l) <- elementBody var numRead
  return $ v l . minus s . fromMaybe 1 $ n

other :: Num a => CharParser () Char -> ReadNum a -> CharParser () (Var a)
other var numRead = do
  s <- sign
  option () spaces
  (n,l) <- elementBody var numRead
  return $ v l . minus (Just s) . fromMaybe 1 $ n

overlap :: Eq a => [a] -> Bool
overlap n = (length . nub) n /= length n

constructRow :: Num a => [Var a] -> [a]
constructRow = map (fromMaybe 0 . listToMaybe . catMaybes) . transpose . map toArray
  where
    toArray (X n) = [Just n,Nothing,Nothing,Nothing]
    toArray (Y n) = [Nothing,Just n,Nothing,Nothing]
    toArray (Z n) = [Nothing,Nothing,Just n,Nothing]
    toArray (W n) = [Nothing,Nothing,Nothing,Just n]

component :: Num b => CharParser () Char -> ReadNum b -> CharParser () [b]
component var numRead = do
  option () spaces
  x <- one var numRead
  xs <- many (other var numRead)
  option () spaces
  let mm = x : xs
  if overlap (map void mm)
    then
      fail "overlaps var type"
    else
      return (constructRow mm)

components :: Num a => CharParser () Char -> ReadNum a -> CharParser () [[a]]
components var conv = do
  a <- component var conv
  char ','
  b <- component var conv
  char ','
  c <- component var conv
  return [a,b,c]

xyz :: CharParser () Char
xyz = oneOf "xyzXYZ"

abc :: CharParser () Char
abc = oneOf "abcABC"
