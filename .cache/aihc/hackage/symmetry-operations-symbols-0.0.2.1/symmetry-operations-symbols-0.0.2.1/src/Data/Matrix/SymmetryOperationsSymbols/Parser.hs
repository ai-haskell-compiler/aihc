{-|
Module      : Data.Matrix.SymmetryOperationsSymbols.Parser
Description : Parser of geometric representation
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
-}
module Data.Matrix.SymmetryOperationsSymbols.Parser (
  -- symbolSenseVectorOrientation,
  notHexagonal,
  hexagonal,
  symmetryElement,
  ) where

import Control.Monad (join,guard)
import Data.Maybe (fromMaybe,isJust)
import Data.List (intercalate)

import Data.Matrix (Matrix)
import Data.Ratio (Ratio)

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.Calc
import Data.Matrix.SymmetryOperationsSymbols.Symbol


sign :: Parser Char
sign = oneOf "-+"

zero :: Parser String
zero = do
  char '0'
  return "0"

num :: Parser String
num = do
  x <- oneOf "123456789"
  xs <- many digit
  return $ x : xs

int :: Parser String
int = zero <|> num

integer :: Parser String
integer = int

fract :: Parser String
fract = do
  n <- int
  optionSpaces
  char '/'
  optionSpaces
  d <- int
  return $ n ++ "/" ++ d

number :: Parser String
number =   try fract
      <|> integer

optionSpaces :: Parser ()
optionSpaces = skipMany space

vector :: Parser String
vector = do
  n1 <- num
  char ','
  n2 <- num
  char ','
  n3 <- num
  return $ intercalate "," [n1,n2,n3]
  where
    num = do
      optionSpaces
      s <- optionMaybe sign
      n1 <- number
      optionSpaces
      return $ maybe n1 (:n1) s

elementBody :: Parser String
elementBody = do
  n <- optionMaybe number
  optionSpaces
  v <- optionMaybe (oneOf "xyzXYZ")
  optionSpaces
  guard (isJust n || isJust v)
  return $ fromMaybe "" n ++ maybe "" (:[]) v

one :: Parser String
one = do
  s <- optionMaybe sign
  optionSpaces
  n <- elementBody
  return $ maybe n (:n) s

other :: Parser String
other = do
  s <- sign
  optionSpaces
  l <- elementBody
  return $ s:l

component :: Parser String
component = do
  optionSpaces
  x <- one
  xs <- many other
  optionSpaces
  return $ join (x:xs)

matrix :: Parser String
matrix = do
  n1 <- component
  char ','
  n2 <- component
  char ','
  n3 <- component
  return $ intercalate "," [n1,n2,n3]

parenVector :: Parser String
parenVector = do
  char '('
  v <- vector
  char ')'
  return v

elementFull :: Parser (String,String)
elementFull = do -- (x,x,x) n,n,n
  v <- parenVector
  optionSpaces
  m <- matrix
  return (v,m)

elementHalf :: Parser (String,String)
elementHalf = do -- n,n,n
  m <- matrix
  return ("",m)

element :: Parser (String,String)
element = try elementFull <|> elementHalf

identity :: Parser SymbolSenseVectorOrientation
identity = do
  optionSpaces
  char '1'
  optionSpaces
  return ( Id, "", "", "" )

transform :: Parser SymbolSenseVectorOrientation
transform = do
  optionSpaces
  char 't'
  optionSpaces
  vec <- parenVector
  return ( T, "", vec, "" )

inversion :: Parser SymbolSenseVectorOrientation
inversion = do
  optionSpaces
  string "-1"
  optionSpaces
  vec <- vector
  optionSpaces
  return ( Inv, "", vec, "" )

miller :: Parser SymbolSenseVectorOrientation
miller = do
  optionSpaces
  sy <- oneOf "abcm"
  optionSpaces
  ori <- matrix
  optionSpaces
  return ( read [sy], "", "", ori )

glide :: Parser SymbolSenseVectorOrientation
glide = do
  optionSpaces
  sy <- oneOf "ndg"
  optionSpaces
  (vec,ori) <- element
  optionSpaces
  return ( read [sy], "", vec, ori )

millerOrGlide :: Parser SymbolSenseVectorOrientation
millerOrGlide = try miller <|> glide

rotation :: Parser SymbolSenseVectorOrientation
rotation = do
  optionSpaces
  sy <- oneOf "2436"
  se <- optionMaybe sign
  optionSpaces
  (vec,ori) <- element
  optionSpaces
  return ( read [sy],maybe "" (:[]) se, vec, ori )

invRotation :: Parser SymbolSenseVectorOrientation
invRotation = do
  optionSpaces
  char '-'
  c <- oneOf "436"
  se <- sign
  optionSpaces
  ori <- matrix
  optionSpaces
  char ';'
  optionSpaces
  vec <- vector
  optionSpaces
  return ( read ['-',c], [se], vec, ori )

symbolSenseVectorOrientation :: Parser SymbolSenseVectorOrientation
symbolSenseVectorOrientation
  = do
    elements <- try identity
            <|> try transform
            <|> try inversion
            <|> try millerOrGlide
            <|> try rotation
            <|> invRotation
    optionSpaces
    eof
    return elements

symmetryElement :: (SymbolSenseVectorOrientation -> Parser b) -> Parser b
symmetryElement f = do
    elements <- symbolSenseVectorOrientation
    f elements

notHexagonal :: Integral a => Parser (Matrix (Ratio a))
-- | referred to a cubic, tetragonal, orthorhombic, monoclinic, triclinic or rhombohedral
notHexagonal = symmetryElement (deriveSymmetryOperation properMatrixW)

hexagonal :: Integral a => Parser (Matrix (Ratio a))
-- | referred to a hexagonal
hexagonal = symmetryElement (deriveSymmetryOperation hexagonalMatrixW)

