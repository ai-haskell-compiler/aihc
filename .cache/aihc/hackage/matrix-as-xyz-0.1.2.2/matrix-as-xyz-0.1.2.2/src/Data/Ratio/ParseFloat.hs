{- |
Module      : Data.Ratio.ParseFloat
Copyright   : (c) Jun Narumi 2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Floating point parser

Temporary solution to the problem below

> ghci> realToFrac (read "1.1" :: Double) :: Rational
> 2476979795053773 % 2251799813685248

-}
module Data.Ratio.ParseFloat (
  readFloatingPoint,
  floating,
  ) where

import Data.Ratio
import Text.ParserCombinators.Parsec

-- このような一見、車輪の再発明に思えるコードをわざわざ書いたのは
-- ghci> realToFrac (read "1.1" :: Double) :: Rational
-- 2476979795053773 % 2251799813685248
-- という問題に対処するため。
-- これ以外に良い方法、良い書き方が分かれば、削除します。

-- | Obtain fractions from floating point representation string
--
-- >>> readFloatingPoint "1.1"
-- 11 % 10
-- >>> readFloatingPoint "0.5"
-- 1 % 2
-- >>> readFloatingPoint ".5"
-- 1 % 2
-- >>> readFloatingPoint "10."
-- 10 % 1
-- >>> readFloatingPoint "10"
-- 10 % 1
-- >>> readFloatingPoint "10.2"
-- 51 % 5
-- >>> readFloatingPoint "1e-1"
-- 1 % 10
-- >>> readFloatingPoint "-0.5e-1"
-- (-1) % 20
-- >>> readFloatingPoint "5e2"
-- 500 % 1
-- >>> readFloatingPoint "5e+2"
-- 500 % 1
readFloatingPoint :: Integral a => String -> Ratio a
readFloatingPoint s = case parse floating s s of
  Left  e -> error $ show e
  Right r -> r

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

sign :: CharParser () Char
sign = oneOf "-+"

decimal :: CharParser () String
decimal = do
  char '.'
  many digit

exponent' :: Integral a => CharParser () a
exponent' = do
  oneOf "eE"
  s <- optionMaybe sign
  e <- int
  return $ signPart s $ read' e

-- | Parser section
floating :: Integral a => CharParser () (Ratio a)
floating = do
  s <- optionMaybe sign
  i <- option "" int
  f <- option "" decimal
  e <- optionMaybe exponent'
  return $ signPart s . expPart e $ intPart i + decimalPart f

signPart :: Num a => Maybe Char -> a -> a
signPart (Just '-') = negate
signPart _          = id

intPart :: Integral a => String -> Ratio a
intPart "" = 0
intPart i  = read' i % 1

decimalPart :: Integral a => String -> Ratio a
decimalPart "" = 0
decimalPart f  | read f == 0 = 0
               | otherwise = read' f % (10 ^ length f)

-- 整数型のｒead instanceを隠蔽している
read' :: Integral a => String -> a
read' s = fromIntegral (read s :: Integer)

expPart :: Integral a => Maybe Integer -> Ratio a -> Ratio a
expPart Nothing      = id
expPart (Just s)     | s == 0    = id
                     | s < 0     = flip (/) (10 ^ abs s)
                     | otherwise = flip (*) (10 ^ s)
