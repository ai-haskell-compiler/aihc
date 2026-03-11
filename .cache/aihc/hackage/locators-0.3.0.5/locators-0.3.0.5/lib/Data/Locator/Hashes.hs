{-# LANGUAGE OverloadedStrings #-}

module Data.Locator.Hashes (
    toBase62,
    fromBase62,
    padWithZeros,
    hashStringToBase62,
) where

import Prelude hiding (toInteger)

import Crypto.Hash as Crypto
import qualified Data.ByteArray as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Char (chr, isDigit, isLower, isUpper, ord)
import Data.Word
import Numeric (showIntAtBase)

--
-- Conversion between decimal and base 62
--

represent :: Int -> Char
represent x
    | x < 10 = chr (48 + x)
    | x < 36 = chr (65 + x - 10)
    | x < 62 = chr (97 + x - 36)
    | otherwise = '@'

toBase62 :: Integer -> String
toBase62 x =
    showIntAtBase 62 represent x ""

--

{- | Utility function to prepend \'0\' characters to a string representing a
 number. This allows you to ensure a fixed width for numbers that are less
 than the desired width in size. This comes up frequently when representing
 numbers in other bases greater than 10 as they are inevitably presented as
 text, and not having them evenly justified can (at best) be ugly and (at
 worst) actually lead to parsing and conversion bugs.
-}
padWithZeros :: Int -> String -> String
padWithZeros digits str =
    pad ++ str
  where
    pad = take len (replicate digits '0')
    len = digits - length str

value :: Char -> Int
value c
    | isDigit c = ord c - 48
    | isUpper c = ord c - 65 + 10
    | isLower c = ord c - 97 + 36
    | otherwise = 0

multiply :: Integer -> Char -> Integer
multiply acc c =
    acc * 62 + (fromIntegral $ value c)

fromBase62 :: String -> Integer
fromBase62 ss =
    foldl multiply 0 ss

concatToInteger :: [Word8] -> Integer
concatToInteger bytes =
    foldl fn 0 bytes
  where
    fn acc b = (acc * 256) + (fromIntegral b)

digest :: String -> Integer
digest ws =
    i
  where
    i = concatToInteger h
    h = B.unpack h'
    h' = Crypto.hash x' :: Crypto.Digest Crypto.SHA1
    x' = S.pack ws

--

{- | Take an arbitrary string, hash it, then pad it with zeros up to be a
 @digits@-long string in base 62.

 You may be interested to know that the 160-bit SHA1 hash used here can be
 expressed without loss as 27 digits of base 62, for example:

 >>> hashStringToBase62 27 "Hello World"
 1T8Sj4C5jVU6iQXCwCwJEPSWX6u
-}
hashStringToBase62 :: Int -> ByteString -> ByteString
hashStringToBase62 digits s' =
    r'
  where
    s = S.unpack s'
    n = digest s -- SHA1 hash
    limit = 62 ^ digits
    x = mod n limit -- trim to specified number base62 chars
    str = toBase62 x
    r = padWithZeros digits str -- convert to String
    r' = S.pack r
