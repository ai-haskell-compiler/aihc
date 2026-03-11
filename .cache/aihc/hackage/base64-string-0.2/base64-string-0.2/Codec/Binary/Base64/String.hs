
-- Copyright (c) Ian Lynagh, 2005, 2007.

module Codec.Binary.Base64.String (encode, decode) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (ord, chr, isAscii, isAlphaNum, isUpper, isLower, isDigit)
import Data.List (intersperse)

chars_per_line :: Int
chars_per_line = 64
-- PEM mandates 64. MIME allows anything up to 76.

encode :: String -> String
encode = concat . intersperse "\n" . splits chars_per_line . enc

-- It is up to the caller to make sure the right sort of line breaks are
-- in the input
enc :: String -> String
enc (c1:c2:c3:cs) = toChar   (o1 `shiftR` 2)           -- First 6 bits of c1
                  : toChar (((o1 `shiftL` 4) .&. 0x30) -- Last 2 bits of c1
                         .|. (o2 `shiftR` 4))          -- First 4 bits of c2
                  : toChar (((o2 `shiftL` 2) .&. 0x3C) -- Last 4 bits of c2
                         .|. (o3 `shiftR` 6))          -- First 2 bits of c3
                  : toChar   (o3 .&. 0x3F)             -- Last 6 bits of c3
                  : enc cs
    where o1 = ord c1
          o2 = ord c2
          o3 = ord c3
enc [c1, c2] = toChar   (o1 `shiftR` 2)           -- First 6 bits of c1
             : toChar (((o1 `shiftL` 4) .&. 0x30) -- Last 2 bits of c1
                    .|. (o2 `shiftR` 4))          -- First 4 bits of c2
             : toChar  ((o2 `shiftL` 2) .&. 0x3C) -- Last 4 bits of c2
             : "="
    where o1 = ord c1
          o2 = ord c2
enc [c1] = toChar  (o1 `shiftR` 2)           -- First 6 bits of c1
         : toChar ((o1 `shiftL` 4) .&. 0x30) -- Last 2 bits of c1
         : "=="
    where o1 = ord c1
enc "" = ""

toChar :: Int -> Char
toChar n | n <= 25 = chr (ord 'A' + n)
         | n <= 51 = chr (ord 'a' + n - 26)
         | n <= 61 = chr (ord '0' + n - 52)
         | n == 62 = '+'
         | n == 63 = '/'
         | otherwise
             = error ("toChar: Can't happen: Bad input: " ++ show n)

decode :: String -> String
decode = dec . filter valid
    where valid c = isAscii c
                 && (isAlphaNum c || c == '+' || c == '/' || c == '=')

dec :: String -> String
dec "" = ""
dec ('=':_) = "" -- This can't be valid, so give up
dec (_:'=':_) = "" -- This can't be valid, so give up
dec (c1:c2:'=':_) -- Should have [c1,c2,'=','='], but if not we'd decode
                  -- this bit and stop anyway
 = take 1 $ dec [c1, c2, 'A', 'A']
dec (c1:c2:c3:'=':_) -- Should have [c1,c2,c3,'='], but if not we'd decode
                     -- this bit and stop anyway
 = take 2 $ dec [c1, c2, c3, 'A']
dec (c1:c2:c3:c4:cs)
 = let x1 = fromChar c1
       x2 = fromChar c2
       x3 = fromChar c3
       x4 = fromChar c4
   in -- 6 bits from x1, 2 bits from x2
      chr ((x1 `shiftL` 2) .|. (x2 `shiftR` 4))
      -- 4 bits from x2, 4 bits from x3
    : chr (((x2 `shiftL` 4) .&. 0xF0) .|. (x3 `shiftR` 2))
      -- 2 bits from x3, 6 bits from x4
    : chr (((x3 `shiftL` 6) .&. 0xC0) .|. x4)
    : dec cs
-- Drop more invalid things
dec [_] = ""
dec [_, _] = ""
dec [_, _, _] = ""

fromChar :: Char -> Int
fromChar c
 | isUpper c = ord c - ord 'A'
 | isLower c = ord c - ord 'a' + 26
 | isDigit c = ord c - ord '0' + 52
 | c == '+'  = 62
 | c == '/'  = 63
 | otherwise = error ("fromChar: Can't happen: Bad input: " ++ show c)

splits :: Int -> [a] -> [[a]]
splits _ [] = []
splits n xs = case splitAt n xs of
                  (ys, zs) -> ys:splits n zs

