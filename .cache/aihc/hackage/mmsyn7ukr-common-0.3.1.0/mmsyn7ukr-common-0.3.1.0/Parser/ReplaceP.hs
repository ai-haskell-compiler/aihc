-- |
-- Module      :  Parser.ReplaceP
-- Copyright   :  (c) OleksandrZhabenko 2020-2021, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- This module provides some basic parsing functions for some special used 'Show' representation.
--

module Parser.ReplaceP where

-- | Function is used internally to parse a given third command line argument as a @[String]@ representing the Ukrainian sounds, which will be produced.
replaceP :: String -> String
replaceP (x:y:z:u:v:xs)
 | x == '[' && y == '\"' && z == '[' && u == '\\' && v == '\\' = "[\"\\" ++ replaceP xs
 | x == ',' && y == '\\' && z == '\\' = "\",\"\\" ++ replaceP (u:v:xs)
 | x == ']' && y == '\"' && z == ']' = "\"]" ++ replaceP (u:v:xs)
 | otherwise = x:replaceP (y:z:u:v:xs)
replaceP (x:y:z:u:xs)
 | x == ',' && y == '\\' && z == '\\' = "\",\"\\" ++ replaceP (u:xs)
 | x == ']' && y == '\"' && z == ']' = "\"]" ++ replaceP (u:xs)
 | otherwise = x:replaceP (y:z:u:xs)
replaceP (x:y:z:xs)
 | x == ',' && y == '\\' && z == '\\' = "\",\"\\" ++ replaceP xs
 | x == ']' && y == '\"' && z == ']' = "\"]" ++ replaceP xs
 | otherwise = x:replaceP (y:z:xs)
replaceP xs = xs

-- | Function is used internally to parse further the result dealt with 'replaceP' function.
replaceP4 :: String -> String
replaceP4 (t:x:y:z:u:v:xs)
 | [t,x,y,z,u,v] == "\\\\1078" = '\1078':replaceP4 xs
 | [t,x,y,z,u,v] == "\\\\1079" = '\1079':replaceP4 xs
 | [t,x,y,z,u,v] == "\\\\1100" = '\1100':replaceP4 xs
 | otherwise = t:replaceP4 (x:y:z:u:v:xs)
replaceP4 (x:xs) = x:replaceP4 xs
replaceP4 [] = []
