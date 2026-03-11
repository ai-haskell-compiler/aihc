-- |
-- Module      : Text.PercentFormat
-- Copyright   : (c) 2016-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This file is part of PercentFormat a library for printf-style string
-- formatting.
--
-- This module provides the Spec type which represents a @%...@ specification
-- format.
module Text.PercentFormat.Spec where

import Data.Char (isDigit)

data Spec = Spec
  { ty        :: SpecType
  , width     :: Int
  , leftAlign :: Bool
  , padWith   :: Char
  , base      :: Int       -- ^ only for Number types
  , precision :: Maybe Int -- ^ only for Number types, Nothing for infinite
  , minPrecision :: Int    -- ^ minimum precision to show
  , positivePrefix :: String
  , capitalizeDigits :: Bool -- ^ whether to capitalize (hex) digits
  }
  deriving (Eq, Show)

data SpecType = NumberSpec
              | ReprSpec
              | StringSpec
              | CharSpec
              | Percent
  deriving (Eq, Show)

spec :: Spec
spec = Spec
  { ty        = error "undefined Spec ty"
  , width     = 0
  , leftAlign = False
  , padWith   = ' '
  , base      = 10
  , precision = Nothing
  , minPrecision = 0
  , positivePrefix = ""
  , capitalizeDigits = False
  }

parseSpec :: String -> (Spec,String)
parseSpec ('%':cs) = (spec {ty = Percent   }, cs)
parseSpec ('r':cs) = (spec {ty = ReprSpec  }, cs)
parseSpec ('s':cs) = (spec {ty = StringSpec}, cs)
parseSpec ('c':cs) = (spec {ty = CharSpec  }, cs)
parseSpec ('i':cs) = (spec {ty = NumberSpec, precision = Just 0}, cs)
parseSpec ('d':cs) = (spec {ty = NumberSpec}, cs)
parseSpec ('x':cs) = (spec {ty = NumberSpec, base = 16}, cs)
parseSpec ('X':cs) = (spec {ty = NumberSpec, base = 16, capitalizeDigits = True}, cs)
parseSpec ('o':cs) = (spec {ty = NumberSpec, base =  8}, cs)
parseSpec ('b':cs) = (spec {ty = NumberSpec, base =  2}, cs)
parseSpec ('f':cs) = (spec {ty = NumberSpec, minPrecision = 1}, cs)
parseSpec ('q':cs) = error $ "`q' format will be implemented in a future version"
parseSpec ('e':cs) = error $ "`e' format will be implemented in a future version"
parseSpec ('E':cs) = error $ "`E' format will be implemented in a future version"
parseSpec ('0':cs) = (s {padWith = '0'}, cs') where (s,cs') = parseSpec cs
parseSpec ( n :cs) | isDigit n = let (w,cs') = span isDigit (n:cs)
                                     (s,cs'') = parseSpec cs'
                                 in (s {width = read w}, cs'')
parseSpec ('.':'*':cs) = let (s,cs') = parseSpec cs
                         in (s {precision = Nothing}, cs')
parseSpec ('.':cs)             = let (w,cs')  = span isDigit cs
                                     (s,cs'') = parseSpec cs'
                                 in (s {precision = Just (read ('0':w))}, cs'')
parseSpec ('-':cs) = (s {leftAlign = True}, cs')     where (s,cs') = parseSpec cs
parseSpec (' ':cs) = (s {positivePrefix = " "}, cs') where (s,cs') = parseSpec cs
parseSpec ('+':cs) = (s {positivePrefix = "+"}, cs') where (s,cs') = parseSpec cs
parseSpec (c:_)    = error $ "unknown format string `" ++ (c:"'")
-- NOTE: for some reason:
-- > reads "4a" :: [(Int,String)]
-- [(4,"a")]
-- > reads "4." :: [(Int,String)]
-- [(4,".")]
-- > reads "4.0" :: [(Int,String)]
-- []
-- > reads "4.1" :: [(Int,String)]
-- []
-- > reads "4..1" :: [(Int,String)]
-- [(4,"..1")]
-- that's why I am using takeWhile above.
