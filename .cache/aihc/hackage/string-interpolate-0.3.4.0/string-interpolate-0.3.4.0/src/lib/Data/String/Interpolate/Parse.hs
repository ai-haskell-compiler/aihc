-- |
-- Module      : Data.String.Interpolate.Parse
-- Copyright   : (c) William Yao, 2019-2023
-- License     : BSD-3
-- Maintainer  : williamyaoh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- YOU SHOULD NOT USE THIS MODULE.
--
-- This is exported mainly so tests can introspect on the implementation.

{-# LANGUAGE PackageImports #-}

module Data.String.Interpolate.Parse
  ( ParseOutput(..)
  , parseInput, parseInterpSegments
  , dosToUnix
  )
where

import           "base" Data.Bifunctor
import           Data.Char
import qualified "base" Numeric        as N

import Data.String.Interpolate.Lines ( isBlankLine )
import Data.String.Interpolate.Types

-- |
-- Each section here is a list of lines.
--
-- "Content" here is defined by the contiguous sequence of lines begining
-- with the first non-blank line and ending with the last non-blank line
data ParseOutput = ParseOutput
  { poHeaderWS :: Lines
  , poContent  :: Lines
  , poFooterWS :: Lines
  }
  deriving (Eq, Show)

-- |
-- Given the raw input from a quasiquote, parse it into the information
-- we need to output the actual expression.
--
-- Returns an error message if parsing fails.
parseInterpSegments :: String -> Either String Lines
parseInterpSegments = switch []
  -- Given how complicated this is getting, it might be worth switching
  -- to megaparsec instead of hand-rolling this.
  where
    switch :: Line -> String -> Either String Lines
    switch line ""             = pure [reverse line]
    switch line ('#':'{':rest) = expr line rest
    switch _ ('#':_)           = Left "unescaped # symbol without interpolation brackets"
    switch line ('\n':rest)    = newline line rest  -- CRLF handled by `dosToUnix'
    switch line (' ':rest)     = spaces line 1 rest
    switch line ('\t':rest)    = tabs line 1 rest
    switch line other          = verbatim line "" other

    verbatim :: Line -> String -> String -> Either String Lines
    verbatim line acc parsee = case parsee of
      "" ->
        switch ((Verbatim . reverse) acc : line) parsee
      (c:_) | c `elem` ['#', ' ', '\t', '\n'] ->
        switch ((Verbatim . reverse) acc : line) parsee
      ('\\':'#':rest) ->
        verbatim line ('#':acc) rest
      ('\\':_) -> case unescapeChar parsee of
        (FoundChar c, rest)     -> verbatim line (c:acc) rest
        (EscapeEmpty, rest)     -> verbatim line acc rest
        (EscapeUnterminated, _) -> Left "unterminated backslash escape at end of string"
        (UnknownEscape esc, _)  -> Left ("unknown escape character: " ++ [esc])
      c:cs ->
        verbatim line (c:acc) cs

    expr :: Line -> String -> Either String Lines
    expr line parsee = case span (/= '}') parsee of
      (_, "")        -> Left "unterminated #{...} interpolation"
      (expr, _:rest) -> switch (Expression expr : line) rest

    newline :: Line -> String -> Either String Lines
    newline line parsee = (reverse line :) <$> switch [] parsee

    spaces :: Line -> Int -> String -> Either String Lines
    spaces line n (' ':rest) = spaces line (n+1) rest
    spaces line n other      = switch (Spaces n : line) other

    tabs :: Line -> Int -> String -> Either String Lines
    tabs line n ('\t':rest) = tabs line (n+1) rest
    tabs line n other       = switch (Tabs n : line) other

-- |
-- Like `parseInterpSegments', but for cases where we need to do
-- more complicated transformations on the input. Separates the
-- interpolation input into its content, whitespace header, and
-- whitespace footer.
parseInput :: String -> Either String ParseOutput
parseInput parsee = do
  lines <- parseInterpSegments parsee
  let (headerWS, tail) = break (not . isBlankLine) lines
      (footerWS, init) = bimap reverse reverse $
        break (not . isBlankLine) (reverse tail)
  pure $! ParseOutput
    { poHeaderWS = headerWS
    , poContent = init
    , poFooterWS = footerWS
    }

dosToUnix :: String -> String
dosToUnix = go
  where
    go xs = case xs of
      '\r' : '\n' : ys -> '\n' : go ys
      y : ys           -> y : go ys
      []               -> []

data EscapeResult
  = FoundChar Char
  | EscapeEmpty         -- ^ Haskell's lexical syntax has \& as an escape that produces an empty string
  | EscapeUnterminated
  | UnknownEscape Char

-- |
-- Haskell 2010 character unescaping, see:
-- <http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6>
--
-- Unescape the very first backslashed character of the string, if it's a known
-- escape.
unescapeChar :: String -> (EscapeResult, String)
unescapeChar input = case input of
  "" -> (EscapeEmpty, input)
  '\\' : 'x' : x : xs | isHexDigit x -> case span isHexDigit xs of
    (ys, zs) -> ((FoundChar . chr . readHex $ x:ys), zs)
  '\\' : 'o' : x : xs | isOctDigit x -> case span isOctDigit xs of
    (ys, zs) -> ((FoundChar . chr . readOct $ x:ys), zs)
  '\\' : x : xs | isDigit x -> case span isDigit xs of
    (ys, zs) -> ((FoundChar . chr . read $ x:ys), zs)
  '\\' : input_ -> case input_ of
    '\\' : xs        -> (FoundChar ('\\'), xs)
    'a' : xs         -> (FoundChar ('\a'), xs)
    'b' : xs         -> (FoundChar ('\b'), xs)
    'f' : xs         -> (FoundChar ('\f'), xs)
    'n' : xs         -> (FoundChar ('\n'), xs)
    'r' : xs         -> (FoundChar ('\r'), xs)
    't' : xs         -> (FoundChar ('\t'), xs)
    'v' : xs         -> (FoundChar ('\v'), xs)
    '&' : xs         -> (EscapeEmpty, xs)
    'N':'U':'L' : xs -> (FoundChar ('\NUL'), xs)
    'S':'O':'H' : xs -> (FoundChar ('\SOH'), xs)
    'S':'T':'X' : xs -> (FoundChar ('\STX'), xs)
    'E':'T':'X' : xs -> (FoundChar ('\ETX'), xs)
    'E':'O':'T' : xs -> (FoundChar ('\EOT'), xs)
    'E':'N':'Q' : xs -> (FoundChar ('\ENQ'), xs)
    'A':'C':'K' : xs -> (FoundChar ('\ACK'), xs)
    'B':'E':'L' : xs -> (FoundChar ('\BEL'), xs)
    'B':'S' : xs     -> (FoundChar ('\BS'), xs)
    'H':'T' : xs     -> (FoundChar ('\HT'), xs)
    'L':'F' : xs     -> (FoundChar ('\LF'), xs)
    'V':'T' : xs     -> (FoundChar ('\VT'), xs)
    'F':'F' : xs     -> (FoundChar ('\FF'), xs)
    'C':'R' : xs     -> (FoundChar ('\CR'), xs)
    'S':'O' : xs     -> (FoundChar ('\SO'), xs)
    'S':'I' : xs     -> (FoundChar ('\SI'), xs)
    'D':'L':'E' : xs -> (FoundChar ('\DLE'), xs)
    'D':'C':'1' : xs -> (FoundChar ('\DC1'), xs)
    'D':'C':'2' : xs -> (FoundChar ('\DC2'), xs)
    'D':'C':'3' : xs -> (FoundChar ('\DC3'), xs)
    'D':'C':'4' : xs -> (FoundChar ('\DC4'), xs)
    'N':'A':'K' : xs -> (FoundChar ('\NAK'), xs)
    'S':'Y':'N' : xs -> (FoundChar ('\SYN'), xs)
    'E':'T':'B' : xs -> (FoundChar ('\ETB'), xs)
    'C':'A':'N' : xs -> (FoundChar ('\CAN'), xs)
    'E':'M' : xs     -> (FoundChar ('\EM'), xs)
    'S':'U':'B' : xs -> (FoundChar ('\SUB'), xs)
    'E':'S':'C' : xs -> (FoundChar ('\ESC'), xs)
    'F':'S' : xs     -> (FoundChar ('\FS'), xs)
    'G':'S' : xs     -> (FoundChar ('\GS'), xs)
    'R':'S' : xs     -> (FoundChar ('\RS'), xs)
    'U':'S' : xs     -> (FoundChar ('\US'), xs)
    'S':'P' : xs     -> (FoundChar ('\SP'), xs)
    'D':'E':'L' : xs -> (FoundChar ('\DEL'), xs)
    '^':'@' : xs     -> (FoundChar ('\^@'), xs)
    '^':'A' : xs     -> (FoundChar ('\^A'), xs)
    '^':'B' : xs     -> (FoundChar ('\^B'), xs)
    '^':'C' : xs     -> (FoundChar ('\^C'), xs)
    '^':'D' : xs     -> (FoundChar ('\^D'), xs)
    '^':'E' : xs     -> (FoundChar ('\^E'), xs)
    '^':'F' : xs     -> (FoundChar ('\^F'), xs)
    '^':'G' : xs     -> (FoundChar ('\^G'), xs)
    '^':'H' : xs     -> (FoundChar ('\^H'), xs)
    '^':'I' : xs     -> (FoundChar ('\^I'), xs)
    '^':'J' : xs     -> (FoundChar ('\^J'), xs)
    '^':'K' : xs     -> (FoundChar ('\^K'), xs)
    '^':'L' : xs     -> (FoundChar ('\^L'), xs)
    '^':'M' : xs     -> (FoundChar ('\^M'), xs)
    '^':'N' : xs     -> (FoundChar ('\^N'), xs)
    '^':'O' : xs     -> (FoundChar ('\^O'), xs)
    '^':'P' : xs     -> (FoundChar ('\^P'), xs)
    '^':'Q' : xs     -> (FoundChar ('\^Q'), xs)
    '^':'R' : xs     -> (FoundChar ('\^R'), xs)
    '^':'S' : xs     -> (FoundChar ('\^S'), xs)
    '^':'T' : xs     -> (FoundChar ('\^T'), xs)
    '^':'U' : xs     -> (FoundChar ('\^U'), xs)
    '^':'V' : xs     -> (FoundChar ('\^V'), xs)
    '^':'W' : xs     -> (FoundChar ('\^W'), xs)
    '^':'X' : xs     -> (FoundChar ('\^X'), xs)
    '^':'Y' : xs     -> (FoundChar ('\^Y'), xs)
    '^':'Z' : xs     -> (FoundChar ('\^Z'), xs)
    '^':'[' : xs     -> (FoundChar ('\^['), xs)
    '^':'\\' : xs    -> (FoundChar ('\^\'), xs)
    '^':']' : xs     -> (FoundChar ('\^]'), xs)
    '^':'^' : xs     -> (FoundChar ('\^^'), xs)
    '^':'_' : xs     -> (FoundChar ('\^_'), xs)
    x:xs             -> (UnknownEscape x, xs)
    ""               -> (EscapeUnterminated, "")
  x:xs -> (FoundChar x, xs)

  where
    readHex :: String -> Int
    readHex xs = case N.readHex xs of
      [(n, "")] -> n
      _         -> error "Data.String.Interpolate.Util.readHex: no parse"

    readOct :: String -> Int
    readOct xs = case N.readOct xs of
      [(n, "")] -> n
      _         -> error "Data.String.Interpolate.Util.readHex: no parse"
