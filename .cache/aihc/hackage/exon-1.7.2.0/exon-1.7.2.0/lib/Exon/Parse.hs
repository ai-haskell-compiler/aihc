{-# options_haddock prune #-}

-- | Description: The parser for the quasiquote body, using parsec.
module Exon.Parse where

import Data.Char (isSpace)
import Prelude hiding ((<|>))
import Text.Parsec as Parsec (
  Parsec,
  anyChar,
  char,
  choice,
  eof,
  getState,
  lookAhead,
  modifyState,
  putState,
  runParser,
  satisfy,
  string,
  try,
  (<|>),
  )

import Exon.Data.RawSegment (RawSegment (AutoExpSegment, ExpSegment, StringSegment, WsSegment))

type Parser = Parsec String Int

ws :: Parser Char
ws =
  satisfy isSpace

whitespace :: Parser RawSegment
whitespace =
  WsSegment <$> some ws

expr :: Parser String
expr =
  choice [try opening, try closing, anyChars]
  where
    opening = do
      char '{'
      modifyState (1 +)
      e <- expr
      pure ('{' : e)

    closing = do
      void $ lookAhead (char '}')
      getState >>= \case
        0 -> pure ""
        cur -> do
          putState (cur - 1)
          char '}'
          e <- expr
          pure ('}' : e)

    anyChars = do
      c <- anyChar
      e <- expr
      pure (c : e)

autoInterpolation :: Parser RawSegment
autoInterpolation =
  string "##{" *> (AutoExpSegment <$> expr) <* char '}'

verbatimInterpolation :: Parser RawSegment
verbatimInterpolation =
  string "#{" *> (ExpSegment <$> expr) <* char '}'

interpolations :: Parser RawSegment
interpolations =
  try autoInterpolation <|> try verbatimInterpolation

verbatimStep :: Bool -> Parser Bool
verbatimStep =
  lookAhead . \case
    True -> False <$ ws <|> basic
    False -> basic
    where
      basic =
        False <$ (try (string "##{") <|> try (string "#{"))
        <|>
        False <$ eof
        <|>
        pure True

verbatimWith :: Parser Bool -> Parser String
verbatimWith step =
  step >>= \case
    False -> fail "Empty verbatim segment"
    True -> go
  where
    go =
      step >>= \case
        False -> pure ""
        True -> do
          h <- anyChar
          t <- go
          pure (h : t)

verbatim :: Parser String
verbatim =
  verbatimWith (verbatimStep False)

verbatimWs :: Parser String
verbatimWs =
  verbatimWith (verbatimStep True)

text :: Parser RawSegment
text =
  StringSegment <$> verbatim

textWs :: Parser RawSegment
textWs =
  StringSegment <$> verbatimWs

segment :: Parser RawSegment
segment =
  interpolations <|> text

segmentWs :: Parser RawSegment
segmentWs =
  try whitespace <|> interpolations <|> textWs

parser :: Parser [RawSegment]
parser =
  many segment <* eof

parserWs :: Parser [RawSegment]
parserWs =
  many segmentWs <* eof

parseWith :: Parser [RawSegment] -> String -> Either Text [RawSegment]
parseWith p =
  first (unwords . lines . show) . runParser p 0 ""

parse :: String -> Either Text [RawSegment]
parse =
  parseWith parser

parseWs :: String -> Either Text [RawSegment]
parseWs =
  parseWith parserWs
