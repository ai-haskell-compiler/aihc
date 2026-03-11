{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Nat (termToInt, intToTerm, parseTerm, pp, s, z) where


import Data.Text
import Text.Parsec (Parsec, ParsecT, Stream)
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.String

import qualified Language.REST.MetaTerm as MT
import           Language.REST.Op
import           Language.REST.Types
import           Language.REST.RuntimeTerm as RT

z, s :: Op
s      = Op "s"
z      = Op "z"

intToTerm :: Int -> RuntimeTerm
intToTerm 0 = App z []
intToTerm n = App s [intToTerm (n - 1)]

termToInt :: (MT.ToMetaTerm a) => a -> Maybe Int
termToInt t = go (MT.toMetaTerm t) where
  go (MT.RWApp mop [])   | mop == z = Just 0
  go (MT.RWApp mop [t1]) | mop == s = (1 +) <$> go t1
  go _                  = Nothing

instance ToRuntimeTerm Int where
  toRuntimeTerm = intToTerm

pp :: MT.ToMetaTerm a => a -> String
pp = prettyPrint (PPArgs []
                  [ ("<", "<")
                  , ("+", "+")
                  , ("*", "*")
                  , ("∪", "∪")
                  , ("union", "∪")
                  , ("intersect", "∩")
                  ] showInt)
  where
    showInt :: MT.MetaTerm -> Maybe Text
    showInt t = pack . Prelude.show <$> termToInt t

op :: GenParser Char st Op
op = fmap (Op . pack) (many (alphaNum <|> char '\''))

parens :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
parens p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

term :: Parsec String u RuntimeTerm
term = try infixTerm <|> nonInfixTerm
  where

    nonInfixTerm = try (parens term) <|> try appTerm <|> try numberTerm <|> nullTerm

    numberTerm = do
      d1 <- digit
      n <- many digit
      return $ intToTerm (read (d1 : n))

    infixOp =
          try (string "+")
      <|> try (string "<")
      <|> (try (string "\\/") >> return "∪")
      <|> string "*"

    infixTerm = do
      t1 <- nonInfixTerm
      _  <- spaces
      top <- infixOp
      _  <- spaces
      t2 <- nonInfixTerm
      return $ App (Op (pack top)) [t1, t2]

    nullTerm = do
      o <- op
      return $ App o []

    appTerm = do
      o    <- op
      trms <- parens $ sepBy1 term (char ',' >> spaces)
      return $ App o trms


parseTerm :: String -> RuntimeTerm
parseTerm str =
  case parse term "" str of
    Left err -> error (Prelude.show err)
    Right t  -> t

instance IsString RuntimeTerm where
  fromString = parseTerm
