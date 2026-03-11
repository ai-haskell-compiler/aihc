{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.AST.Lexer
  ( dropWS
  , lexeme
  , symbol
  , integer
  , hexadecimal
  , floating
  ) where

import Data.Text (Text)

import qualified Control.Monad.Combinators as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Data.EDN.AST.Types (Parser)

dropWS :: Parser ()
dropWS = P.skipMany $ P.choice
  [ P.hidden P.space1
  , P.hidden $ () <$ P.char ','
  , P.hidden $ L.skipLineComment ";"
  ]

-- | Whitespace will be consumed after every lexeme automatically, but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme dropWS

symbol :: Text -> Parser Text
symbol = L.symbol dropWS

integer :: Parser Int
integer = lexeme $ L.signed (pure ()) L.decimal

hexadecimal :: Parser Int
hexadecimal = lexeme L.hexadecimal

floating :: Parser Double
floating = lexeme $ L.signed (pure ()) L.float
