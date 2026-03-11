-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim
-- Description: Parser monad and primitive parsers
-- Stability:   experimental
--
-- A parser monad and primitive parsers.
--
-- Note that all dependencies on parser library are in this module.

module Language.Egison.Parser.Pattern.Prim
  (
  -- * Parser Configuration
    ParseFixity(..)
  , ParseMode(..)
  , ExtParser
  -- * Parser Monad
  , Parse
  , runParse
  -- * Primitive Parsers
  , extParser
  , space
  , lexeme
  , name
  , varName
  , valueExpr
  -- * Errors
  , Errors
  , Error(..)
  , ErrorItem(..)
  -- * Locations
  , Position(..)
  , Location(..)
  , Locate(..)
  -- * Source Stream Class
  , Source
  , Token
  , Tokens
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Text.Megaparsec               as X
                                                ( MonadParsec(..)
                                                , (<?>)
                                                , single
                                                , chunk
                                                )

-- main
import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( ask )
import           Control.Applicative            ( Alternative((<|>))
                                                , empty
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( takeWhile1P
                                                , takeWhileP
                                                , manyTill
                                                , chunk
                                                , customFailure
                                                , single
                                                , anySingle
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( lexeme
                                                , space
                                                )

import qualified Language.Egison.Parser.Pattern.Token
                                               as Token
                                                ( isSpace
                                                , comma
                                                , parenLeft
                                                , parenRight
                                                , bracketLeft
                                                , bracketRight
                                                , newline
                                                )
import           Language.Egison.Parser.Pattern.Prim.Location
                                                ( Position(..)
                                                , Location(..)
                                                , Locate(..)
                                                )
import           Language.Egison.Parser.Pattern.Prim.Error
                                                ( Error(..)
                                                , ErrorItem(..)
                                                , Errors
                                                , CustomError(..)
                                                )

import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Source(..)
                                                , Token
                                                , Tokens
                                                )
import           Language.Egison.Parser.Pattern.Prim.ParseMode
                                                ( ParseMode(..)
                                                , ParseFixity(..)
                                                , ExtParser
                                                )
import           Language.Egison.Parser.Pattern.Prim.Parse
                                                ( Parse
                                                , runParse
                                                )


skipBlockComment :: Source s => Tokens s -> Tokens s -> Parse n v e s ()
skipBlockComment start end = cs *> void (Parsec.manyTill Parsec.anySingle ce)
 where
  cs = Parsec.chunk start
  ce = Parsec.chunk end

skipLineComment :: Source s => Tokens s -> Parse n v e s ()
skipLineComment prefix = Parsec.chunk prefix
  *> void (Parsec.takeWhileP (Just "chars") (/= Token.newline))

-- | Skip one or more spaces.
space :: Source s => Parse n v e s ()
space = do
  ParseMode { blockComment, lineComment } <- ask
  let block = emptyOr (uncurry skipBlockComment) blockComment
      line  = emptyOr skipLineComment lineComment
  L.space space1 line block
 where
  space1  = void $ Parsec.takeWhile1P (Just "whitespace") Token.isSpace
  emptyOr = maybe empty

-- | Parse a lexical chunk.
takeChunk :: forall n v e s . Source s => Parse n v e s (Tokens s)
takeChunk = withParens <|> withBrackets <|> withoutParens
 where
  withParens = do
    left <- Parsec.single Token.parenLeft
    ck   <- Parsec.takeWhileP (Just "lexical chunk (in parens)")
                              (/= Token.parenRight)
    right <- Parsec.single Token.parenRight
    pure $ consTokens @s left (snocTokens @s ck right)
  withBrackets = do
    left <- Parsec.single Token.bracketLeft
    ck   <- Parsec.takeWhileP (Just "lexical chunk (in brackets)")
                              (/= Token.bracketRight)
    right <- Parsec.single Token.bracketRight
    pure $ consTokens @s left (snocTokens @s ck right)
  withoutParens = Parsec.takeWhileP (Just "lexical chunk") endOfChunk
  endOfChunk x = not (isDelimiter x) && x /= Token.parenRight
  isDelimiter x =
    Token.isSpace x
      || Token.comma
      == x
      || Token.parenRight
      == x
      || Token.bracketRight
      == x

-- | Apply an external parser.
extParser :: Source s => ExtParser s a -> Parse n v e s a
extParser p = try $ do
  lchunk <- takeChunk
  case p lchunk of
    Left  err -> Parsec.customFailure (ExtParserError lchunk err)
    Right x   -> pure x

-- | Make a lexical token.
-- @lexeme p@ first applies parser @p@ then 'space' parser.
lexeme :: Source s => Parse n v e s a -> Parse n v e s a
lexeme = L.lexeme space

-- | Parser for @n@ in @Parse n v e s@ monad.
name :: Source s => Parse n v e s n
name = do
  ParseMode { nameParser } <- ask
  extParser nameParser

-- | Parser for @v@ in @Parse n v e s@ monad.
varName :: Source s => Parse n v e s v
varName = do
  ParseMode { varNameParser } <- ask
  extParser varNameParser

-- | Parser for @e@ in @Parse n v e s@ monad.
valueExpr :: Source s => Parse n v e s e
valueExpr = do
  ParseMode { valueExprParser } <- ask
  extParser valueExprParser
