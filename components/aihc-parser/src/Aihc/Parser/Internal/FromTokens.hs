{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Aihc.Parser.Internal.FromTokens
-- Description : Internal parsing functions from token streams
-- License     : Unlicense
--
-- @since 0.1.0.0
--
-- __Warning:__ This is an internal module and is not meant to be used directly.
-- The API may change without notice.
--
-- This module exposes parsing functions that work directly on token streams.
-- These are primarily used for testing and internal purposes.
module Aihc.Parser.Internal.FromTokens
  ( parseExprFromTokens,
    parsePatternFromTokens,
    parseTypeFromTokens,
    parseModuleFromTokens,
    parseDeclFromTokens,
    parseImportDeclFromTokens,
    parseModuleHeaderFromTokens,
  )
where

import AIhc.Parser.Syntax (Decl, Expr, ImportDecl, Module, ModuleHead, Pattern, Type)
import Aihc.Parser.Internal.Common (TokParser)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, moduleHeaderParser)
import Aihc.Parser.Internal.Expr (exprParser, patternParser, typeParser)
import Aihc.Parser.Internal.Module (moduleParser)
import Aihc.Parser.Lex (LexToken)
import Aihc.Parser.Types
import Text.Megaparsec (runParser)
import qualified Text.Megaparsec as MP

parseFromTokens :: TokParser a -> FilePath -> [LexToken] -> ParseResult a
parseFromTokens parser sourceName toks =
  case runParser (parser <* MP.eof) sourceName (TokStream toks) of
    Left bundle -> ParseErr bundle
    Right parsed -> ParseOk parsed

parseExprFromTokens :: FilePath -> [LexToken] -> ParseResult Expr
parseExprFromTokens = parseFromTokens exprParser

parsePatternFromTokens :: FilePath -> [LexToken] -> ParseResult Pattern
parsePatternFromTokens = parseFromTokens patternParser

parseTypeFromTokens :: FilePath -> [LexToken] -> ParseResult Type
parseTypeFromTokens = parseFromTokens typeParser

parseModuleFromTokens :: FilePath -> [LexToken] -> ParseResult Module
parseModuleFromTokens = parseFromTokens moduleParser

parseDeclFromTokens :: FilePath -> [LexToken] -> ParseResult Decl
parseDeclFromTokens = parseFromTokens declParser

parseImportDeclFromTokens :: FilePath -> [LexToken] -> ParseResult ImportDecl
parseImportDeclFromTokens = parseFromTokens importDeclParser

parseModuleHeaderFromTokens :: FilePath -> [LexToken] -> ParseResult ModuleHead
parseModuleHeaderFromTokens = parseFromTokens moduleHeaderParser
