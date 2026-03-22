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

import Aihc.Lexer (LexToken, LexTokenKind (..))
import Aihc.Parser.Ast (Decl, ExportSpec, Expr, ImportDecl, Module (..), Pattern, Type, WarningText)
import Aihc.Parser.Internal.Common (TokParser, expectedTok, skipSemicolons, withSpan)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Internal.Expr (exprParser, patternParser, typeParser)
import Aihc.Parser.Types
import Data.Text (Text)
import Text.Megaparsec (runParser)
import qualified Text.Megaparsec as MP

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  languagePragmas <- MP.many (languagePragmaParser <* MP.many (expectedTok TkSpecialSemicolon))
  mHeader <- MP.optional (moduleHeaderParser <* MP.many (expectedTok TkSpecialSemicolon))
  (imports, decls) <- moduleBodyParser
  let (mName, mWarning, mExports) =
        case mHeader of
          Nothing -> (Nothing, Nothing, Nothing)
          Just (name, warn, exports) -> (Just name, warn, exports)
  pure $ \span' ->
    Module
      { moduleSpan = span',
        moduleName = mName,
        moduleLanguagePragmas = concat languagePragmas,
        moduleWarningText = mWarning,
        moduleExports = mExports,
        moduleImports = imports,
        moduleDecls = decls
      }

moduleBodyParser :: TokParser ([ImportDecl], [Decl])
moduleBodyParser = MP.try bracedModuleBodyParser MP.<|> plainModuleBodyParser
  where
    plainModuleBodyParser = do
      imports <- MP.many (importDeclParser <* skipSemicolons)
      decls <- MP.many (declParser <* skipSemicolons)
      pure (imports, decls)

    bracedModuleBodyParser = do
      expectedTok TkSpecialLBrace
      skipSemicolons
      imports <- MP.many (importDeclParser <* skipSemicolons)
      decls <- MP.many (declParser <* skipSemicolons)
      skipSemicolons
      expectedTok TkSpecialRBrace
      pure (imports, decls)

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

parseModuleHeaderFromTokens :: FilePath -> [LexToken] -> ParseResult (Text, Maybe WarningText, Maybe [ExportSpec])
parseModuleHeaderFromTokens = parseFromTokens moduleHeaderParser
