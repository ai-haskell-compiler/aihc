{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parseExprAt,
    parseExprFromTokens,
    parsePattern,
    parsePatternAt,
    parsePatternFromTokens,
    parseType,
    parseTypeAt,
    parseTypeFromTokens,
    parseModule,
    parseModuleAt,
    parseModuleFromTokens,
    parseDeclFromTokens,
    parseImportDeclFromTokens,
    parseModuleHeaderFromTokens,
    defaultConfig,
    errorBundlePretty,
    LexToken (..),
    LexTokenKind (..),
    Extension (..),
    ExtensionSetting (..),
    readModuleHeaderExtensions,
    readModuleHeaderExtensionsFromChunks,
    lexTokensFromChunks,
    lexModuleTokensFromChunks,
    lexTokens,
    lexModuleTokens,
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    isReservedIdentifier,
  )
where

import Data.Text (Text)
import Parser.Ast (Decl, ExportSpec, Expr, Extension (..), ExtensionSetting (..), ImportDecl, Module (..), Pattern, Type, WarningText)
import Parser.Internal.Common (TokParser, symbolLikeTok, withSpan)
import Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Parser.Internal.Expr (exprParser, patternParser, typeParser)
import Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    isReservedIdentifier,
    lexModuleTokens,
    lexModuleTokensFromChunks,
    lexModuleTokensWithExtensions,
    lexTokens,
    lexTokensFromChunks,
    lexTokensWithExtensions,
    readModuleHeaderExtensions,
    readModuleHeaderExtensionsFromChunks,
  )
import Parser.Types
import Text.Megaparsec (runParser)
import qualified Text.Megaparsec as MP

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  languagePragmas <- MP.many (languagePragmaParser <* MP.many (symbolLikeTok ";"))
  mHeader <- MP.optional (moduleHeaderParser <* MP.many (symbolLikeTok ";"))
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
      imports <- MP.many (importDeclParser <* MP.many (symbolLikeTok ";"))
      decls <- MP.many (declParser <* MP.many (symbolLikeTok ";"))
      pure (imports, decls)

    bracedModuleBodyParser = do
      symbolLikeTok "{"
      _ <- MP.many (symbolLikeTok ";")
      imports <- MP.many (importDeclParser <* MP.many (symbolLikeTok ";"))
      decls <- MP.many (declParser <* MP.many (symbolLikeTok ";"))
      _ <- MP.many (symbolLikeTok ";")
      symbolLikeTok "}"
      pure (imports, decls)

defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { allowLineComments = True
    }

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr = parseExprAt ""

parseExprAt :: FilePath -> ParserConfig -> Text -> ParseResult Expr
parseExprAt sourceName _cfg input =
  case runParser (exprParser <* MP.eof) sourceName (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right expr -> ParseOk expr

parseExprFromTokens :: FilePath -> [LexToken] -> ParseResult Expr
parseExprFromTokens = parseFromTokens exprParser

parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern = parsePatternAt ""

parsePatternAt :: FilePath -> ParserConfig -> Text -> ParseResult Pattern
parsePatternAt sourceName _cfg input =
  case runParser (patternParser <* MP.eof) sourceName (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right pat -> ParseOk pat

parsePatternFromTokens :: FilePath -> [LexToken] -> ParseResult Pattern
parsePatternFromTokens = parseFromTokens patternParser

parseType :: ParserConfig -> Text -> ParseResult Type
parseType = parseTypeAt ""

parseTypeAt :: FilePath -> ParserConfig -> Text -> ParseResult Type
parseTypeAt sourceName _cfg input =
  case runParser (typeParser <* MP.eof) sourceName (TokStream (lexTokens input)) of
    Left bundle -> ParseErr bundle
    Right ty -> ParseOk ty

parseTypeFromTokens :: FilePath -> [LexToken] -> ParseResult Type
parseTypeFromTokens = parseFromTokens typeParser

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule = parseModuleAt ""

parseModuleAt :: FilePath -> ParserConfig -> Text -> ParseResult Module
parseModuleAt sourceName _cfg input =
  case runParser (moduleParser <* MP.eof) sourceName (TokStream (lexModuleTokens input)) of
    Left bundle -> ParseErr bundle
    Right m -> ParseOk m

parseModuleFromTokens :: FilePath -> [LexToken] -> ParseResult Module
parseModuleFromTokens = parseFromTokens moduleParser

parseDeclFromTokens :: FilePath -> [LexToken] -> ParseResult Decl
parseDeclFromTokens = parseFromTokens declParser

parseImportDeclFromTokens :: FilePath -> [LexToken] -> ParseResult ImportDecl
parseImportDeclFromTokens = parseFromTokens importDeclParser

parseModuleHeaderFromTokens :: FilePath -> [LexToken] -> ParseResult (Text, Maybe WarningText, Maybe [ExportSpec])
parseModuleHeaderFromTokens = parseFromTokens moduleHeaderParser

parseFromTokens :: TokParser a -> FilePath -> [LexToken] -> ParseResult a
parseFromTokens parser sourceName toks =
  case runParser (parser <* MP.eof) sourceName (TokStream toks) of
    Left bundle -> ParseErr bundle
    Right parsed -> ParseOk parsed

errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = MP.errorBundlePretty
