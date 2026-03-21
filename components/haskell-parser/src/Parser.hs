{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpr,
    parsePattern,
    parseType,
    parseModule,
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

import qualified Data.List as List
import Data.Text (Text)
import Parser.Ast (Decl, Expr, Extension (..), ExtensionSetting (..), ImportDecl, Module (..), Pattern, Type)
import Parser.Internal.Common (TokParser, skipSemicolons, symbolLikeTok, withSpan)
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
      imports <- MP.many (importDeclParser <* skipSemicolons)
      decls <- MP.many (declParser <* skipSemicolons)
      pure (imports, decls)

    bracedModuleBodyParser = do
      symbolLikeTok "{"
      skipSemicolons
      imports <- MP.many (importDeclParser <* skipSemicolons)
      decls <- MP.many (declParser <* skipSemicolons)
      skipSemicolons
      symbolLikeTok "}"
      pure (imports, decls)

defaultConfig :: ParserConfig
defaultConfig =
  ParserConfig
    { parserSourceName = "<input>",
      parserExtensions = []
    }

parseExpr :: ParserConfig -> Text -> ParseResult Expr
parseExpr cfg input =
  case lexTokensWithExtensions (parserExtensions cfg) input of
    Left err -> ParseErr (lexerErrorBundle (parserSourceName cfg) err)
    Right toks ->
      case runParser (exprParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right expr -> ParseOk expr

parsePattern :: ParserConfig -> Text -> ParseResult Pattern
parsePattern cfg input =
  case lexTokensWithExtensions (parserExtensions cfg) input of
    Left err -> ParseErr (lexerErrorBundle (parserSourceName cfg) err)
    Right toks ->
      case runParser (patternParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right pat -> ParseOk pat

parseType :: ParserConfig -> Text -> ParseResult Type
parseType cfg input =
  case lexTokensWithExtensions (parserExtensions cfg) input of
    Left err -> ParseErr (lexerErrorBundle (parserSourceName cfg) err)
    Right toks ->
      case runParser (typeParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right ty -> ParseOk ty

parseModule :: ParserConfig -> Text -> ParseResult Module
parseModule cfg input =
  case lexModuleTokensWithExtensions effectiveExtensions input of
    Left err -> ParseErr (lexerErrorBundle (parserSourceName cfg) err)
    Right toks ->
      case runParser (moduleParser <* MP.eof) (parserSourceName cfg) (TokStream toks) of
        Left bundle -> ParseErr bundle
        Right modu -> ParseOk modu
  where
    effectiveExtensions =
      applyExtensionSettings
        (parserExtensions cfg)
        (readModuleHeaderExtensions input)

applyExtensionSettings :: [Extension] -> [ExtensionSetting] -> [Extension]
applyExtensionSettings = List.foldl' applySetting
  where
    applySetting exts setting =
      case setting of
        EnableExtension ext
          | ext `elem` exts -> exts
          | otherwise -> exts <> [ext]
        DisableExtension ext -> filter (/= ext) exts

errorBundlePretty :: ParseErrorBundle -> String
errorBundlePretty = MP.errorBundlePretty
