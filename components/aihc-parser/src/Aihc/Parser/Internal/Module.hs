-- |
-- Module      : Aihc.Parser.Internal.Module
-- Description : Internal module parser
-- License     : Unlicense
--
-- Internal module containing the core module parser.
-- This is used by both 'Aihc.Parser' and 'Aihc.Parser.Internal.FromTokens'.
module Aihc.Parser.Internal.Module
  ( moduleParser,
  )
where

import Aihc.Parser.Internal.Common (TokParser, braces, expectedTok, skipSemicolons, withSpan)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax (Decl, ImportDecl, Module (..))
import Control.Monad (void)
import Text.Megaparsec qualified as MP

data ImportParseStep
  = ImportDone
  | ImportParsed !ImportDecl
  | ImportRecovered

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  languagePragmas <- MP.many (languagePragmaParser <* MP.many (expectedTok TkSpecialSemicolon))
  mHeader <- MP.optional (moduleHeaderParser <* MP.many (expectedTok TkSpecialSemicolon))
  (imports, decls) <- moduleBodyParser
  pure $ \span' ->
    Module
      { moduleSpan = span',
        moduleHead = mHeader,
        moduleLanguagePragmas = concat languagePragmas,
        moduleImports = imports,
        moduleDecls = decls
      }

moduleBodyParser :: TokParser ([ImportDecl], [Decl])
moduleBodyParser = braces $ do
  skipSemicolons
  imports <- importDeclsWithRecovery
  decls <- MP.many (declParser <* skipSemicolons)
  skipSemicolons
  pure (imports, decls)

importDeclsWithRecovery :: TokParser [ImportDecl]
importDeclsWithRecovery = go []
  where
    go acc = do
      skipSemicolons
      step <- MP.withRecovery recoverImportDecl parseImportStep
      case step of
        ImportDone -> pure (reverse acc)
        ImportParsed importDecl -> do
          skipSemicolons
          go (importDecl : acc)
        ImportRecovered -> do
          skipSemicolons
          go acc

    parseImportStep = do
      mImport <- MP.optional importDeclParser
      pure $ maybe ImportDone ImportParsed mImport

    recoverImportDecl err = do
      MP.registerParseError err
      skipUntilImportBoundary
      pure ImportRecovered

    skipUntilImportBoundary = do
      _ <-
        MP.takeWhileP
          Nothing
          ( \tok ->
              let kind = lexTokenKind tok
               in kind /= TkSpecialSemicolon && kind /= TkSpecialRBrace
          )
      void (MP.optional (expectedTok TkSpecialSemicolon))
