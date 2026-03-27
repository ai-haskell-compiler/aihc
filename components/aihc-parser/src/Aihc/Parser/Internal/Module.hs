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

import Aihc.Parser.Internal.Common (TokParser, expectedTok, skipSemicolons, withSpan)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax (Decl, ImportDecl, Module (..))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Megaparsec (anySingle, lookAhead)
import Text.Megaparsec qualified as MP

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
moduleBodyParser = bracedModuleBodyParser MP.<|> plainModuleBodyParser
  where
    plainModuleBodyParser = do
      imports <- importDeclsWithRecovery
      decls <- MP.many (declParser <* skipSemicolons)
      pure (imports, decls)

    bracedModuleBodyParser = do
      expectedTok TkSpecialLBrace
      skipSemicolons
      imports <- importDeclsWithRecovery
      decls <- MP.many (declParser <* skipSemicolons)
      skipSemicolons
      expectedTok TkSpecialRBrace
      pure (imports, decls)

importDeclsWithRecovery :: TokParser [ImportDecl]
importDeclsWithRecovery = catMaybes <$> go
  where
    go = do
      skipSemicolons
      mTok <- MP.optional (lookAhead anySingle)
      case mTok of
        Just tok
          | lexTokenKind tok == TkKeywordImport -> do
              mImport <- MP.withRecovery recoverImportDecl (Just <$> importDeclParser)
              skipSemicolons
              (mImport :) <$> go
        _ -> pure []

    recoverImportDecl err = do
      MP.registerParseError err
      skipUntilImportBoundary
      pure Nothing

    skipUntilImportBoundary = do
      mTok <- MP.optional (lookAhead anySingle)
      case mTok of
        Nothing -> pure ()
        Just tok ->
          case lexTokenKind tok of
            TkSpecialSemicolon -> anySingle $> ()
            TkSpecialRBrace -> pure ()
            _ -> anySingle *> skipUntilImportBoundary
