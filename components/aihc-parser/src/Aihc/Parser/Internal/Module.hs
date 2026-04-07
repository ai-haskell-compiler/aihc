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

import Aihc.Parser.Internal.Common (TokParser, braces, expectedTok, skipSemicolons, tokenSatisfy, withSpan)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax (Decl, ImportDecl, Module (..))
import Control.Monad (void)
import Text.Megaparsec qualified as MP

data RecoverParseStep a
  = RecoverDone
  | RecoverParsed !a
  | RecoverFailed

moduleParser :: TokParser Module
moduleParser = withSpan $ do
  -- Skip any non-LANGUAGE pragmas before the module header (e.g. INCLUDE, OPTIONS_GHC)
  -- These are captured as TkPragmaDeclaration tokens but belong in the file header,
  -- not the declaration stream.
  _ <- MP.many skipPragmaDeclToken
  languagePragmas <- MP.many (languagePragmaParser <* MP.many (expectedTok TkSpecialSemicolon))
  -- Also skip pragmas between LANGUAGE pragmas and the module header
  _ <- MP.many skipPragmaDeclToken
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

-- | Skip a pragma declaration token (used for file-header pragmas that aren't LANGUAGE)
skipPragmaDeclToken :: TokParser ()
skipPragmaDeclToken = do
  void $
    tokenSatisfy "pragma declaration (header)" $ \tok ->
      case lexTokenKind tok of
        TkPragmaDeclaration _ -> Just ()
        _ -> Nothing

moduleBodyParser :: TokParser ([ImportDecl], [Decl])
moduleBodyParser = braces $ do
  skipSemicolons
  imports <- importDeclsWithRecovery
  decls <- declsWithRecovery
  skipSemicolons
  pure (imports, decls)

importDeclsWithRecovery :: TokParser [ImportDecl]
importDeclsWithRecovery = recoverDeclLike importDeclParser

declsWithRecovery :: TokParser [Decl]
declsWithRecovery = recoverDeclLike declParser

recoverDeclLike :: TokParser a -> TokParser [a]
recoverDeclLike parser = go []
  where
    go acc = do
      skipSemicolons
      step <- MP.withRecovery recoverParseError parseStep
      case step of
        RecoverDone -> pure (reverse acc)
        RecoverParsed parsed -> do
          skipSemicolons
          go (parsed : acc)
        RecoverFailed -> do
          skipSemicolons
          go acc

    parseStep = do
      mParsed <- MP.optional parser
      pure $ maybe RecoverDone RecoverParsed mParsed

    recoverParseError err = do
      MP.registerParseError err
      skipUntilDeclBoundary
      pure RecoverFailed

skipUntilDeclBoundary :: TokParser ()
skipUntilDeclBoundary = do
  _ <-
    MP.takeWhileP
      Nothing
      ( \tok ->
          let kind = lexTokenKind tok
           in kind /= TkSpecialSemicolon && kind /= TkSpecialRBrace
      )
  void (MP.optional (expectedTok TkSpecialSemicolon))
