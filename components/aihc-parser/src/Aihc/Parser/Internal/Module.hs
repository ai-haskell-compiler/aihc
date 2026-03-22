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

import Aihc.Lexer (LexTokenKind (..))
import Aihc.Parser.Ast (Decl, ImportDecl, Module (..))
import Aihc.Parser.Internal.Common (TokParser, expectedTok, skipSemicolons, withSpan)
import Aihc.Parser.Internal.Decl (declParser, importDeclParser, languagePragmaParser, moduleHeaderParser)
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
