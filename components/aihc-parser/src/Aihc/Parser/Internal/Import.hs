{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Internal.Import
  ( languagePragmaParser,
    moduleHeaderParser,
    importDeclParser,
    warningPragmaParser,
  )
where

import Aihc.Parser.Internal.Common
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind, pattern TkVarAs, pattern TkVarHiding, pattern TkVarQualified, pattern TkVarSafe)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), mkFoundToken)
import Control.Monad (when)
import Data.Char (isUpper)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as MP

languagePragmaParser :: TokParser [ExtensionSetting]
languagePragmaParser =
  hiddenPragma "LANGUAGE pragma" $ \p -> case pragmaType p of
    PragmaLanguage names -> Just names
    _ -> Nothing

moduleHeaderParser :: TokParser ModuleHead
moduleHeaderParser = withSpan $ do
  expectedTok TkKeywordModule
  name <- moduleNameParser
  mWarning <- MP.optional warningPragmaParser
  exports <- MP.optional exportSpecListParser
  expectedTok TkKeywordWhere
  pure $ \span' ->
    ModuleHead
      { moduleHeadAnns = [mkAnnotation span'],
        moduleHeadName = name,
        moduleHeadWarningPragma = mWarning,
        moduleHeadExports = exports
      }

warningPragmaParser :: TokParser Pragma
warningPragmaParser =
  hiddenPragma "warning pragma" $ \p -> case pragmaType p of
    PragmaWarning _ -> Just p
    PragmaDeprecated _ -> Just p
    _ -> Nothing

exportSpecListParser :: TokParser [ExportSpec]
exportSpecListParser = parens $ exportSpecParser `MP.sepEndBy` expectedTok TkSpecialComma

exportSpecParser :: TokParser ExportSpec
exportSpecParser = withSpanAnn (ExportAnn . mkAnnotation) $ do
  mWarning <- MP.optional warningPragmaParser
  exportModuleParser mWarning <|> exportNameParser mWarning

exportModuleParser :: Maybe Pragma -> TokParser ExportSpec
exportModuleParser mWarning = do
  expectedTok TkKeywordModule
  ExportModule mWarning <$> moduleNameParser

exportNameParser :: Maybe Pragma -> TokParser ExportSpec
exportNameParser mWarning = do
  namespace <- MP.optional exportImportNamespaceParser
  name <- identifierNameParser <|> parens operatorNameParser
  members <- MP.optional exportMembersParser
  pure $
    case members of
      Just MembersAll -> ExportAll mWarning namespace name
      Just (MembersList names) -> ExportWith mWarning namespace name names
      Just (MembersListAll wildcardIndex names) -> ExportWithAll mWarning namespace name wildcardIndex names
      Nothing
        | namespace == Just IEEntityNamespaceType || isTypeName name ->
            ExportAbs mWarning namespace name
        | otherwise ->
            ExportVar mWarning namespace name

data MembersResult
  = MembersAll
  | MembersList [IEBundledMember]
  | MembersListAll Int [IEBundledMember]

exportMembersParser :: TokParser MembersResult
exportMembersParser = membersParser

importMembersParser :: TokParser MembersResult
importMembersParser = membersParser

membersParser :: TokParser MembersResult
membersParser =
  parens (parseDotDotFirst <|> parseMemberList <|> emptyMembers)
  where
    emptyMembers = pure (MembersList [])

    parseDotDotFirst = do
      expectedTok TkReservedDotDot
      MP.optional (expectedTok TkSpecialComma) >>= \case
        Nothing -> pure MembersAll
        Just _ -> do
          trailingMembers <- memberNameParser `MP.sepBy` expectedTok TkSpecialComma
          pure (MembersListAll 0 trailingMembers)

    parseMemberList = do
      firstMember <- memberNameParser
      parseMemberSegments [firstMember]

    parseMemberSegments members =
      MP.optional (expectedTok TkSpecialComma) >>= \case
        Nothing -> pure (MembersList members)
        Just _ ->
          (expectedTok TkReservedDotDot >> parseWildcardTail members)
            <|> do
              nextMember <- memberNameParser
              parseMemberSegments (members <> [nextMember])

    parseWildcardTail members =
      MP.optional (expectedTok TkSpecialComma) >>= \case
        Nothing -> pure (MembersListAll (length members) members)
        Just _ -> do
          trailingMembers <- memberNameParser `MP.sepBy` expectedTok TkSpecialComma
          pure (MembersListAll (length members) (members <> trailingMembers))

    memberNameParser = do
      namespace <- MP.optional bundledNamespaceParser
      name <- identifierNameParser <|> parens operatorNameParser
      pure (IEBundledMember namespace name)

-- | Checks if a name refers to a type/class (as opposed to a variable/function).
-- In Haskell:
-- - Identifiers starting with uppercase letters are type constructors/classes
-- - Symbolic operators starting with ':' are constructor operators (type-level)
isTypeName :: Name -> Bool
isTypeName name =
  case T.uncons (nameText name) of
    Just (c, _) -> isUpper c || c == ':'
    Nothing -> False

importDeclParser :: TokParser ImportDecl
importDeclParser = withSpan $ do
  expectedTok TkKeywordImport
  importedSafe <-
    MP.option False (expectedTok TkVarSafe >> pure True)
  importedSource <- optionalHiddenPragma $ \p -> case pragmaType p of
    PragmaSource {} -> Just p
    _ -> Nothing
  preQualified <-
    MP.option False (expectedTok TkVarQualified >> pure True)
  importedLevel <- MP.optional importLevelParser
  importedPackage <- MP.optional packageNameParser
  importedModule <- moduleNameParser
  postQualified <-
    MP.optional $
      tokenSatisfy "'qualified'" $ \tok ->
        if lexTokenKind tok == TkVarQualified then Just (mkFoundToken tok) else Nothing
  when (preQualified && isJust postQualified) $
    MP.customFailure
      UnexpectedTokenExpecting
        { unexpectedFound = postQualified,
          unexpectedExpecting = "import declaration without duplicate 'qualified'",
          unexpectedContext = []
        }
  importAlias <- MP.optional (expectedTok TkVarAs *> moduleNameParser)
  importSpec <- MP.optional importSpecParser
  let isQualified = preQualified || isJust postQualified
  pure $ \span' ->
    ImportDecl
      { importDeclAnns = [mkAnnotation span'],
        importDeclLevel = importedLevel,
        importDeclPackage = importedPackage,
        importDeclSourcePragma = importedSource,
        importDeclSafe = importedSafe,
        importDeclQualified = isQualified,
        importDeclQualifiedPost = isJust postQualified,
        importDeclModule = importedModule,
        importDeclAs = importAlias,
        importDeclSpec = importSpec
      }

importLevelParser :: TokParser ImportLevel
importLevelParser =
  (varIdTok "quote" >> pure ImportLevelQuote)
    <|> (varIdTok "splice" >> pure ImportLevelSplice)

packageNameParser :: TokParser Text
packageNameParser = stringTextParser

importSpecParser :: TokParser ImportSpec
importSpecParser = withSpan $ do
  isHiding <-
    MP.option False (expectedTok TkVarHiding >> pure True)
  items <- parens $ importItemParser `MP.sepEndBy` expectedTok TkSpecialComma
  pure $ \span' ->
    ImportSpec
      { importSpecAnns = [mkAnnotation span'],
        importSpecHiding = isHiding,
        importSpecItems = items
      }

importItemParser :: TokParser ImportItem
importItemParser = withSpanAnn (ImportAnn . mkAnnotation) $ do
  namespace <- MP.optional exportImportNamespaceParser
  itemName <- identifierUnqualifiedNameParser <|> parens importOperatorParser
  -- When there's no explicit namespace, we still need to try parsing members
  -- for type constructors and type classes (uppercase names or parenthesized operators)
  let shouldTryMembers = case namespace of
        Just _ -> True
        Nothing -> isTypeName (qualifyName Nothing itemName)
  members <- if shouldTryMembers then MP.optional importMembersParser else pure Nothing
  let effectiveNamespace = namespace
  pure $
    case members of
      Just MembersAll -> ImportItemAll effectiveNamespace itemName
      Just (MembersList names) -> ImportItemWith effectiveNamespace itemName names
      Just (MembersListAll wildcardIndex names) -> ImportItemAllWith effectiveNamespace itemName wildcardIndex names
      Nothing
        | effectiveNamespace == Just IEEntityNamespaceType || isTypeName (qualifyName Nothing itemName) -> ImportItemAbs effectiveNamespace itemName
        | otherwise -> ImportItemVar effectiveNamespace itemName

importOperatorParser :: TokParser UnqualifiedName
importOperatorParser = operatorUnqualifiedNameParser

exportImportNamespaceParser :: TokParser IEEntityNamespace
exportImportNamespaceParser =
  (expectedTok TkKeywordType >> pure IEEntityNamespaceType)
    <|> (expectedTok TkKeywordData >> pure IEEntityNamespaceData)
    <|> patternNamespaceParser
  where
    patternNamespaceParser = do
      patSynEnabled <- isExtensionEnabled PatternSynonyms
      if patSynEnabled
        then expectedTok TkKeywordPattern >> pure IEEntityNamespacePattern
        else MP.empty

bundledNamespaceParser :: TokParser IEBundledNamespace
bundledNamespaceParser =
  (expectedTok TkKeywordType >> pure IEBundledNamespaceType)
    <|> (expectedTok TkKeywordData >> pure IEBundledNamespaceData)
