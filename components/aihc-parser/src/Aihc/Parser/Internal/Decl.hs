{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Internal.Decl
  ( declParser,
    importDeclParser,
    moduleHeaderParser,
    languagePragmaParser,
  )
where

import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (equationRhsParser, exprParser, patternParser, simplePatternParser, startsWithContextType, startsWithTypeSig, typeAppParser, typeAtomParser, typeInfixOperatorParser, typeParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind, pattern TkVarFamily, pattern TkVarPattern, pattern TkVarRole)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), mkFoundToken)
import Control.Monad (when)
import Data.Char (isAsciiLower, isUpper)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

languagePragmaParser :: TokParser [ExtensionSetting]
languagePragmaParser =
  hiddenPragma "LANGUAGE pragma" $ \case
    PragmaLanguage names -> Just names
    _ -> Nothing

instanceOverlapPragmaParser :: TokParser InstanceOverlapPragma
instanceOverlapPragmaParser =
  hiddenPragma "instance overlap pragma" $ \case
    PragmaInstanceOverlap overlapPragma -> Just overlapPragma
    _ -> Nothing

anyPragmaParser :: String -> TokParser Pragma
anyPragmaParser expectedLabel = hiddenPragma expectedLabel Just

moduleHeaderParser :: TokParser ModuleHead
moduleHeaderParser = withSpan $ do
  expectedTok TkKeywordModule
  name <- moduleNameParser
  mWarning <- MP.optional warningTextParser
  exports <- MP.optional exportSpecListParser
  expectedTok TkKeywordWhere
  pure $ \span' ->
    ModuleHead
      { moduleHeadSpan = span',
        moduleHeadName = name,
        moduleHeadWarningText = mWarning,
        moduleHeadExports = exports
      }

warningTextParser :: TokParser WarningText
warningTextParser =
  withSpan $
    hiddenPragma "warning pragma" $ \case
      PragmaWarning msg -> Just (`WarnText` msg)
      PragmaDeprecated msg -> Just (`DeprText` msg)
      _ -> Nothing

exportSpecListParser :: TokParser [ExportSpec]
exportSpecListParser = parens $ exportSpecParser `MP.sepEndBy` expectedTok TkSpecialComma

exportSpecParser :: TokParser ExportSpec
exportSpecParser = withSpan $ do
  mWarning <- MP.optional warningTextParser
  exportModuleParser mWarning <|> exportNameParser mWarning

exportModuleParser :: Maybe WarningText -> TokParser (SourceSpan -> ExportSpec)
exportModuleParser mWarning = do
  expectedTok TkKeywordModule
  modName <- moduleNameParser
  pure $ \span' -> ExportModule span' mWarning modName

exportNameParser :: Maybe WarningText -> TokParser (SourceSpan -> ExportSpec)
exportNameParser mWarning = do
  namespace <- MP.optional exportImportNamespaceParser
  name <- identifierTextParser <|> parens operatorTextParser
  members <- MP.optional exportMembersParser
  pure $ \span' ->
    case members of
      Just Nothing -> ExportAll span' mWarning namespace name
      Just (Just names) -> ExportWith span' mWarning namespace name names
      Nothing
        | namespace == Just IEEntityNamespaceType || isTypeName name ->
            ExportAbs span' mWarning namespace name
        | otherwise ->
            ExportVar span' mWarning namespace name

exportMembersParser :: TokParser (Maybe [IEBundledMember])
exportMembersParser =
  parens $
    (expectedTok TkReservedDotDot >> pure Nothing)
      <|> (Just <$> (memberNameParser `MP.sepEndBy` expectedTok TkSpecialComma))
  where
    memberNameParser = do
      namespace <- MP.optional bundledNamespaceParser
      name <- identifierTextParser <|> parens operatorTextParser
      pure (IEBundledMember namespace name)

-- | Checks if a name refers to a type/class (as opposed to a variable/function).
-- In Haskell:
-- - Identifiers starting with uppercase letters are type constructors/classes
-- - Symbolic operators starting with ':' are constructor operators (type-level)
isTypeName :: Text -> Bool
isTypeName txt =
  case T.uncons txt of
    Just (c, _) -> isUpper c || c == ':'
    Nothing -> False

importDeclParser :: TokParser ImportDecl
importDeclParser = withSpan $ do
  expectedTok TkKeywordImport
  preQualified <-
    MP.option False (expectedTok TkKeywordQualified >> pure True)
  importedLevel <- MP.optional importLevelParser
  importedPackage <- MP.optional packageNameParser
  let isSourcePragma :: Pragma -> Maybe Bool
      isSourcePragma = \case
        PragmaSource {} -> Just True
        _ -> Nothing
  importedSource <-
    fromMaybe False <$> optionalHiddenPragma isSourcePragma
  importedModule <- moduleNameParser
  postQualified <-
    MP.optional $
      tokenSatisfy "keyword 'qualified'" $ \tok ->
        if lexTokenKind tok == TkKeywordQualified then Just (mkFoundToken tok) else Nothing
  when (preQualified && isJust postQualified) $
    MP.customFailure
      UnexpectedTokenExpecting
        { unexpectedFound = postQualified,
          unexpectedExpecting = "import declaration without duplicate 'qualified'",
          unexpectedContext = []
        }
  importAlias <- MP.optional (expectedTok TkKeywordAs *> moduleNameParser)
  importSpec <- MP.optional importSpecParser
  let isQualified = preQualified || isJust postQualified
  pure $ \span' ->
    ImportDecl
      { importDeclSpan = span',
        importDeclLevel = importedLevel,
        importDeclPackage = importedPackage,
        importDeclSource = importedSource,
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
    MP.option False (expectedTok TkKeywordHiding >> pure True)
  items <- parens $ importItemParser `MP.sepEndBy` expectedTok TkSpecialComma
  pure $ \span' ->
    ImportSpec
      { importSpecSpan = span',
        importSpecHiding = isHiding,
        importSpecItems = items
      }

importItemParser :: TokParser ImportItem
importItemParser = withSpan $ do
  namespace <- MP.optional exportImportNamespaceParser
  itemName <- identifierTextParser <|> parens importOperatorParser
  -- When there's no explicit namespace, we still need to try parsing members
  -- for type constructors and type classes (uppercase names or parenthesized operators)
  let shouldTryMembers = case namespace of
        Just _ -> True
        Nothing -> isTypeName itemName
  members <- if shouldTryMembers then MP.optional exportMembersParser else pure Nothing
  let effectiveNamespace = namespace
  pure $ \span' ->
    case members of
      Just Nothing -> ImportItemAll span' effectiveNamespace itemName
      Just (Just names) -> ImportItemWith span' effectiveNamespace itemName names
      Nothing
        | effectiveNamespace == Just IEEntityNamespaceType || isTypeName itemName -> ImportItemAbs span' effectiveNamespace itemName
        | otherwise -> ImportItemVar span' effectiveNamespace itemName

importOperatorParser :: TokParser Text
importOperatorParser = operatorTextParser

exportImportNamespaceParser :: TokParser IEEntityNamespace
exportImportNamespaceParser =
  (expectedTok TkKeywordType >> pure IEEntityNamespaceType)
    <|> (expectedTok TkKeywordData >> pure IEEntityNamespaceData)
    <|> patternNamespaceParser
  where
    patternNamespaceParser = do
      patSynEnabled <- isExtensionEnabled PatternSynonyms
      if patSynEnabled
        then expectedTok TkVarPattern >> pure IEEntityNamespacePattern
        else MP.empty

bundledNamespaceParser :: TokParser IEBundledNamespace
bundledNamespaceParser =
  expectedTok TkKeywordData >> pure IEBundledNamespaceData

declParser :: TokParser Decl
declParser = do
  mPragmaDecl <- MP.optional pragmaDeclParser
  maybe ordinaryDeclParser pure mPragmaDecl

ordinaryDeclParser :: TokParser Decl
ordinaryDeclParser = do
  (tok, nextTok) <- lookAhead ((,) <$> anySingle <*> anySingle)
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  let valueOrSpliceParser =
        if thFullEnabled
          then MP.try valueDeclParser <|> implicitSpliceDeclParser
          else valueDeclParser
      patternOrSpliceParser =
        if thFullEnabled
          then MP.try patternBindDeclParser <|> implicitSpliceDeclParser
          else MP.try patternBindDeclParser
      typeSigOrValueOrSpliceParser =
        MP.try typeSigDeclParser <|> valueOrSpliceParser
      typeSigOrPatternOrValueOrSpliceParser =
        MP.try typeSigDeclParser <|> patternOrSpliceParser <|> valueOrSpliceParser
  case lexTokenKind tok of
    TkKeywordData ->
      case lexTokenKind nextTok of
        TkVarFamily -> dataFamilyDeclParser
        TkKeywordInstance -> dataFamilyInstParser
        _ -> dataDeclParser
    TkKeywordClass -> classDeclParser
    TkKeywordDefault -> defaultDeclParser
    TkKeywordDeriving -> standaloneDerivingDeclParser
    TkKeywordForeign -> foreignDeclParser
    TkKeywordInfix -> fixityDeclParser Infix
    TkKeywordInfixl -> fixityDeclParser InfixL
    TkKeywordInfixr -> fixityDeclParser InfixR
    TkKeywordInstance -> instanceDeclParser
    TkKeywordNewtype
      | lexTokenKind nextTok == TkKeywordInstance -> newtypeFamilyInstParser
    TkKeywordNewtype -> newtypeDeclParser
    TkKeywordType ->
      case lexTokenKind nextTok of
        TkVarRole -> roleAnnotationDeclParser
        TkVarFamily -> typeFamilyDeclParser
        TkKeywordData -> typeDataDeclParser
        TkKeywordInstance -> typeFamilyInstParser
        _ -> typeDeclarationParser
    TkVarPattern -> patternSynonymParser
    TkSpecialLParen -> typeSigOrPatternOrValueOrSpliceParser
    TkSpecialLBracket -> patternOrSpliceParser
    TkPrefixTilde -> patternOrSpliceParser
    TkKeywordUnderscore -> patternOrSpliceParser
    TkTHSplice -> spliceDeclParser
    _ -> typeSigOrValueOrSpliceParser

-- | Parse a pragma declaration (e.g. {-# INLINE f #-}, {-# SPECIALIZE ... #-})
pragmaDeclParser :: TokParser Decl
pragmaDeclParser = withSpan $ do
  pragma <- anyPragmaParser "pragma declaration"
  pure (`DeclPragma` pragma)

-- | Parse a top-level Template Haskell declaration splice: $expr or $(expr)
spliceDeclParser :: TokParser Decl
spliceDeclParser = withSpan $ do
  expectedTok TkTHSplice
  body <-
    parenSpliceBody <|> bareSpliceBody
  pure (`DeclSplice` body)
  where
    parenSpliceBody = withSpan $ do
      body <- parens exprParser
      pure (`EParen` body)
    bareSpliceBody = withSpan $ do
      name <- identifierTextParser
      pure (`EVar` name)

-- | Parse an implicit top-level Template Haskell declaration splice: @expr@.
-- GHC accepts bare declaration splices under TemplateHaskell and also pretty-prints
-- them as explicit @$...@ splices, so we parse the expression body directly here.
implicitSpliceDeclParser :: TokParser Decl
implicitSpliceDeclParser = withSpan $ do
  body <- exprParser
  pure (`DeclSplice` body)

-- | Parse a @type@ declaration after the @type@ keyword has been consumed.
-- Uses 'typeDeclHeadParser' to handle both prefix and infix type heads,
-- then dispatches based on the next token:
-- - @::@ → standalone kind signature (must have zero type parameters)
-- - @=@ → type synonym
typeDeclarationParser :: TokParser Decl
typeDeclarationParser = withSpan $ do
  expectedTok TkKeywordType
  (typeName, typeParams) <- typeDeclHeadParser
  nextTok <- anySingle
  case lexTokenKind nextTok of
    TkReservedDoubleColon -> do
      -- Standalone kind signature: cannot have type parameters
      if null typeParams
        then do
          kind <- typeParser
          pure (\span' -> DeclStandaloneKindSig span' typeName kind)
        else
          fail "Standalone kind signatures cannot have type parameters."
    TkReservedEquals -> do
      body <- typeParser
      pure $ \span' ->
        DeclTypeSyn
          span'
          TypeSynDecl
            { typeSynSpan = span',
              typeSynName = typeName,
              typeSynParams = typeParams,
              typeSynBody = body
            }
    _ ->
      fail "expected '::' or '=' after type declaration head"

roleAnnotationDeclParser :: TokParser Decl
roleAnnotationDeclParser = withSpan $ do
  expectedTok TkKeywordType
  expectedTok TkVarRole
  typeName <- constructorIdentifierParser
  roles <- MP.some roleParser
  pure $ \span' ->
    DeclRoleAnnotation
      span'
      RoleAnnotation
        { roleAnnotationSpan = span',
          roleAnnotationName = typeName,
          roleAnnotationRoles = roles
        }

roleParser :: TokParser Role
roleParser =
  (varIdTok "nominal" >> pure RoleNominal)
    <|> (varIdTok "representational" >> pure RoleRepresentational)
    <|> (varIdTok "phantom" >> pure RolePhantom)
    <|> (expectedTok TkKeywordUnderscore >> pure RoleInfer)

-- ---------------------------------------------------------------------------
-- TypeFamilies: shared helpers

forallPrefixDispatch :: TokParser [a] -> TokParser [a]
forallPrefixDispatch forallParser = forallParser <|> pure []

-- | Non-consuming lookahead dispatch for optional declaration contexts.
-- Uses 'startsWithContextType' to probe for @=>@ at top bracket depth.
-- Returns @Nothing@ when the input doesn't look like a context,
-- otherwise parses the context and the @=>@ that follows it.
-- Eliminates 'MP.try' around @declContextParser \<* expectedTok TkReservedDoubleArrow@.
contextPrefixDispatch :: TokParser (Maybe [Type])
contextPrefixDispatch = do
  hasContext <- startsWithContextType
  if hasContext
    then Just <$> (declContextParser <* expectedTok TkReservedDoubleArrow)
    else pure Nothing

-- | Like 'contextPrefixDispatch' but returns @[]@ instead of @Nothing@.
contextPrefixDispatchList :: TokParser [Type]
contextPrefixDispatchList = do
  hasContext <- startsWithContextType
  if hasContext
    then declContextParser <* expectedTok TkReservedDoubleArrow
    else pure []

-- | Parse an optional explicit forall for type family instances/equations.
-- Handles @forall a (b :: Kind).@ syntax.
typeFamilyForallParser :: TokParser [TyVarBinder]
typeFamilyForallParser = do
  expectedTok TkKeywordForall
  binders <- MP.some typeParamParser
  expectedTok (TkVarSym ".")
  pure binders

-- | Parse an optional explicit forall for instance heads.
-- Handles @forall a (b :: Kind).@ syntax.
instanceForallParser :: TokParser [TyVarBinder]
instanceForallParser = do
  expectedTok TkKeywordForall
  binders <- MP.some typeParamParser
  expectedTok (TkVarSym ".")
  pure binders

-- | Parse the optional @:: Kind@ result annotation on a type/data family head.
familyResultKindParser :: TokParser (Maybe Type)
familyResultKindParser =
  MP.optional (expectedTok TkReservedDoubleColon *> typeParser)

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level type family declaration

-- | Parse @type family Name params [:: Kind] [where { equations }]@
typeFamilyDeclParser :: TokParser Decl
typeFamilyDeclParser = withSpan $ do
  expectedTok TkKeywordType
  expectedTok TkVarFamily
  (headForm, headType, params) <- typeFamilyHeadParser
  kind <- familyResultKindParser
  -- A closed type family has a `where` clause with equations.
  equations <- MP.optional (MP.try closedTypeFamilyWhereParser)
  pure $ \span' ->
    DeclTypeFamilyDecl
      span'
      TypeFamilyDecl
        { typeFamilyDeclSpan = span',
          typeFamilyDeclHeadForm = headForm,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclKind = kind,
          typeFamilyDeclEquations = equations
        }

-- | Parse the @where { eq; ... }@ block of a closed type family.
closedTypeFamilyWhereParser :: TokParser [TypeFamilyEq]
closedTypeFamilyWhereParser =
  whereClauseItemsParser
    (bracedSemiSep typeFamilyEqParser)
    (plainSemiSep1 typeFamilyEqParser)

-- | Parse one closed type family equation: @[forall binders.] LhsType = RhsType@
typeFamilyEqParser :: TokParser TypeFamilyEq
typeFamilyEqParser = withSpan $ do
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $ \span' ->
    TypeFamilyEq
      { typeFamilyEqSpan = span',
        typeFamilyEqForall = forallBinders,
        typeFamilyEqHeadForm = headForm,
        typeFamilyEqLhs = lhs,
        typeFamilyEqRhs = rhs
      }

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level data family declaration

-- | Parse @data family Name params [:: Kind]@
dataFamilyDeclParser :: TokParser Decl
dataFamilyDeclParser = withSpan $ do
  expectedTok TkKeywordData
  varIdTok "family"
  name <- constructorIdentifierParser
  params <- MP.many typeParamParser
  kind <- familyResultKindParser
  pure $ \span' ->
    DeclDataFamilyDecl
      span'
      DataFamilyDecl
        { dataFamilyDeclSpan = span',
          dataFamilyDeclName = name,
          dataFamilyDeclParams = params,
          dataFamilyDeclKind = kind
        }

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level type/data/newtype family instances

-- | Parse @type instance [forall binders.] LhsType = RhsType@
typeFamilyInstParser :: TokParser Decl
typeFamilyInstParser = withSpan $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $ \span' ->
    DeclTypeFamilyInst
      span'
      TypeFamilyInst
        { typeFamilyInstSpan = span',
          typeFamilyInstForall = forallBinders,
          typeFamilyInstHeadForm = headForm,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

-- | Parse @data instance [forall binders.] HeadType = Cons | ...@ (also GADT style)
dataFamilyInstParser :: TokParser Decl
dataFamilyInstParser = withSpan $ do
  expectedTok TkKeywordData
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  head' <- typeAppParser
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $ \span' ->
    DeclDataFamilyInst
      span'
      DataFamilyInst
        { dataFamilyInstSpan = span',
          dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = forallBinders,
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = constructors,
          dataFamilyInstDeriving = derivingClauses
        }
  where
    traditionalStyleDataDecl = do
      constructors <- MP.optional (expectedTok TkReservedEquals *> dataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe)
      derivingClauses <- MP.many derivingClauseParser
      pure (fromMaybe [] constructors, derivingClauses)
    gadtStyleDataDecl = do
      constructors <- gadtWhereClauseParser
      derivingClauses <- MP.many derivingClauseParser
      pure (constructors, derivingClauses)

-- | Parse @newtype instance [forall binders.] HeadType = Constructor@
newtypeFamilyInstParser :: TokParser Decl
newtypeFamilyInstParser = withSpan $ do
  expectedTok TkKeywordNewtype
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  head' <- typeAppParser
  expectedTok TkReservedEquals
  constructor <- newtypeConDeclParser
  derivingClauses <- MP.many derivingClauseParser
  pure $ \span' ->
    DeclDataFamilyInst
      span'
      DataFamilyInst
        { dataFamilyInstSpan = span',
          dataFamilyInstIsNewtype = True,
          dataFamilyInstForall = forallBinders,
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = [constructor],
          dataFamilyInstDeriving = derivingClauses
        }

-- ---------------------------------------------------------------------------
-- TypeFamilies: class body items (associated type/data families + defaults)

-- | Parse @type Name params [:: Kind]@ as an associated type family in a class.
-- Note: no @family@ keyword inside class bodies.
-- Callers must ensure the next token after @type@ is not @instance@
-- (which is handled by 'classDefaultTypeInstParser' via token dispatch).
classTypeFamilyDeclParser :: TokParser ClassDeclItem
classTypeFamilyDeclParser = withSpan $ do
  expectedTok TkKeywordType
  (headForm, headType, params) <- typeFamilyHeadParser
  kind <- familyResultKindParser
  pure $ \span' ->
    ClassItemTypeFamilyDecl
      span'
      TypeFamilyDecl
        { typeFamilyDeclSpan = span',
          typeFamilyDeclHeadForm = headForm,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclKind = kind,
          typeFamilyDeclEquations = Nothing
        }

-- | Parse @data Name params [:: Kind]@ as an associated data family in a class.
classDataFamilyDeclParser :: TokParser ClassDeclItem
classDataFamilyDeclParser = withSpan $ do
  expectedTok TkKeywordData
  name <- constructorIdentifierParser
  params <- MP.many typeParamParser
  kind <- familyResultKindParser
  pure $ \span' ->
    ClassItemDataFamilyDecl
      span'
      DataFamilyDecl
        { dataFamilyDeclSpan = span',
          dataFamilyDeclName = name,
          dataFamilyDeclParams = params,
          dataFamilyDeclKind = kind
        }

-- | Parse @type instance LhsType = RhsType@ as a default type family instance in a class.
classDefaultTypeInstParser :: TokParser ClassDeclItem
classDefaultTypeInstParser = withSpan $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $ \span' ->
    ClassItemDefaultTypeInst
      span'
      TypeFamilyInst
        { typeFamilyInstSpan = span',
          typeFamilyInstForall = forallBinders,
          typeFamilyInstHeadForm = headForm,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

-- ---------------------------------------------------------------------------
-- TypeFamilies: instance body items

-- | Parse @type LhsType = RhsType@ inside an instance body (no @instance@ keyword here).
instanceTypeFamilyInstParser :: TokParser InstanceDeclItem
instanceTypeFamilyInstParser = withSpan $ do
  expectedTok TkKeywordType
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $ \span' ->
    InstanceItemTypeFamilyInst
      span'
      TypeFamilyInst
        { typeFamilyInstSpan = span',
          typeFamilyInstForall = forallBinders,
          typeFamilyInstHeadForm = headForm,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

-- | Parse @data HeadType = Cons | ...@ (or GADT style) inside an instance body.
instanceDataFamilyInstParser :: TokParser InstanceDeclItem
instanceDataFamilyInstParser = withSpan $ do
  expectedTok TkKeywordData
  head' <- typeAppParser
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $ \span' ->
    InstanceItemDataFamilyInst
      span'
      DataFamilyInst
        { dataFamilyInstSpan = span',
          dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = constructors,
          dataFamilyInstDeriving = derivingClauses
        }
  where
    traditionalStyleDataDecl = do
      constructors <- MP.optional (expectedTok TkReservedEquals *> dataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe)
      derivingClauses <- MP.many derivingClauseParser
      pure (fromMaybe [] constructors, derivingClauses)
    gadtStyleDataDecl = do
      constructors <- gadtWhereClauseParser
      derivingClauses <- MP.many derivingClauseParser
      pure (constructors, derivingClauses)

-- | Parse @newtype HeadType = Constructor@ inside an instance body.
instanceNewtypeFamilyInstParser :: TokParser InstanceDeclItem
instanceNewtypeFamilyInstParser = withSpan $ do
  expectedTok TkKeywordNewtype
  head' <- typeAppParser
  expectedTok TkReservedEquals
  constructor <- newtypeConDeclParser
  derivingClauses <- MP.many derivingClauseParser
  pure $ \span' ->
    InstanceItemDataFamilyInst
      span'
      DataFamilyInst
        { dataFamilyInstSpan = span',
          dataFamilyInstIsNewtype = True,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = [constructor],
          dataFamilyInstDeriving = derivingClauses
        }

-- ---------------------------------------------------------------------------

typeSigDeclParser :: TokParser Decl
typeSigDeclParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

defaultDeclParser :: TokParser Decl
defaultDeclParser = withSpan $ do
  expectedTok TkKeywordDefault
  tys <- parens (typeParser `MP.sepEndBy1` expectedTok TkSpecialComma)
  pure (`DeclDefault` tys)

fixityDeclParser :: FixityAssoc -> TokParser Decl
fixityDeclParser assoc = withSpan $ do
  (parsedAssoc, prec, ops) <- fixityDeclPartsParser
  when (assoc /= parsedAssoc) $
    fail "internal fixity dispatch mismatch"
  pure (\span' -> DeclFixity span' parsedAssoc prec ops)

fixityDeclPartsParser :: TokParser (FixityAssoc, Maybe Int, [Text])
fixityDeclPartsParser = do
  assoc <- fixityAssocParser
  prec <- MP.optional fixityPrecedenceParser
  ops <- fixityOperatorParser `MP.sepBy1` expectedTok TkSpecialComma
  pure (assoc, prec, ops)

fixityAssocParser :: TokParser FixityAssoc
fixityAssocParser =
  (expectedTok TkKeywordInfix >> pure Infix)
    <|> (expectedTok TkKeywordInfixl >> pure InfixL)
    <|> (expectedTok TkKeywordInfixr >> pure InfixR)

fixityPrecedenceParser :: TokParser Int
fixityPrecedenceParser =
  tokenSatisfy "fixity precedence" $ \tok ->
    case lexTokenKind tok of
      TkInteger n
        | n >= 0 && n <= 9 -> Just (fromInteger n)
      _ -> Nothing

fixityOperatorParser :: TokParser Text
fixityOperatorParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "fixity operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just op
          TkConSym op -> Just op
          TkQVarSym op -> Just op
          TkQConSym op -> Just op
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- identifierTextParser
      expectedTok TkSpecialBacktick
      pure op

classDeclParser :: TokParser Decl
classDeclParser = withSpan $ do
  expectedTok TkKeywordClass
  context <- contextPrefixDispatch
  (headForm, className, classParams) <- classHeadParser
  classFundeps <- MP.option [] (MP.try classFundepsParser)
  items <- MP.option [] classWhereClauseParser
  pure $ \span' ->
    DeclClass
      span'
      ClassDecl
        { classDeclSpan = span',
          classDeclContext = context,
          classDeclHeadForm = headForm,
          classDeclName = className,
          classDeclParams = classParams,
          classDeclFundeps = classFundeps,
          classDeclItems = items
        }

classFundepsParser :: TokParser [FunctionalDependency]
classFundepsParser = do
  expectedTok TkReservedPipe
  classFundepParser `MP.sepBy1` expectedTok TkSpecialComma

classFundepParser :: TokParser FunctionalDependency
classFundepParser = withSpan $ do
  determinedBy <- MP.many lowerIdentifierParser
  expectedTok TkReservedRightArrow
  determines <- MP.many lowerIdentifierParser
  pure $ \span' ->
    FunctionalDependency
      { functionalDependencySpan = span',
        functionalDependencyDeterminers = determinedBy,
        functionalDependencyDetermined = determines
      }

classWhereClauseParser :: TokParser [ClassDeclItem]
classWhereClauseParser = whereClauseItemsParser classItemsBracedParser classItemsPlainParser

whereClauseItemsParser :: TokParser [item] -> TokParser [item] -> TokParser [item]
whereClauseItemsParser bracedParser plainParser = do
  expectedTok TkKeywordWhere
  bracedParser <|> plainParser <|> pure []

classItemsPlainParser :: TokParser [ClassDeclItem]
classItemsPlainParser = plainSemiSep1 classDeclItemParser

classItemsBracedParser :: TokParser [ClassDeclItem]
classItemsBracedParser = bracedSemiSep classDeclItemParser

classDeclItemParser :: TokParser ClassDeclItem
classDeclItemParser = do
  mPragmaItem <- MP.optional classPragmaItemParser
  maybe ordinaryClassDeclItemParser pure mPragmaItem

ordinaryClassDeclItemParser :: TokParser ClassDeclItem
ordinaryClassDeclItemParser = do
  (tok, nextTok) <- lookAhead ((,) <$> anySingle <*> anySingle)
  case lexTokenKind tok of
    TkKeywordInfix -> classFixityItemParser
    TkKeywordInfixl -> classFixityItemParser
    TkKeywordInfixr -> classFixityItemParser
    TkKeywordData -> classDataFamilyDeclParser
    TkKeywordDefault -> classDefaultSigItemParser
    TkKeywordType
      | lexTokenKind nextTok == TkKeywordInstance -> classDefaultTypeInstParser
    TkKeywordType -> classTypeFamilyDeclParser
    _ -> do
      isSig <- startsWithTypeSig
      if isSig then classTypeSigItemParser else classDefaultItemParser

classPragmaItemParser :: TokParser ClassDeclItem
classPragmaItemParser = withSpan $ do
  pragma <- anyPragmaParser "pragma declaration"
  pure (`ClassItemPragma` pragma)

classTypeSigItemParser :: TokParser ClassDeclItem
classTypeSigItemParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> ClassItemTypeSig span' names ty)

classDefaultSigItemParser :: TokParser ClassDeclItem
classDefaultSigItemParser = withSpan $ do
  expectedTok TkKeywordDefault
  name <- binderNameParser
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> ClassItemDefaultSig span' name ty)

classFixityItemParser :: TokParser ClassDeclItem
classFixityItemParser = withSpan $ do
  (assoc, prec, ops) <- fixityDeclPartsParser
  pure (\span' -> ClassItemFixity span' assoc prec ops)

classDefaultItemParser :: TokParser ClassDeclItem
classDefaultItemParser = withSpan $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  pure (\span' -> ClassItemDefault span' (functionBindValue span' headForm name pats (UnguardedRhs span' rhsExpr)))

instanceDeclParser :: TokParser Decl
instanceDeclParser = withSpan $ do
  expectedTok TkKeywordInstance
  overlapPragma <- MP.optional instanceOverlapPragmaParser
  forallBinders <- MP.optional instanceForallParser
  context <- contextPrefixDispatch
  (parenthesizedHead, className, instanceTypes) <- instanceHeadParser
  items <- MP.option [] instanceWhereClauseParser
  pure $ \span' ->
    DeclInstance
      span'
      InstanceDecl
        { instanceDeclSpan = span',
          instanceDeclOverlapPragma = overlapPragma,
          instanceDeclForall = fromMaybe [] forallBinders,
          instanceDeclContext = fromMaybe [] context,
          instanceDeclParenthesizedHead = parenthesizedHead,
          instanceDeclClassName = className,
          instanceDeclTypes = instanceTypes,
          instanceDeclItems = items
        }

standaloneDerivingDeclParser :: TokParser Decl
standaloneDerivingDeclParser = withSpan $ do
  expectedTok TkKeywordDeriving
  strategy <- MP.optional derivingStrategyParser
  viaTy <- MP.optional (MP.try derivingViaTypeParser)
  expectedTok TkKeywordInstance
  overlapPragma <- MP.optional instanceOverlapPragmaParser
  forallBinders <- MP.optional instanceForallParser
  context <- contextPrefixDispatch
  (parenthesizedHead, className, instanceTypes) <- instanceHeadParser
  pure $ \span' ->
    DeclStandaloneDeriving
      span'
      StandaloneDerivingDecl
        { standaloneDerivingSpan = span',
          standaloneDerivingStrategy = strategy,
          standaloneDerivingOverlapPragma = overlapPragma,
          standaloneDerivingForall = fromMaybe [] forallBinders,
          standaloneDerivingContext = fromMaybe [] context,
          standaloneDerivingParenthesizedHead = parenthesizedHead,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = instanceTypes,
          standaloneDerivingViaType = viaTy
        }

instanceHeadParser :: TokParser (Bool, Text, [Type])
instanceHeadParser =
  MP.try (parens bareInstanceHeadParser >>= \(className, instanceTypes) -> pure (True, className, instanceTypes))
    <|> ( do
            (className, instanceTypes) <- bareInstanceHeadParser
            pure (False, className, instanceTypes)
        )
  where
    bareInstanceHeadParser = do
      className <- constructorIdentifierParser
      instanceTypes <- MP.some typeAtomParser
      pure (className, instanceTypes)

instanceWhereClauseParser :: TokParser [InstanceDeclItem]
instanceWhereClauseParser = whereClauseItemsParser instanceItemsBracedParser instanceItemsPlainParser

instanceItemsPlainParser :: TokParser [InstanceDeclItem]
instanceItemsPlainParser = plainSemiSep1 instanceDeclItemParser

instanceItemsBracedParser :: TokParser [InstanceDeclItem]
instanceItemsBracedParser = bracedSemiSep instanceDeclItemParser

instanceDeclItemParser :: TokParser InstanceDeclItem
instanceDeclItemParser = do
  mPragmaItem <- MP.optional instancePragmaItemParser
  maybe ordinaryInstanceDeclItemParser pure mPragmaItem

ordinaryInstanceDeclItemParser :: TokParser InstanceDeclItem
ordinaryInstanceDeclItemParser =
  instanceFixityItemParser
    <|> instanceFixityItemParser
    <|> instanceFixityItemParser
    <|> instanceTypeFamilyInstParser
    <|> instanceDataFamilyInstParser
    <|> instanceNewtypeFamilyInstParser
    <|> ( do
            isSig <- startsWithTypeSig
            if isSig then instanceTypeSigItemParser else instanceValueItemParser
        )

instancePragmaItemParser :: TokParser InstanceDeclItem
instancePragmaItemParser = withSpan $ do
  pragma <- anyPragmaParser "pragma declaration"
  pure (`InstanceItemPragma` pragma)

instanceTypeSigItemParser :: TokParser InstanceDeclItem
instanceTypeSigItemParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> InstanceItemTypeSig span' names ty)

instanceFixityItemParser :: TokParser InstanceDeclItem
instanceFixityItemParser = withSpan $ do
  (assoc, prec, ops) <- fixityDeclPartsParser
  pure (\span' -> InstanceItemFixity span' assoc prec ops)

instanceValueItemParser :: TokParser InstanceDeclItem
instanceValueItemParser = withSpan $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  rhs <- equationRhsParser
  pure (\span' -> InstanceItemBind span' (functionBindValue span' headForm name pats rhs))

foreignDeclParser :: TokParser Decl
foreignDeclParser = withSpan $ do
  expectedTok TkKeywordForeign
  direction <- foreignDirectionParser
  callConv <- callConvParser
  safety <-
    case direction of
      ForeignImport -> MP.optional foreignSafetyParser
      ForeignExport -> pure Nothing
  entity <- MP.optional foreignEntityParser
  name <- identifierTextParser
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure $ \span' ->
    DeclForeign
      span'
      ForeignDecl
        { foreignDeclSpan = span',
          foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = fromMaybe ForeignEntityOmitted entity,
          foreignName = name,
          foreignType = ty
        }

foreignDirectionParser :: TokParser ForeignDirection
foreignDirectionParser =
  (expectedTok TkKeywordImport >> pure ForeignImport)
    <|> (varIdTok "export" >> pure ForeignExport)

callConvParser :: TokParser CallConv
callConvParser =
  (varIdTok "ccall" >> pure CCall)
    <|> (varIdTok "stdcall" >> pure StdCall)
    <|> (varIdTok "capi" >> pure CApi)

foreignSafetyParser :: TokParser ForeignSafety
foreignSafetyParser =
  (varIdTok "safe" >> pure Safe)
    <|> (varIdTok "unsafe" >> pure Unsafe)

foreignEntityParser :: TokParser ForeignEntitySpec
foreignEntityParser = foreignEntityFromString <$> stringTextParser

foreignEntityFromString :: Text -> ForeignEntitySpec
foreignEntityFromString txt
  | txt == "dynamic" = ForeignEntityDynamic
  | txt == "wrapper" = ForeignEntityWrapper
  | txt == "static" = ForeignEntityStatic Nothing
  | Just rest <- T.stripPrefix "static " txt = ForeignEntityStatic (Just rest)
  | txt == "&" = ForeignEntityAddress Nothing
  | Just rest <- T.stripPrefix "&" txt = ForeignEntityAddress (Just rest)
  | otherwise = ForeignEntityNamed txt

dataDeclParser :: TokParser Decl
dataDeclParser = withSpan $ do
  expectedTok TkKeywordData
  context <- contextPrefixDispatch
  (typeName, typeParams) <- typeDeclHeadParser
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $ \span' ->
    DeclData
      span'
      DataDecl
        { dataDeclSpan = span',
          dataDeclContext = fromMaybe [] context,
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclConstructors = constructors,
          dataDeclDeriving = derivingClauses
        }
  where
    traditionalStyleDataDecl = do
      constructors <- MP.optional (expectedTok TkReservedEquals *> dataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe)
      derivingClauses <- MP.many derivingClauseParser
      pure (fromMaybe [] constructors, derivingClauses)

    gadtStyleDataDecl = do
      constructors <- gadtWhereClauseParser
      derivingClauses <- MP.many derivingClauseParser
      pure (constructors, derivingClauses)

-- | Parse a @type data@ declaration.
-- Similar to @data@ but with restrictions:
--   - No datatype context
--   - No labelled fields in constructors
--   - No strictness annotations in constructors
--   - No deriving clause
typeDataDeclParser :: TokParser Decl
typeDataDeclParser = withSpan $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordData
  -- type data may not have a datatype context
  (typeName, typeParams) <- typeDeclHeadParser
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  constructors <- gadtStyleTypeDataDecl <|> traditionalStyleTypeDataDecl
  -- type data may not have a deriving clause
  pure $ \span' ->
    DeclTypeData
      span'
      DataDecl
        { dataDeclSpan = span',
          dataDeclContext = [],
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclConstructors = constructors,
          dataDeclDeriving = []
        }
  where
    traditionalStyleTypeDataDecl =
      fromMaybe [] <$> MP.optional (expectedTok TkReservedEquals *> typeDataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe)

    gadtStyleTypeDataDecl = gadtTypeDataWhereClauseParser

-- | Parse constructors for type data (traditional style, after `=`)
-- No labelled fields, no strictness annotations
typeDataConDeclParser :: TokParser DataConDecl
typeDataConDeclParser = withSpan $ do
  (_forallVars, context) <- dataConQualifiersParser
  -- Parse constructor name
  conName <- constructorIdentifierParser
  -- Parse arguments (no strictness, no records)
  args <- MP.many $ BangType noSourceSpan NoSourceUnpackedness False <$> typeAppParser
  pure $ \span' -> PrefixCon span' [] context conName args

-- | Parse GADT-style constructors for type data (after `where`)
-- No labelled fields, no strictness annotations
gadtTypeDataWhereClauseParser :: TokParser [DataConDecl]
gadtTypeDataWhereClauseParser = whereClauseItemsParser gadtTypeDataConsBracedParser gadtTypeDataConsPlainParser

gadtTypeDataConsPlainParser :: TokParser [DataConDecl]
gadtTypeDataConsPlainParser = plainSemiSep1 gadtTypeDataConDeclParser

gadtTypeDataConsBracedParser :: TokParser [DataConDecl]
gadtTypeDataConsBracedParser = bracedSemiSep gadtTypeDataConDeclParser

-- | Parse a GADT constructor for type data
-- Only equality constraints permitted, no strictness, no records
gadtTypeDataConDeclParser :: TokParser DataConDecl
gadtTypeDataConDeclParser = withSpan $ do
  -- Parse constructor names (can be multiple separated by commas)
  names <- gadtConNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  -- Parse optional forall
  forallBinders <- MP.option [] gadtForallParser
  -- Parse context (only equality constraints permitted, but we parse generally)
  context <- contextPrefixDispatchList
  -- Parse the body (prefix only for type data - no record style)
  body <- gadtTypeDataBodyParser
  pure $ \span' -> GadtCon span' forallBinders context names body

-- | Parse the body of a GADT constructor for type data
-- Only prefix style allowed (no records), no strictness annotations
gadtTypeDataBodyParser :: TokParser GadtBody
gadtTypeDataBodyParser = do
  -- Parse types separated by arrows
  -- Each component is a type application (no strictness annotations)
  firstTy <- typeAppParser
  moreArgs <- MP.many $ expectedTok TkReservedRightArrow *> typeAppParser
  -- Build list of all types
  let allTypes = firstTy : moreArgs
  -- If there's more than one type, all but last are argument types, last is result
  -- If there's only one type, it's just the result type with no arguments
  case allTypes of
    [resultTy] -> pure (GadtPrefixBody [] resultTy)
    _ ->
      let argTypes = map (BangType noSourceSpan NoSourceUnpackedness False) (init allTypes)
          resultTy = last allTypes
       in pure (GadtPrefixBody argTypes resultTy)

dataConDeclParser :: TokParser DataConDecl
dataConDeclParser = withSpan $ do
  (forallVars, context) <- dataConQualifiersParser
  MP.try (dataConRecordOrPrefixParser forallVars context) <|> dataConInfixParser forallVars context

newtypeDeclParser :: TokParser Decl
newtypeDeclParser = withSpan $ do
  expectedTok TkKeywordNewtype
  context <- contextPrefixDispatch
  (typeName, typeParams) <- typeDeclHeadParser
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  (constructor, derivingClauses) <- gadtStyleNewtypeDecl <|> traditionalStyleNewtypeDecl
  pure $ \span' ->
    DeclNewtype
      span'
      NewtypeDecl
        { newtypeDeclSpan = span',
          newtypeDeclContext = fromMaybe [] context,
          newtypeDeclName = typeName,
          newtypeDeclParams = typeParams,
          newtypeDeclConstructor = constructor,
          newtypeDeclDeriving = derivingClauses
        }
  where
    traditionalStyleNewtypeDecl = do
      constructor <- MP.optional (expectedTok TkReservedEquals *> newtypeConDeclParser)
      derivingClauses <- MP.many derivingClauseParser
      pure (constructor, derivingClauses)

    gadtStyleNewtypeDecl = do
      constructors <- gadtWhereClauseParser
      -- newtype can only have one constructor
      case constructors of
        [ctor] -> do
          derivingClauses <- MP.many derivingClauseParser
          pure (Just ctor, derivingClauses)
        _ -> fail "newtype must have exactly one constructor"

newtypeConDeclParser :: TokParser DataConDecl
newtypeConDeclParser = withSpan $ do
  (forallVars, context) <- dataConQualifiersParser
  MP.try (dataConRecordOrPrefixParser forallVars context) <|> dataConInfixParser forallVars context

-- | Parse GADT-style constructors after 'where'
gadtWhereClauseParser :: TokParser [DataConDecl]
gadtWhereClauseParser = whereClauseItemsParser gadtConsBracedParser gadtConsPlainParser

gadtConsPlainParser :: TokParser [DataConDecl]
gadtConsPlainParser = plainSemiSep1 gadtConDeclParser

gadtConsBracedParser :: TokParser [DataConDecl]
gadtConsBracedParser = bracedSemiSep gadtConDeclParser

-- | Parse a GADT constructor declaration: @Con1, Con2 :: forall a. Ctx => Type@
gadtConDeclParser :: TokParser DataConDecl
gadtConDeclParser = withSpan $ do
  -- Parse constructor names (can be multiple separated by commas)
  names <- gadtConNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  -- Parse optional forall
  forallBinders <- MP.option [] gadtForallParser
  -- Parse optional context
  context <- contextPrefixDispatchList
  -- Parse the body (record or prefix style)
  body <- gadtBodyParser
  pure $ \span' -> GadtCon span' forallBinders context names body

-- | Parse constructor name for GADT - can be regular or operator in parens
gadtConNameParser :: TokParser Text
gadtConNameParser =
  constructorIdentifierParser
    <|> parens constructorOperatorParser

-- | Parse forall in GADT context: @forall a b.@
gadtForallParser :: TokParser [TyVarBinder]
gadtForallParser = do
  expectedTok TkKeywordForall
  binders <- MP.some typeParamParser
  expectedTok (TkVarSym ".")
  pure binders

-- | Parse the body of a GADT constructor (after :: and optional forall/context)
-- Can be either prefix style: @a -> b -> T a@
-- Or record style: @{ field :: Type } -> T a@
-- Record style is distinguished by the leading @{@ token.
gadtBodyParser :: TokParser GadtBody
gadtBodyParser = gadtRecordBodyParser <|> gadtPrefixBodyParser

-- | Parse record-style GADT body: @{ field :: Type, ... } -> ResultType@
gadtRecordBodyParser :: TokParser GadtBody
gadtRecordBodyParser = do
  fields <- recordFieldsParser
  expectedTok TkReservedRightArrow
  GadtRecordBody fields <$> gadtResultTypeParser

-- | Parse prefix-style GADT body: @!Type1 -> Type2 -> ... -> ResultType@
-- Each argument can have an optional strictness annotation (!).
-- The result type is the final type in a chain of arrows.
gadtPrefixBodyParser :: TokParser GadtBody
gadtPrefixBodyParser = do
  -- Parse the first component (could be an argument with bang or the result type)
  firstBang <- gadtBangTypeParser
  -- Try to parse more arguments after ->
  moreArgs <- MP.many $ do
    expectedTok TkReservedRightArrow
    gadtBangTypeParser
  -- The last component is the result type, everything before it are arguments
  case moreArgs of
    [] ->
      -- No arrows - this is just a result type
      pure (GadtPrefixBody [] (bangType firstBang))
    _ ->
      -- Multiple components - last is result, rest are args
      let allBangs = firstBang : moreArgs
          args = init allBangs
          result = last allBangs
       in pure (GadtPrefixBody args (bangType result))

-- | Parse a potentially strict type for GADT prefix body: @!Type@ or @Type@
-- This handles strictness annotations on both simple and complex (parenthesized) types.
gadtBangTypeParser :: TokParser BangType
gadtBangTypeParser = withSpan $ do
  unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
  ty <- typeAppParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangType = ty
      }

-- | Parse the result type of a GADT constructor
-- This is a simple type application like @T a b@
gadtResultTypeParser :: TokParser Type
gadtResultTypeParser = typeParser

declContextParser :: TokParser [Type]
declContextParser = contextParserWith typeParser typeAtomParser

typeDeclHeadParser :: TokParser (Text, [TyVarBinder])
typeDeclHeadParser =
  MP.try infixDeclHeadParser <|> prefixDeclHeadParser
  where
    prefixDeclHeadParser = do
      name <- constructorIdentifierParser <|> parens operatorTextParser
      params <- MP.many typeParamParser
      pure (name, params)

    infixDeclHeadParser = do
      lhs <- typeParamParser
      op <- typeSynonymOperatorParser
      rhs <- typeParamParser
      pure (op, [lhs, rhs])

typeSynonymOperatorParser :: TokParser Text
typeSynonymOperatorParser =
  operatorTextParser <|> backtickTypeSynonymIdentifierParser
  where
    backtickTypeSynonymIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- identifierTextParser
      expectedTok TkSpecialBacktick
      pure op

typeFamilyHeadParser :: TokParser (TypeHeadForm, Type, [TyVarBinder])
typeFamilyHeadParser =
  MP.try infixHeadParser <|> prefixHeadParser
  where
    prefixHeadParser = do
      headType <- withSpan $ do
        name <- constructorIdentifierParser
        pure (\span' -> TCon span' name Unpromoted)
      params <- MP.many typeParamParser
      pure (TypeHeadPrefix, headType, params)

    infixHeadParser = do
      lhs <- typeParamParser
      op <- constructorOperatorParser
      rhs <- typeParamParser
      let lhsType = TVar (tyVarBinderSpan lhs) (tyVarBinderName lhs)
          rhsType = TVar (tyVarBinderSpan rhs) (tyVarBinderName rhs)
      headType <- withSpan $ do
        pure $ \span' ->
          TApp span' (TApp span' (TCon span' op Unpromoted) lhsType) rhsType
      pure (TypeHeadInfix, headType, [lhs, rhs])

typeFamilyLhsParser :: TokParser (TypeHeadForm, Type)
typeFamilyLhsParser = do
  lhs <- typeAtomParser
  hasInfixTail <- MP.optional (lookAhead typeInfixOperatorParser)
  case hasInfixTail of
    Just _ -> do
      rest <- typeHeadInfixTailParser
      pure (TypeHeadInfix, foldl buildInfixType lhs rest)
    Nothing -> do
      rest <- MP.many typeAtomParser
      pure (TypeHeadPrefix, foldl buildTypeApp lhs rest)
  where
    typeHeadInfixTailParser = MP.many $ MP.try $ do
      op <- typeInfixOperatorParser
      atom <- typeAtomParser
      pure (op, atom)

    buildInfixType left ((op, promoted), right) =
      let span' = mergeSourceSpans (getSourceSpan left) (getSourceSpan right)
          opType = TCon span' op promoted
       in TApp span' (TApp span' opType left) right

    buildTypeApp left right =
      TApp (mergeSourceSpans (getSourceSpan left) (getSourceSpan right)) left right

classHeadParser :: TokParser (TypeHeadForm, Text, [TyVarBinder])
classHeadParser =
  MP.try infixDeclHeadParser <|> prefixDeclHeadParser
  where
    prefixDeclHeadParser = do
      name <- constructorIdentifierParser
      params <- MP.many typeParamParser
      pure (TypeHeadPrefix, name, params)

    infixDeclHeadParser = do
      lhs <- typeParamParser
      op <- constructorOperatorParser
      rhs <- typeParamParser
      pure (TypeHeadInfix, op, [lhs, rhs])

typeParamParser :: TokParser TyVarBinder
typeParamParser =
  withSpan $
    ( do
        ident <-
          tokenSatisfy "type parameter binder" $ \tok ->
            case lexTokenKind tok of
              TkVarId name
                | isTypeVarName name ->
                    Just name
              _ -> Nothing
        pure (\span' -> TyVarBinder span' ident Nothing)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- lowerIdentifierParser
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder span' ident (Just kind))
          )

isTypeVarName :: Text -> Bool
isTypeVarName name =
  case T.uncons name of
    Just (c, _) -> c == '_' || isAsciiLower c
    Nothing -> False

derivingClauseParser :: TokParser DerivingClause
derivingClauseParser = do
  expectedTok TkKeywordDeriving
  strategy <- MP.optional derivingStrategyParser
  classes <- parenClasses <|> singleClass
  viaTy <- MP.optional derivingViaTypeParser
  pure (DerivingClause strategy classes viaTy)
  where
    singleClass = (: []) <$> contextItemParserWith typeParser typeAtomParser
    parenClasses = parens $ contextItemParserWith typeParser typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma

derivingViaTypeParser :: TokParser Type
derivingViaTypeParser = do
  varIdTok "via"
  typeParser

derivingStrategyParser :: TokParser DerivingStrategy
derivingStrategyParser =
  (varIdTok "stock" >> pure DerivingStock)
    <|> (expectedTok TkKeywordNewtype >> pure DerivingNewtype)
    <|> (varIdTok "anyclass" >> pure DerivingAnyclass)

dataConQualifiersParser :: TokParser ([Text], [Type])
dataConQualifiersParser = do
  mForall <- forallPrefixDispatch forallBindersParser
  mContext <- contextPrefixDispatchList
  pure (mForall, mContext)

forallBindersParser :: TokParser [Text]
forallBindersParser = do
  expectedTok TkKeywordForall
  binders <- MP.some typeParamParser
  expectedTok (TkVarSym ".")
  pure (map tyVarBinderName binders)

dataConRecordOrPrefixParser :: [Text] -> [Type] -> TokParser (SourceSpan -> DataConDecl)
dataConRecordOrPrefixParser forallVars context = do
  name <- constructorNameParser
  mRecordFields <- MP.optional (MP.try recordFieldsParserAfterLayoutSemicolon)
  case mRecordFields of
    Just fields -> pure (\span' -> RecordCon span' forallVars context name fields)
    Nothing -> do
      args <- MP.many constructorArgParser
      -- Ensure we're not leaving a constructor operator unconsumed.
      -- If there's a constructor operator next, this is actually an infix form
      -- and we should backtrack to let dataConInfixParser handle it.
      MP.notFollowedBy constructorOperatorParser
      pure (\span' -> PrefixCon span' forallVars context name args)
  where
    -- Layout may inject a virtual ';' before a newline-started record field block.
    -- Accept it as part of the constructor declaration.
    recordFieldsParserAfterLayoutSemicolon =
      recordFieldsParser
        <|> (expectedTok TkSpecialSemicolon *> recordFieldsParser)

dataConInfixParser :: [Text] -> [Type] -> TokParser (SourceSpan -> DataConDecl)
dataConInfixParser forallVars context = do
  lhs <- infixConstructorArgParser
  op <- constructorOperatorParser
  rhs <- infixConstructorArgParser
  pure (\span' -> InfixCon span' forallVars context lhs op rhs)

recordFieldsParser :: TokParser [FieldDecl]
recordFieldsParser = braces (recordFieldDeclParser `MP.sepEndBy` expectedTok TkSpecialComma)

recordFieldDeclParser :: TokParser FieldDecl
recordFieldDeclParser = withSpan $ do
  names <- identifierTextParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  fieldTy <- recordFieldBangTypeParser
  pure $ \span' ->
    FieldDecl
      { fieldSpan = span',
        fieldNames = names,
        fieldType = fieldTy
      }

constructorArgParser :: TokParser BangType
constructorArgParser = MP.try $ do
  MP.notFollowedBy derivingKeywordParser
  bangTypeParser

infixConstructorArgParser :: TokParser BangType
infixConstructorArgParser = MP.try $ do
  MP.notFollowedBy derivingKeywordParser
  withSpan $ do
    unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
    strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
    ty <- typeAppParser
    pure $ \span' ->
      BangType
        { bangSpan = span',
          bangSourceUnpackedness = unpackedness,
          bangStrict = strict,
          bangType = ty
        }

derivingKeywordParser :: TokParser ()
derivingKeywordParser =
  tokenSatisfy "identifier \"deriving\"" $ \tok ->
    case lexTokenKind tok of
      TkKeywordDeriving -> Just ()
      _ -> Nothing

bangTypeParser :: TokParser BangType
bangTypeParser = withSpan $ do
  unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
  ty <- typeAtomParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangType = ty
      }

recordFieldBangTypeParser :: TokParser BangType
recordFieldBangTypeParser = withSpan $ do
  unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
  ty <- constructorFieldTypeParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangType = ty
      }

sourceUnpackednessPragmaParser :: TokParser SourceUnpackedness
sourceUnpackednessPragmaParser =
  hiddenPragma "source unpack pragma" $ \case
    PragmaUnpack UnpackPragma -> Just SourceUnpack
    PragmaUnpack NoUnpackPragma -> Just SourceNoUnpack
    _ -> Nothing

-- | Parse a type in a constructor field position.
-- This supports function types (Int -> Int) and type applications (Maybe Int).
constructorFieldTypeParser :: TokParser Type
constructorFieldTypeParser = typeParser

constructorNameParser :: TokParser Text
constructorNameParser = constructorIdentifierParser

constructorOperatorParser :: TokParser Text
constructorOperatorParser =
  symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser
  where
    symbolicConstructorOperatorParser =
      tokenSatisfy "constructor operator" $ \tok ->
        case lexTokenKind tok of
          TkConSym op -> Just op
          TkQConSym op -> Just op
          TkReservedColon -> Just ":"
          _ -> Nothing
    backtickConstructorIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- constructorIdentifierParser
      expectedTok TkSpecialBacktick
      pure op

-- | Parse a pattern binding declaration like @(x, y) = (1, 2)@.
-- This handles bindings where the LHS is a pattern rather than a function name.
patternBindDeclParser :: TokParser Decl
patternBindDeclParser = withSpan $ do
  pat <- region "while parsing pattern binding" patternParser
  rhs <- equationRhsParser
  pure (\span' -> DeclValue span' (PatternBind span' pat rhs))

valueDeclParser :: TokParser Decl
valueDeclParser = withSpan $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  rhs <- equationRhsParser
  pure (\span' -> functionBindDecl span' headForm name pats rhs)

-- ---------------------------------------------------------------------------
-- Pattern synonyms

-- | Parse a pattern synonym declaration or signature.
-- Dispatches between @pattern Name :: Type@ (signature) and
-- @pattern Name args = pat@ / @pattern Name args <- pat [where ...]@ (declaration).
patternSynonymParser :: TokParser Decl
patternSynonymParser =
  MP.try patternSynonymSigDeclParser <|> patternSynonymDeclParser

-- | Parse a pattern synonym type signature: @pattern Name1, Name2 :: Type@
patternSynonymSigDeclParser :: TokParser Decl
patternSynonymSigDeclParser = withSpan $ do
  expectedTok TkVarPattern
  names <- patSynNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> DeclPatSynSig span' names ty)

-- | Parse a pattern synonym name (constructor identifier or parenthesized operator).
patSynNameParser :: TokParser Text
patSynNameParser =
  constructorIdentifierParser <|> parens constructorOperatorParser

-- | Parse a pattern synonym declaration.
-- Handles prefix, infix, and record forms with all three directionalities.
patternSynonymDeclParser :: TokParser Decl
patternSynonymDeclParser = withSpan $ do
  expectedTok TkVarPattern
  (name, args) <- patSynLhsParser
  (dir, pat) <- patSynDirAndPatParser name
  pure $ \span' ->
    DeclPatSyn
      span'
      PatSynDecl
        { patSynDeclSpan = span',
          patSynDeclName = name,
          patSynDeclArgs = args,
          patSynDeclPat = pat,
          patSynDeclDir = dir
        }

-- | Parse the LHS of a pattern synonym declaration.
-- Returns the name and the argument form.
patSynLhsParser :: TokParser (Text, PatSynArgs)
patSynLhsParser =
  MP.try patSynInfixLhsParser <|> patSynRecordOrPrefixLhsParser

-- | Parse an infix pattern synonym LHS: @var ConOp var@ or @var \`Con\` var@
patSynInfixLhsParser :: TokParser (Text, PatSynArgs)
patSynInfixLhsParser = do
  lhs <- lowerIdentifierParser
  op <- constructorOperatorParser
  rhs <- lowerIdentifierParser
  pure (op, PatSynInfixArgs lhs rhs)

-- | Parse a record or prefix pattern synonym LHS.
-- Record: @Con {field1, field2, ...}@
-- Prefix: @Con var1 var2 ...@
patSynRecordOrPrefixLhsParser :: TokParser (Text, PatSynArgs)
patSynRecordOrPrefixLhsParser = do
  name <- patSynNameParser
  mFields <- MP.optional (MP.try patSynRecordFieldsParser)
  case mFields of
    Just fields -> pure (name, PatSynRecordArgs fields)
    Nothing -> do
      args <- MP.many lowerIdentifierParser
      pure (name, PatSynPrefixArgs args)

-- | Parse the record fields of a pattern synonym: @{field1, field2, ...}@
patSynRecordFieldsParser :: TokParser [Text]
patSynRecordFieldsParser = braces (lowerIdentifierParser `MP.sepEndBy` expectedTok TkSpecialComma)

-- | Parse the direction marker and RHS pattern of a pattern synonym.
patSynDirAndPatParser :: Text -> TokParser (PatSynDir, Pattern)
patSynDirAndPatParser name =
  ( do
      expectedTok TkReservedEquals
      pat <- patternParser
      pure (PatSynBidirectional, pat)
  )
    <|> ( do
            expectedTok TkReservedLeftArrow
            pat <- patternParser
            mMatches <- MP.optional (patSynWhereClauseParser name)
            case mMatches of
              Nothing -> pure (PatSynUnidirectional, pat)
              Just matches -> pure (PatSynExplicitBidirectional matches, pat)
        )
    <|> fail "expected '=' or '<-' in pattern synonym declaration"

-- | Parse the where clause of an explicitly bidirectional pattern synonym.
-- @where { Name pats = expr; ... }@
patSynWhereClauseParser :: Text -> TokParser [Match]
patSynWhereClauseParser _name =
  whereClauseItemsParser
    (bracedSemiSep patSynWhereMatch)
    (plainSemiSep1 patSynWhereMatch)

-- | Parse one equation in a pattern synonym where clause.
patSynWhereMatch :: TokParser Match
patSynWhereMatch = withSpan $ do
  (headForm, _name, pats) <- functionHeadParserWith patternParser simplePatternParser
  rhs <- equationRhsParser
  pure $ \span' ->
    Match
      { matchSpan = span',
        matchHeadForm = headForm,
        matchPats = pats,
        matchRhs = rhs
      }
