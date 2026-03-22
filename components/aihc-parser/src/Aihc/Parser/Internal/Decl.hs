{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Decl
  ( declParser,
    importDeclParser,
    moduleHeaderParser,
    languagePragmaParser,
  )
where

import Aihc.Lexer (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Ast
import Aihc.Parser.Internal.Common
import Aihc.Parser.Internal.Expr (equationRhsParser, exprParser, simplePatternParser, typeAtomParser, typeParser)
import Control.Monad (when)
import Data.Char (isAsciiLower, isUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import qualified Text.Megaparsec as MP

languagePragmaParser :: TokParser [ExtensionSetting]
languagePragmaParser =
  tokenSatisfy "LANGUAGE pragma" $ \tok ->
    case lexTokenKind tok of
      TkPragmaLanguage names -> Just names
      _ -> Nothing

moduleHeaderParser :: TokParser (Text, Maybe WarningText, Maybe [ExportSpec])
moduleHeaderParser = do
  keywordTok TkKeywordModule
  name <- moduleNameParser
  mWarning <- MP.optional warningTextParser
  exports <- MP.optional exportSpecListParser
  keywordTok TkKeywordWhere
  pure (name, mWarning, exports)

warningTextParser :: TokParser WarningText
warningTextParser =
  withSpan $
    tokenSatisfy "warning pragma" $ \tok ->
      case lexTokenKind tok of
        TkPragmaWarning msg -> Just (`WarnText` msg)
        TkPragmaDeprecated msg -> Just (`DeprText` msg)
        _ -> Nothing

exportSpecListParser :: TokParser [ExportSpec]
exportSpecListParser = parens $ exportSpecParser `MP.sepEndBy` expectedTok TkSpecialComma

exportSpecParser :: TokParser ExportSpec
exportSpecParser =
  withSpan $
    MP.try exportModuleParser <|> exportNameParser

exportModuleParser :: TokParser (SourceSpan -> ExportSpec)
exportModuleParser = do
  keywordTok TkKeywordModule
  modName <- moduleNameParser
  pure (`ExportModule` modName)

exportNameParser :: TokParser (SourceSpan -> ExportSpec)
exportNameParser = do
  namespace <- MP.optional exportImportNamespaceParser
  name <- identifierTextParser <|> parens operatorTextParser
  members <- MP.optional exportMembersParser
  pure $ \span' ->
    case members of
      Just Nothing -> ExportAll span' namespace name
      Just (Just names) -> ExportWith span' namespace name names
      Nothing
        | namespace == Just "type" || isTypeName name -> ExportAbs span' namespace name
        | otherwise -> ExportVar span' namespace name

exportMembersParser :: TokParser (Maybe [Text])
exportMembersParser =
  parens $
    (expectedTok TkReservedDotDot >> pure Nothing)
      <|> (Just <$> (memberNameParser `MP.sepEndBy` expectedTok TkSpecialComma))
  where
    memberNameParser = identifierTextParser <|> parens operatorTextParser

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
  keywordTok TkKeywordImport
  preQualified <-
    MP.option False (keywordTok TkKeywordQualified >> pure True)
  importedLevel <- MP.optional importLevelParser
  importedPackage <- MP.optional packageNameParser
  importedModule <- moduleNameParser
  postQualified <-
    MP.option False (keywordTok TkKeywordQualified >> pure True)
  when (preQualified && postQualified) $
    fail "import declaration cannot contain 'qualified' both before and after the module name"
  importAlias <- MP.optional (keywordTok TkKeywordAs *> moduleNameParser)
  importSpec <- MP.optional importSpecParser
  let isQualified = preQualified || postQualified
  pure $ \span' ->
    ImportDecl
      { importDeclSpan = span',
        importDeclLevel = importedLevel,
        importDeclPackage = importedPackage,
        importDeclQualified = isQualified,
        importDeclQualifiedPost = postQualified,
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
    MP.option False (keywordTok TkKeywordHiding >> pure True)
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
        | effectiveNamespace == Just "type" || isTypeName itemName -> ImportItemAbs span' effectiveNamespace itemName
        | otherwise -> ImportItemVar span' effectiveNamespace itemName

importOperatorParser :: TokParser Text
importOperatorParser = operatorTextParser

exportImportNamespaceParser :: TokParser Text
exportImportNamespaceParser =
  keywordTok TkKeywordType >> pure "type"

declParser :: TokParser Decl
declParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordData -> dataDeclParser
    TkKeywordClass -> classDeclParser
    TkKeywordDefault -> defaultDeclParser
    TkKeywordForeign -> foreignDeclParser
    TkKeywordInfix -> fixityDeclParser Infix
    TkKeywordInfixl -> fixityDeclParser InfixL
    TkKeywordInfixr -> fixityDeclParser InfixR
    TkKeywordInstance -> instanceDeclParser
    TkKeywordNewtype -> newtypeDeclParser
    TkKeywordType -> MP.try standaloneKindSigDeclParser <|> typeSynDeclParser
    TkVarId ident ->
      case ident of
        "pattern" -> unsupportedDeclParser "pattern synonym declarations are not implemented yet"
        _ -> MP.try typeSigDeclParser <|> valueDeclParser
    TkConId ident ->
      case ident of
        "pattern" -> unsupportedDeclParser "pattern synonym declarations are not implemented yet"
        _ -> MP.try typeSigDeclParser <|> valueDeclParser
    _ -> MP.try typeSigDeclParser <|> valueDeclParser

standaloneKindSigDeclParser :: TokParser Decl
standaloneKindSigDeclParser = withSpan $ do
  keywordTok TkKeywordType
  typeName <- constructorIdentifierParser
  expectedTok TkReservedDoubleColon
  kind <- typeParser
  pure (\span' -> DeclStandaloneKindSig span' typeName kind)

typeSynDeclParser :: TokParser Decl
typeSynDeclParser = withSpan $ do
  keywordTok TkKeywordType
  typeName <- constructorIdentifierParser
  typeParams <- MP.many typeParamParser
  expectedTok TkReservedEquals
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

typeSigDeclParser :: TokParser Decl
typeSigDeclParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> DeclTypeSig span' names ty)

defaultDeclParser :: TokParser Decl
defaultDeclParser = withSpan $ do
  keywordTok TkKeywordDefault
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
  (keywordTok TkKeywordInfix >> pure Infix)
    <|> (keywordTok TkKeywordInfixl >> pure InfixL)
    <|> (keywordTok TkKeywordInfixr >> pure InfixR)

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
  keywordTok TkKeywordClass
  context <- MP.optional (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow))
  className <- constructorIdentifierParser
  classParams <- MP.some typeParamParser
  items <- MP.option [] classWhereClauseParser
  pure $ \span' ->
    DeclClass
      span'
      ClassDecl
        { classDeclSpan = span',
          classDeclContext = fromMaybe [] context,
          classDeclName = className,
          classDeclParams = classParams,
          classDeclItems = items
        }

classWhereClauseParser :: TokParser [ClassDeclItem]
classWhereClauseParser = whereClauseItemsParser classItemsBracedParser classItemsPlainParser

whereClauseItemsParser :: TokParser [item] -> TokParser [item] -> TokParser [item]
whereClauseItemsParser bracedParser plainParser = do
  keywordTok TkKeywordWhere
  bracedParser <|> plainParser <|> pure []

classItemsPlainParser :: TokParser [ClassDeclItem]
classItemsPlainParser = plainSemiSep1 classDeclItemParser

classItemsBracedParser :: TokParser [ClassDeclItem]
classItemsBracedParser = bracedSemiSep classDeclItemParser

classDeclItemParser :: TokParser ClassDeclItem
classDeclItemParser = MP.try classFixityItemParser <|> MP.try classTypeSigItemParser <|> classDefaultItemParser

classTypeSigItemParser :: TokParser ClassDeclItem
classTypeSigItemParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (\span' -> ClassItemTypeSig span' names ty)

classFixityItemParser :: TokParser ClassDeclItem
classFixityItemParser = withSpan $ do
  (assoc, prec, ops) <- fixityDeclPartsParser
  pure (\span' -> ClassItemFixity span' assoc prec ops)

classDefaultItemParser :: TokParser ClassDeclItem
classDefaultItemParser = withSpan $ do
  name <- binderNameParser
  pats <- MP.many simplePatternParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  pure (\span' -> ClassItemDefault span' (functionBindValue span' name pats (UnguardedRhs span' rhsExpr)))

instanceDeclParser :: TokParser Decl
instanceDeclParser = withSpan $ do
  keywordTok TkKeywordInstance
  context <- MP.optional (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow))
  className <- constructorIdentifierParser
  instanceTypes <- MP.some typeAtomParser
  items <- MP.option [] instanceWhereClauseParser
  pure $ \span' ->
    DeclInstance
      span'
      InstanceDecl
        { instanceDeclSpan = span',
          instanceDeclContext = fromMaybe [] context,
          instanceDeclClassName = className,
          instanceDeclTypes = instanceTypes,
          instanceDeclItems = items
        }

instanceWhereClauseParser :: TokParser [InstanceDeclItem]
instanceWhereClauseParser = whereClauseItemsParser instanceItemsBracedParser instanceItemsPlainParser

instanceItemsPlainParser :: TokParser [InstanceDeclItem]
instanceItemsPlainParser = plainSemiSep1 instanceDeclItemParser

instanceItemsBracedParser :: TokParser [InstanceDeclItem]
instanceItemsBracedParser = bracedSemiSep instanceDeclItemParser

instanceDeclItemParser :: TokParser InstanceDeclItem
instanceDeclItemParser = MP.try instanceFixityItemParser <|> MP.try instanceTypeSigItemParser <|> instanceValueItemParser

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
  name <- binderNameParser
  pats <- MP.many simplePatternParser
  expectedTok TkReservedEquals
  rhsExpr <- exprParser
  pure (\span' -> InstanceItemBind span' (functionBindValue span' name pats (UnguardedRhs span' rhsExpr)))

foreignDeclParser :: TokParser Decl
foreignDeclParser = withSpan $ do
  keywordTok TkKeywordForeign
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
  (keywordTok TkKeywordImport >> pure ForeignImport)
    <|> (varIdTok "export" >> pure ForeignExport)

callConvParser :: TokParser CallConv
callConvParser =
  (varIdTok "ccall" >> pure CCall)
    <|> (varIdTok "stdcall" >> pure StdCall)

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
  keywordTok TkKeywordData
  context <- MP.optional (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow))
  typeName <- constructorIdentifierParser
  typeParams <- MP.many typeParamParser
  constructors <- MP.optional (expectedTok TkReservedEquals *> dataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe)
  derivingClauses <- MP.many derivingClauseParser
  pure $ \span' ->
    DeclData
      span'
      DataDecl
        { dataDeclSpan = span',
          dataDeclContext = fromMaybe [] context,
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclConstructors = fromMaybe [] constructors,
          dataDeclDeriving = derivingClauses
        }

dataConDeclParser :: TokParser DataConDecl
dataConDeclParser = withSpan $ do
  (forallVars, context) <- dataConQualifiersParser
  MP.try (dataConRecordOrPrefixParser forallVars context) <|> dataConInfixParser forallVars context

newtypeDeclParser :: TokParser Decl
newtypeDeclParser = withSpan $ do
  keywordTok TkKeywordNewtype
  context <- MP.optional (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow))
  typeName <- constructorIdentifierParser
  typeParams <- MP.many typeParamParser
  constructor <- MP.optional (expectedTok TkReservedEquals *> newtypeConDeclParser)
  derivingClauses <- MP.many derivingClauseParser
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

newtypeConDeclParser :: TokParser DataConDecl
newtypeConDeclParser = withSpan $ do
  (forallVars, context) <- dataConQualifiersParser
  MP.try (dataConRecordOrPrefixParser forallVars context) <|> dataConInfixParser forallVars context

declContextParser :: TokParser [Constraint]
declContextParser = contextParserWith typeAtomParser

typeParamParser :: TokParser TyVarBinder
typeParamParser =
  withSpan $
    ( do
        ident <-
          tokenSatisfy "type parameter binder" $ \tok ->
            case lexTokenKind tok of
              TkVarId name
                | name /= "deriving",
                  isTypeVarName name ->
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
  keywordTok TkKeywordDeriving
  strategy <- MP.optional derivingStrategyParser
  classes <- parenClasses <|> singleClass
  pure (DerivingClause strategy classes)
  where
    singleClass = (: []) <$> identifierTextParser
    parenClasses = parens $ identifierTextParser `MP.sepEndBy` expectedTok TkSpecialComma

derivingStrategyParser :: TokParser DerivingStrategy
derivingStrategyParser =
  (varIdTok "stock" >> pure DerivingStock)
    <|> (keywordTok TkKeywordNewtype >> pure DerivingNewtype)
    <|> (varIdTok "anyclass" >> pure DerivingAnyclass)

dataConQualifiersParser :: TokParser ([Text], [Constraint])
dataConQualifiersParser = do
  mForall <- MP.optional (MP.try forallBindersParser)
  mContext <- MP.optional (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow))
  pure (fromMaybe [] mForall, fromMaybe [] mContext)

forallBindersParser :: TokParser [Text]
forallBindersParser = do
  varIdTok "forall"
  binders <- MP.some typeParamParser
  expectedTok (TkVarSym ".")
  pure (map tyVarBinderName binders)

dataConRecordOrPrefixParser :: [Text] -> [Constraint] -> TokParser (SourceSpan -> DataConDecl)
dataConRecordOrPrefixParser forallVars context = do
  name <- constructorNameParser
  mRecordFields <- MP.optional (MP.try recordFieldsParserAfterLayoutSemicolon)
  case mRecordFields of
    Just fields -> pure (\span' -> RecordCon span' forallVars context name fields)
    Nothing -> do
      args <- MP.many constructorArgParser
      pure (\span' -> PrefixCon span' forallVars context name args)
  where
    -- Layout may inject a virtual ';' before a newline-started record field block.
    -- Accept it as part of the constructor declaration.
    recordFieldsParserAfterLayoutSemicolon =
      recordFieldsParser
        <|> (expectedTok TkSpecialSemicolon *> recordFieldsParser)

dataConInfixParser :: [Text] -> [Constraint] -> TokParser (SourceSpan -> DataConDecl)
dataConInfixParser forallVars context = do
  lhs <- constructorArgParser
  op <- constructorOperatorParser
  rhs <- constructorArgParser
  pure (\span' -> InfixCon span' forallVars context lhs op rhs)

recordFieldsParser :: TokParser [FieldDecl]
recordFieldsParser = do
  expectedTok TkSpecialLBrace
  fields <- recordFieldDeclParser `MP.sepEndBy` expectedTok TkSpecialComma
  expectedTok TkSpecialRBrace
  pure fields

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

derivingKeywordParser :: TokParser ()
derivingKeywordParser =
  tokenSatisfy "identifier \"deriving\"" $ \tok ->
    case lexTokenKind tok of
      TkKeywordDeriving -> Just ()
      _ -> Nothing

bangTypeParser :: TokParser BangType
bangTypeParser = withSpan $ do
  strict <- MP.option False (expectedTok (TkVarSym "!") >> pure True)
  ty <- typeAtomParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangStrict = strict,
        bangType = ty
      }

recordFieldBangTypeParser :: TokParser BangType
recordFieldBangTypeParser = withSpan $ do
  strict <- MP.option False (expectedTok (TkVarSym "!") >> pure True)
  ty <- constructorFieldTypeParser
  pure $ \span' ->
    BangType
      { bangSpan = span',
        bangStrict = strict,
        bangType = ty
      }

constructorFieldTypeParser :: TokParser Type
constructorFieldTypeParser = do
  first <- typeAtomParser
  rest <- MP.many typeAtomParser
  pure (foldl appendTypeArg first rest)
  where
    appendTypeArg lhs rhs =
      TApp (mergeSourceSpans (getSourceSpan lhs) (getSourceSpan rhs)) lhs rhs

constructorNameParser :: TokParser Text
constructorNameParser = constructorIdentifierParser

constructorOperatorParser :: TokParser Text
constructorOperatorParser =
  tokenSatisfy "constructor operator" $ \tok ->
    case lexTokenKind tok of
      TkConSym op -> Just op
      TkQConSym op -> Just op
      TkReservedColon -> Just ":"
      _ -> Nothing

unsupportedDeclParser :: String -> TokParser Decl
unsupportedDeclParser = fail

valueDeclParser :: TokParser Decl
valueDeclParser = withSpan $ do
  name <- binderNameParser
  pats <- MP.many simplePatternParser
  rhs <- equationRhsParser
  pure (\span' -> functionBindDecl span' name pats rhs)
