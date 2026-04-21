{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Internal.Decl
  ( declParser,
    fixityDeclParser,
    pragmaDeclParser,
  )
where

import Aihc.Parser.Internal.Common
import {-# SOURCE #-} Aihc.Parser.Internal.Expr (equationRhsParser, exprParser)
import Aihc.Parser.Internal.Import (warningTextParser)
import Aihc.Parser.Internal.Pattern (patternParser, simplePatternParser)
import Aihc.Parser.Internal.Type (forallTelescopeParser, typeAppParser, typeAtomParser, typeInfixOperatorParser, typeInfixParser, typeParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind, pattern TkVarFamily, pattern TkVarRole)
import Aihc.Parser.Syntax
import Control.Monad (when)
import Data.Char (isLower)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP

instanceOverlapPragmaParser :: TokParser InstanceOverlapPragma
instanceOverlapPragmaParser =
  hiddenPragma "instance overlap pragma" $ \case
    PragmaInstanceOverlap overlapPragma -> Just overlapPragma
    _ -> Nothing

anyPragmaParser :: String -> TokParser Pragma
anyPragmaParser expectedLabel = hiddenPragma expectedLabel Just

declParser :: TokParser Decl
declParser = do
  mPragmaDecl <- MP.optional pragmaDeclParser
  maybe ordinaryDeclParser pure mPragmaDecl

ordinaryDeclParser :: TokParser Decl
ordinaryDeclParser = do
  (tok, nextTok) <- lookAhead ((,) <$> anySingle <*> anySingle)
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  let thAny = thEnabled || thFullEnabled
  let tokKind = lexTokenKind tok
      nextTokKind = lexTokenKind nextTok
      valueOrSpliceParser =
        if thAny
          then MP.try valueDeclParser <|> implicitSpliceDeclParser
          else valueDeclParser
      patternOrValueOrSpliceParser =
        if thAny
          then MP.try patternBindDeclParser <|> MP.try valueDeclParser <|> implicitSpliceDeclParser
          else MP.try patternBindDeclParser <|> valueDeclParser
      nonBareVarPatternOrValueOrSpliceParser =
        if thAny
          then MP.try nonBareVarPatternBindDeclParser <|> MP.try valueDeclParser <|> implicitSpliceDeclParser
          else MP.try nonBareVarPatternBindDeclParser <|> valueDeclParser
      typeSigOrValueOrSpliceParser =
        MP.try typeSigDeclParser <|> valueOrSpliceParser
      typeSigOrPatternOrValueOrSpliceParser =
        MP.try typeSigDeclParser <|> patternOrValueOrSpliceParser
  case tokKind of
    TkKeywordData ->
      case nextTokKind of
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
      | nextTokKind == TkKeywordInstance -> newtypeFamilyInstParser
    TkKeywordNewtype -> newtypeDeclParser
    TkKeywordType ->
      case nextTokKind of
        TkVarRole -> roleAnnotationDeclParser
        TkVarFamily -> typeFamilyDeclParser
        TkKeywordData -> typeDataDeclParser
        TkKeywordInstance -> typeFamilyInstParser
        _ -> typeDeclarationParser
    TkKeywordPattern -> patternSynonymParser
    TkVarId {} ->
      case nextTokKind of
        TkReservedDoubleColon -> typeSigOrValueOrSpliceParser
        TkSpecialComma -> typeSigOrValueOrSpliceParser
        TkReservedEquals -> valueOrSpliceParser
        _ -> nonBareVarPatternOrValueOrSpliceParser
    TkTHSplice ->
      if thAny
        then MP.try patternBindDeclParser <|> MP.try valueDeclParser <|> spliceDeclParser
        else spliceDeclParser
    _ -> typeSigOrPatternOrValueOrSpliceParser

-- | Like 'patternBindDeclParser' but rejects bare variable patterns.
-- When the leading token is a variable identifier, a bare @x = 5@ must be
-- parsed as a zero-argument function bind, not a pattern bind.  This parser
-- detects that case early (after parsing the pattern) and fails, letting
-- 'valueDeclParser' handle it instead.
nonBareVarPatternBindDeclParser :: TokParser Decl
nonBareVarPatternBindDeclParser = MP.try $ withSpanAnn (DeclAnn . mkAnnotation) $ do
  pat <- region "while parsing pattern binding" patternParser
  case pat of
    PVar {} -> fail "bare variable bindings are parsed as function declarations"
    _ -> do
      DeclValue . PatternBind pat <$> equationRhsParser

-- | Parse a pragma declaration (e.g. {-# INLINE f #-}, {-# SPECIALIZE ... #-})
pragmaDeclParser :: TokParser Decl
pragmaDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ DeclPragma <$> anyPragmaParser "pragma declaration"

-- | Parse a top-level Template Haskell declaration splice: $expr or $(expr)
spliceDeclParser :: TokParser Decl
spliceDeclParser = do
  expectedTok TkTHSplice
  DeclSplice <$> exprParser

-- | Parse an implicit top-level Template Haskell declaration splice: @expr@.
-- GHC accepts bare declaration splices under TemplateHaskell and also pretty-prints
-- them as explicit @$...@ splices, so we parse the expression body directly here.
implicitSpliceDeclParser :: TokParser Decl
implicitSpliceDeclParser = DeclSplice <$> exprParser

-- | Parse a @type@ declaration after the @type@ keyword has been consumed.
-- Uses 'typeDeclHeadParser' to handle both prefix and infix type heads,
-- then dispatches based on the next token:
-- - @::@ → standalone kind signature (must have zero type parameters)
-- - @=@ → type synonym
typeDeclarationParser :: TokParser Decl
typeDeclarationParser = do
  expectedTok TkKeywordType
  (headForm, typeName, typeParams) <- typeDeclHeadParser
  nextTok <- anySingle
  case lexTokenKind nextTok of
    TkReservedDoubleColon -> do
      -- Standalone kind signature: cannot have type parameters
      if null typeParams
        then
          DeclStandaloneKindSig typeName <$> typeParser
        else
          fail "Standalone kind signatures cannot have type parameters."
    TkReservedEquals -> do
      body <- typeParser
      pure $
        DeclTypeSyn
          TypeSynDecl
            { typeSynHeadForm = headForm,
              typeSynName = typeName,
              typeSynParams = typeParams,
              typeSynBody = body
            }
    _ ->
      fail "expected '::' or '=' after type declaration head"

roleAnnotationDeclParser :: TokParser Decl
roleAnnotationDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  expectedTok TkVarRole
  typeName <- constructorUnqualifiedNameParser <|> parens constructorOperatorUnqualifiedNameParser
  roles <- MP.many roleParser
  pure $
    DeclRoleAnnotation
      RoleAnnotation
        { roleAnnotationName = typeName,
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
  binders <- MP.some explicitForallBinderParser
  expectedTok (TkVarSym ".")
  pure binders

-- | Parse an optional explicit forall for instance heads.
-- Handles @forall a (b :: Kind).@ syntax.
instanceForallParser :: TokParser [TyVarBinder]
instanceForallParser = do
  expectedTok TkKeywordForall
  binders <- MP.some explicitForallBinderParser
  expectedTok (TkVarSym ".")
  pure binders

-- | Parse an optional unnamed @:: Kind@ result signature for a family head.
familyResultKindParser :: TokParser (Maybe Type)
familyResultKindParser =
  MP.optional (expectedTok TkReservedDoubleColon *> typeParser)

-- | Parse an optional type family result signature. GHC admits either an unnamed
-- @:: Kind@ annotation or a named result variable with optional injectivity annotation,
-- such as @= r@, @= r | r -> a@, or @= (r :: Type) | r -> a where ...@.
typeFamilyResultSigParser :: TokParser (Maybe TypeFamilyResultSig)
typeFamilyResultSigParser =
  MP.optional (kindSigParser <|> namedSigParser)
  where
    kindSigParser =
      TypeFamilyKindSig <$> (expectedTok TkReservedDoubleColon *> typeParser)

    namedSigParser = do
      expectedTok TkReservedEquals
      result <- namedResultBinderParser
      mInjectivity <- MP.optional typeFamilyInjectivityParser
      pure $ case mInjectivity of
        Just injectivity -> TypeFamilyInjectiveSig result injectivity
        Nothing -> TypeFamilyTyVarSig result

    namedResultBinderParser =
      withSpan $
        ( do
            ident <- lowerIdentifierParser
            pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)
        )
          <|> ( do
                  expectedTok TkSpecialLParen
                  ident <- lowerIdentifierParser
                  expectedTok TkReservedDoubleColon
                  kind <- typeParser
                  expectedTok TkSpecialRParen
                  pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)
              )

typeFamilyInjectivityParser :: TokParser TypeFamilyInjectivity
typeFamilyInjectivityParser = withSpan $ do
  expectedTok TkReservedPipe
  result <- lowerIdentifierParser
  expectedTok TkReservedRightArrow
  determined <- MP.some lowerIdentifierParser
  pure $ \span' ->
    TypeFamilyInjectivity
      { typeFamilyInjectivityAnns = [mkAnnotation span'],
        typeFamilyInjectivityResult = result,
        typeFamilyInjectivityDetermined = determined
      }

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level type family declaration

-- | Parse @type family Name params [:: Kind] [where { equations }]@
typeFamilyDeclParser :: TokParser Decl
typeFamilyDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  tf <- typeFamilyDeclBodyParser FamilyKeywordRequired
  pure $
    DeclTypeFamilyDecl tf

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
      { typeFamilyEqAnns = [mkAnnotation span'],
        typeFamilyEqForall = forallBinders,
        typeFamilyEqHeadForm = headForm,
        typeFamilyEqLhs = lhs,
        typeFamilyEqRhs = rhs
      }

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level data family declaration

-- | Parse @data family Name params [:: Kind]@
dataFamilyDeclParser :: TokParser Decl
dataFamilyDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordData
  varIdTok "family"
  (headForm, name, params) <- typeDeclHeadParser
  kind <- familyResultKindParser
  pure $
    DeclDataFamilyDecl
      DataFamilyDecl
        { dataFamilyDeclHeadForm = headForm,
          dataFamilyDeclName = name,
          dataFamilyDeclParams = params,
          dataFamilyDeclKind = kind
        }

-- ---------------------------------------------------------------------------
-- TypeFamilies: top-level type/data/newtype family instances

-- | Parse @type instance [forall binders.] LhsType = RhsType@
typeFamilyInstParser :: TokParser Decl
typeFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $
    DeclTypeFamilyInst
      TypeFamilyInst
        { typeFamilyInstForall = forallBinders,
          typeFamilyInstHeadForm = headForm,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

-- | Parse @data instance [forall binders.] HeadType = Cons | ...@ (also GADT style)
dataFamilyInstParser :: TokParser Decl
dataFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordData
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (_, head') <- typeFamilyLhsParser
  kind <- familyResultKindParser
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $
    DeclDataFamilyInst
      DataFamilyInst
        { dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = forallBinders,
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
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
newtypeFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordNewtype
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (_, head') <- typeFamilyLhsParser
  kind <- familyResultKindParser
  expectedTok TkReservedEquals
  constructor <- newtypeConDeclParser
  derivingClauses <- MP.many derivingClauseParser
  pure $
    DeclDataFamilyInst
      DataFamilyInst
        { dataFamilyInstIsNewtype = True,
          dataFamilyInstForall = forallBinders,
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
          dataFamilyInstConstructors = [constructor],
          dataFamilyInstDeriving = derivingClauses
        }

-- ---------------------------------------------------------------------------
-- TypeFamilies: class body items (associated type/data families + defaults)

-- | Parse @type [family] Name params [:: Kind]@ as an associated type family in a class.
-- Callers must ensure the next token after @type@ is not @instance@
-- (which is handled by 'classDefaultTypeInstParser' via token dispatch).
classTypeFamilyDeclParser :: TokParser ClassDeclItem
classTypeFamilyDeclParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  ClassItemTypeFamilyDecl <$> typeFamilyDeclBodyParser FamilyKeywordOptional

-- | Parse @data Name params [:: Kind]@ as an associated data family in a class.
classDataFamilyDeclParser :: TokParser ClassDeclItem
classDataFamilyDeclParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordData
  (headForm, name, params) <- typeDeclHeadParser
  kind <- familyResultKindParser
  pure
    ( ClassItemDataFamilyDecl
        DataFamilyDecl
          { dataFamilyDeclHeadForm = headForm,
            dataFamilyDeclName = name,
            dataFamilyDeclParams = params,
            dataFamilyDeclKind = kind
          }
    )

-- | Parse @type instance LhsType = RhsType@ as a default type family instance in a class.
classDefaultTypeInstParser :: TokParser ClassDeclItem
classDefaultTypeInstParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordInstance
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure
    ( ClassItemDefaultTypeInst
        TypeFamilyInst
          { typeFamilyInstForall = forallBinders,
            typeFamilyInstHeadForm = headForm,
            typeFamilyInstLhs = lhs,
            typeFamilyInstRhs = rhs
          }
    )

-- | Parse @type [forall binders.] LhsType = RhsType@ as a shorthand default
-- associated type instance in a class body.
classDefaultTypeInstShorthandParser :: TokParser ClassDeclItem
classDefaultTypeInstShorthandParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure
    ( ClassItemDefaultTypeInst
        TypeFamilyInst
          { typeFamilyInstForall = forallBinders,
            typeFamilyInstHeadForm = headForm,
            typeFamilyInstLhs = lhs,
            typeFamilyInstRhs = rhs
          }
    )

-- ---------------------------------------------------------------------------
-- TypeFamilies: instance body items

-- | Parse @type [instance] LhsType = RhsType@ inside an instance body.
-- The @instance@ keyword is accepted but optional (GHC normalizes both forms
-- to the same AST, so we treat them identically).
instanceTypeFamilyInstParser :: TokParser InstanceDeclItem
instanceTypeFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  _ <- MP.optional (expectedTok TkKeywordInstance)
  forallBinders <- forallPrefixDispatch typeFamilyForallParser
  (headForm, lhs) <- typeFamilyLhsParser
  expectedTok TkReservedEquals
  rhs <- typeParser
  pure $
    InstanceItemTypeFamilyInst
      TypeFamilyInst
        { typeFamilyInstForall = forallBinders,
          typeFamilyInstHeadForm = headForm,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

-- | Parse @data [instance] HeadType = Cons | ...@ (or GADT style) inside an instance body.
-- The @instance@ keyword is accepted but optional.
instanceDataFamilyInstParser :: TokParser InstanceDeclItem
instanceDataFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordData
  _ <- MP.optional (expectedTok TkKeywordInstance)
  (_, head') <- typeFamilyLhsParser
  kind <- familyResultKindParser
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $
    InstanceItemDataFamilyInst
      DataFamilyInst
        { dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
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

-- | Parse @newtype [instance] HeadType = Constructor@ inside an instance body.
-- The @instance@ keyword is accepted but optional.
instanceNewtypeFamilyInstParser :: TokParser InstanceDeclItem
instanceNewtypeFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordNewtype
  _ <- MP.optional (expectedTok TkKeywordInstance)
  (_, head') <- typeFamilyLhsParser
  kind <- familyResultKindParser
  expectedTok TkReservedEquals
  constructor <- newtypeConDeclParser
  derivingClauses <- MP.many derivingClauseParser
  pure $
    InstanceItemDataFamilyInst
      DataFamilyInst
        { dataFamilyInstIsNewtype = True,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
          dataFamilyInstConstructors = [constructor],
          dataFamilyInstDeriving = derivingClauses
        }

-- ---------------------------------------------------------------------------

typeSigDeclParser :: TokParser Decl
typeSigDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  DeclTypeSig names <$> typeParser

defaultDeclParser :: TokParser Decl
defaultDeclParser = do
  expectedTok TkKeywordDefault
  DeclDefault <$> parens (typeParser `MP.sepEndBy` expectedTok TkSpecialComma)

fixityDeclParser :: FixityAssoc -> TokParser Decl
fixityDeclParser assoc = withSpanAnn (DeclAnn . mkAnnotation) $ do
  (parsedAssoc, prec, mNamespace, ops) <- fixityDeclPartsParser
  when (assoc /= parsedAssoc) $
    fail "internal fixity dispatch mismatch"
  pure (DeclFixity parsedAssoc mNamespace prec ops)

fixityDeclPartsParser :: TokParser (FixityAssoc, Maybe Int, Maybe IEEntityNamespace, [UnqualifiedName])
fixityDeclPartsParser = do
  assoc <- fixityAssocParser
  prec <- MP.optional fixityPrecedenceParser
  mNamespace <- MP.optional fixityNamespaceParser
  ops <- fixityOperatorParser `MP.sepBy1` expectedTok TkSpecialComma
  pure (assoc, prec, mNamespace, ops)

fixityNamespaceParser :: TokParser IEEntityNamespace
fixityNamespaceParser =
  (expectedTok TkKeywordType >> pure IEEntityNamespaceType)
    <|> (expectedTok TkKeywordData >> pure IEEntityNamespaceData)

fixityAssocParser :: TokParser FixityAssoc
fixityAssocParser =
  (expectedTok TkKeywordInfix >> pure Infix)
    <|> (expectedTok TkKeywordInfixl >> pure InfixL)
    <|> (expectedTok TkKeywordInfixr >> pure InfixR)

fixityPrecedenceParser :: TokParser Int
fixityPrecedenceParser =
  tokenSatisfy "fixity precedence" $ \tok ->
    case lexTokenKind tok of
      TkInteger n _
        | n >= 0 && n <= 9 -> Just (fromInteger n)
      _ -> Nothing

fixityOperatorParser :: TokParser UnqualifiedName
fixityOperatorParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "fixity operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkUnqualifiedName NameVarSym op)
          TkConSym op -> Just (mkUnqualifiedName NameConSym op)
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- identifierUnqualifiedNameParser
      expectedTok TkSpecialBacktick
      pure op

classDeclParser :: TokParser Decl
classDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordClass
  context <- contextPrefixDispatch
  (headForm, className, classParams) <- classHeadParser
  classFundeps <- MP.option [] (MP.try classFundepsParser)
  items <- MP.option [] classWhereClauseParser
  pure $
    DeclClass
      ClassDecl
        { classDeclContext = context,
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
      { functionalDependencyAnns = [mkAnnotation span'],
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
    TkKeywordType -> MP.try classTypeFamilyDeclParser <|> classDefaultTypeInstShorthandParser
    _ -> do
      isSig <- startsWithTypeSig
      if isSig then classTypeSigItemParser else classDefaultItemParser

classPragmaItemParser :: TokParser ClassDeclItem
classPragmaItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  pragma <- anyPragmaParser "pragma declaration"
  pure (ClassItemPragma pragma)

classTypeSigItemParser :: TokParser ClassDeclItem
classTypeSigItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ClassItemTypeSig names <$> typeParser

classDefaultSigItemParser :: TokParser ClassDeclItem
classDefaultSigItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  expectedTok TkKeywordDefault
  name <- binderNameParser
  expectedTok TkReservedDoubleColon
  ClassItemDefaultSig name <$> typeParser

classFixityItemParser :: TokParser ClassDeclItem
classFixityItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  (assoc, prec, mNamespace, ops) <- fixityDeclPartsParser
  pure (ClassItemFixity assoc mNamespace prec ops)

classDefaultItemParser :: TokParser ClassDeclItem
classDefaultItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  ClassItemDefault . functionBindValue headForm name pats <$> equationRhsParser

instanceDeclParser :: TokParser Decl
instanceDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordInstance
  overlapPragma <- MP.optional instanceOverlapPragmaParser
  warningText <- MP.optional warningTextParser
  forallBinders <- MP.optional instanceForallParser
  context <- contextPrefixDispatch
  (parenthesizedHead, headForm, className, instanceTypes) <- instanceHeadParser
  items <- MP.option [] instanceWhereClauseParser
  pure $
    DeclInstance
      InstanceDecl
        { instanceDeclOverlapPragma = overlapPragma,
          instanceDeclWarning = warningText,
          instanceDeclForall = fromMaybe [] forallBinders,
          instanceDeclContext = fromMaybe [] context,
          instanceDeclParenthesizedHead = parenthesizedHead,
          instanceDeclHeadForm = headForm,
          instanceDeclClassName = className,
          instanceDeclTypes = instanceTypes,
          instanceDeclItems = items
        }

standaloneDerivingDeclParser :: TokParser Decl
standaloneDerivingDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordDeriving
  strategy <- MP.optional derivingStrategyParser
  viaTy <- MP.optional (MP.try derivingViaTypeParser)
  expectedTok TkKeywordInstance
  overlapPragma <- MP.optional instanceOverlapPragmaParser
  warningText <- MP.optional warningTextParser
  forallBinders <- MP.optional instanceForallParser
  context <- contextPrefixDispatch
  (parenthesizedHead, headForm, className, instanceTypes) <- standaloneDerivingHeadParser
  pure $
    DeclStandaloneDeriving
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = viaTy,
          standaloneDerivingOverlapPragma = overlapPragma,
          standaloneDerivingWarning = warningText,
          standaloneDerivingForall = fromMaybe [] forallBinders,
          standaloneDerivingContext = fromMaybe [] context,
          standaloneDerivingParenthesizedHead = parenthesizedHead,
          standaloneDerivingHeadForm = headForm,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = instanceTypes
        }

standaloneDerivingHeadParser :: TokParser (Bool, TypeHeadForm, Name, [Type])
standaloneDerivingHeadParser =
  MP.try
    ( do
        parsed <- parens bareStandaloneDerivingHeadParser
        _ <- MP.notFollowedBy (lookAhead typeInfixOperatorParser)
        let (headForm, className, instanceTypes) = parsed
        pure (True, headForm, className, instanceTypes)
    )
    <|> ( do
            (headForm, className, instanceTypes) <- bareStandaloneDerivingHeadParser
            pure (False, headForm, className, instanceTypes)
        )
  where
    bareStandaloneDerivingHeadParser = MP.try infixStandaloneDerivingHeadParser <|> prefixStandaloneDerivingHeadParser

    prefixStandaloneDerivingHeadParser = do
      className <- constructorNameParser <|> parens constructorOperatorParser
      instanceTypes <- MP.many typeAtomParser
      pure (TypeHeadPrefix, className, instanceTypes)

    infixStandaloneDerivingHeadParser = do
      lhs <- typeAtomParser
      _ <- lookAhead typeInfixOperatorParser
      op <- typeFamilyOperatorParser
      rhs <- typeAtomParser
      pure (TypeHeadInfix, op, [lhs, rhs])

instanceHeadParser :: TokParser (Bool, TypeHeadForm, UnqualifiedName, [Type])
instanceHeadParser =
  MP.try
    ( do
        parsed <- parens bareInstanceHeadParser
        _ <- MP.notFollowedBy (lookAhead typeInfixOperatorParser)
        let (headForm, className, instanceTypes) = parsed
        pure (True, headForm, className, instanceTypes)
    )
    <|> ( do
            (headForm, className, instanceTypes) <- bareInstanceHeadParser
            pure (False, headForm, className, instanceTypes)
        )
  where
    bareInstanceHeadParser = MP.try infixInstanceHeadParser <|> prefixInstanceHeadParser

    prefixInstanceHeadParser = do
      className <- nameToUnqualified <$> (constructorNameParser <|> parens constructorOperatorParser)
      instanceTypes <- MP.many typeAtomParser
      pure (TypeHeadPrefix, className, instanceTypes)

    infixInstanceHeadParser = do
      lhs <- typeAtomParser
      _ <- lookAhead typeInfixOperatorParser
      op <- typeFamilyOperatorParser
      rhs <- typeAtomParser
      pure (TypeHeadInfix, nameToUnqualified op, [lhs, rhs])

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
ordinaryInstanceDeclItemParser = do
  tok <- lookAhead anySingle
  case lexTokenKind tok of
    TkKeywordInfix -> instanceFixityItemParser
    TkKeywordInfixl -> instanceFixityItemParser
    TkKeywordInfixr -> instanceFixityItemParser
    TkKeywordData -> instanceDataFamilyInstParser
    TkKeywordNewtype -> instanceNewtypeFamilyInstParser
    TkKeywordType -> instanceTypeFamilyInstParser
    _ -> do
      isSig <- startsWithTypeSig
      if isSig then instanceTypeSigItemParser else instanceValueItemParser

instancePragmaItemParser :: TokParser InstanceDeclItem
instancePragmaItemParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  pragma <- anyPragmaParser "pragma declaration"
  pure (InstanceItemPragma pragma)

instanceTypeSigItemParser :: TokParser InstanceDeclItem
instanceTypeSigItemParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  InstanceItemTypeSig names <$> typeParser

instanceFixityItemParser :: TokParser InstanceDeclItem
instanceFixityItemParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  (assoc, prec, mNamespace, ops) <- fixityDeclPartsParser
  pure (InstanceItemFixity assoc mNamespace prec ops)

instanceValueItemParser :: TokParser InstanceDeclItem
instanceValueItemParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  InstanceItemBind . functionBindValue headForm name pats <$> equationRhsParser

foreignDeclParser :: TokParser Decl
foreignDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordForeign
  direction <- foreignDirectionParser
  callConv <- callConvParser
  safety <-
    case direction of
      ForeignImport -> MP.optional foreignSafetyParser
      ForeignExport -> pure Nothing
  entity <- MP.optional foreignEntityParser
  name <- binderNameParser
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure $
    DeclForeign
      ForeignDecl
        { foreignDirection = direction,
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
    <|> (varIdTok "interruptible" >> pure Interruptible)

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
dataDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordData
  context <- contextPrefixDispatch
  (headForm, typeName, typeParams) <- typeDeclHeadParser
  -- Parse optional inline kind signature: @:: Kind@
  inlineKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  (constructors, derivingClauses) <- gadtStyleDataDecl <|> traditionalStyleDataDecl
  pure $
    DeclData
      DataDecl
        { dataDeclHeadForm = headForm,
          dataDeclContext = fromMaybe [] context,
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclKind = inlineKind,
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
typeDataDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordType
  expectedTok TkKeywordData
  -- type data may not have a datatype context
  (headForm, typeName, typeParams) <- typeDeclHeadParser
  -- Parse optional inline kind signature: @:: Kind@
  inlineKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  constructors <- gadtStyleTypeDataDecl <|> traditionalStyleTypeDataDecl
  -- type data may not have a deriving clause
  pure $
    DeclTypeData
      DataDecl
        { dataDeclHeadForm = headForm,
          dataDeclContext = [],
          dataDeclName = typeName,
          dataDeclParams = typeParams,
          dataDeclKind = inlineKind,
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
  MP.try (typeDataConPrefixParser context) <|> typeDataConInfixParser context

typeDataConPrefixParser :: [Type] -> TokParser (SourceSpan -> DataConDecl)
typeDataConPrefixParser context = do
  conName <- constructorUnqualifiedNameParser <|> parens constructorOperatorUnqualifiedNameParser
  -- Parse arguments (no strictness, no records).
  -- Use typeAtomParser to keep adjacent atoms as separate fields instead of
  -- merging them into a type application.
  args <- MP.many $ BangType [] NoSourceUnpackedness False False <$> typeAtomParser
  -- If a constructor operator follows, this declaration is actually infix.
  MP.notFollowedBy constructorOperatorParser
  pure $ \span' -> DataConAnn (mkAnnotation span') (PrefixCon [] context conName args)

typeDataConInfixParser :: [Type] -> TokParser (SourceSpan -> DataConDecl)
typeDataConInfixParser context = do
  lhs <- typeDataConArgParser
  op <- constructorOperatorUnqualifiedNameParser <|> backtickConstructorUnqualifiedParser
  rhs <- typeDataConArgParser
  pure $ \span' -> DataConAnn (mkAnnotation span') (InfixCon [] context lhs op rhs)
  where
    backtickConstructorUnqualifiedParser = do
      expectedTok TkSpecialBacktick
      name <- constructorUnqualifiedNameParser
      expectedTok TkSpecialBacktick
      pure name

typeDataConArgParser :: TokParser BangType
typeDataConArgParser = BangType [] NoSourceUnpackedness False False <$> typeAtomParser

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
  forallBinders <- MP.many gadtForallParser
  -- Parse context (only equality constraints permitted, but we parse generally)
  context <- contextPrefixDispatchList
  -- Parse the body (prefix only for type data - no record style)
  body <- gadtTypeDataBodyParser
  pure $ \span' -> DataConAnn (mkAnnotation span') (GadtCon forallBinders context names body)

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
      let argTypes = map (BangType [] NoSourceUnpackedness False False) (init allTypes)
          resultTy = last allTypes
       in pure (GadtPrefixBody argTypes resultTy)

dataConDeclParser :: TokParser DataConDecl
dataConDeclParser = withSpan $ do
  (forallVars, context) <- dataConQualifiersParser
  MP.try (dataConRecordOrPrefixParser forallVars context) <|> dataConInfixParser forallVars context

newtypeDeclParser :: TokParser Decl
newtypeDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordNewtype
  context <- contextPrefixDispatch
  (headForm, typeName, typeParams) <- typeDeclHeadParser
  -- Parse optional inline kind signature: @:: Kind@
  inlineKind <- MP.optional (expectedTok TkReservedDoubleColon *> typeParser)
  -- GADT syntax starts with `where`, traditional syntax starts with `=` or nothing
  (constructor, derivingClauses) <- gadtStyleNewtypeDecl <|> traditionalStyleNewtypeDecl
  pure $
    DeclNewtype
      NewtypeDecl
        { newtypeDeclHeadForm = headForm,
          newtypeDeclContext = fromMaybe [] context,
          newtypeDeclName = typeName,
          newtypeDeclParams = typeParams,
          newtypeDeclKind = inlineKind,
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
  forallBinders <- MP.many gadtForallParser
  -- Parse optional context
  context <- contextPrefixDispatchList
  -- Parse the body (record or prefix style)
  body <- gadtBodyParser
  pure $ \span' -> DataConAnn (mkAnnotation span') (GadtCon forallBinders context names body)

-- | Parse constructor name for GADT - can be regular or operator in parens
gadtConNameParser :: TokParser UnqualifiedName
gadtConNameParser =
  constructorUnqualifiedNameParser
    <|> parens constructorOperatorUnqualifiedNameParser

-- | Parse forall in GADT context: @forall a b.@
gadtForallParser :: TokParser ForallTelescope
gadtForallParser = forallTelescopeParser

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
-- Uses 'typeInfixParser' so that infix type operators (e.g. @key := v@) are
-- accepted as argument types without requiring parentheses.
gadtBangTypeParser :: TokParser BangType
gadtBangTypeParser = withSpan $ do
  unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
  lazy <- MP.option False (expectedTok TkPrefixTilde >> pure True)
  ty <- typeInfixParser
  pure $ \span' ->
    BangType
      { bangAnns = [mkAnnotation span'],
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangLazy = lazy,
        bangType = ty
      }

-- | Parse the result type of a GADT constructor
-- This is a simple type application like @T a b@
gadtResultTypeParser :: TokParser Type
gadtResultTypeParser = typeParser

declContextParser :: TokParser [Type]
declContextParser = contextParserWith typeParser typeAtomParser

typeDeclHeadParser :: TokParser (TypeHeadForm, UnqualifiedName, [TyVarBinder])
typeDeclHeadParser =
  MP.try parenthesizedInfixDeclHeadParser <|> MP.try infixDeclHeadParser <|> prefixDeclHeadParser
  where
    prefixDeclHeadParser = do
      name <- constructorUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser
      params <- MP.many declTypeParamParser
      pure (TypeHeadPrefix, name, params)

    infixDeclHeadParser = do
      lhs <- declTypeParamParser
      op <- unqualifiedNameFromText <$> typeSynonymOperatorParser
      rhs <- declTypeParamParser
      pure (TypeHeadInfix, op, [lhs, rhs])

    parenthesizedInfixDeclHeadParser = do
      expectedTok TkSpecialLParen
      lhs <- declTypeParamParser
      op <- unqualifiedNameFromText <$> typeSynonymOperatorParser
      rhs <- declTypeParamParser
      expectedTok TkSpecialRParen
      tailParams <- MP.many declTypeParamParser
      pure (TypeHeadInfix, op, [lhs, rhs] <> tailParams)

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
        name <-
          constructorNameParser
            <|> (qualifyName Nothing <$> parens operatorUnqualifiedNameParser)
        pure (\span' -> typeAnnSpan span' (TCon name Unpromoted))
      params <- MP.many declTypeParamParser
      pure (TypeHeadPrefix, headType, params)

    infixHeadParser = do
      lhs <- declTypeParamParser
      op <- typeFamilyOperatorParser
      rhs <- declTypeParamParser
      let lhsType =
            TVar (mkUnqualifiedName NameVarId (tyVarBinderName lhs))
          rhsType =
            TVar (mkUnqualifiedName NameVarId (tyVarBinderName rhs))
      headType <- withSpan $ do
        pure $ \span' -> typeAnnSpan span' (TInfix lhsType op Unpromoted rhsType)
      pure (TypeHeadInfix, headType, [lhs, rhs])

-- | Parse an operator for type family declarations.
-- Unlike 'constructorOperatorParser', this accepts both constructor symbols (@:+:@)
-- and variable symbols (@**@), since type families can use either.
typeFamilyOperatorParser :: TokParser Name
typeFamilyOperatorParser =
  operatorNameParser <|> backtickTypeFamilyIdentifierParser
  where
    backtickTypeFamilyIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- constructorNameParser
      expectedTok TkSpecialBacktick
      pure op

typeFamilyLhsParser :: TokParser (TypeHeadForm, Type)
typeFamilyLhsParser = do
  lhs <- typeAppParser
  hasInfixTail <- MP.optional (lookAhead typeInfixOperatorParser)
  case hasInfixTail of
    Just _ -> do
      rest <- typeHeadInfixTailParser
      pure (TypeHeadInfix, foldl buildInfixType lhs rest)
    Nothing ->
      pure (TypeHeadPrefix, lhs)
  where
    typeHeadInfixTailParser :: TokParser [((Name, TypePromotion), Type)]
    typeHeadInfixTailParser = MP.many $ MP.try $ do
      op <- typeInfixOperatorParser
      rhs <- typeAppParser
      pure (op, rhs)

    buildInfixType left ((op, promoted), right) = TInfix left op promoted right

classHeadParser :: TokParser (TypeHeadForm, UnqualifiedName, [TyVarBinder])
classHeadParser =
  MP.try parenthesizedInfixDeclHeadParser <|> MP.try infixDeclHeadParser <|> prefixDeclHeadParser
  where
    prefixDeclHeadParser = do
      name <- constructorUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser
      params <- MP.many declTypeParamParser
      pure (TypeHeadPrefix, name, params)

    infixDeclHeadParser = do
      lhs <- declTypeParamParser
      op <- constructorOperatorParser
      rhs <- declTypeParamParser
      pure (TypeHeadInfix, nameToUnqualified op, [lhs, rhs])

    parenthesizedInfixDeclHeadParser = do
      expectedTok TkSpecialLParen
      lhs <- declTypeParamParser
      op <- constructorOperatorParser
      rhs <- declTypeParamParser
      expectedTok TkSpecialRParen
      tailParams <- MP.many declTypeParamParser
      pure (TypeHeadInfix, nameToUnqualified op, [lhs, rhs] <> tailParams)

nameToUnqualified :: Name -> UnqualifiedName
nameToUnqualified name = mkUnqualifiedName (nameType name) (nameText name)

explicitForallBinderParser :: TokParser TyVarBinder
explicitForallBinderParser =
  withSpan $
    ( do
        ident <-
          tokenSatisfy "type parameter binder" $ \tok ->
            case lexTokenKind tok of
              TkVarId name
                | isTypeVarName name ->
                    Just name
              _ -> Nothing
        pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)
    )
      <|> ( do
              expectedTok TkSpecialLParen
              ident <- lowerIdentifierParser
              expectedTok TkReservedDoubleColon
              kind <- typeParser
              expectedTok TkSpecialRParen
              pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)
          )

declTypeParamParser :: TokParser TyVarBinder
declTypeParamParser = MP.try invisibleDeclTypeParamParser <|> explicitForallBinderParser

invisibleDeclTypeParamParser :: TokParser TyVarBinder
invisibleDeclTypeParamParser = withSpan $ do
  expectedTok TkTypeApp
  ( do
      ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
      pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBInvisible)
    )
    <|> do
      expectedTok TkSpecialLParen
      ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
      expectedTok TkReservedDoubleColon
      kind <- typeParser
      expectedTok TkSpecialRParen
      pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBInvisible)

isTypeVarName :: Text -> Bool
isTypeVarName name =
  case T.uncons name of
    Just (c, _) -> c == '_' || isLower c
    Nothing -> False

derivingClauseParser :: TokParser DerivingClause
derivingClauseParser = do
  expectedTok TkKeywordDeriving
  strategy <- MP.optional derivingStrategyParser
  (classes, parenthesized) <- parenClasses <|> singleClass
  viaTy <- MP.optional derivingViaTypeParser
  pure (DerivingClause strategy classes viaTy parenthesized)
  where
    singleClass = (\c -> ([c], False)) <$> contextItemParserWith typeParser typeAtomParser
    parenClasses = fmap (,True) $ parens $ contextItemParserWith typeParser typeAtomParser `MP.sepEndBy` expectedTok TkSpecialComma

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

data FamilyKeywordMode
  = FamilyKeywordRequired
  | FamilyKeywordOptional

typeFamilyDeclBodyParser :: FamilyKeywordMode -> TokParser TypeFamilyDecl
typeFamilyDeclBodyParser familyKeywordMode = do
  expectedTok TkKeywordType
  explicitFamilyKeyword <- case familyKeywordMode of
    FamilyKeywordRequired -> expectedTok TkVarFamily $> True
    FamilyKeywordOptional -> isJust <$> MP.optional (expectedTok TkVarFamily)
  (headForm, headType, params) <- typeFamilyHeadParser
  resultSig <- typeFamilyResultSigParser
  equations <-
    case familyKeywordMode of
      FamilyKeywordRequired -> MP.optional (MP.try closedTypeFamilyWhereParser)
      FamilyKeywordOptional -> pure Nothing
  pure
    TypeFamilyDecl
      { typeFamilyDeclHeadForm = headForm,
        typeFamilyDeclExplicitFamilyKeyword = explicitFamilyKeyword,
        typeFamilyDeclHead = headType,
        typeFamilyDeclParams = params,
        typeFamilyDeclResultSig = resultSig,
        typeFamilyDeclEquations = equations
      }

forallBindersParser :: TokParser [Text]
forallBindersParser = do
  expectedTok TkKeywordForall
  binders <- MP.some explicitForallBinderParser
  expectedTok (TkVarSym ".")
  pure (map tyVarBinderName binders)

dataConRecordOrPrefixParser :: [Text] -> [Type] -> TokParser (SourceSpan -> DataConDecl)
dataConRecordOrPrefixParser forallVars context = do
  name <- constructorUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser
  mRecordFields <- MP.optional (MP.try recordFieldsParserAfterLayoutSemicolon)
  case mRecordFields of
    Just fields -> pure (\span' -> DataConAnn (mkAnnotation span') (RecordCon forallVars context name fields))
    Nothing -> do
      args <- MP.many constructorArgParser
      -- Ensure we're not leaving a constructor operator unconsumed.
      -- If there's a constructor operator next, this is actually an infix form
      -- and we should backtrack to let dataConInfixParser handle it.
      MP.notFollowedBy constructorOperatorParser
      pure (\span' -> DataConAnn (mkAnnotation span') (PrefixCon forallVars context name args))
  where
    -- Layout may inject a virtual ';' before a newline-started record field block.
    -- Accept it as part of the constructor declaration.
    recordFieldsParserAfterLayoutSemicolon =
      recordFieldsParser
        <|> (expectedTok TkSpecialSemicolon *> recordFieldsParser)

dataConInfixParser :: [Text] -> [Type] -> TokParser (SourceSpan -> DataConDecl)
dataConInfixParser forallVars context = do
  lhs <- infixConstructorArgParser
  op <- constructorOperatorUnqualifiedNameParser <|> backtickConstructorUnqualifiedParser
  rhs <- infixConstructorArgParser
  pure (\span' -> DataConAnn (mkAnnotation span') (InfixCon forallVars context lhs op rhs))
  where
    backtickConstructorUnqualifiedParser = do
      expectedTok TkSpecialBacktick
      name <- constructorUnqualifiedNameParser
      expectedTok TkSpecialBacktick
      pure name

recordFieldsParser :: TokParser [FieldDecl]
recordFieldsParser = braces (recordFieldDeclParser `MP.sepEndBy` expectedTok TkSpecialComma)

recordFieldDeclParser :: TokParser FieldDecl
recordFieldDeclParser = withSpan $ do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  fieldTy <- recordFieldBangTypeParser
  pure $ \span' ->
    FieldDecl
      { fieldAnns = [mkAnnotation span'],
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
    lazy <- MP.option False (expectedTok TkPrefixTilde >> pure True)
    ty <- typeAppParser
    pure $ \span' ->
      BangType
        { bangAnns = [mkAnnotation span'],
          bangSourceUnpackedness = unpackedness,
          bangStrict = strict,
          bangLazy = lazy,
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
  lazy <- MP.option False (expectedTok TkPrefixTilde >> pure True)
  ty <- typeAtomParser
  pure $ \span' ->
    BangType
      { bangAnns = [mkAnnotation span'],
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangLazy = lazy,
        bangType = ty
      }

recordFieldBangTypeParser :: TokParser BangType
recordFieldBangTypeParser = withSpan $ do
  unpackedness <- MP.option NoSourceUnpackedness sourceUnpackednessPragmaParser
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True)
  lazy <- MP.option False (expectedTok TkPrefixTilde >> pure True)
  ty <- constructorFieldTypeParser
  pure $ \span' ->
    BangType
      { bangAnns = [mkAnnotation span'],
        bangSourceUnpackedness = unpackedness,
        bangStrict = strict,
        bangLazy = lazy,
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

constructorOperatorParser :: TokParser Name
constructorOperatorParser =
  symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser
  where
    symbolicConstructorOperatorParser =
      tokenSatisfy "constructor operator" $ \tok ->
        case lexTokenKind tok of
          TkConSym op -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym op))
          TkQConSym modName op -> Just (mkName (Just modName) NameConSym op)
          TkReservedColon -> Just (qualifyName Nothing (mkUnqualifiedName NameConSym ":"))
          _ -> Nothing
    backtickConstructorIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- constructorNameParser
      expectedTok TkSpecialBacktick
      pure op

-- | Parse a pattern binding declaration like @(x, y) = (1, 2)@.
-- This handles bindings where the LHS is a pattern rather than a function name.
patternBindDeclParser :: TokParser Decl
patternBindDeclParser = MP.try $ withSpanAnn (DeclAnn . mkAnnotation) $ do
  pat <- region "while parsing pattern binding" patternParser
  DeclValue . PatternBind pat <$> equationRhsParser

valueDeclParser :: TokParser Decl
valueDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  (headForm, name, pats) <- functionHeadParserWith patternParser simplePatternParser
  functionBindDecl headForm name pats <$> equationRhsParser

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
patternSynonymSigDeclParser = do
  expectedTok TkKeywordPattern
  names <- patSynNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  DeclPatSynSig names <$> typeParser

patSynNameParser :: TokParser UnqualifiedName
patSynNameParser =
  constructorUnqualifiedNameParser
    <|> do
      op <- parens constructorOperatorParser
      pure (mkUnqualifiedName (nameType op) (nameText op))

-- | Parse a pattern synonym declaration.
-- Handles prefix, infix, and record forms with all three directionalities.
patternSynonymDeclParser :: TokParser Decl
patternSynonymDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do
  expectedTok TkKeywordPattern
  (name, args) <- patSynLhsParser
  (dir, pat) <- patSynDirAndPatParser name
  pure $
    DeclPatSyn
      PatSynDecl
        { patSynDeclName = name,
          patSynDeclArgs = args,
          patSynDeclPat = pat,
          patSynDeclDir = dir
        }

-- | Parse the LHS of a pattern synonym declaration.
-- Returns the name and the argument form.
patSynLhsParser :: TokParser (UnqualifiedName, PatSynArgs)
patSynLhsParser =
  MP.try patSynInfixLhsParser <|> patSynRecordOrPrefixLhsParser

-- | Parse an infix pattern synonym LHS: @var ConOp var@ or @var \`Con\` var@
patSynInfixLhsParser :: TokParser (UnqualifiedName, PatSynArgs)
patSynInfixLhsParser = do
  lhs <- lowerIdentifierParser
  op <- constructorOperatorParser
  rhs <- lowerIdentifierParser
  pure (mkUnqualifiedName (nameType op) (nameText op), PatSynInfixArgs lhs rhs)

-- | Parse a record or prefix pattern synonym LHS.
-- Record: @Con {field1, field2, ...}@
-- Prefix: @Con var1 var2 ...@
patSynRecordOrPrefixLhsParser :: TokParser (UnqualifiedName, PatSynArgs)
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
patSynDirAndPatParser :: UnqualifiedName -> TokParser (PatSynDir, Pattern)
patSynDirAndPatParser name =
  ( do
      expectedTok TkReservedEquals
      pat <- patternParser
      pure (PatSynBidirectional, pat)
  )
    <|> ( do
            expectedTok TkReservedLeftArrow
            pat <- patternParser
            mMatches <- MP.optional (patSynWhereClauseParser (renderUnqualifiedName name))
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
  (headForm, _name, pats) <- functionHeadParserWithBinder patSynNameParser patternParser simplePatternParser
  rhs <- equationRhsParser
  pure $ \span' ->
    Match
      { matchAnns = [mkAnnotation span'],
        matchHeadForm = headForm,
        matchPats = pats,
        matchRhs = rhs
      }
