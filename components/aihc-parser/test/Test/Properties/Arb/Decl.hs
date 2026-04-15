{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Decl
  ( genDecl,
    genFunctionDecl,
    shrinkDecl,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Expr (genExpr, genOperator, isValidGeneratedOperator, shrinkExpr)
import Test.Properties.Arb.Identifiers
  ( genConIdent,
    genConSym,
    genIdent,
    shrinkIdent,
    span0,
  )
import Test.Properties.Arb.Pattern (canonicalPatternAtom, genPattern, shrinkPattern)
import Test.Properties.Arb.Type (canonicalFunLeft, canonicalTopLevelType, genType)
import Test.QuickCheck

-- | Annotation choices for BangType
data FieldAnnotation = NoAnnotation | StrictAnnotation | LazyAnnotation
  deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Decl where
  arbitrary = genDecl
  shrink = shrinkDecl

genDecl :: Gen Decl
genDecl = sized $ \n ->
  oneof
    [ genDeclValue n,
      genDeclTypeSig,
      genDeclFixity,
      genDeclRoleAnnotation,
      genDeclTypeSyn,
      genDeclTypeSynInfix,
      genDeclData,
      genDeclTypeData,
      genDeclNewtype,
      genDeclClass,
      genDeclInstance,
      genDeclStandaloneDeriving,
      genDeclDefault,
      genDeclSplice,
      genDeclForeign,
      genDeclTypeFamilyDecl,
      genDeclTypeFamilyDeclInfix,
      genDeclDataFamilyDecl,
      genDeclTypeFamilyInst,
      genDeclDataFamilyInst,
      genDeclPragma,
      genDeclPatSyn,
      genDeclPatSynSig,
      genDeclStandaloneKindSig
    ]

genDeclValue :: Int -> Gen Decl
genDeclValue n =
  oneof
    [ genFunctionValueDecl n,
      genPatternValueDecl n
    ]

genFunctionValueDecl :: Int -> Gen Decl
genFunctionValueDecl n = do
  name <- genVarBinderName
  expr <- resize n genExpr
  genFunctionDecl (name, expr)

genPatternValueDecl :: Int -> Gen Decl
genPatternValueDecl n = do
  pat <- genPatternBindPattern n
  expr <- resize n genExpr
  pure $ DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr Nothing))

genPatternBindPattern :: Int -> Gen Pattern
genPatternBindPattern n =
  frequency
    [ (1, PVar span0 . mkUnqualifiedName NameVarId <$> genIdent),
      (4, sized (genGeneralPatternBindPattern . min 3 . min n))
    ]

genGeneralPatternBindPattern :: Int -> Gen Pattern
genGeneralPatternBindPattern n =
  suchThat (genPattern n) isGeneralPatternBindPattern

isGeneralPatternBindPattern :: Pattern -> Bool
isGeneralPatternBindPattern pat =
  case pat of
    PVar {} -> False
    _ -> True

genFunctionDecl :: (UnqualifiedName, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  headForm <- elements [MatchHeadPrefix, MatchHeadInfix]
  case headForm of
    MatchHeadPrefix ->
      do
        patCount <- chooseInt (1, 3)
        pats <- vectorOf patCount (sized (genPattern . min 3))
        pure $
          DeclValue
            span0
            ( FunctionBind
                span0
                name
                [ Match
                    { matchSpan = span0,
                      matchHeadForm = MatchHeadPrefix,
                      matchPats = pats,
                      matchRhs = UnguardedRhs span0 expr Nothing
                    }
                ]
            )
    MatchHeadInfix ->
      do
        lhsPat <- genInfixLhsPattern
        rhsPat <- canonicalPatternAtom <$> sized (genPattern . min 3)
        pure $
          DeclValue
            span0
            ( FunctionBind
                span0
                name
                [ Match
                    { matchSpan = span0,
                      matchHeadForm = MatchHeadInfix,
                      matchPats = [lhsPat, rhsPat],
                      matchRhs = UnguardedRhs span0 expr Nothing
                    }
                ]
            )

genInfixLhsPattern :: Gen Pattern
genInfixLhsPattern =
  canonicalPatternAtom <$> sized (genPatternWithoutLeadingNegArg . min 3)

genPatternWithoutLeadingNegArg :: Int -> Gen Pattern
genPatternWithoutLeadingNegArg n =
  suchThat (genPattern n) (not . startsWithConstructorNegativeLiteral)

startsWithConstructorNegativeLiteral :: Pattern -> Bool
startsWithConstructorNegativeLiteral pat =
  case pat of
    PCon _ _ (PNegLit {} : _) -> True
    PParen _ inner -> startsWithConstructorNegativeLiteral inner
    _ -> False

genDeclTypeSig :: Gen Decl
genDeclTypeSig = do
  nameCount <- chooseInt (1, 3)
  names <- vectorOf nameCount genVarBinderName
  DeclTypeSig span0 names <$> genSimpleType

genVarBinderName :: Gen UnqualifiedName
genVarBinderName =
  oneof
    [ mkUnqualifiedName NameVarId <$> genIdent,
      mkUnqualifiedName NameVarSym <$> genOperator
    ]

genDeclFixity :: Gen Decl
genDeclFixity = do
  assoc <- elements [Infix, InfixL, InfixR]
  prec <- elements [Nothing, Just 0, Just 6, Just 9]
  n <- chooseInt (1, 2)
  ops <- vectorOf n (mkUnqualifiedName NameVarSym <$> genSymbolicOp)
  pure $ DeclFixity span0 assoc Nothing prec ops

genSymbolicOp :: Gen Text
genSymbolicOp = elements ["+", "<>", "&&", "||", "**", "^", ">>"]

genDeclRoleAnnotation :: Gen Decl
genDeclRoleAnnotation = do
  name <- genConIdent
  n <- chooseInt (0, 3)
  roles <- vectorOf n (elements [RoleNominal, RoleRepresentational, RolePhantom, RoleInfer])
  pure $ DeclRoleAnnotation span0 (RoleAnnotation span0 name roles)

genDeclTypeSyn :: Gen Decl
genDeclTypeSyn = do
  name <- genConIdent
  params <- genSimpleTyVarBinders
  DeclTypeSyn span0 . TypeSynDecl span0 TypeHeadPrefix name params <$> genSimpleType

-- | Generate an infix type synonym, covering both symbolic operators
-- (e.g. @type a :+: b = (a, b)@) and backtick-wrapped identifiers
-- (e.g. @type a \`Plus\` b = (a, b)@).
genDeclTypeSynInfix :: Gen Decl
genDeclTypeSynInfix = do
  name <- oneof [genConSym, genConIdent]
  lhsName <- genIdent
  rhsName <- genIdent
  let lhs = TyVarBinder span0 lhsName Nothing TyVarBSpecified
      rhs = TyVarBinder span0 rhsName Nothing TyVarBSpecified
  DeclTypeSyn span0 . TypeSynDecl span0 TypeHeadInfix name [lhs, rhs] <$> genSimpleType

genDeclData :: Gen Decl
genDeclData =
  oneof
    [ DeclData span0 <$> genSimpleDataDecl,
      genDeclDataGadt
    ]

genDeclDataGadt :: Gen Decl
genDeclDataGadt = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctors <- genGadtDataCons
  pure $
    DeclData span0 $
      DataDecl
        { dataDeclSpan = span0,
          dataDeclHeadForm = TypeHeadPrefix,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = params,
          dataDeclKind = Nothing,
          dataDeclConstructors = ctors,
          dataDeclDeriving = []
        }

genDeclTypeData :: Gen Decl
genDeclTypeData = genDeclTypeDataPrefix

genDeclTypeDataPrefix :: Gen Decl
genDeclTypeDataPrefix = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctors <- genTypeDataCons
  pure $
    DeclTypeData span0 $
      DataDecl
        { dataDeclSpan = span0,
          dataDeclHeadForm = TypeHeadPrefix,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = params,
          dataDeclKind = Nothing,
          dataDeclConstructors = ctors,
          dataDeclDeriving = []
        }

genTypeDataCons :: Gen [DataConDecl]
genTypeDataCons = do
  n <- chooseInt (0, 3)
  vectorOf n genTypeDataCon
  where
    genTypeDataCon = do
      conName <- mkUnqualifiedName NameConId <$> genConIdent
      n <- chooseInt (0, 3)
      -- Type data constructors don't support strictness annotations
      fields <- vectorOf n genNonStrictBangType
      pure $ PrefixCon span0 [] [] conName fields

-- | Generate a BangType that is never strict (for type data constructors).
-- Type data constructors don't support strictness or lazy annotations.
genNonStrictBangType :: Gen BangType
genNonStrictBangType = do
  ty <- genSimpleType
  pure $
    BangType
      { bangSpan = span0,
        bangSourceUnpackedness = NoSourceUnpackedness,
        bangStrict = False,
        bangLazy = False,
        bangType = ty
      }

genSimpleDataDecl :: Gen DataDecl
genSimpleDataDecl = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DataDecl
      { dataDeclSpan = span0,
        dataDeclHeadForm = TypeHeadPrefix,
        dataDeclContext = [],
        dataDeclName = name,
        dataDeclParams = params,
        dataDeclKind = Nothing,
        dataDeclConstructors = ctors,
        dataDeclDeriving = deriving'
      }

genSimpleDataCons :: Gen [DataConDecl]
genSimpleDataCons = do
  n <- chooseInt (0, 3)
  vectorOf n genMixedDataCon

genMixedDataCon :: Gen DataConDecl
genMixedDataCon =
  oneof
    [ genPrefixCon,
      genInfixCon,
      genRecordCon
    ]

genPrefixCon :: Gen DataConDecl
genPrefixCon = do
  -- Prefix constructors can be alphabetic (Cons) or symbolic ((:+))
  name <-
    oneof
      [ mkUnqualifiedName NameConId <$> genConIdent,
        mkUnqualifiedName NameConSym <$> genConSym
      ]
  n <- chooseInt (0, 2)
  fields <- vectorOf n genSimpleBangType
  pure $ PrefixCon span0 [] [] name fields

genInfixCon :: Gen DataConDecl
genInfixCon = do
  -- Infix constructors can be symbolic (:+) or alphabetic (`Cons`)
  opName <-
    oneof
      [ mkUnqualifiedName NameConSym <$> genConSym,
        mkUnqualifiedName NameConId <$> genConIdent
      ]
  lhs <- genSimpleBangTypeWithoutFun
  InfixCon span0 [] [] lhs opName <$> genSimpleBangTypeWithoutFun

genRecordCon :: Gen DataConDecl
genRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  n <- chooseInt (0, 3)
  fields <- vectorOf n genFieldDecl
  pure $ RecordCon span0 [] [] conName fields

genFieldDecl :: Gen FieldDecl
genFieldDecl = do
  fieldCount <- chooseInt (1, 3)
  fieldNames <- vectorOf fieldCount genVarBinderName
  FieldDecl span0 fieldNames <$> genSimpleBangType

genGadtDataCons :: Gen [DataConDecl]
genGadtDataCons = do
  n <- chooseInt (1, 3)
  vectorOf n genGadtCon

genGadtCon :: Gen DataConDecl
genGadtCon = do
  n <- chooseInt (1, 2)
  names <- vectorOf n (mkUnqualifiedName NameConId <$> genConIdent)
  GadtCon span0 [] [] names <$> genGadtBody

genGadtBody :: Gen GadtBody
genGadtBody =
  oneof
    [ genGadtPrefixBody,
      genGadtRecordBody
    ]

genGadtPrefixBody :: Gen GadtBody
genGadtPrefixBody = do
  n <- chooseInt (0, 2)
  args <- vectorOf n genGadtBangType
  result <- canonicalFunLeft . canonicalTopLevelType <$> sized (genType . min 6)
  pure $ GadtPrefixBody args result

-- | Generate a BangType for GADT prefix body arg position.
-- Uses the full type generator with canonicalFunLeft applied, since the parser
-- uses typeInfixParser (which cannot parse bare forall/->/(=>) without parens).
-- Does not generate lazy/strict annotations on types that start with symbolic
-- characters (TStar, TTHSplice, TTuple, etc.) since the lexer treats ~! or !*
-- as single operator tokens.
genGadtBangType :: Gen BangType
genGadtBangType = do
  ty <- canonicalFunLeft . canonicalTopLevelType <$> sized (genType . min 6)
  -- Only generate lazy/strict annotations on types that start with alphabetic characters
  let canAnnotate = typeStartsWithAlpha ty
  annotation <- if canAnnotate then elements [NoAnnotation, StrictAnnotation, LazyAnnotation] else pure NoAnnotation
  case annotation of
    NoAnnotation -> pure $ BangType span0 NoSourceUnpackedness False False ty
    StrictAnnotation -> pure $ BangType span0 NoSourceUnpackedness True False ty
    LazyAnnotation -> pure $ BangType span0 NoSourceUnpackedness False True ty
  where
    typeStartsWithAlpha :: Type -> Bool
    typeStartsWithAlpha (TVar _ _) = True
    typeStartsWithAlpha (TCon _ n _) = let txt = nameText n in not (T.null txt) && isAlpha (T.head txt)
    typeStartsWithAlpha (TParen _ inner) = typeStartsWithAlpha inner
    typeStartsWithAlpha _ = False

-- | Generate a BangType without function types at the top level.
-- Does not generate lazy/strict annotations on kind-like types (TStar, etc.) since
-- GHC rejects those (e.g., ~* or !* are treated as operators).
genSimpleBangTypeWithoutFun :: Gen BangType
genSimpleBangTypeWithoutFun = do
  ty <- genSimpleTypeWithoutFun
  -- genSimpleTypeWithoutFun only generates TVar and TCon, which are safe for annotations
  annotation <- elements [NoAnnotation, StrictAnnotation, LazyAnnotation]
  case annotation of
    NoAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = False,
            bangType = ty
          }
    StrictAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = True,
            bangLazy = False,
            bangType = ty
          }
    LazyAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = True,
            bangType = ty
          }

-- | Generate a simple type without function types at the top level.
genSimpleTypeWithoutFun :: Gen Type
genSimpleTypeWithoutFun =
  oneof
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent
    ]

genGadtRecordBody :: Gen GadtBody
genGadtRecordBody = do
  n <- chooseInt (1, 3)
  fields <- vectorOf n genGadtFieldDecl
  result <- canonicalTopLevelType <$> sized (genType . min 6)
  pure $ GadtRecordBody fields result

-- | Generate a field declaration for GADT record body position.
-- Uses the full type generator since record field types are parsed by typeParser.
genGadtFieldDecl :: Gen FieldDecl
genGadtFieldDecl = do
  fieldName <- mkUnqualifiedName NameVarId <$> genIdent
  ty <- canonicalTopLevelType <$> sized (genType . min 6)
  pure $ FieldDecl span0 [fieldName] (BangType span0 NoSourceUnpackedness False False ty)

genSimpleBangType :: Gen BangType
genSimpleBangType = do
  ty <- genSimpleType
  -- genSimpleType only generates TVar and TCon, which are safe for lazy annotations
  annotation <- elements [NoAnnotation, StrictAnnotation, LazyAnnotation]
  case annotation of
    NoAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = False,
            bangType = ty
          }
    StrictAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = True,
            bangLazy = False,
            bangType = ty
          }
    LazyAnnotation ->
      pure $
        BangType
          { bangSpan = span0,
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = True,
            bangType = ty
          }

genDeclNewtype :: Gen Decl
genDeclNewtype = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctor <- genNewtypeCon
  deriving' <- genDerivingClauses
  pure $
    DeclNewtype span0 $
      NewtypeDecl
        { newtypeDeclSpan = span0,
          newtypeDeclHeadForm = TypeHeadPrefix,
          newtypeDeclContext = [],
          newtypeDeclName = name,
          newtypeDeclParams = params,
          newtypeDeclKind = Nothing,
          newtypeDeclConstructor = Just ctor,
          newtypeDeclDeriving = deriving'
        }

genNewtypeCon :: Gen DataConDecl
genNewtypeCon =
  oneof
    [ genNewtypePrefixCon,
      genNewtypeRecordCon
    ]

genNewtypePrefixCon :: Gen DataConDecl
genNewtypePrefixCon = do
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  ty <- genSimpleType
  pure $ PrefixCon span0 [] [] conName [BangType span0 NoSourceUnpackedness False False ty]

genNewtypeRecordCon :: Gen DataConDecl
genNewtypeRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  fieldName <- genVarBinderName
  ty <- genSimpleType
  pure $ RecordCon span0 [] [] conName [FieldDecl span0 [fieldName] (BangType span0 NoSourceUnpackedness False False ty)]

genDeclClass :: Gen Decl
genDeclClass = do
  name <- genConIdent
  params <- genSimpleTyVarBinders
  ctx <- genOptionalSimpleContext
  items <- genClassDeclItems params
  pure $
    DeclClass span0 $
      ClassDecl
        { classDeclSpan = span0,
          classDeclContext = ctx,
          classDeclHeadForm = TypeHeadPrefix,
          classDeclName = name,
          classDeclParams = params,
          classDeclFundeps = [],
          classDeclItems = items
        }

genClassDeclItems :: [TyVarBinder] -> Gen [ClassDeclItem]
genClassDeclItems params =
  frequency
    [ (3, pure []),
      (2, (: []) <$> genClassTypeSigItem),
      (2, (: []) <$> genClassAssociatedTypeDeclItem params),
      (1, genClassAssociatedTypeItems params)
    ]

genClassTypeSigItem :: Gen ClassDeclItem
genClassTypeSigItem = do
  name <- genVarBinderName
  ClassItemTypeSig span0 [name] <$> genSimpleType

genClassAssociatedTypeDeclItem :: [TyVarBinder] -> Gen ClassDeclItem
genClassAssociatedTypeDeclItem params = do
  tf <- genAssociatedTypeFamilyDecl params
  pure $ ClassItemTypeFamilyDecl span0 tf

genClassAssociatedTypeItems :: [TyVarBinder] -> Gen [ClassDeclItem]
genClassAssociatedTypeItems params = do
  tf <- genAssociatedTypeFamilyDecl params
  mDefault <- genAssociatedTypeDefaultInst tf params
  pure $ ClassItemTypeFamilyDecl span0 tf : maybe [] (pure . ClassItemDefaultTypeInst span0) mDefault

genAssociatedTypeFamilyDecl :: [TyVarBinder] -> Gen TypeFamilyDecl
genAssociatedTypeFamilyDecl classParams = do
  name <- genConIdent
  paramCount <- chooseInt (0, min 2 (length classParams))
  params <- take paramCount <$> shuffle classParams
  let headType = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  pure $
    TypeFamilyDecl
      { typeFamilyDeclSpan = span0,
        typeFamilyDeclHeadForm = TypeHeadPrefix,
        typeFamilyDeclHead = headType,
        typeFamilyDeclParams = params,
        typeFamilyDeclKind = Nothing,
        typeFamilyDeclEquations = Nothing
      }

genAssociatedTypeDefaultInst :: TypeFamilyDecl -> [TyVarBinder] -> Gen (Maybe TypeFamilyInst)
genAssociatedTypeDefaultInst tf classParams =
  if null classParams || null (typeFamilyDeclParams tf)
    then pure Nothing
    else frequency [(1, pure Nothing), (3, Just <$> mkDefaultInst)]
  where
    mkDefaultInst = do
      rhs <- genSimpleType
      let argTypes = [TVar span0 (mkUnqualifiedName NameVarId (tyVarBinderName param)) | param <- typeFamilyDeclParams tf]
          lhs = foldl (TApp span0) (typeFamilyDeclHead tf) argTypes
      pure $
        TypeFamilyInst
          { typeFamilyInstSpan = span0,
            typeFamilyInstForall = [],
            typeFamilyInstHeadForm = typeFamilyDeclHeadForm tf,
            typeFamilyInstLhs = lhs,
            typeFamilyInstRhs = rhs
          }

genDeclInstance :: Gen Decl
genDeclInstance = do
  className <- genConIdent
  n <- chooseInt (0, 2)
  types <- vectorOf n genSimpleType
  ctx <- genSimpleContext
  pure $
    DeclInstance span0 $
      InstanceDecl
        { instanceDeclSpan = span0,
          instanceDeclOverlapPragma = Nothing,
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclParenthesizedHead = False,
          instanceDeclClassName = className,
          instanceDeclTypes = types,
          instanceDeclItems = []
        }

genDeclStandaloneDeriving :: Gen Decl
genDeclStandaloneDeriving = do
  className <- mkUnqualifiedName NameConId <$> genConIdent
  n <- chooseInt (0, 2)
  types <- vectorOf n genSimpleType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving span0 $
      StandaloneDerivingDecl
        { standaloneDerivingSpan = span0,
          standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingOverlapPragma = Nothing,
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingParenthesizedHead = False,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = types
        }

genDeclDefault :: Gen Decl
genDeclDefault = do
  n <- chooseInt (0, 3)
  types <- vectorOf n genSimpleType
  pure $ DeclDefault span0 types

genDeclSplice :: Gen Decl
genDeclSplice = do
  name <- qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent
  pure $ DeclSplice span0 (EVar span0 name)

genDeclForeign :: Gen Decl
genDeclForeign = do
  callConv <- elements [CCall, StdCall, CApi]
  direction <- elements [ForeignImport, ForeignExport]
  -- Safety is only valid for imports, not exports
  safety <- case direction of
    ForeignImport -> elements [Nothing, Just Safe, Just Unsafe]
    ForeignExport -> pure Nothing
  name <- genIdent
  ty <- genSimpleType
  pure $
    DeclForeign span0 $
      ForeignDecl
        { foreignDeclSpan = span0,
          foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = ForeignEntityOmitted,
          foreignName = name,
          foreignType = ty
        }

genDeclTypeFamilyDecl :: Gen Decl
genDeclTypeFamilyDecl = do
  name <- genConIdent
  let headType = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  params <- genSimpleTyVarBinders
  pure $
    DeclTypeFamilyDecl span0 $
      TypeFamilyDecl
        { typeFamilyDeclSpan = span0,
          typeFamilyDeclHeadForm = TypeHeadPrefix,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclKind = Nothing,
          typeFamilyDeclEquations = Nothing
        }

-- | Generate an infix type family declaration, covering both symbolic operators
-- (e.g. @type family a ** b@) and backtick-wrapped identifiers
-- (e.g. @type family a \`And\` b@).
genDeclTypeFamilyDeclInfix :: Gen Decl
genDeclTypeFamilyDeclInfix = do
  (nameType, nameText) <- elements [(NameConSym, genConSym), (NameConId, genConIdent)]
  name <- nameText
  lhsName <- genIdent
  rhsName <- genIdent
  let lhs = TyVarBinder span0 lhsName Nothing TyVarBSpecified
      rhs = TyVarBinder span0 rhsName Nothing TyVarBSpecified
      lhsType = TVar span0 (mkUnqualifiedName NameVarId lhsName)
      rhsType = TVar span0 (mkUnqualifiedName NameVarId rhsName)
      headType = TApp span0 (TApp span0 (TCon span0 (qualifyName Nothing (mkUnqualifiedName nameType name)) Unpromoted) lhsType) rhsType
  pure $
    DeclTypeFamilyDecl span0 $
      TypeFamilyDecl
        { typeFamilyDeclSpan = span0,
          typeFamilyDeclHeadForm = TypeHeadInfix,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = [lhs, rhs],
          typeFamilyDeclKind = Nothing,
          typeFamilyDeclEquations = Nothing
        }

genDeclDataFamilyDecl :: Gen Decl
genDeclDataFamilyDecl = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  pure $
    DeclDataFamilyDecl span0 $
      DataFamilyDecl
        { dataFamilyDeclSpan = span0,
          dataFamilyDeclName = name,
          dataFamilyDeclParams = params,
          dataFamilyDeclKind = Nothing
        }

genDeclTypeFamilyInst :: Gen Decl
genDeclTypeFamilyInst = do
  lhs <- genFamilyLhsType
  rhs <- genSimpleType
  pure $
    DeclTypeFamilyInst span0 $
      TypeFamilyInst
        { typeFamilyInstSpan = span0,
          typeFamilyInstForall = [],
          typeFamilyInstHeadForm = TypeHeadPrefix,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

genDeclDataFamilyInst :: Gen Decl
genDeclDataFamilyInst =
  oneof
    [ genDeclDataFamilyInstPrefix,
      genDeclDataFamilyInstGadt
    ]

genDeclDataFamilyInstPrefix :: Gen Decl
genDeclDataFamilyInstPrefix = do
  head' <- genFamilyLhsType
  ctors <- genSimpleDataCons
  pure $
    DeclDataFamilyInst span0 $
      DataFamilyInst
        { dataFamilyInstSpan = span0,
          dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = ctors,
          dataFamilyInstDeriving = []
        }

genDeclDataFamilyInstGadt :: Gen Decl
genDeclDataFamilyInstGadt = do
  head' <- genFamilyLhsType
  ctors <- genGadtDataCons
  pure $
    DeclDataFamilyInst span0 $
      DataFamilyInst
        { dataFamilyInstSpan = span0,
          dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstConstructors = ctors,
          dataFamilyInstDeriving = []
        }

-- | Generate a type family LHS: a type constructor applied to a type constructor argument.
genFamilyLhsType :: Gen Type
genFamilyLhsType = do
  familyName <- genConIdent
  argName <- genConIdent
  let familyCon = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId familyName)) Unpromoted
      argCon = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId argName)) Unpromoted
  pure $ TApp span0 familyCon argCon

genDeclPragma :: Gen Decl
genDeclPragma = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  DeclPragma span0 . PragmaInline kind <$> genIdent

genDeclPatSyn :: Gen Decl
genDeclPatSyn = do
  synName <- mkUnqualifiedName NameConId <$> genConIdent
  argName <- genIdent
  conName <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent
  let args = PatSynPrefixArgs [argName]
      pat = PCon span0 conName [PVar span0 (mkUnqualifiedName NameVarId argName)]
  dir <- elements [PatSynBidirectional, PatSynUnidirectional]
  pure $ DeclPatSyn span0 (PatSynDecl span0 synName args pat dir)

genDeclPatSynSig :: Gen Decl
genDeclPatSynSig = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  DeclPatSynSig span0 [name] <$> genSimpleType

genDeclStandaloneKindSig :: Gen Decl
genDeclStandaloneKindSig = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  kind <- sized (genType . min 6)
  pure $ DeclStandaloneKindSig span0 name (canonicalTopLevelType kind)

-- | Generate simple type variable binders (0-2 params).
genSimpleTyVarBinders :: Gen [TyVarBinder]
genSimpleTyVarBinders = do
  n <- chooseInt (0, 2)
  vectorOf n (TyVarBinder span0 <$> genIdent <*> pure Nothing <*> pure TyVarBSpecified)

-- | Generate a simple type for use in declaration contexts.
genSimpleType :: Gen Type
genSimpleType =
  oneof
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent,
      ( TFun span0 . TVar span0 . mkUnqualifiedName NameVarId
          <$> genIdent
      )
        <*> (TVar span0 . mkUnqualifiedName NameVarId <$> genIdent)
    ]

-- | Generate deriving clauses (0-2).
genDerivingClauses :: Gen [DerivingClause]
genDerivingClauses = do
  n <- frequency [(3, pure 0), (2, pure 1), (1, pure 2)]
  vectorOf n genDerivingClause

genDerivingClause :: Gen DerivingClause
genDerivingClause = do
  strategy <- elements [Nothing, Just DerivingStock]
  n <- chooseInt (0, 3)
  classes <- vectorOf n genSimpleConType
  pure $
    DerivingClause
      { derivingStrategy = strategy,
        derivingClasses = classes,
        derivingViaType = Nothing,
        derivingParenthesized = n /= 1
      }

-- | Generate a simple constructor type (used in deriving/context).
genSimpleConType :: Gen Type
genSimpleConType =
  (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent

-- | Generate a simple constraint context (0-2 constraints).
-- For instance contexts, 0 constraints means no context at all.
genSimpleContext :: Gen [Type]
genSimpleContext = do
  n <- frequency [(3, pure 0), (2, pure 1), (1, pure 2)]
  vectorOf n genSimpleConstraint

-- | Generate an optional context (Nothing or Just [constraints]).
-- Never generates Just [] since that prints as () => which roundtrips
-- to a unit tuple in the constraint list.
genOptionalSimpleContext :: Gen (Maybe [Type])
genOptionalSimpleContext =
  frequency
    [ (3, pure Nothing),
      (1, Just <$> do n <- chooseInt (1, 2); vectorOf n genSimpleConstraint)
    ]

-- | Generate a simple constraint: ClassName tyvar
genSimpleConstraint :: Gen Type
genSimpleConstraint =
  TApp span0
    <$> genSimpleConType
    <*> (TVar span0 . mkUnqualifiedName NameVarId <$> genIdent)

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr _)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr' Nothing)) | expr' <- shrinkExpr expr]
        <> [DeclValue span0 (PatternBind span0 pat' (UnguardedRhs span0 expr Nothing)) | pat' <- shrinkPatternBindPat pat]
    DeclValue _ (FunctionBind _ name [match@Match {matchRhs = UnguardedRhs _ expr _}]) ->
      [ DeclValue
          span0
          ( FunctionBind
              span0
              name
              [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr' Nothing}]
          )
      | expr' <- shrinkExpr expr
      ]
        <> [ DeclValue
               span0
               ( FunctionBind
                   span0
                   name
                   [match {matchSpan = span0, matchPats = pats'}]
               )
           | pats' <- shrinkFunctionHeadPats (matchHeadForm match) (matchPats match)
           ]
        <> [DeclValue span0 (FunctionBind span0 name' [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr Nothing}]) | name' <- shrinkUnqualifiedVarName name]
    DeclTypeSig _ names ty ->
      [DeclTypeSig span0 names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
    _ -> []

shrinkPatternBindPat :: Pattern -> [Pattern]
shrinkPatternBindPat = shrinkPattern

shrinkUnqualifiedVarName :: UnqualifiedName -> [UnqualifiedName]
shrinkUnqualifiedVarName name =
  [mkUnqualifiedName (unqualifiedNameType name) candidate | candidate <- shrinkBinderText name]

shrinkBinderText :: UnqualifiedName -> [Text]
shrinkBinderText name =
  case unqualifiedNameType name of
    NameVarId -> shrinkIdent (renderUnqualifiedName name)
    NameVarSym -> shrinkSymbolicName (renderUnqualifiedName name)
    _ -> []

shrinkSymbolicName :: Text -> [Text]
shrinkSymbolicName txt =
  filter (not . T.null) $
    shrinkList noShrink (T.unpack txt) >>= \chars ->
      let candidate = T.pack chars
       in [candidate | isValidGeneratedOperator candidate]
  where
    noShrink _ = []

shrinkBinderName :: BinderName -> [BinderName]
shrinkBinderName = shrinkUnqualifiedVarName

shrinkFunctionHeadPats :: MatchHeadForm -> [Pattern] -> [[Pattern]]
shrinkFunctionHeadPats headForm pats =
  case headForm of
    MatchHeadPrefix ->
      [ shrunk
      | shrunk <- shrinkList shrinkPattern pats,
        not (null shrunk)
      ]
    MatchHeadInfix ->
      [ canonicalPatternAtom lhs' : canonicalPatternAtom rhs : tailPats
      | lhs : rhs : tailPats <- [pats],
        lhs' <- shrinkPattern lhs
      ]
        <> [ canonicalPatternAtom lhs : canonicalPatternAtom rhs' : tailPats
           | lhs : rhs : tailPats <- [pats],
             rhs' <- shrinkPattern rhs
           ]
        <> [ canonicalPatternAtom lhs : canonicalPatternAtom rhs : shrunkTail
           | lhs : rhs : tailPats <- [pats],
             shrunkTail <- shrinkList shrinkPattern tailPats
           ]
