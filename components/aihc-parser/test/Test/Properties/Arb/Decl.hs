{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Decl
  ( genDecl,
    genDeclClass,
    genDeclDataFamilyInst,
    genDeclTypeFamilyInst,
    genDeclValue,
    genWhereDecls,
    shrinkDecl,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isAlpha)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import {-# SOURCE #-} Test.Properties.Arb.Expr (genRhsWith, shrinkExpr, shrinkGuardQualifier)
import Test.Properties.Arb.Identifiers
  ( genConId,
    genConName,
    genConSym,
    genConUnqualifiedName,
    genVarId,
    genVarName,
    genVarSym,
    genVarUnqualifiedName,
    isValidGeneratedVarSym,
    shrinkIdent,
    shrinkUnqualifiedName,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import Test.Properties.Arb.Type (genType, shrinkType)
import Test.Properties.Arb.Utils (optional, smallList0, smallList1, smallList2)
import Test.QuickCheck

-- | Annotation choices for BangType
data FieldAnnotation = NoAnnotation | StrictAnnotation | LazyAnnotation
  deriving (Eq, Ord, Enum, Bounded)

instance Arbitrary Decl where
  arbitrary = genDecl
  shrink = shrinkDecl

genDecl :: Gen Decl
genDecl =
  scale (`div` 2) $
    oneof
      [ genDeclValue,
        genDeclTypeSig,
        genDeclFixity,
        DeclRoleAnnotation <$> genDeclRoleAnnotation,
        DeclTypeSyn <$> genDeclTypeSyn,
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

genDeclValue :: Gen Decl
genDeclValue =
  DeclValue
    <$> oneof
      [ genFunctionValueDecl,
        genPatternValueDecl
      ]

genFunctionValueDecl :: Gen ValueDecl
genFunctionValueDecl = do
  name <- genVarUnqualifiedName
  rhs <- genRhsWith False
  oneof
    [ do
        pats <- smallList1 genPattern
        pure
          ( FunctionBind
              name
              [ Match
                  { matchAnns = [],
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = pats,
                    matchRhs = rhs
                  }
              ]
          ),
      do
        lhsPat <- genPattern
        rhsPat <- scale (min 3) genPattern
        extraCount <- chooseInt (0, 2)
        extraPats <- vectorOf extraCount (scale (min 3) genPattern)
        pure
          ( FunctionBind
              name
              [ Match
                  { matchAnns = [],
                    matchHeadForm = MatchHeadInfix,
                    matchPats = [lhsPat, rhsPat] <> extraPats,
                    matchRhs = rhs
                  }
              ]
          )
    ]

genPatternValueDecl :: Gen ValueDecl
genPatternValueDecl =
  PatternBind NoMultiplicityTag <$> genPattern <*> genRhsWith False

genWhereDecls :: Gen (Maybe [Decl])
genWhereDecls = optional $ scale (`div` 2) $ listOf genDeclValue

genDeclTypeSig :: Gen Decl
genDeclTypeSig = DeclTypeSig <$> smallList1 genVarUnqualifiedName <*> genType

genDeclFixity :: Gen Decl
genDeclFixity = do
  assoc <- elements [Infix, InfixL, InfixR]
  prec <- elements [Nothing, Just 0, Just 6, Just 9]
  ops <- smallList1 genVarUnqualifiedName
  pure $ DeclFixity assoc Nothing prec ops

genDeclRoleAnnotation :: Gen RoleAnnotation
genDeclRoleAnnotation =
  RoleAnnotation
    <$> genConUnqualifiedName
    <*> smallList0 (elements [RoleNominal, RoleRepresentational, RolePhantom, RoleInfer])

genBinderHead :: Gen UnqualifiedName -> Gen (BinderHead UnqualifiedName)
genBinderHead genName =
  oneof
    [ PrefixBinderHead <$> genName <*> genSimpleTyVarBinders,
      InfixBinderHead <$> genSimpleTyVarBinder <*> genName <*> genSimpleTyVarBinder <*> genSimpleTyVarBinders
    ]

genDeclTypeSyn :: Gen TypeSynDecl
genDeclTypeSyn =
  TypeSynDecl
    <$> genBinderHead genConUnqualifiedName
    <*> genType

genDeclData :: Gen Decl
genDeclData =
  oneof
    [ DeclData <$> genSimpleDataDecl,
      DeclData <$> genDeclDataGadt
    ]

genDeclDataGadt :: Gen DataDecl
genDeclDataGadt = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctors <- genGadtDataCons
  pure $
    DataDecl
      { dataDeclCTypePragma = Nothing,
        dataDeclHead = PrefixBinderHead name params,
        dataDeclContext = [],
        dataDeclKind = Nothing,
        dataDeclConstructors = ctors,
        dataDeclDeriving = []
      }

genDeclTypeData :: Gen Decl
genDeclTypeData = genDeclTypeDataPrefix

genDeclTypeDataPrefix :: Gen Decl
genDeclTypeDataPrefix = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctors <- genTypeDataCons
  pure $
    DeclTypeData $
      DataDecl
        { dataDeclCTypePragma = Nothing,
          dataDeclHead = PrefixBinderHead name params,
          dataDeclContext = [],
          dataDeclKind = Nothing,
          dataDeclConstructors = ctors,
          dataDeclDeriving = []
        }

genTypeDataCons :: Gen [DataConDecl]
genTypeDataCons = smallList0 $ PrefixCon [] [] <$> genConUnqualifiedName <*> smallList0 genNonStrictBangType

-- | Generate a BangType that is never strict (for type data constructors).
-- Type data constructors don't support strictness or lazy annotations.
genNonStrictBangType :: Gen BangType
genNonStrictBangType = do
  ty <- genType
  pure $
    BangType
      { bangAnns = [],
        bangPragmas = [],
        bangStrict = False,
        bangLazy = False,
        bangType = ty
      }

genSimpleDataDecl :: Gen DataDecl
genSimpleDataDecl = do
  head' <- genBinderHead genConUnqualifiedName
  -- GHC does not allow kind annotations (or wildcards) in non-GADT data declarations
  let kind = Nothing
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DataDecl
      { dataDeclCTypePragma = Nothing,
        dataDeclHead = head',
        dataDeclContext = [],
        dataDeclKind = kind,
        dataDeclConstructors = ctors,
        dataDeclDeriving = deriving'
      }

genSimpleDataCons :: Gen [DataConDecl]
genSimpleDataCons = smallList0 genMixedDataCon

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
  name <- genConUnqualifiedName
  fields <- smallList0 genSimpleBangType
  pure (PrefixCon [] [] name fields)

genInfixCon :: Gen DataConDecl
genInfixCon = InfixCon [] [] <$> genSimpleBangType <*> genConUnqualifiedName <*> genSimpleBangType

genRecordCon :: Gen DataConDecl
genRecordCon = RecordCon [] [] <$> genConUnqualifiedName <*> smallList0 genFieldDecl

genFieldDecl :: Gen FieldDecl
genFieldDecl = FieldDecl [] <$> smallList1 genVarUnqualifiedName <*> pure Nothing <*> genSimpleBangType

genGadtDataCons :: Gen [DataConDecl]
genGadtDataCons = smallList1 genGadtCon

genGadtCon :: Gen DataConDecl
genGadtCon = do
  names <- smallList1 genConUnqualifiedName
  GadtCon [] [] names <$> genGadtBody

genGadtBody :: Gen GadtBody
genGadtBody =
  oneof
    [ genGadtPrefixBody,
      genGadtRecordBody
    ]

genGadtPrefixBody :: Gen GadtBody
genGadtPrefixBody = GadtPrefixBody <$> smallList0 genGadtArg <*> genType

genGadtArg :: Gen (BangType, ArrowKind)
genGadtArg = (,) <$> genGadtBangType <*> pure ArrowUnrestricted

-- | Generate a BangType for GADT prefix body arg position.
-- Does not generate lazy/strict annotations on types that start with symbolic
-- characters (TStar, TTHSplice, TTuple, etc.) since the lexer treats ~! or !*
-- as single operator tokens.
genGadtBangType :: Gen BangType
genGadtBangType = do
  ty <- scale (min 6) genType
  -- Only generate lazy/strict annotations on types that start with alphabetic characters
  let canAnnotate = typeStartsWithAlpha ty
  annotation <- if canAnnotate then elements [NoAnnotation, StrictAnnotation, LazyAnnotation] else pure NoAnnotation
  case annotation of
    NoAnnotation -> pure $ BangType [] [] False False ty
    StrictAnnotation -> pure $ BangType [] [] True False ty
    LazyAnnotation -> pure $ BangType [] [] False True ty
  where
    typeStartsWithAlpha :: Type -> Bool
    typeStartsWithAlpha (TVar _) = True
    typeStartsWithAlpha (TCon n _) = let txt = nameText n in not (T.null txt) && isAlpha (T.head txt)
    typeStartsWithAlpha (TParen inner) = typeStartsWithAlpha inner
    typeStartsWithAlpha _ = False

-- | Generate a simple type without function types at the top level.
genSimpleTypeWithoutFun :: Gen Type
genSimpleTypeWithoutFun =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genVarId,
      TCon <$> genConName <*> pure Unpromoted
    ]

genGadtRecordBody :: Gen GadtBody
genGadtRecordBody = GadtRecordBody <$> smallList1 genGadtFieldDecl <*> genType

-- | Generate a field declaration for GADT record body position.
-- Uses the full type generator since record field types are parsed by typeParser.
genGadtFieldDecl :: Gen FieldDecl
genGadtFieldDecl = do
  fieldNames <- smallList1 genVarUnqualifiedName
  FieldDecl [] fieldNames Nothing . BangType [] [] False False <$> genType

genSimpleBangType :: Gen BangType
genSimpleBangType = do
  ty <- genType
  annotation <- elements [NoAnnotation, StrictAnnotation, LazyAnnotation]
  case annotation of
    NoAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = [],
            bangStrict = False,
            bangLazy = False,
            bangType = ty
          }
    StrictAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = [],
            bangStrict = True,
            bangLazy = False,
            bangType = ty
          }
    LazyAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = [],
            bangStrict = False,
            bangLazy = True,
            bangType = ty
          }

genDeclNewtype :: Gen Decl
genDeclNewtype = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctor <- genNewtypeCon
  deriving' <- genDerivingClauses
  pure $
    DeclNewtype $
      NewtypeDecl
        { newtypeDeclCTypePragma = Nothing,
          newtypeDeclHead = PrefixBinderHead name params,
          newtypeDeclContext = [],
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
  conName <- genConUnqualifiedName
  ty <- genType
  pure (PrefixCon [] [] conName [BangType [] [] False False ty])

genNewtypeRecordCon :: Gen DataConDecl
genNewtypeRecordCon = do
  conName <- genConUnqualifiedName
  fieldName <- genVarUnqualifiedName
  ty <- genType
  pure (RecordCon [] [] conName [FieldDecl [] [fieldName] Nothing (BangType [] [] False False ty)])

genDeclClass :: Gen Decl
genDeclClass = oneof [genDeclClassPrefix, genDeclClassInfix]

genDeclClassPrefix :: Gen Decl
genDeclClassPrefix = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctx <- genOptionalSimpleContext
  items <- genClassDeclItems params
  pure $
    DeclClass $
      ClassDecl
        { classDeclContext = ctx,
          classDeclHead = PrefixBinderHead name params,
          classDeclFundeps = [],
          classDeclItems = items
        }

genClassDeclItems :: [TyVarBinder] -> Gen [ClassDeclItem]
genClassDeclItems params =
  smallList0 $
    oneof
      [ genClassTypeSigItem,
        genClassAssociatedTypeDeclItem,
        genClassAssociatedDataDeclItem params,
        ClassItemTypeFamilyDecl <$> genAssociatedTypeFamilyDecl,
        ClassItemDefaultTypeInst <$> genAssociatedTypeDefaultInst
      ]

genClassTypeSigItem :: Gen ClassDeclItem
genClassTypeSigItem = do
  name <- genVarUnqualifiedName
  ClassItemTypeSig [name] <$> genType

genClassAssociatedTypeDeclItem :: Gen ClassDeclItem
genClassAssociatedTypeDeclItem = do
  ClassItemTypeFamilyDecl <$> genAssociatedTypeFamilyDecl

-- genClassAssociatedTypeItems :: [TyVarBinder] -> Gen [ClassDeclItem]
-- genClassAssociatedTypeItems params = do
--   tf <- genAssociatedTypeFamilyDecl
--   mDefault <- genAssociatedTypeDefaultInst tf params
--   pure $ ClassItemTypeFamilyDecl tf : maybe [] (pure . ClassItemDefaultTypeInst) mDefault

genClassAssociatedDataDeclItem :: [TyVarBinder] -> Gen ClassDeclItem
genClassAssociatedDataDeclItem params = do
  df <- genAssociatedDataFamilyDecl params
  pure $ ClassItemDataFamilyDecl df

genAssociatedTypeFamilyDecl :: Gen TypeFamilyDecl
genAssociatedTypeFamilyDecl = do
  name <- genConUnqualifiedName
  params <- smallList0 genSimpleTyVarBinder
  explicitFamilyKeyword <- arbitrary
  let headType = TCon (qualifyName Nothing name) Unpromoted
  pure $
    TypeFamilyDecl
      { typeFamilyDeclHeadForm = TypeHeadPrefix,
        typeFamilyDeclExplicitFamilyKeyword = explicitFamilyKeyword,
        typeFamilyDeclHead = headType,
        typeFamilyDeclParams = params,
        typeFamilyDeclResultSig = Nothing,
        typeFamilyDeclEquations = Nothing
      }

genAssociatedDataFamilyDecl :: [TyVarBinder] -> Gen DataFamilyDecl
genAssociatedDataFamilyDecl classParams = do
  let canUseInfixHead = length classParams >= 2
  infixHead <- if canUseInfixHead then elements [False, True] else pure False
  head' <-
    if infixHead
      then do
        name <- mkUnqualifiedName NameConSym <$> genConSym
        shuffled <- shuffle classParams
        case shuffled of
          lhs : rhs : _ -> pure (InfixBinderHead lhs name rhs [])
          _ -> error "genAssociatedDataFamilyDecl: expected at least two class params"
      else do
        name <- genConUnqualifiedName
        paramCount <- chooseInt (0, min 2 (length classParams))
        params <- take paramCount <$> shuffle classParams
        pure (PrefixBinderHead name params)
  kind <- optional genType
  pure $
    DataFamilyDecl
      { dataFamilyDeclHead = head',
        dataFamilyDeclKind = kind
      }

genAssociatedTypeDefaultInst :: Gen TypeFamilyInst
genAssociatedTypeDefaultInst = do
  lhs <- TCon <$> genConName <*> pure Unpromoted
  rhs <- genType
  pure $
    TypeFamilyInst
      { typeFamilyInstForall = [],
        typeFamilyInstHeadForm = TypeHeadPrefix,
        typeFamilyInstLhs = lhs,
        typeFamilyInstRhs = rhs
      }

genDeclClassInfix :: Gen Decl
genDeclClassInfix = do
  name <- genConUnqualifiedName
  params <- smallList2 genSimpleTyVarBinder
  ctx <- genOptionalSimpleContext
  items <- genClassDeclItems params
  pure $
    DeclClass $
      ClassDecl
        { classDeclContext = ctx,
          classDeclHead =
            case params of
              lhs : rhs : tailParams -> InfixBinderHead lhs name rhs tailParams
              _ -> error "genDeclClassInfix: expected at least two parameters",
          classDeclFundeps = [],
          classDeclItems = items
        }

genDeclInstance :: Gen Decl
genDeclInstance = oneof [genDeclInstancePrefix, genDeclInstanceInfix, genDeclInstanceParenInfix]

genInstanceDeclItems :: Gen [InstanceDeclItem]
genInstanceDeclItems = smallList0 genInstanceAssociatedDataFamilyInstItem

genInstanceAssociatedDataFamilyInstItem :: Gen InstanceDeclItem
genInstanceAssociatedDataFamilyInstItem = do
  inst <- genDataFamilyInstWith (pure Nothing) genFamilyInfixHead genSimpleDataCons
  pure (InstanceItemDataFamilyInst inst)

genDeclInstancePrefix :: Gen Decl
genDeclInstancePrefix = do
  className <- genConName
  types <- smallList0 genInstanceHeadType
  ctx <- genSimpleContext
  items <- if null types then pure [] else genInstanceDeclItems
  pure $
    DeclInstance $
      InstanceDecl
        { instanceDeclPragmas = [],
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclHead = foldl TApp (TCon className Unpromoted) types,
          instanceDeclItems = items
        }

genDeclInstanceInfix :: Gen Decl
genDeclInstanceInfix = do
  className <- genConName
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  ctx <- genSimpleContext
  items <- genInstanceDeclItems
  pure $
    DeclInstance $
      InstanceDecl
        { instanceDeclPragmas = [],
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclHead = TInfix lhs className Unpromoted rhs,
          instanceDeclItems = items
        }

-- | Generate a parenthesized infix instance head with trailing type arguments.
-- Covers syntax like @instance (f \`C\` g) x@ and @instance (c & d) a@.
genDeclInstanceParenInfix :: Gen Decl
genDeclInstanceParenInfix = do
  className <- genConName
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  tailTypes <- smallList1 genInstanceHeadType
  ctx <- genSimpleContext
  items <- genInstanceDeclItems
  pure $
    DeclInstance $
      InstanceDecl
        { instanceDeclPragmas = [],
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclHead = foldl TApp (TParen (TInfix lhs className Unpromoted rhs)) tailTypes,
          instanceDeclItems = items
        }

genDeclStandaloneDeriving :: Gen Decl
genDeclStandaloneDeriving = oneof [genDeclStandaloneDerivingPrefix, genDeclStandaloneDerivingInfix, genDeclStandaloneDerivingParenInfix]

genDeclStandaloneDerivingPrefix :: Gen Decl
genDeclStandaloneDerivingPrefix = do
  className <- genConName
  types <- smallList0 genInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingPragmas = [],
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingHead = foldl TApp (TCon className Unpromoted) types
        }

genDeclStandaloneDerivingInfix :: Gen Decl
genDeclStandaloneDerivingInfix = do
  className <- genConName
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingPragmas = [],
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingHead = TInfix lhs className Unpromoted rhs
        }

-- | Generate a parenthesized infix standalone deriving head with trailing type arguments.
-- Covers syntax like @deriving instance (f \`C\` g) x@.
genDeclStandaloneDerivingParenInfix :: Gen Decl
genDeclStandaloneDerivingParenInfix = do
  className <- genConName
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  tailTypes <- smallList1 genInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingPragmas = [],
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingHead = foldl TApp (TParen (TInfix lhs className Unpromoted rhs)) tailTypes
        }

genInstanceHeadType :: Gen Type
genInstanceHeadType = suchThat (scale (min 4) genType) isValidInstanceHeadType

isValidInstanceHeadType :: Type -> Bool
isValidInstanceHeadType ty =
  case ty of
    TStar -> False
    TForall {} -> False
    TContext {} -> False
    TImplicitParam {} -> False
    TTypeApp {} -> False
    TAnn _ inner -> isValidInstanceHeadType inner
    _ -> True

isInfixInstanceHeadType :: Type -> Bool
isInfixInstanceHeadType ty =
  case ty of
    TVar {} -> True
    TCon {} -> True
    TTypeLit {} -> True
    TStar -> True
    TTuple {} -> True
    TList {} -> True
    TApp fn arg -> isInfixInstanceHeadType fn && isInfixInstanceHeadType arg
    TTypeApp inner _ -> isInfixInstanceHeadType inner
    TParen inner -> isInfixInstanceHeadType inner
    _ -> False

genInfixInstanceHeadType :: Gen Type
genInfixInstanceHeadType = suchThat genInstanceHeadType isInfixInstanceHeadType

genDeclDefault :: Gen Decl
genDeclDefault = DeclDefault <$> smallList0 genType

genDeclSplice :: Gen Decl
genDeclSplice = do
  DeclSplice . EVar <$> genVarName

genDeclForeign :: Gen Decl
genDeclForeign = do
  callConv <- elements [CCall, StdCall, CApi]
  direction <- elements [ForeignImport, ForeignExport]
  -- Safety is only valid for imports, not exports
  safety <- case direction of
    ForeignImport -> elements [Nothing, Just Safe, Just Unsafe]
    ForeignExport -> pure Nothing
  name <- genVarUnqualifiedName
  ty <- genType
  pure $
    DeclForeign $
      ForeignDecl
        { foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = ForeignEntityOmitted,
          foreignName = name,
          foreignType = ty
        }

genDeclTypeFamilyDecl :: Gen Decl
genDeclTypeFamilyDecl = do
  name <- genConId
  let headType = TCon (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  params <- genSimpleTyVarBinders
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadPrefix,
          typeFamilyDeclExplicitFamilyKeyword = True,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclResultSig = Nothing,
          typeFamilyDeclEquations = Nothing
        }

-- | Generate an infix type family declaration, covering both symbolic operators
-- (e.g. @type family a ** b@) and backtick-wrapped identifiers
-- (e.g. @type family a \`And\` b@).
genDeclTypeFamilyDeclInfix :: Gen Decl
genDeclTypeFamilyDeclInfix = do
  (nameType, nameText) <- elements [(NameConSym, genConSym), (NameConId, genConId)]
  name <- nameText
  lhsName <- genVarId
  rhsName <- genVarId
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
      lhsType = TVar (mkUnqualifiedName NameVarId lhsName)
      rhsType = TVar (mkUnqualifiedName NameVarId rhsName)
      headType = TInfix lhsType (qualifyName Nothing (mkUnqualifiedName nameType name)) Unpromoted rhsType
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadInfix,
          typeFamilyDeclExplicitFamilyKeyword = True,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = [lhs, rhs],
          typeFamilyDeclResultSig = Nothing,
          typeFamilyDeclEquations = Nothing
        }

genDeclDataFamilyDecl :: Gen Decl
genDeclDataFamilyDecl = do
  infixHead <- elements [False, True]
  head' <-
    if infixHead
      then do
        name <- mkUnqualifiedName NameConSym <$> genConSym
        params <- smallList2 genSimpleTyVarBinder
        case params of
          lhs : rhs : tailParams -> pure (InfixBinderHead lhs name rhs tailParams)
          _ -> error "genDeclDataFamilyDecl: expected at least two parameters"
      else do
        name <- mkUnqualifiedName NameConId <$> genConId
        PrefixBinderHead name <$> genSimpleTyVarBinders
  pure $
    DeclDataFamilyDecl $
      DataFamilyDecl
        { dataFamilyDeclHead = head',
          dataFamilyDeclKind = Nothing
        }

genDeclTypeFamilyInst :: Gen Decl
genDeclTypeFamilyInst =
  oneof
    [ genDeclTypeFamilyInstPrefix,
      genDeclTypeFamilyInstInfix
    ]

genDeclTypeFamilyInstPrefix :: Gen Decl
genDeclTypeFamilyInstPrefix = do
  lhs <- genFamilyLhsType
  rhs <- genType
  pure $
    DeclTypeFamilyInst $
      TypeFamilyInst
        { typeFamilyInstForall = [],
          typeFamilyInstHeadForm = TypeHeadPrefix,
          typeFamilyInstLhs = lhs,
          typeFamilyInstRhs = rhs
        }

genDeclTypeFamilyInstInfix :: Gen Decl
genDeclTypeFamilyInstInfix = do
  op <- genTypeFamilyInstOperator
  lhsArg <- genFamilyInfixOperand
  rhsArg <- genFamilyInfixOperand
  rhs <- genType
  pure $
    DeclTypeFamilyInst $
      TypeFamilyInst
        { typeFamilyInstForall = [],
          typeFamilyInstHeadForm = TypeHeadInfix,
          typeFamilyInstLhs = TInfix lhsArg op Unpromoted rhsArg,
          typeFamilyInstRhs = rhs
        }

genDeclDataFamilyInst :: Gen Decl
genDeclDataFamilyInst =
  oneof
    [ genDeclDataFamilyInstPrefix,
      genDeclDataFamilyInstInfix,
      genDeclDataFamilyInstGadt
    ]

genDataFamilyInstWith :: Gen (Maybe Type) -> Gen Type -> Gen [DataConDecl] -> Gen DataFamilyInst
genDataFamilyInstWith genKind genHead genConstructors = do
  head' <- genHead
  kind <- genKind
  ctors <- genConstructors
  pure $
    DataFamilyInst
      { dataFamilyInstIsNewtype = False,
        dataFamilyInstForall = [],
        dataFamilyInstHead = head',
        dataFamilyInstKind = kind,
        dataFamilyInstConstructors = ctors,
        dataFamilyInstDeriving = []
      }

genDeclDataFamilyInstPrefix :: Gen Decl
genDeclDataFamilyInstPrefix = do
  -- Kind annotations are not valid in non-GADT data instance declarations
  DeclDataFamilyInst <$> genDataFamilyInstWith (pure Nothing) genFamilyLhsType genSimpleDataCons

genDeclDataFamilyInstInfix :: Gen Decl
genDeclDataFamilyInstInfix =
  -- Kind annotations are not valid in non-GADT data instance declarations
  DeclDataFamilyInst <$> genDataFamilyInstWith (pure Nothing) genFamilyInfixHead genSimpleDataCons

genDeclDataFamilyInstGadt :: Gen Decl
genDeclDataFamilyInstGadt = do
  DeclDataFamilyInst <$> genDataFamilyInstWith genOptionalDataFamilyInstKind genFamilyLhsType genGadtDataCons

genOptionalDataFamilyInstKind :: Gen (Maybe Type)
genOptionalDataFamilyInstKind =
  frequency
    [ (3, pure Nothing),
      (1, Just <$> genDataFamilyInstKind)
    ]

genDataFamilyInstKind :: Gen Type
genDataFamilyInstKind =
  scale (min 6) genType

-- | Generate a type family LHS: a type constructor applied to an arbitrary type argument.
genFamilyLhsType :: Gen Type
genFamilyLhsType = do
  familyName <- genConId
  let familyCon = TCon (qualifyName Nothing (mkUnqualifiedName NameConId familyName)) Unpromoted
  TApp familyCon <$> genFamilyLhsArg

genTypeFamilyInstOperator :: Gen Name
genTypeFamilyInstOperator =
  oneof
    [ qualifyName Nothing . mkUnqualifiedName NameVarSym <$> genFamilyInstVarOperator,
      qualifyName Nothing . mkUnqualifiedName NameConSym <$> genFamilyInstConOperator,
      qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId
    ]

genFamilyInstVarOperator :: Gen Text
genFamilyInstVarOperator =
  suchThat genVarSym (`notElem` ["*", ".", "!", "'"])

genFamilyInstConOperator :: Gen Text
genFamilyInstConOperator =
  suchThat genConSym (`notElem` ["!"])

genFamilyInfixOperand :: Gen Type
genFamilyInfixOperand =
  frequency
    [ (1, genFamilyTypeAtom),
      (3, genFamilyTypeApp)
    ]

genFamilyInfixHead :: Gen Type
genFamilyInfixHead = do
  op <- genTypeFamilyInstOperator
  lhs <- genFamilyInfixOperand
  TInfix lhs op Unpromoted <$> genFamilyInfixOperand

genFamilyTypeApp :: Gen Type
genFamilyTypeApp = do
  f <- genFamilyTypeAtom
  argCount <- chooseInt (1, 2)
  args <- vectorOf argCount genFamilyTypeAtom
  pure (foldl TApp f args)

genFamilyTypeAtom :: Gen Type
genFamilyTypeAtom = genSimpleTypeWithoutFun

genFamilyLhsArg :: Gen Type
genFamilyLhsArg = suchThat (scale (min 4) genType) (not . isStarType)

isStarType :: Type -> Bool
isStarType TStar = True
isStarType _ = False

genDeclPragma :: Gen Decl
genDeclPragma = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  DeclPragma . (\pt -> Pragma {pragmaType = pt, pragmaRawText = ""}) . PragmaInline kind <$> genVarId

genDeclPatSyn :: Gen Decl
genDeclPatSyn = do
  synName <- mkUnqualifiedName NameConId <$> genConId
  argName <- genVarId
  conName <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId
  let args = PatSynPrefixArgs [argName]
      pat = PCon conName [] [PVar (mkUnqualifiedName NameVarId argName)]
  dir <- elements [PatSynBidirectional, PatSynUnidirectional]
  pure $ DeclPatSyn (PatSynDecl synName args pat dir)

genDeclPatSynSig :: Gen Decl
genDeclPatSynSig = do
  name <- mkUnqualifiedName NameConId <$> genConId
  DeclPatSynSig [name] <$> genType

genDeclStandaloneKindSig :: Gen Decl
genDeclStandaloneKindSig = DeclStandaloneKindSig <$> genConUnqualifiedName <*> genType

genSimpleTyVarBinder :: Gen TyVarBinder
genSimpleTyVarBinder = TyVarBinder [] <$> genVarId <*> pure Nothing <*> pure TyVarBSpecified <*> pure TyVarBVisible

-- | Generate simple type variable binders (0-2 params).
genSimpleTyVarBinders :: Gen [TyVarBinder]
genSimpleTyVarBinders =
  smallList0 genSimpleTyVarBinder

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
  (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConId

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
  TApp
    <$> genSimpleConType
    <*> (TVar . mkUnqualifiedName NameVarId <$> genVarId)

-- ---------------------------------------------------------------------------
-- Shrinking declarations
-- ---------------------------------------------------------------------------

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclAnn _ inner -> inner : shrinkDecl inner
    DeclValue vd -> map DeclValue (shrinkValueDecl vd)
    DeclTypeSig names ty ->
      [DeclTypeSig names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
        <> [DeclTypeSig names ty' | ty' <- shrinkType ty]
    DeclPatSyn ps ->
      [DeclPatSyn ps' | ps' <- shrinkPatSynDecl ps]
    DeclPatSynSig names ty ->
      [DeclPatSynSig names' ty | names' <- shrinkList shrinkConName names, not (null names')]
        <> [DeclPatSynSig names ty' | ty' <- shrinkType ty]
    DeclStandaloneKindSig name ty ->
      [DeclStandaloneKindSig name' ty | name' <- shrinkConName name]
        <> [DeclStandaloneKindSig name ty' | ty' <- shrinkType ty]
    DeclFixity assoc ns prec ops ->
      [DeclFixity assoc ns prec ops' | ops' <- shrinkList (const []) ops, not (null ops')]
    DeclRoleAnnotation ra ->
      [DeclRoleAnnotation ra' | ra' <- shrinkRoleAnnotation ra]
    DeclTypeSyn ts ->
      [DeclTypeSyn ts' | ts' <- shrinkTypeSynDecl ts]
    DeclData dd ->
      [DeclData dd' | dd' <- shrinkDataDecl dd]
    DeclTypeData dd ->
      [DeclTypeData dd' | dd' <- shrinkDataDecl dd]
    DeclNewtype nd ->
      [DeclNewtype nd' | nd' <- shrinkNewtypeDecl nd]
    DeclClass cd ->
      [DeclClass cd' | cd' <- shrinkClassDecl cd]
    DeclInstance inst ->
      [DeclInstance inst' | inst' <- shrinkInstanceDecl inst]
    DeclStandaloneDeriving sd ->
      [DeclStandaloneDeriving sd' | sd' <- shrinkStandaloneDerivingDecl sd]
    DeclDefault types ->
      [DeclDefault types' | types' <- shrinkList shrinkType types]
    DeclSplice expr ->
      [DeclSplice expr' | expr' <- shrinkExpr expr]
    DeclForeign fd ->
      [DeclForeign fd' | fd' <- shrinkForeignDecl fd]
    DeclTypeFamilyDecl tf ->
      [DeclTypeFamilyDecl tf' | tf' <- shrinkTypeFamilyDecl tf]
    DeclDataFamilyDecl df ->
      [DeclDataFamilyDecl df' | df' <- shrinkDataFamilyDecl df]
    DeclTypeFamilyInst tfi ->
      [DeclTypeFamilyInst tfi' | tfi' <- shrinkTypeFamilyInst tfi]
    DeclDataFamilyInst dfi ->
      [DeclDataFamilyInst dfi' | dfi' <- shrinkDataFamilyInst dfi]
    DeclPragma _ -> []

-- ---------------------------------------------------------------------------
-- Value declarations (function binds and pattern binds)
-- ---------------------------------------------------------------------------

shrinkValueDecl :: ValueDecl -> [ValueDecl]
shrinkValueDecl vd =
  case vd of
    PatternBind multTag pat rhs ->
      [PatternBind multTag pat rhs' | rhs' <- shrinkRhs rhs]
        <> [PatternBind multTag pat' rhs | pat' <- shrinkPattern pat]
    FunctionBind name matches ->
      -- Shrink multiple matches to a single match
      [FunctionBind name [m {matchAnns = []}] | length matches > 1, m <- matches]
        -- Shrink the list of matches
        <> [FunctionBind name ms' | ms' <- shrinkList shrinkMatch matches, not (null ms')]
        -- Shrink the function name
        <> [FunctionBind name' matches | name' <- shrinkBinderName name]

-- | Shrink an individual match clause.
shrinkMatch :: Match -> [Match]
shrinkMatch match =
  -- Shrink the RHS
  [match {matchAnns = [], matchRhs = rhs'} | rhs' <- shrinkRhs (matchRhs match)]
    -- Shrink the patterns
    <> [match {matchAnns = [], matchPats = pats'} | pats' <- shrinkFunctionHeadPats (matchHeadForm match) (matchPats match)]

-- ---------------------------------------------------------------------------
-- Right-hand sides
-- ---------------------------------------------------------------------------

-- | Shrink an RHS: try removing the where clause, simplifying guards to
-- unguarded, and recursively shrinking sub-expressions.
shrinkRhs :: Rhs Expr -> [Rhs Expr]
shrinkRhs rhs =
  case rhs of
    UnguardedRhs _ expr mWhere ->
      -- Drop the where clause first (big win)
      [UnguardedRhs [] expr Nothing | isJust mWhere]
        -- Shrink the expression
        <> [UnguardedRhs [] expr' mWhere | expr' <- shrinkExpr expr]
        -- Shrink the where clause
        <> [UnguardedRhs [] expr (Just ds') | Just ds <- [mWhere], ds' <- shrinkWhereDecls ds]
    GuardedRhss _ grhss mWhere ->
      -- Collapse to unguarded keeping the where clause (try both with and without)
      [UnguardedRhs [] (guardedRhsBody firstGrhs) mWhere | firstGrhs : _ <- [grhss]]
        <> [UnguardedRhs [] (guardedRhsBody firstGrhs) Nothing | firstGrhs : _ <- [grhss]]
        -- Drop the where clause
        <> [GuardedRhss [] grhss Nothing | isJust mWhere]
        -- Shrink the guard list (keep at least one)
        <> [GuardedRhss [] grhss' mWhere | grhss' <- shrinkList shrinkGuardedRhs grhss, not (null grhss')]
        -- Shrink the where clause
        <> [GuardedRhss [] grhss (Just ds') | Just ds <- [mWhere], ds' <- shrinkWhereDecls ds]

-- | Shrink a where-clause declaration list (keep at least one decl).
shrinkWhereDecls :: [Decl] -> [[Decl]]
shrinkWhereDecls ds =
  [ds' | ds' <- shrinkList shrinkDecl ds, not (null ds')]

-- | Shrink a guarded RHS: shrink the body and the guards.
shrinkGuardedRhs :: GuardedRhs Expr -> [GuardedRhs Expr]
shrinkGuardedRhs grhs =
  [grhs {guardedRhsBody = body'} | body' <- shrinkExpr (guardedRhsBody grhs)]
    <> [grhs {guardedRhsGuards = gs'} | gs' <- shrinkList shrinkGuardQualifier (guardedRhsGuards grhs), not (null gs')]

-- ---------------------------------------------------------------------------
-- Pattern synonyms
-- ---------------------------------------------------------------------------

shrinkPatSynDecl :: PatSynDecl -> [PatSynDecl]
shrinkPatSynDecl ps =
  [ps {patSynDeclPat = pat'} | pat' <- shrinkPattern (patSynDeclPat ps)]
    <> [ps {patSynDeclName = name'} | name' <- shrinkConName (patSynDeclName ps)]

-- ---------------------------------------------------------------------------
-- Type synonyms
-- ---------------------------------------------------------------------------

shrinkTypeSynDecl :: TypeSynDecl -> [TypeSynDecl]
shrinkTypeSynDecl ts =
  [ts {typeSynHead = head'} | head' <- shrinkBinderHeadName shrinkUnqualifiedName (typeSynHead ts)]
    <> [ts {typeSynBody = ty'} | ty' <- shrinkType (typeSynBody ts)]
    <> [ts {typeSynHead = head'} | head' <- shrinkBinderHeadParams (typeSynHead ts)]

-- ---------------------------------------------------------------------------
-- Data declarations
-- ---------------------------------------------------------------------------

shrinkDataDecl :: DataDecl -> [DataDecl]
shrinkDataDecl dd =
  -- Shrink constructors
  [dd {dataDeclConstructors = cs'} | cs' <- shrinkList shrinkDataConDecl (dataDeclConstructors dd)]
    -- Shrink kind
    <> [dd {dataDeclKind = Just ty'} | Just ty <- [dataDeclKind dd], ty' <- shrinkType ty]
    -- Shrink head
    <> [dd {dataDeclHead = head'} | head' <- shrinkBinderHeadName shrinkUnqualifiedName (dataDeclHead dd)]
    -- Shrink deriving clauses
    <> [dd {dataDeclDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (dataDeclDeriving dd)]
    -- Shrink type parameters
    <> [dd {dataDeclHead = head'} | head' <- shrinkBinderHeadParams (dataDeclHead dd)]
    -- Shrink context
    <> [dd {dataDeclContext = ctx'} | ctx' <- shrinkList shrinkType (dataDeclContext dd)]

shrinkNewtypeDecl :: NewtypeDecl -> [NewtypeDecl]
shrinkNewtypeDecl nd =
  -- Shrink deriving
  [nd {newtypeDeclDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (newtypeDeclDeriving nd)]
    -- Shrink type parameters
    <> [nd {newtypeDeclHead = head'} | head' <- shrinkBinderHeadParams (newtypeDeclHead nd)]
    -- Shrink context
    <> [nd {newtypeDeclContext = ctx'} | ctx' <- shrinkList shrinkType (newtypeDeclContext nd)]

shrinkDataConDecl :: DataConDecl -> [DataConDecl]
shrinkDataConDecl con =
  case con of
    DataConAnn _ inner -> inner : shrinkDataConDecl inner
    PrefixCon forall' ctx name fields ->
      [PrefixCon forall' ctx name fields' | fields' <- shrinkList shrinkBangType fields]
        <> [PrefixCon forall' ctx' name fields | ctx' <- shrinkList shrinkType ctx]
    InfixCon forall' ctx lhs name rhs ->
      [InfixCon forall' ctx lhs' name rhs | lhs' <- shrinkBangType lhs]
        <> [InfixCon forall' ctx lhs name rhs' | rhs' <- shrinkBangType rhs]
        <> [InfixCon forall' ctx' lhs name rhs | ctx' <- shrinkList shrinkType ctx]
    RecordCon forall' ctx name fields ->
      [RecordCon forall' ctx name' fields | name' <- shrinkUnqualifiedName name]
        <> [RecordCon forall' ctx name fields' | fields' <- shrinkList shrinkFieldDecl fields]
        <> [RecordCon forall' ctx' name fields | ctx' <- shrinkList shrinkType ctx]
    GadtCon forall' ctx names body ->
      [GadtCon forall' ctx names' body | names' <- shrinkList (const []) names, not (null names')]
        <> [GadtCon forall' ctx names body' | body' <- shrinkGadtBody body]
        <> [GadtCon forall' ctx' names body | ctx' <- shrinkList shrinkType ctx]
        <> [GadtCon forall'' ctx names body | forall'' <- shrinkForallTelescopes forall']
    TupleCon forall' ctx flavor fields ->
      [TupleCon forall' ctx flavor fields' | fields' <- shrinkList shrinkBangType fields]
        <> [TupleCon forall' ctx' flavor fields | ctx' <- shrinkList shrinkType ctx]
    UnboxedSumCon forall' ctx pos arity field ->
      [UnboxedSumCon forall' ctx pos arity field' | field' <- shrinkBangType field]
        <> [UnboxedSumCon forall' ctx' pos arity field | ctx' <- shrinkList shrinkType ctx]
    ListCon forall' ctx ->
      [ListCon forall' ctx' | ctx' <- shrinkList shrinkType ctx]

shrinkGadtBody :: GadtBody -> [GadtBody]
shrinkGadtBody body =
  case body of
    GadtPrefixBody args result ->
      [GadtPrefixBody args' result | args' <- shrinkList (\(bt, ak) -> map (,ak) (shrinkBangType bt)) args]
        <> [GadtPrefixBody args result' | result' <- shrinkType result]
    GadtRecordBody fields result ->
      [GadtRecordBody fields' result | fields' <- shrinkList shrinkFieldDecl fields, not (null fields')]
        <> [GadtRecordBody fields result' | result' <- shrinkType result]

shrinkBangType :: BangType -> [BangType]
shrinkBangType bt =
  [bt {bangType = ty'} | ty' <- shrinkType (bangType bt)]

shrinkFieldDecl :: FieldDecl -> [FieldDecl]
shrinkFieldDecl fd =
  [fd {fieldNames = ns'} | ns' <- shrinkList (const []) (fieldNames fd), not (null ns')]
    <> [fd {fieldType = bt'} | bt' <- shrinkBangType (fieldType fd)]

shrinkDerivingClause :: DerivingClause -> [DerivingClause]
shrinkDerivingClause dc =
  [dc {derivingClasses = cs'} | cs' <- shrinkList shrinkType (derivingClasses dc)]

-- ---------------------------------------------------------------------------
-- Class and instance declarations
-- ---------------------------------------------------------------------------

shrinkClassDecl :: ClassDecl -> [ClassDecl]
shrinkClassDecl cd =
  [cd {classDeclHead = head'} | head' <- shrinkBinderHeadName shrinkConName (classDeclHead cd)]
    <> [cd {classDeclItems = is'} | is' <- shrinkList (const []) (classDeclItems cd)]
    <> [cd {classDeclHead = head'} | head' <- shrinkBinderHeadParams (classDeclHead cd)]
    <> [cd {classDeclContext = ctx'} | Just ctx <- [classDeclContext cd], ctx' <- Nothing : [Just ctx'' | ctx'' <- shrinkList shrinkType ctx]]

shrinkInstanceDecl :: InstanceDecl -> [InstanceDecl]
shrinkInstanceDecl inst =
  [inst {instanceDeclItems = is'} | is' <- shrinkList (const []) (instanceDeclItems inst)]
    <> [inst {instanceDeclHead = head'} | head' <- shrinkType (instanceDeclHead inst)]
    <> [inst {instanceDeclContext = ctx'} | ctx' <- shrinkList shrinkType (instanceDeclContext inst)]

-- ---------------------------------------------------------------------------
-- Standalone deriving
-- ---------------------------------------------------------------------------

shrinkStandaloneDerivingDecl :: StandaloneDerivingDecl -> [StandaloneDerivingDecl]
shrinkStandaloneDerivingDecl sd =
  [sd {standaloneDerivingHead = head'} | head' <- shrinkType (standaloneDerivingHead sd)]
    <> [sd {standaloneDerivingContext = ctx'} | ctx' <- shrinkList shrinkType (standaloneDerivingContext sd)]

-- ---------------------------------------------------------------------------
-- Foreign declarations
-- ---------------------------------------------------------------------------

shrinkForeignDecl :: ForeignDecl -> [ForeignDecl]
shrinkForeignDecl fd =
  [fd {foreignType = ty'} | ty' <- shrinkType (foreignType fd)]
    <> [fd {foreignName = n'} | n' <- shrinkUnqualifiedName (foreignName fd)]

-- ---------------------------------------------------------------------------
-- Type/data families
-- ---------------------------------------------------------------------------

shrinkTypeFamilyDecl :: TypeFamilyDecl -> [TypeFamilyDecl]
shrinkTypeFamilyDecl tf =
  [tf {typeFamilyDeclParams = ps'} | ps' <- shrinkTypeHeadParams (typeFamilyDeclHeadForm tf) (typeFamilyDeclParams tf)]
    <> [tf {typeFamilyDeclExplicitFamilyKeyword = False} | typeFamilyDeclExplicitFamilyKeyword tf]

shrinkDataFamilyDecl :: DataFamilyDecl -> [DataFamilyDecl]
shrinkDataFamilyDecl df =
  [df {dataFamilyDeclHead = head'} | head' <- shrinkBinderHeadParams (dataFamilyDeclHead df)]

shrinkTypeFamilyInst :: TypeFamilyInst -> [TypeFamilyInst]
shrinkTypeFamilyInst tfi =
  [tfi {typeFamilyInstLhs = lhs'} | lhs' <- shrinkTypeFamilyInstLhs (typeFamilyInstHeadForm tfi) (typeFamilyInstLhs tfi)]
    <> [tfi {typeFamilyInstRhs = rhs'} | rhs' <- shrinkType (typeFamilyInstRhs tfi)]

shrinkTypeFamilyInstLhs :: TypeHeadForm -> Type -> [Type]
shrinkTypeFamilyInstLhs headForm lhs =
  case headForm of
    TypeHeadPrefix -> shrinkType lhs
    TypeHeadInfix ->
      case lhs of
        TApp (TApp op lhsArg) rhsArg ->
          [TApp (TApp op lhsArg') rhsArg | lhsArg' <- shrinkType lhsArg]
            <> [TApp (TApp op lhsArg) rhsArg' | rhsArg' <- shrinkType rhsArg]
        _ -> []

shrinkDataFamilyInst :: DataFamilyInst -> [DataFamilyInst]
shrinkDataFamilyInst dfi =
  [dfi {dataFamilyInstConstructors = cs'} | cs' <- shrinkList shrinkDataConDecl (dataFamilyInstConstructors dfi)]
    <> [dfi {dataFamilyInstHead = h'} | h' <- shrinkType (dataFamilyInstHead dfi)]
    <> [dfi {dataFamilyInstDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (dataFamilyInstDeriving dfi)]

-- ---------------------------------------------------------------------------
-- Role annotations
-- ---------------------------------------------------------------------------

shrinkRoleAnnotation :: RoleAnnotation -> [RoleAnnotation]
shrinkRoleAnnotation ra =
  [ra {roleAnnotationRoles = rs'} | rs' <- shrinkList (const []) (roleAnnotationRoles ra)]
    <> [ra {roleAnnotationName = n'} | n' <- shrinkConName (roleAnnotationName ra)]

-- ---------------------------------------------------------------------------
-- Name shrinking helpers
-- ---------------------------------------------------------------------------

shrinkConName :: UnqualifiedName -> [UnqualifiedName]
shrinkConName = shrinkUnqualifiedName

shrinkBinderName :: BinderName -> [BinderName]
shrinkBinderName = shrinkUnqualifiedVarName

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
       in [candidate | isValidGeneratedVarSym candidate]
  where
    noShrink _ = []

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

shrinkTyVarBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkTyVarBinders = shrinkList shrinkTyVarBinder
  where
    shrinkTyVarBinder tvb =
      [tvb {tyVarBinderName = n'} | n' <- shrinkIdent (tyVarBinderName tvb)]

shrinkForallTelescopes :: [ForallTelescope] -> [[ForallTelescope]]
shrinkForallTelescopes = shrinkList shrinkForallTelescope
  where
    shrinkForallTelescope telescope =
      [ telescope {forallTelescopeBinders = binders'}
      | binders' <- shrinkTyVarBinders (forallTelescopeBinders telescope)
      ]

shrinkTypeHeadParams :: TypeHeadForm -> [TyVarBinder] -> [[TyVarBinder]]
shrinkTypeHeadParams headForm params =
  case headForm of
    TypeHeadPrefix -> shrinkTyVarBinders params
    TypeHeadInfix -> [ps' | ps' <- shrinkTyVarBinders params, length ps' >= 2]

shrinkBinderHeadName :: (name -> [name]) -> BinderHead name -> [BinderHead name]
shrinkBinderHeadName shrinkNameFn head' =
  case head' of
    PrefixBinderHead name params -> [PrefixBinderHead name' params | name' <- shrinkNameFn name]
    InfixBinderHead lhs name rhs tailParams ->
      [InfixBinderHead lhs name' rhs tailParams | name' <- shrinkNameFn name]

shrinkBinderHeadParams :: BinderHead name -> [BinderHead name]
shrinkBinderHeadParams head' =
  case head' of
    PrefixBinderHead name params ->
      [PrefixBinderHead name params' | params' <- shrinkTyVarBinders params]
    InfixBinderHead lhs name rhs tailParams ->
      [ head''
      | params' <- shrinkTypeHeadParams TypeHeadInfix (lhs : rhs : tailParams),
        head'' <- case params' of
          lhs' : rhs' : tailParams' -> [InfixBinderHead lhs' name rhs' tailParams']
          _ -> []
      ]

shrinkFunctionHeadPats :: MatchHeadForm -> [Pattern] -> [[Pattern]]
shrinkFunctionHeadPats headForm pats =
  case headForm of
    MatchHeadPrefix ->
      [ shrunk
      | shrunk <- shrinkList shrinkPattern pats,
        not (null shrunk)
      ]
    MatchHeadInfix ->
      [ lhs' : rhs : tailPats
      | lhs : rhs : tailPats <- [pats],
        lhs' <- shrinkPattern lhs
      ]
        <> [ lhs : rhs' : tailPats
           | lhs : rhs : tailPats <- [pats],
             rhs' <- shrinkPattern rhs
           ]
        <> [ lhs : rhs : shrunkTail
           | lhs : rhs : tailPats <- [pats],
             shrunkTail <- shrinkList shrinkPattern tailPats
           ]
