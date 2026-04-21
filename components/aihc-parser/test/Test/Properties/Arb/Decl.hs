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
    genConSym,
    genConUnqualifiedName,
    genVarId,
    genVarSym,
    genVarUnqualifiedName,
    isValidGeneratedVarSym,
    shrinkConIdent,
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
        DeclTypeSyn <$> genDeclTypeSynInfix,
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
  PatternBind <$> genPattern <*> genRhsWith False

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

genDeclTypeSyn :: Gen TypeSynDecl
genDeclTypeSyn = TypeSynDecl TypeHeadPrefix <$> genConUnqualifiedName <*> genSimpleTyVarBinders <*> genType

-- | Generate an infix type synonym, covering both symbolic operators
-- (e.g. @type a :+: b = (a, b)@) and backtick-wrapped identifiers
-- (e.g. @type a \`Plus\` b = (a, b)@).
genDeclTypeSynInfix :: Gen TypeSynDecl
genDeclTypeSynInfix = TypeSynDecl TypeHeadInfix <$> genConUnqualifiedName <*> smallList2 genSimpleTyVarBinder <*> genType

genDeclData :: Gen Decl
genDeclData =
  oneof
    [ DeclData <$> genSimpleDataDecl,
      DeclData <$> genDeclDataGadt,
      DeclData <$> genDeclDataInfix
    ]

genDeclDataGadt :: Gen DataDecl
genDeclDataGadt = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctors <- genGadtDataCons
  pure $
    DataDecl
      { dataDeclHeadForm = TypeHeadPrefix,
        dataDeclContext = [],
        dataDeclName = name,
        dataDeclParams = params,
        dataDeclKind = Nothing,
        dataDeclConstructors = ctors,
        dataDeclDeriving = []
      }

-- | Generate an infix data declaration with 2-4 type parameters,
-- covering both symbolic operators (e.g. @data (f :+: g) x = ...@)
-- and backtick-wrapped identifiers (e.g. @data (f \`Dot\` g) x = ...@).
genDeclDataInfix :: Gen DataDecl
genDeclDataInfix = do
  name <- genConUnqualifiedName
  params <- smallList2 genSimpleTyVarBinder
  kind <- optional genSimpleType
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DataDecl
      { dataDeclHeadForm = TypeHeadInfix,
        dataDeclContext = [],
        dataDeclName = name,
        dataDeclParams = params,
        dataDeclKind = kind,
        dataDeclConstructors = ctors,
        dataDeclDeriving = deriving'
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
        { dataDeclHeadForm = TypeHeadPrefix,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = params,
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
  ty <- genSimpleType
  pure $
    BangType
      { bangAnns = [],
        bangSourceUnpackedness = NoSourceUnpackedness,
        bangStrict = False,
        bangLazy = False,
        bangType = ty
      }

genSimpleDataDecl :: Gen DataDecl
genSimpleDataDecl = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  kind <- optional genSimpleType
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DataDecl
      { dataDeclHeadForm = TypeHeadPrefix,
        dataDeclContext = [],
        dataDeclName = name,
        dataDeclParams = params,
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
genInfixCon = do
  -- Infix constructors can be symbolic (:+) or alphabetic (`Cons`)
  opName <- genConUnqualifiedName
  lhs <- genSimpleBangType
  InfixCon [] [] lhs opName <$> genSimpleBangType

genRecordCon :: Gen DataConDecl
genRecordCon = RecordCon [] [] <$> genConUnqualifiedName <*> smallList0 genFieldDecl

genFieldDecl :: Gen FieldDecl
genFieldDecl = FieldDecl [] <$> smallList1 genRecordFieldName <*> genSimpleBangType

genRecordFieldName :: Gen UnqualifiedName
genRecordFieldName = genVarUnqualifiedName

-- mkUnqualifiedName NameVarId <$> genFieldName

genGadtDataCons :: Gen [DataConDecl]
genGadtDataCons = smallList1 genGadtCon

genGadtCon :: Gen DataConDecl
genGadtCon = do
  n <- chooseInt (1, 2)
  names <- vectorOf n (mkUnqualifiedName NameConId <$> genConId)
  GadtCon [] [] names <$> genGadtBody

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
  result <- scale (min 6) genType
  pure $ GadtPrefixBody args result

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
    NoAnnotation -> pure $ BangType [] NoSourceUnpackedness False False ty
    StrictAnnotation -> pure $ BangType [] NoSourceUnpackedness True False ty
    LazyAnnotation -> pure $ BangType [] NoSourceUnpackedness False True ty
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
      (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConId
    ]

genGadtRecordBody :: Gen GadtBody
genGadtRecordBody = do
  n <- chooseInt (1, 3)
  fields <- vectorOf n genGadtFieldDecl
  result <- scale (min 6) genType
  pure $ GadtRecordBody fields result

-- | Generate a field declaration for GADT record body position.
-- Uses the full type generator since record field types are parsed by typeParser.
genGadtFieldDecl :: Gen FieldDecl
genGadtFieldDecl = do
  fieldName <- mkUnqualifiedName NameVarId <$> genVarId
  ty <- scale (min 6) genType
  pure $ FieldDecl [] [fieldName] (BangType [] NoSourceUnpackedness False False ty)

genSimpleBangType :: Gen BangType
genSimpleBangType = do
  ty <- genSimpleType
  -- genSimpleType only generates TVar and TCon, which are safe for lazy annotations
  annotation <- elements [NoAnnotation, StrictAnnotation, LazyAnnotation]
  case annotation of
    NoAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = False,
            bangType = ty
          }
    StrictAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = True,
            bangLazy = False,
            bangType = ty
          }
    LazyAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangSourceUnpackedness = NoSourceUnpackedness,
            bangStrict = False,
            bangLazy = True,
            bangType = ty
          }

genDeclNewtype :: Gen Decl
genDeclNewtype = do
  name <- mkUnqualifiedName NameConId <$> genConId
  params <- genSimpleTyVarBinders
  ctor <- genNewtypeCon
  deriving' <- genDerivingClauses
  pure $
    DeclNewtype $
      NewtypeDecl
        { newtypeDeclHeadForm = TypeHeadPrefix,
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
  conName <- mkUnqualifiedName NameConId <$> genConId
  ty <- genSimpleType
  pure (PrefixCon [] [] conName [BangType [] NoSourceUnpackedness False False ty])

genNewtypeRecordCon :: Gen DataConDecl
genNewtypeRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genConId
  fieldName <- genRecordFieldName
  ty <- genSimpleType
  pure (RecordCon [] [] conName [FieldDecl [] [fieldName] (BangType [] NoSourceUnpackedness False False ty)])

genDeclClass :: Gen Decl
genDeclClass = oneof [genDeclClassPrefix, genDeclClassInfix]

genDeclClassPrefix :: Gen Decl
genDeclClassPrefix = do
  name <- genConId
  params <- genSimpleTyVarBinders
  ctx <- genOptionalSimpleContext
  items <- genClassDeclItems params
  pure $
    DeclClass $
      ClassDecl
        { classDeclContext = ctx,
          classDeclHeadForm = TypeHeadPrefix,
          classDeclName = mkUnqualifiedName NameConId name,
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
      (2, (: []) <$> genClassAssociatedDataDeclItem params),
      (1, genClassAssociatedTypeItems params)
    ]

genClassTypeSigItem :: Gen ClassDeclItem
genClassTypeSigItem = do
  name <- genVarUnqualifiedName
  ClassItemTypeSig [name] <$> genSimpleType

genClassAssociatedTypeDeclItem :: [TyVarBinder] -> Gen ClassDeclItem
genClassAssociatedTypeDeclItem params = do
  tf <- genAssociatedTypeFamilyDecl params
  pure $ ClassItemTypeFamilyDecl tf

genClassAssociatedTypeItems :: [TyVarBinder] -> Gen [ClassDeclItem]
genClassAssociatedTypeItems params = do
  tf <- genAssociatedTypeFamilyDecl params
  mDefault <- genAssociatedTypeDefaultInst tf params
  pure $ ClassItemTypeFamilyDecl tf : maybe [] (pure . ClassItemDefaultTypeInst) mDefault

genClassAssociatedDataDeclItem :: [TyVarBinder] -> Gen ClassDeclItem
genClassAssociatedDataDeclItem params = do
  df <- genAssociatedDataFamilyDecl params
  pure $ ClassItemDataFamilyDecl df

genAssociatedTypeFamilyDecl :: [TyVarBinder] -> Gen TypeFamilyDecl
genAssociatedTypeFamilyDecl classParams = do
  name <- genConId
  paramCount <- chooseInt (0, min 2 (length classParams))
  params <- take paramCount <$> shuffle classParams
  explicitFamilyKeyword <- arbitrary
  let headType = TCon (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
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
  (headForm, name, params) <-
    if infixHead
      then do
        name <- mkUnqualifiedName NameConSym <$> genConSym
        shuffled <- shuffle classParams
        case shuffled of
          lhs : rhs : _ -> pure (TypeHeadInfix, name, [lhs, rhs])
          _ -> error "genAssociatedDataFamilyDecl: expected at least two class params"
      else do
        name <- genConUnqualifiedName
        paramCount <- chooseInt (0, min 2 (length classParams))
        params <- take paramCount <$> shuffle classParams
        pure (TypeHeadPrefix, name, params)
  kind <- frequency [(3, pure Nothing), (1, Just <$> genSimpleType)]
  pure $
    DataFamilyDecl
      { dataFamilyDeclHeadForm = headForm,
        dataFamilyDeclName = name,
        dataFamilyDeclParams = params,
        dataFamilyDeclKind = kind
      }

genAssociatedTypeDefaultInst :: TypeFamilyDecl -> [TyVarBinder] -> Gen (Maybe TypeFamilyInst)
genAssociatedTypeDefaultInst tf classParams =
  if null classParams || null (typeFamilyDeclParams tf)
    then pure Nothing
    else frequency [(1, pure Nothing), (3, Just <$> mkDefaultInst)]
  where
    mkDefaultInst = do
      rhs <- genSimpleType
      let argTypes = [TVar (mkUnqualifiedName NameVarId (tyVarBinderName param)) | param <- typeFamilyDeclParams tf]
          lhs = foldl TApp (typeFamilyDeclHead tf) argTypes
      pure $
        TypeFamilyInst
          { typeFamilyInstForall = [],
            typeFamilyInstHeadForm = typeFamilyDeclHeadForm tf,
            typeFamilyInstLhs = lhs,
            typeFamilyInstRhs = rhs
          }

genDeclClassInfix :: Gen Decl
genDeclClassInfix = do
  name <- genConId
  lhsName <- genVarId
  rhsName <- genVarId
  ctx <- genOptionalSimpleContext
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
      params = [lhs, rhs]
  items <- genClassDeclItems params
  pure $
    DeclClass $
      ClassDecl
        { classDeclContext = ctx,
          classDeclHeadForm = TypeHeadInfix,
          classDeclName = mkUnqualifiedName NameConId name,
          classDeclParams = params,
          classDeclFundeps = [],
          classDeclItems = items
        }

genDeclInstance :: Gen Decl
genDeclInstance = oneof [genDeclInstancePrefix, genDeclInstanceInfix]

genInstanceDeclItems :: Gen [InstanceDeclItem]
genInstanceDeclItems =
  frequency
    [ (4, pure []),
      (1, (: []) <$> genInstanceAssociatedDataFamilyInstItem)
    ]

genInstanceAssociatedDataFamilyInstItem :: Gen InstanceDeclItem
genInstanceAssociatedDataFamilyInstItem = do
  inst <- genDataFamilyInstWith genFamilyInfixHead genSimpleDataCons
  pure (InstanceItemDataFamilyInst inst)

genDeclInstancePrefix :: Gen Decl
genDeclInstancePrefix = do
  className <- genConId
  n <- chooseInt (0, 2)
  types <- vectorOf n genInstanceHeadType
  ctx <- genSimpleContext
  items <- if null types then pure [] else genInstanceDeclItems
  pure $
    DeclInstance $
      InstanceDecl
        { instanceDeclOverlapPragma = Nothing,
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclParenthesizedHead = False,
          instanceDeclHeadForm = TypeHeadPrefix,
          instanceDeclClassName = mkUnqualifiedName NameConId className,
          instanceDeclTypes = types,
          instanceDeclItems = items
        }

genDeclInstanceInfix :: Gen Decl
genDeclInstanceInfix = do
  className <- genConId
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  ctx <- genSimpleContext
  items <- genInstanceDeclItems
  pure $
    DeclInstance $
      InstanceDecl
        { instanceDeclOverlapPragma = Nothing,
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = ctx,
          instanceDeclParenthesizedHead = False,
          instanceDeclHeadForm = TypeHeadInfix,
          instanceDeclClassName = mkUnqualifiedName NameConId className,
          instanceDeclTypes = [lhs, rhs],
          instanceDeclItems = items
        }

genDeclStandaloneDeriving :: Gen Decl
genDeclStandaloneDeriving = oneof [genDeclStandaloneDerivingPrefix, genDeclStandaloneDerivingInfix]

genDeclStandaloneDerivingPrefix :: Gen Decl
genDeclStandaloneDerivingPrefix = do
  className <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId
  n <- chooseInt (0, 2)
  types <- vectorOf n genInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingOverlapPragma = Nothing,
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingParenthesizedHead = False,
          standaloneDerivingHeadForm = TypeHeadPrefix,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = types
        }

genDeclStandaloneDerivingInfix :: Gen Decl
genDeclStandaloneDerivingInfix = do
  className <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingOverlapPragma = Nothing,
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = ctx,
          standaloneDerivingParenthesizedHead = False,
          standaloneDerivingHeadForm = TypeHeadInfix,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = [lhs, rhs]
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
    TTypeApp inner _ -> isInfixInstanceHeadType inner
    TParen inner -> isInfixInstanceHeadType inner
    _ -> False

genInfixInstanceHeadType :: Gen Type
genInfixInstanceHeadType = suchThat genInstanceHeadType isInfixInstanceHeadType

genDeclDefault :: Gen Decl
genDeclDefault = do
  n <- chooseInt (0, 3)
  types <- vectorOf n genSimpleType
  pure $ DeclDefault types

genDeclSplice :: Gen Decl
genDeclSplice = do
  name <- qualifyName Nothing . mkUnqualifiedName NameVarId <$> genVarId
  pure $ DeclSplice (EVar name)

genDeclForeign :: Gen Decl
genDeclForeign = do
  callConv <- elements [CCall, StdCall, CApi]
  direction <- elements [ForeignImport, ForeignExport]
  -- Safety is only valid for imports, not exports
  safety <- case direction of
    ForeignImport -> elements [Nothing, Just Safe, Just Unsafe]
    ForeignExport -> pure Nothing
  name <- genVarId
  ty <- genSimpleType
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
  (headForm, name, params) <-
    if infixHead
      then do
        name <- mkUnqualifiedName NameConSym <$> genConSym
        lhs <- TyVarBinder [] <$> genVarId <*> pure Nothing <*> pure TyVarBSpecified <*> pure TyVarBVisible
        rhs <- TyVarBinder [] <$> genVarId <*> pure Nothing <*> pure TyVarBSpecified <*> pure TyVarBVisible
        pure (TypeHeadInfix, name, [lhs, rhs])
      else do
        name <- mkUnqualifiedName NameConId <$> genConId
        params <- genSimpleTyVarBinders
        pure (TypeHeadPrefix, name, params)
  pure $
    DeclDataFamilyDecl $
      DataFamilyDecl
        { dataFamilyDeclHeadForm = headForm,
          dataFamilyDeclName = name,
          dataFamilyDeclParams = params,
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
  rhs <- genFamilyRhsType
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
  rhs <- genFamilyRhsType
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

genDataFamilyInstWith :: Gen Type -> Gen [DataConDecl] -> Gen DataFamilyInst
genDataFamilyInstWith genHead genConstructors = do
  head' <- genHead
  kind <- genOptionalDataFamilyInstKind
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
  DeclDataFamilyInst <$> genDataFamilyInstWith genFamilyLhsType genSimpleDataCons

genDeclDataFamilyInstInfix :: Gen Decl
genDeclDataFamilyInstInfix =
  DeclDataFamilyInst <$> genDataFamilyInstWith genFamilyInfixHead genSimpleDataCons

genDeclDataFamilyInstGadt :: Gen Decl
genDeclDataFamilyInstGadt = do
  DeclDataFamilyInst <$> genDataFamilyInstWith genFamilyLhsType genGadtDataCons

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

genFamilyRhsType :: Gen Type
genFamilyRhsType =
  frequency
    [ (2, genSimpleType),
      (3, genFamilyTypeApp)
    ]

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
  DeclPragma . PragmaInline kind <$> genVarId

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
  DeclPatSynSig [name] <$> genSimpleType

genDeclStandaloneKindSig :: Gen Decl
genDeclStandaloneKindSig = do
  name <- mkUnqualifiedName NameConId <$> genConId
  kind <- scale (min 6) genType
  pure $ DeclStandaloneKindSig name kind

genSimpleTyVarBinder :: Gen TyVarBinder
genSimpleTyVarBinder = TyVarBinder [] <$> genVarId <*> pure Nothing <*> pure TyVarBSpecified <*> pure TyVarBVisible

-- | Generate simple type variable binders (0-2 params).
genSimpleTyVarBinders :: Gen [TyVarBinder]
genSimpleTyVarBinders =
  smallList0 genSimpleTyVarBinder

-- | Generate a simple type for use in declaration contexts.
genSimpleType :: Gen Type
genSimpleType =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genVarId,
      (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConId,
      ( TFun . TVar . mkUnqualifiedName NameVarId
          <$> genVarId
      )
        <*> (TVar . mkUnqualifiedName NameVarId <$> genVarId)
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
    PatternBind pat rhs ->
      [PatternBind pat rhs' | rhs' <- shrinkRhs rhs]
        <> [PatternBind pat' rhs | pat' <- shrinkPattern pat]
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
shrinkRhs :: Rhs -> [Rhs]
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
shrinkGuardedRhs :: GuardedRhs -> [GuardedRhs]
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
  [ts {typeSynName = name'} | name' <- shrinkUnqualifiedName (typeSynName ts)]
    <> [ts {typeSynBody = ty'} | ty' <- shrinkType (typeSynBody ts)]
    <> [ts {typeSynParams = ps'} | ps' <- shrinkTypeHeadParams (typeSynHeadForm ts) (typeSynParams ts)]

-- ---------------------------------------------------------------------------
-- Data declarations
-- ---------------------------------------------------------------------------

shrinkDataDecl :: DataDecl -> [DataDecl]
shrinkDataDecl dd =
  -- Shrink constructors
  [dd {dataDeclConstructors = cs'} | cs' <- shrinkList shrinkDataConDecl (dataDeclConstructors dd)]
    -- Shrink deriving clauses
    <> [dd {dataDeclDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (dataDeclDeriving dd)]
    -- Shrink type parameters
    <> [dd {dataDeclParams = ps'} | ps' <- shrinkTypeHeadParams (dataDeclHeadForm dd) (dataDeclParams dd)]
    -- Shrink context
    <> [dd {dataDeclContext = ctx'} | ctx' <- shrinkList shrinkType (dataDeclContext dd)]

shrinkNewtypeDecl :: NewtypeDecl -> [NewtypeDecl]
shrinkNewtypeDecl nd =
  -- Shrink deriving
  [nd {newtypeDeclDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (newtypeDeclDeriving nd)]
    -- Shrink type parameters
    <> [nd {newtypeDeclParams = ps'} | ps' <- shrinkTyVarBinders (newtypeDeclParams nd)]
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
      [RecordCon forall' ctx name fields' | fields' <- shrinkList shrinkFieldDecl fields]
        <> [RecordCon forall' ctx' name fields | ctx' <- shrinkList shrinkType ctx]
    GadtCon forall' ctx names body ->
      [GadtCon forall' ctx names' body | names' <- shrinkList (const []) names, not (null names')]
        <> [GadtCon forall' ctx names body' | body' <- shrinkGadtBody body]
        <> [GadtCon forall' ctx' names body | ctx' <- shrinkList shrinkType ctx]
        <> [GadtCon forall'' ctx names body | forall'' <- shrinkForallTelescopes forall']

shrinkGadtBody :: GadtBody -> [GadtBody]
shrinkGadtBody body =
  case body of
    GadtPrefixBody args result ->
      [GadtPrefixBody args' result | args' <- shrinkList shrinkBangType args]
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
  [cd {classDeclItems = is'} | is' <- shrinkList (const []) (classDeclItems cd)]
    <> [cd {classDeclParams = ps'} | ps' <- shrinkTypeHeadParams (classDeclHeadForm cd) (classDeclParams cd)]
    <> [cd {classDeclContext = ctx'} | Just ctx <- [classDeclContext cd], ctx' <- Nothing : [Just ctx'' | ctx'' <- shrinkList shrinkType ctx]]

shrinkInstanceDecl :: InstanceDecl -> [InstanceDecl]
shrinkInstanceDecl inst =
  [inst {instanceDeclItems = is'} | is' <- shrinkList (const []) (instanceDeclItems inst)]
    <> [inst {instanceDeclTypes = ts'} | ts' <- shrinkTypeHeadTypes (instanceDeclHeadForm inst) (instanceDeclTypes inst)]
    <> [inst {instanceDeclContext = ctx'} | ctx' <- shrinkList shrinkType (instanceDeclContext inst)]

-- ---------------------------------------------------------------------------
-- Standalone deriving
-- ---------------------------------------------------------------------------

shrinkStandaloneDerivingDecl :: StandaloneDerivingDecl -> [StandaloneDerivingDecl]
shrinkStandaloneDerivingDecl sd =
  [sd {standaloneDerivingTypes = ts'} | ts' <- shrinkList shrinkType (standaloneDerivingTypes sd)]
    <> [sd {standaloneDerivingContext = ctx'} | ctx' <- shrinkList shrinkType (standaloneDerivingContext sd)]

-- ---------------------------------------------------------------------------
-- Foreign declarations
-- ---------------------------------------------------------------------------

shrinkForeignDecl :: ForeignDecl -> [ForeignDecl]
shrinkForeignDecl fd =
  [fd {foreignType = ty'} | ty' <- shrinkType (foreignType fd)]
    <> [fd {foreignName = n'} | n' <- shrinkIdent (foreignName fd)]

-- ---------------------------------------------------------------------------
-- Type/data families
-- ---------------------------------------------------------------------------

shrinkTypeFamilyDecl :: TypeFamilyDecl -> [TypeFamilyDecl]
shrinkTypeFamilyDecl tf =
  [tf {typeFamilyDeclParams = ps'} | ps' <- shrinkTypeHeadParams (typeFamilyDeclHeadForm tf) (typeFamilyDeclParams tf)]
    <> [tf {typeFamilyDeclExplicitFamilyKeyword = False} | typeFamilyDeclExplicitFamilyKeyword tf]

shrinkDataFamilyDecl :: DataFamilyDecl -> [DataFamilyDecl]
shrinkDataFamilyDecl df =
  [df {dataFamilyDeclParams = ps'} | ps' <- shrinkTypeHeadParams (dataFamilyDeclHeadForm df) (dataFamilyDeclParams df)]

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
shrinkConName name =
  case unqualifiedNameType name of
    NameConId -> [mkUnqualifiedName NameConId n' | n' <- shrinkConIdent (renderUnqualifiedName name)]
    _ -> []

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

shrinkTypeHeadTypes :: TypeHeadForm -> [Type] -> [[Type]]
shrinkTypeHeadTypes headForm tys =
  case headForm of
    TypeHeadPrefix -> shrinkList shrinkType tys
    TypeHeadInfix -> [tys' | tys' <- shrinkList shrinkType tys, length tys' >= 2]

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
