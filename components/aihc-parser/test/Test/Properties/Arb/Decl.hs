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
    shrinkFunctionHeadPats,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import {-# SOURCE #-} Test.Properties.Arb.Expr (genRhs, shrinkExpr, shrinkGuardQualifier)
import Test.Properties.Arb.Identifiers
  ( genConId,
    genConName,
    genConSym,
    genConUnqualifiedName,
    genVarId,
    genVarName,
    genVarSym,
    genVarUnqualifiedName,
    shrinkName,
    shrinkUnqualifiedName,
  )
import Test.Properties.Arb.Pattern (genPattern, shrinkPattern)
import {-# SOURCE #-} Test.Properties.Arb.Type (genType, shrinkForallTelescope, shrinkTyVarBinders, shrinkType)
import Test.Properties.Arb.Utils (optional, smallList0, smallList1)
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
      [ DeclValue <$> genDeclValue,
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

genDeclValue :: Gen ValueDecl
genDeclValue =
  oneof
    [ genFunctionValueDecl,
      genPatternValueDecl
    ]

genFunctionValueDecl :: Gen ValueDecl
genFunctionValueDecl = do
  name <- genVarUnqualifiedName
  rhs <- genRhs
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
        rhsPat <- genPattern
        extraPats <- smallList0 genPattern
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
  PatternBind NoMultiplicityTag <$> genPattern <*> genRhs

genWhereDecls :: Gen (Maybe [Decl])
genWhereDecls = optional $ scale (`div` 2) $ listOf (DeclValue <$> genDeclValue)

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
      genRecordCon,
      genTupleCon,
      genUnboxedSumCon,
      genListCon
    ]

genPrefixCon :: Gen DataConDecl
genPrefixCon = do
  -- Prefix constructors can be alphabetic (Cons) or symbolic ((:+))
  name <- genConUnqualifiedName
  fields <- smallList0 genSimpleBangType
  pure (PrefixCon [] [] name fields)

genInfixCon :: Gen DataConDecl
genInfixCon = do
  forallVars <- genDataConTyVarBinders
  ctx <- genDataConContext
  InfixCon forallVars ctx <$> genSimpleBangType <*> genConUnqualifiedName <*> genSimpleBangType

genRecordCon :: Gen DataConDecl
genRecordCon = do
  RecordCon [] [] <$> genConUnqualifiedName <*> smallList0 genFieldDecl

genTupleCon :: Gen DataConDecl
genTupleCon = do
  forallVars <- genDataConTyVarBinders
  ctx <- genDataConContext
  flavor <- elements [Boxed, Unboxed]
  fieldCount <-
    case flavor of
      Boxed -> elements [0, 2, 3]
      Unboxed -> elements [0, 1, 2, 3]
  TupleCon forallVars ctx flavor <$> vectorOf fieldCount genSimpleBangType

genUnboxedSumCon :: Gen DataConDecl
genUnboxedSumCon = do
  forallVars <- genDataConTyVarBinders
  ctx <- genDataConContext
  arity <- chooseInt (2, 4)
  pos <- chooseInt (1, arity)
  UnboxedSumCon forallVars ctx pos arity <$> genSimpleBangType

genListCon :: Gen DataConDecl
genListCon = ListCon <$> genDataConTyVarBinders <*> genDataConContext

genDataConTyVarBinders :: Gen [TyVarBinder]
genDataConTyVarBinders = frequency [(4, pure []), (1, smallList1 genSimpleTyVarBinder)]

genDataConContext :: Gen [Type]
genDataConContext = frequency [(4, pure []), (1, smallList1 genSimpleConstraint)]

genFieldDecl :: Gen FieldDecl
genFieldDecl = FieldDecl [] <$> smallList1 genVarUnqualifiedName <*> genFieldMultiplicity <*> genSimpleBangType

genFieldMultiplicity :: Gen (Maybe Type)
genFieldMultiplicity =
  frequency
    [ (4, pure Nothing),
      (1, Just <$> genSimpleMultiplicityType)
    ]

genSimpleMultiplicityType :: Gen Type
genSimpleMultiplicityType =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genVarId,
      TCon <$> genConName <*> pure Unpromoted
    ]

genGadtDataCons :: Gen [DataConDecl]
genGadtDataCons = smallList1 genGadtCon

genGadtCon :: Gen DataConDecl
genGadtCon = do
  names <- smallList1 genConUnqualifiedName
  foralls <- genGadtForalls
  ctx <- genDataConContext
  GadtCon foralls ctx names <$> genGadtBody

genGadtForalls :: Gen [ForallTelescope]
genGadtForalls = pure []

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
  multiplicity <- genFieldMultiplicity
  FieldDecl [] fieldNames multiplicity . BangType [] [] False False <$> genType

genSimpleBangType :: Gen BangType
genSimpleBangType = do
  annotation <- elements [NoAnnotation, StrictAnnotation, LazyAnnotation]
  (ty, pragmas) <- genBangTypePayload annotation
  case annotation of
    NoAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = pragmas,
            bangStrict = False,
            bangLazy = False,
            bangType = ty
          }
    StrictAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = pragmas,
            bangStrict = True,
            bangLazy = False,
            bangType = ty
          }
    LazyAnnotation ->
      pure $
        BangType
          { bangAnns = [],
            bangPragmas = pragmas,
            bangStrict = False,
            bangLazy = True,
            bangType = ty
          }

genBangTypePayload :: FieldAnnotation -> Gen (Type, [Pragma])
genBangTypePayload StrictAnnotation =
  frequency
    [ (4, fmap withNoBangPragmas genType),
      (1, fmap (withBangPragmas [mkPragma (PragmaUnpack UnpackPragma)]) genSimpleTypeWithoutFun),
      (1, fmap (withBangPragmas [mkPragma (PragmaUnpack NoUnpackPragma)]) genSimpleTypeWithoutFun)
    ]
genBangTypePayload _ = fmap withNoBangPragmas genType

withNoBangPragmas :: Type -> (Type, [Pragma])
withNoBangPragmas ty = (ty, [])

withBangPragmas :: [Pragma] -> Type -> (Type, [Pragma])
withBangPragmas pragmas ty = (ty, pragmas)

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
genDeclClass = do
  name <- genConUnqualifiedName
  params <- genSimpleTyVarBinders
  ctx <- genOptionalSimpleContext
  items <- genClassDeclItems params
  declHead <-
    oneof
      [ pure $ PrefixBinderHead name params,
        InfixBinderHead <$> genSimpleTyVarBinder <*> pure name <*> genSimpleTyVarBinder <*> genSimpleTyVarBinders
      ]
  pure $
    DeclClass $
      ClassDecl
        { classDeclContext = ctx,
          classDeclHead = declHead,
          classDeclFundeps = genClassFundepsFromHead declHead,
          classDeclItems = items
        }

genClassFundepsFromHead :: BinderHead UnqualifiedName -> [FunctionalDependency]
genClassFundepsFromHead head' =
  case binderHeadTyVarNames head' of
    determiner : determined : _ -> [FunctionalDependency [] [determiner] [determined]]
    _ -> []

binderHeadTyVarNames :: BinderHead UnqualifiedName -> [Text]
binderHeadTyVarNames head' =
  case head' of
    PrefixBinderHead _ params -> map tyVarBinderName params
    InfixBinderHead lhs _ rhs tailParams -> map tyVarBinderName (lhs : rhs : tailParams)

genClassDeclItems :: [TyVarBinder] -> Gen [ClassDeclItem]
genClassDeclItems params =
  smallList0 $
    oneof
      [ genClassTypeSigItem,
        genClassDefaultSigItem,
        genClassFixityItem,
        genClassDefaultItem,
        genClassAssociatedTypeDeclItem,
        genClassAssociatedDataDeclItem params,
        ClassItemDefaultTypeInst <$> genAssociatedTypeDefaultInst,
        genClassPragmaItem
      ]

genClassTypeSigItem :: Gen ClassDeclItem
genClassTypeSigItem = ClassItemTypeSig <$> smallList1 genVarUnqualifiedName <*> genType

genClassDefaultSigItem :: Gen ClassDeclItem
genClassDefaultSigItem = do
  name <- genVarUnqualifiedName
  ClassItemDefaultSig name <$> genType

genClassFixityItem :: Gen ClassDeclItem
genClassFixityItem = do
  assoc <- elements [Infix, InfixL, InfixR]
  prec <- elements [Nothing, Just 0, Just 6, Just 9]
  namespace <- elements [Nothing, Just IEEntityNamespaceType, Just IEEntityNamespaceData]
  ops <- smallList1 genVarUnqualifiedName
  pure (ClassItemFixity assoc namespace prec ops)

genClassDefaultItem :: Gen ClassDeclItem
genClassDefaultItem = ClassItemDefault <$> genFunctionValueDecl

genClassAssociatedTypeDeclItem :: Gen ClassDeclItem
genClassAssociatedTypeDeclItem = do
  ClassItemTypeFamilyDecl <$> genAssociatedTypeFamilyDecl

genClassAssociatedDataDeclItem :: [TyVarBinder] -> Gen ClassDeclItem
genClassAssociatedDataDeclItem params = do
  df <- genAssociatedDataFamilyDecl params
  pure $ ClassItemDataFamilyDecl df

genClassPragmaItem :: Gen ClassDeclItem
genClassPragmaItem = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  ClassItemPragma . mkPragma . PragmaInline kind <$> genVarId

genAssociatedTypeFamilyDecl :: Gen TypeFamilyDecl
genAssociatedTypeFamilyDecl = do
  name <- genConUnqualifiedName
  params <- smallList0 genSimpleTyVarBinder
  explicitFamilyKeyword <- arbitrary
  let headType = TCon (qualifyName Nothing name) Unpromoted
  resultSig <- genAssociatedTypeFamilyResultSig params
  pure $
    TypeFamilyDecl
      { typeFamilyDeclHeadForm = TypeHeadPrefix,
        typeFamilyDeclExplicitFamilyKeyword = explicitFamilyKeyword,
        typeFamilyDeclHead = headType,
        typeFamilyDeclParams = params,
        typeFamilyDeclResultSig = resultSig,
        typeFamilyDeclEquations = Nothing
      }

genAssociatedDataFamilyDecl :: [TyVarBinder] -> Gen DataFamilyDecl
genAssociatedDataFamilyDecl classParams = do
  let canUseInfixHead = length classParams >= 2
  infixHead <- if canUseInfixHead then elements [False, True] else pure False
  head' <-
    if infixHead
      then do
        name <- genConUnqualifiedName
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

genTypeFamilyResultSig :: Bool -> [TyVarBinder] -> Gen (Maybe TypeFamilyResultSig)
genTypeFamilyResultSig explicitFamily params =
  frequency $
    [(4, pure Nothing), (1, Just . TypeFamilyKindSig <$> genType)]
      <> [(1, Just <$> genTypeFamilyTyVarSig) | explicitFamily]
      <> [(1, Just <$> genTypeFamilyInjectiveSig params) | not (null params)]

genAssociatedTypeFamilyResultSig :: [TyVarBinder] -> Gen (Maybe TypeFamilyResultSig)
genAssociatedTypeFamilyResultSig params =
  frequency $
    [(4, pure Nothing), (1, Just . TypeFamilyKindSig <$> genType)]
      <> [(1, Just <$> genTypeFamilyInjectiveSig params) | not (null params)]

genTypeFamilyTyVarSig :: Gen TypeFamilyResultSig
genTypeFamilyTyVarSig = TypeFamilyTyVarSig <$> genResultTyVarBinder

genTypeFamilyInjectiveSig :: [TyVarBinder] -> Gen TypeFamilyResultSig
genTypeFamilyInjectiveSig params = do
  result <- genResultTyVarBinder
  let resultName = tyVarBinderName result
      determined = map tyVarBinderName params
  pure $
    TypeFamilyInjectiveSig
      result
      TypeFamilyInjectivity
        { typeFamilyInjectivityAnns = [],
          typeFamilyInjectivityResult = resultName,
          typeFamilyInjectivityDetermined = determined
        }

genResultTyVarBinder :: Gen TyVarBinder
genResultTyVarBinder =
  pure (TyVarBinder [] "r" Nothing TyVarBSpecified TyVarBVisible)

genTypeFamilyEquations :: TypeHeadForm -> Type -> [TyVarBinder] -> Gen (Maybe [TypeFamilyEq])
genTypeFamilyEquations headForm headType params =
  frequency
    [ (4, pure Nothing),
      (1, Just <$> smallList1 (genTypeFamilyEq headForm headType params))
    ]

genTypeFamilyEq :: TypeHeadForm -> Type -> [TyVarBinder] -> Gen TypeFamilyEq
genTypeFamilyEq headForm headType params = do
  lhs <- genTypeFamilyEqLhs headForm headType params
  rhs <- genType
  forallVars <- frequency [(4, pure []), (1, smallList1 genSimpleTyVarBinder)]
  pure $
    TypeFamilyEq
      { typeFamilyEqAnns = [],
        typeFamilyEqForall = forallVars,
        typeFamilyEqHeadForm = headForm,
        typeFamilyEqLhs = lhs,
        typeFamilyEqRhs = rhs
      }

genTypeFamilyEqLhs :: TypeHeadForm -> Type -> [TyVarBinder] -> Gen Type
genTypeFamilyEqLhs headForm headType params =
  case headForm of
    TypeHeadPrefix -> do
      args <- vectorOf (length params) genFamilyLhsArg
      pure (foldl TApp headType args)
    TypeHeadInfix ->
      case headType of
        TInfix _ op promoted _ -> TInfix <$> genFamilyInfixOperand <*> pure op <*> pure promoted <*> genFamilyInfixOperand
        _ -> pure headType

genDeclInstance :: Gen Decl
genDeclInstance = oneof [genDeclInstancePrefix, genDeclInstanceInfix, genDeclInstanceParenInfix]

genInstanceDeclItems :: Gen [InstanceDeclItem]
genInstanceDeclItems =
  smallList0 $
    oneof
      [ InstanceItemBind <$> genFunctionValueDecl,
        InstanceItemTypeSig <$> smallList1 genVarUnqualifiedName <*> genType,
        genInstanceFixityItem,
        InstanceItemTypeFamilyInst <$> genAssociatedTypeDefaultInst,
        genInstanceAssociatedDataFamilyInstItem,
        InstanceItemPragma <$> genInstancePragma
      ]

genInstanceFixityItem :: Gen InstanceDeclItem
genInstanceFixityItem = do
  assoc <- elements [Infix, InfixL, InfixR]
  prec <- elements [Nothing, Just 0, Just 6, Just 9]
  namespace <- elements [Nothing, Just IEEntityNamespaceType, Just IEEntityNamespaceData]
  ops <- smallList1 genVarUnqualifiedName
  pure (InstanceItemFixity assoc namespace prec ops)

genInstancePragma :: Gen Pragma
genInstancePragma = mkPragma . PragmaInline "INLINE" <$> genVarId

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
  strategy <- optional genDerivingStrategy
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
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
  strategy <- optional genDerivingStrategy
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
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
  strategy <- optional genDerivingStrategy
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
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
    TStar {} -> False
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
    TStar {} -> True
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
  direction <- elements [ForeignImport, ForeignExport]
  entity <- genForeignEntity direction
  callConv <-
    case (direction, entity) of
      (ForeignImport, ForeignEntityDynamic) -> pure CCall
      (ForeignImport, ForeignEntityWrapper) -> pure CCall
      (ForeignImport, ForeignEntityAddress {}) -> pure CCall
      (ForeignImport, _) -> elements [CCall, StdCall, CApi, JavaScript]
      (ForeignExport, _) -> elements [CCall, StdCall, CApi, JavaScript]
  -- Safety is only valid for imports, not exports
  safety <- case (direction, entity) of
    (ForeignImport, ForeignEntityDynamic) -> pure Nothing
    (ForeignImport, ForeignEntityWrapper) -> pure Nothing
    (ForeignImport, ForeignEntityStatic {}) -> pure Nothing
    (ForeignImport, ForeignEntityAddress {}) -> pure Nothing
    (ForeignImport, ForeignEntityOmitted) -> optional $ elements [Safe, Unsafe]
    (ForeignImport, ForeignEntityNamed _)
      | callConv == CCall -> optional $ elements [Safe, Unsafe, Interruptible]
      | otherwise -> optional $ elements [Safe, Unsafe]
    (ForeignExport, _) -> pure Nothing
  name <- genVarUnqualifiedName
  ty <- genType
  pure $
    DeclForeign $
      ForeignDecl
        { foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = entity,
          foreignName = name,
          foreignType = ty
        }

genForeignEntity :: ForeignDirection -> Gen ForeignEntitySpec
genForeignEntity direction =
  case direction of
    ForeignImport ->
      oneof
        [ pure ForeignEntityDynamic,
          pure ForeignEntityWrapper,
          ForeignEntityStatic . Just <$> genForeignSymbol,
          ForeignEntityAddress . Just <$> genForeignSymbol,
          ForeignEntityNamed <$> genForeignSymbol,
          pure ForeignEntityOmitted
        ]
    ForeignExport ->
      oneof
        [ ForeignEntityNamed <$> genForeignSymbol,
          pure ForeignEntityOmitted
        ]

genForeignSymbol :: Gen Text
genForeignSymbol = elements ["foreign_symbol", "static_name", "js_name"]

genDeclTypeFamilyDecl :: Gen Decl
genDeclTypeFamilyDecl = do
  name <- genConId
  let headType = TCon (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  params <- genSimpleTyVarBinders
  resultSig <- genTypeFamilyResultSig True params
  equations <- genTypeFamilyEquations TypeHeadPrefix headType params
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadPrefix,
          typeFamilyDeclExplicitFamilyKeyword = True,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclResultSig = resultSig,
          typeFamilyDeclEquations = equations
        }

-- | Generate an infix type family declaration, covering both symbolic operators
-- (e.g. @type family a ** b@) and backtick-wrapped identifiers
-- (e.g. @type family a \`And\` b@).
genDeclTypeFamilyDeclInfix :: Gen Decl
genDeclTypeFamilyDeclInfix = do
  name <- genConUnqualifiedName
  lhsName <- genVarId
  rhsName <- genVarId
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
      lhsType = TVar (mkUnqualifiedName NameVarId lhsName)
      rhsType = TVar (mkUnqualifiedName NameVarId rhsName)
      headType = TInfix lhsType (qualifyName Nothing name) Unpromoted rhsType
      params = [lhs, rhs]
  resultSig <- genTypeFamilyResultSig True params
  equations <- genTypeFamilyEquations TypeHeadInfix headType params
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadInfix,
          typeFamilyDeclExplicitFamilyKeyword = True,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = params,
          typeFamilyDeclResultSig = resultSig,
          typeFamilyDeclEquations = equations
        }

genDeclDataFamilyDecl :: Gen Decl
genDeclDataFamilyDecl = do
  head' <-
    oneof
      [ InfixBinderHead <$> genSimpleTyVarBinder <*> genConUnqualifiedName <*> genSimpleTyVarBinder <*> genSimpleTyVarBinders,
        PrefixBinderHead <$> genConUnqualifiedName <*> genSimpleTyVarBinders
      ]
  kind <- optional genType
  pure $
    DeclDataFamilyDecl $
      DataFamilyDecl
        { dataFamilyDeclHead = head',
          dataFamilyDeclKind = kind
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
  derivingClauses <- genDerivingClauses
  pure $
    DataFamilyInst
      { dataFamilyInstIsNewtype = False,
        dataFamilyInstForall = [],
        dataFamilyInstHead = head',
        dataFamilyInstKind = kind,
        dataFamilyInstConstructors = ctors,
        dataFamilyInstDeriving = derivingClauses
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
genOptionalDataFamilyInstKind = optional genType

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
  args <- smallList1 genFamilyTypeAtom
  pure (foldl TApp f args)

genFamilyTypeAtom :: Gen Type
genFamilyTypeAtom = genSimpleTypeWithoutFun

genFamilyLhsArg :: Gen Type
genFamilyLhsArg = suchThat (scale (min 4) genType) (not . isStarType)

isStarType :: Type -> Bool
isStarType TStar {} = True
isStarType _ = False

genDeclPragma :: Gen Decl
genDeclPragma = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  DeclPragma . mkPragma . PragmaInline kind <$> genVarId

mkPragma :: PragmaType -> Pragma
mkPragma pt = Pragma {pragmaType = pt, pragmaRawText = ""}

genDeclPatSyn :: Gen Decl
genDeclPatSyn = do
  synName <- genConUnqualifiedName
  argName <- genVarId
  conName <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId
  let args = PatSynPrefixArgs [argName]
      pat = PCon conName [] [PVar (mkUnqualifiedName NameVarId argName)]
  dir <- elements [PatSynBidirectional, PatSynUnidirectional]
  pure $ DeclPatSyn (PatSynDecl synName args pat dir)

genDeclPatSynSig :: Gen Decl
genDeclPatSynSig = DeclPatSynSig <$> smallList1 genConUnqualifiedName <*> genType

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
genDerivingClauses = smallList0 genDerivingClause

genDerivingClause :: Gen DerivingClause
genDerivingClause = do
  strategy <- optional genDerivingStrategy
  classes <- oneof [Left <$> genSimpleConName, Right <$> smallList0 genType]
  pure $
    DerivingClause
      { derivingStrategy = strategy,
        derivingClasses = classes
      }

genDerivingStrategy :: Gen DerivingStrategy
genDerivingStrategy =
  oneof [pure DerivingStock, pure DerivingNewtype, pure DerivingAnyclass, DerivingVia <$> genType]

-- GHC does not allow singleton symbolic class names in deriving clauses.
genSimpleConName :: Gen Name
genSimpleConName =
  qualifyName Nothing . mkUnqualifiedName NameConId <$> genConId

-- | Generate a simple constructor type (used in deriving/context).
genSimpleConType :: Gen Type
genSimpleConType =
  (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConId

-- | Generate a simple constraint context (0-2 constraints).
-- For instance contexts, 0 constraints means no context at all.
genSimpleContext :: Gen [Type]
genSimpleContext = smallList0 genSimpleConstraint

-- | Generate an optional context (Nothing or Just [constraints]).
-- Never generates Just [] since that prints as () => which roundtrips
-- to a unit tuple in the constraint list.
genOptionalSimpleContext :: Gen (Maybe [Type])
genOptionalSimpleContext = optional $ smallList1 genSimpleConstraint

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
      [DeclFixity assoc ns prec ops' | ops' <- shrinkList shrinkBinderName ops, not (null ops')]
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
      [PatternBind multTag simpleVarPattern rhs | pat /= PWildcard && not (isSimpleVarPattern pat)]
        <> [PatternBind multTag pat rhs' | rhs' <- shrinkRhs rhs]
        <> [PatternBind multTag pat' rhs | pat' <- shrinkPattern pat]
    FunctionBind name matches ->
      [ PatternBind NoMultiplicityTag (PVar name) (matchRhs match)
      | match <- matches
      ]
        <>
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
      -- Hoist failing where-bound RHSs before shrinking the surrounding layout.
      whereValueRhss (fromMaybe [] mWhere)
        <> [UnguardedRhs [] expr Nothing | isJust mWhere]
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

isSimpleVarPattern :: Pattern -> Bool
isSimpleVarPattern pat =
  case pat of
    PVar name -> unqualifiedNameType name == NameVarId && unqualifiedNameText name == "a"
    _ -> False

simpleVarPattern :: Pattern
simpleVarPattern = PVar (mkUnqualifiedName NameVarId "a")

-- | Shrink a where-clause declaration list (keep at least one decl).
shrinkWhereDecls :: [Decl] -> [[Decl]]
shrinkWhereDecls ds =
  [ds' | ds' <- shrinkList shrinkDecl ds, not (null ds')]

whereValueRhss :: [Decl] -> [Rhs Expr]
whereValueRhss decls =
  [ rhs
  | DeclValue valueDecl <- decls,
    rhs <- case valueDecl of
      PatternBind _ _ patternRhs -> [patternRhs]
      FunctionBind _ matches -> map matchRhs matches
  ]

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
    -- Shrink constructor
    <> [nd {newtypeDeclConstructor = Just ctor'} | Just ctor <- [newtypeDeclConstructor nd], ctor' <- shrinkDataConDecl ctor]
    -- Shrink deriving clauses
    <> [nd {newtypeDeclDeriving = ds'} | ds' <- shrinkList shrinkDerivingClause (newtypeDeclDeriving nd)]

shrinkDataConDecl :: DataConDecl -> [DataConDecl]
shrinkDataConDecl con =
  case con of
    DataConAnn _ inner -> inner : shrinkDataConDecl inner
    PrefixCon forall' ctx name fields ->
      [PrefixCon forall' ctx name fields' | fields' <- shrinkList shrinkBangType fields]
        <> [PrefixCon forall' ctx name' fields | name' <- shrinkUnqualifiedName name]
        <> [PrefixCon forall' ctx' name fields | ctx' <- shrinkList shrinkType ctx]
    InfixCon forall' ctx lhs name rhs ->
      [InfixCon forall' ctx lhs' name rhs | lhs' <- shrinkBangType lhs]
        <> [InfixCon forall' ctx lhs name rhs' | rhs' <- shrinkBangType rhs]
        <> [InfixCon forall' ctx lhs name' rhs | name' <- shrinkUnqualifiedName name]
        <> [InfixCon forall' ctx' lhs name rhs | ctx' <- shrinkList shrinkType ctx]
    RecordCon forall' ctx name fields ->
      [RecordCon forall' ctx name' fields | name' <- shrinkUnqualifiedName name]
        <> [RecordCon forall' ctx name fields' | fields' <- shrinkList shrinkFieldDecl fields]
        <> [RecordCon forall' ctx' name fields | ctx' <- shrinkList shrinkType ctx]
    GadtCon forall' ctx names body ->
      [GadtCon forall' ctx names' body | names' <- shrinkList shrinkUnqualifiedName names, not (null names')]
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
  [fd {fieldNames = ns'} | ns' <- shrinkList shrinkBinderName (fieldNames fd), not (null ns')]
    <> [fd {fieldType = bt'} | bt' <- shrinkBangType (fieldType fd)]

shrinkDerivingClause :: DerivingClause -> [DerivingClause]
shrinkDerivingClause dc =
  [dc {derivingStrategy = Just strategy'} | Just strategy <- [derivingStrategy dc], strategy' <- shrinkDerivingStrategy strategy]
    <> case derivingClasses dc of
      Left name -> [dc {derivingClasses = Left name'} | name' <- shrinkName name]
      Right classes -> [dc {derivingClasses = Right cs'} | cs' <- shrinkList shrinkType classes]

shrinkDerivingStrategy :: DerivingStrategy -> [DerivingStrategy]
shrinkDerivingStrategy strategy =
  case strategy of
    DerivingStock -> []
    DerivingNewtype -> []
    DerivingAnyclass -> []
    DerivingVia ty -> [DerivingVia ty' | ty' <- shrinkType ty]

-- ---------------------------------------------------------------------------
-- Class and instance declarations
-- ---------------------------------------------------------------------------

shrinkClassDecl :: ClassDecl -> [ClassDecl]
shrinkClassDecl cd =
  [cd {classDeclHead = head', classDeclFundeps = genClassFundepsFromHead head'} | head' <- shrinkBinderHeadName shrinkConName (classDeclHead cd)]
    <> [cd {classDeclItems = is'} | is' <- shrinkList shrinkClassDeclItem (classDeclItems cd)]
    <> [cd {classDeclHead = head', classDeclFundeps = genClassFundepsFromHead head'} | head' <- shrinkBinderHeadParams (classDeclHead cd)]
    <> [cd {classDeclContext = ctx'} | Just ctx <- [classDeclContext cd], ctx' <- Nothing : [Just ctx'' | ctx'' <- shrinkList shrinkType ctx]]
    <> [cd {classDeclFundeps = []} | not (null (classDeclFundeps cd))]

shrinkClassDeclItem :: ClassDeclItem -> [ClassDeclItem]
shrinkClassDeclItem item =
  case item of
    ClassItemAnn _ inner -> inner : shrinkClassDeclItem inner
    ClassItemTypeSig names ty ->
      [ClassItemTypeSig names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
        <> [ClassItemTypeSig names ty' | ty' <- shrinkType ty]
    ClassItemDefaultSig name ty ->
      [ClassItemDefaultSig name' ty | name' <- shrinkBinderName name]
        <> [ClassItemDefaultSig name ty' | ty' <- shrinkType ty]
    ClassItemFixity assoc ns prec ops ->
      [ClassItemFixity assoc ns prec ops' | ops' <- shrinkList shrinkBinderName ops, not (null ops')]
        <> [ClassItemFixity assoc Nothing prec ops | isJust ns]
        <> [ClassItemFixity assoc ns Nothing ops | isJust prec]
    ClassItemDefault vd ->
      [ClassItemDefault vd' | vd' <- shrinkClassDefaultDecl vd]
    ClassItemTypeFamilyDecl tf ->
      [ClassItemTypeFamilyDecl tf' | tf' <- shrinkTypeFamilyDecl tf]
    ClassItemDataFamilyDecl df ->
      [ClassItemDataFamilyDecl df' | df' <- shrinkDataFamilyDecl df]
    ClassItemDefaultTypeInst tfi ->
      [ClassItemDefaultTypeInst tfi' | tfi' <- shrinkTypeFamilyInst tfi]
    ClassItemPragma _ -> []

shrinkClassDefaultDecl :: ValueDecl -> [ValueDecl]
shrinkClassDefaultDecl vd =
  case vd of
    PatternBind multTag pat rhs ->
      [PatternBind multTag simpleClassDefaultPattern rhs | pat /= PWildcard && pat /= simpleClassDefaultPattern]
        <> [PatternBind multTag pat rhs' | rhs' <- shrinkRhs rhs]
        <> [PatternBind multTag pat' rhs | pat' <- shrinkPattern pat]
    FunctionBind name matches ->
      [FunctionBind name [m {matchAnns = []}] | length matches > 1, m <- matches]
        <> [FunctionBind name ms' | ms' <- shrinkList shrinkMatch matches, not (null ms')]
        <> [FunctionBind name' matches | name' <- shrinkBinderName name]

simpleClassDefaultPattern :: Pattern
simpleClassDefaultPattern = PVar (mkUnqualifiedName NameVarId "x")

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
shrinkBinderName = shrinkUnqualifiedName

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

shrinkForallTelescopes :: [ForallTelescope] -> [[ForallTelescope]]
shrinkForallTelescopes = shrinkList shrinkForallTelescope

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

shrinkBinderHeadParams :: BinderHead UnqualifiedName -> [BinderHead UnqualifiedName]
shrinkBinderHeadParams head' =
  case head' of
    PrefixBinderHead name params ->
      [PrefixBinderHead name' params | name' <- shrinkUnqualifiedName name]
        <> [PrefixBinderHead name params' | params' <- shrinkTyVarBinders params]
    InfixBinderHead lhs name rhs tailParams ->
      [InfixBinderHead lhs name' rhs tailParams | name' <- shrinkUnqualifiedName name]
        <> [ head''
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
