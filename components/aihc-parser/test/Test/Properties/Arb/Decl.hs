{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Decl
  ( genDecl,
    genDeclDataFamilyInst,
    genDeclTypeFamilyInst,
    genFunctionDecl,
    shrinkDecl,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isAlpha)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Expr (genExpr, shrinkExpr)
import Test.Properties.Arb.Identifiers
  ( genConIdent,
    genConSym,
    genFieldName,
    genIdent,
    genVarSym,
    isValidGeneratedVarSym,
    shrinkConIdent,
    shrinkIdent,
  )
import Test.Properties.Arb.Pattern (canonicalPatternAtom, genPattern, shrinkPattern)
import Test.Properties.Arb.Type (genType, shrinkType)
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
  pure $ DeclValue (PatternBind pat (UnguardedRhs [] expr Nothing))

genPatternBindPattern :: Int -> Gen Pattern
genPatternBindPattern n =
  frequency
    [ (1, PVar . mkUnqualifiedName NameVarId <$> genIdent),
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
            ( FunctionBind
                name
                [ Match
                    { matchAnns = [],
                      matchHeadForm = MatchHeadPrefix,
                      matchPats = pats,
                      matchRhs = UnguardedRhs [] expr Nothing
                    }
                ]
            )
    MatchHeadInfix ->
      do
        lhsPat <- genInfixLhsPattern
        rhsPat <- canonicalPatternAtom <$> sized (genPattern . min 3)
        extraCount <- chooseInt (0, 2)
        extraPats <- vectorOf extraCount (canonicalPatternAtom <$> sized (genPattern . min 3))
        pure $
          DeclValue
            ( FunctionBind
                name
                [ Match
                    { matchAnns = [],
                      matchHeadForm = MatchHeadInfix,
                      matchPats = [lhsPat, rhsPat] <> extraPats,
                      matchRhs = UnguardedRhs [] expr Nothing
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
    PParen inner -> startsWithConstructorNegativeLiteral inner
    _ -> False

genDeclTypeSig :: Gen Decl
genDeclTypeSig = do
  nameCount <- chooseInt (1, 3)
  names <- vectorOf nameCount genVarBinderName
  DeclTypeSig names <$> sized (genType . min 4)

genVarBinderName :: Gen UnqualifiedName
genVarBinderName =
  oneof
    [ mkUnqualifiedName NameVarId <$> genIdent,
      mkUnqualifiedName NameVarSym <$> genVarSym
    ]

genDeclFixity :: Gen Decl
genDeclFixity = do
  assoc <- elements [Infix, InfixL, InfixR]
  prec <- elements [Nothing, Just 0, Just 6, Just 9]
  n <- chooseInt (1, 2)
  ops <- vectorOf n (mkUnqualifiedName NameVarSym <$> genSymbolicOp)
  pure $ DeclFixity assoc Nothing prec ops

genSymbolicOp :: Gen Text
genSymbolicOp = elements ["+", "<>", "&&", "||", "**", "^", ">>"]

genDeclRoleAnnotation :: Gen Decl
genDeclRoleAnnotation = do
  name <- genConIdent
  n <- chooseInt (0, 3)
  roles <- vectorOf n (elements [RoleNominal, RoleRepresentational, RolePhantom, RoleInfer])
  pure $
    DeclRoleAnnotation
      RoleAnnotation
        { roleAnnotationName = mkUnqualifiedName NameConId name,
          roleAnnotationRoles = roles
        }

genDeclTypeSyn :: Gen Decl
genDeclTypeSyn = do
  name <- genConIdent
  params <- genSimpleTyVarBinders
  body <- genSimpleType
  pure $
    DeclTypeSyn
      TypeSynDecl
        { typeSynHeadForm = TypeHeadPrefix,
          typeSynName = mkUnqualifiedName NameConId name,
          typeSynParams = params,
          typeSynBody = body
        }

-- | Generate an infix type synonym, covering both symbolic operators
-- (e.g. @type a :+: b = (a, b)@) and backtick-wrapped identifiers
-- (e.g. @type a \`Plus\` b = (a, b)@).
genDeclTypeSynInfix :: Gen Decl
genDeclTypeSynInfix = do
  name <- oneof [genConSym, genConIdent]
  lhsName <- genIdent
  rhsName <- genIdent
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
  body <- genSimpleType
  pure $
    DeclTypeSyn
      TypeSynDecl
        { typeSynHeadForm = TypeHeadInfix,
          typeSynName = unqualifiedNameFromText name,
          typeSynParams = [lhs, rhs],
          typeSynBody = body
        }

genDeclData :: Gen Decl
genDeclData =
  oneof
    [ DeclData <$> genSimpleDataDecl,
      genDeclDataGadt,
      genDeclDataInfix
    ]

genDeclDataGadt :: Gen Decl
genDeclDataGadt = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctors <- genGadtDataCons
  pure $
    DeclData $
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
genDeclDataInfix :: Gen Decl
genDeclDataInfix = do
  name <- oneof [mkUnqualifiedName NameConSym <$> genConSym, mkUnqualifiedName NameConId <$> genConIdent]
  lhsName <- genIdent
  rhsName <- genIdent
  extraCount <- chooseInt (0, 2)
  extraNames <- vectorOf extraCount genIdent
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
      extraParams = [TyVarBinder [] n Nothing TyVarBSpecified TyVarBVisible | n <- extraNames]
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DeclData $
      DataDecl
        { dataDeclHeadForm = TypeHeadInfix,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = [lhs, rhs] <> extraParams,
          dataDeclKind = Nothing,
          dataDeclConstructors = ctors,
          dataDeclDeriving = deriving'
        }

genDeclTypeData :: Gen Decl
genDeclTypeData = genDeclTypeDataPrefix

genDeclTypeDataPrefix :: Gen Decl
genDeclTypeDataPrefix = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
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
genTypeDataCons = do
  n <- chooseInt (0, 3)
  vectorOf n genTypeDataCon
  where
    genTypeDataCon = do
      conName <- mkUnqualifiedName NameConId <$> genConIdent
      n <- chooseInt (0, 3)
      -- Type data constructors don't support strictness annotations
      fields <- vectorOf n genNonStrictBangType
      pure (PrefixCon [] [] conName fields)

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
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  ctors <- genSimpleDataCons
  deriving' <- genDerivingClauses
  pure $
    DataDecl
      { dataDeclHeadForm = TypeHeadPrefix,
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
  pure (PrefixCon [] [] name fields)

genInfixCon :: Gen DataConDecl
genInfixCon = do
  -- Infix constructors can be symbolic (:+) or alphabetic (`Cons`)
  opName <-
    oneof
      [ mkUnqualifiedName NameConSym <$> genConSym,
        mkUnqualifiedName NameConId <$> genConIdent
      ]
  lhs <- genSimpleBangTypeWithoutFun
  InfixCon [] [] lhs opName <$> genSimpleBangTypeWithoutFun

genRecordCon :: Gen DataConDecl
genRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  n <- chooseInt (0, 3)
  fields <- vectorOf n genFieldDecl
  pure (RecordCon [] [] conName fields)

genFieldDecl :: Gen FieldDecl
genFieldDecl = do
  fieldCount <- chooseInt (1, 3)
  fieldNames <- vectorOf fieldCount genRecordFieldName
  FieldDecl [] fieldNames <$> genSimpleBangType

genRecordFieldName :: Gen UnqualifiedName
genRecordFieldName =
  mkUnqualifiedName NameVarId <$> genFieldName

genGadtDataCons :: Gen [DataConDecl]
genGadtDataCons = do
  n <- chooseInt (1, 3)
  vectorOf n genGadtCon

genGadtCon :: Gen DataConDecl
genGadtCon = do
  n <- chooseInt (1, 2)
  names <- vectorOf n (mkUnqualifiedName NameConId <$> genConIdent)
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
  result <- sized (genType . min 6)
  pure $ GadtPrefixBody args result

-- | Generate a BangType for GADT prefix body arg position.
-- Does not generate lazy/strict annotations on types that start with symbolic
-- characters (TStar, TTHSplice, TTuple, etc.) since the lexer treats ~! or !*
-- as single operator tokens.
genGadtBangType :: Gen BangType
genGadtBangType = do
  ty <- sized (genType . min 6)
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

-- | Generate a simple type without function types at the top level.
genSimpleTypeWithoutFun :: Gen Type
genSimpleTypeWithoutFun =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent
    ]

genGadtRecordBody :: Gen GadtBody
genGadtRecordBody = do
  n <- chooseInt (1, 3)
  fields <- vectorOf n genGadtFieldDecl
  result <- sized (genType . min 6)
  pure $ GadtRecordBody fields result

-- | Generate a field declaration for GADT record body position.
-- Uses the full type generator since record field types are parsed by typeParser.
genGadtFieldDecl :: Gen FieldDecl
genGadtFieldDecl = do
  fieldName <- mkUnqualifiedName NameVarId <$> genIdent
  ty <- sized (genType . min 6)
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
  name <- mkUnqualifiedName NameConId <$> genConIdent
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
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  ty <- genSimpleType
  pure (PrefixCon [] [] conName [BangType [] NoSourceUnpackedness False False ty])

genNewtypeRecordCon :: Gen DataConDecl
genNewtypeRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genConIdent
  fieldName <- genRecordFieldName
  ty <- genSimpleType
  pure (RecordCon [] [] conName [FieldDecl [] [fieldName] (BangType [] NoSourceUnpackedness False False ty)])

genDeclClass :: Gen Decl
genDeclClass = oneof [genDeclClassPrefix, genDeclClassInfix]

genDeclClassPrefix :: Gen Decl
genDeclClassPrefix = do
  name <- genConIdent
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
      (1, genClassAssociatedTypeItems params)
    ]

genClassTypeSigItem :: Gen ClassDeclItem
genClassTypeSigItem = do
  name <- genVarBinderName
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

genAssociatedTypeFamilyDecl :: [TyVarBinder] -> Gen TypeFamilyDecl
genAssociatedTypeFamilyDecl classParams = do
  name <- genConIdent
  paramCount <- chooseInt (0, min 2 (length classParams))
  params <- take paramCount <$> shuffle classParams
  let headType = TCon (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  pure $
    TypeFamilyDecl
      { typeFamilyDeclHeadForm = TypeHeadPrefix,
        typeFamilyDeclHead = headType,
        typeFamilyDeclParams = params,
        typeFamilyDeclResultSig = Nothing,
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
  name <- genConIdent
  lhsName <- genIdent
  rhsName <- genIdent
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

genDeclInstancePrefix :: Gen Decl
genDeclInstancePrefix = do
  className <- genConIdent
  n <- chooseInt (0, 2)
  types <- vectorOf n genInstanceHeadType
  ctx <- genSimpleContext
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
          instanceDeclItems = []
        }

genDeclInstanceInfix :: Gen Decl
genDeclInstanceInfix = do
  className <- genConIdent
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  ctx <- genSimpleContext
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
          instanceDeclItems = []
        }

genDeclStandaloneDeriving :: Gen Decl
genDeclStandaloneDeriving = oneof [genDeclStandaloneDerivingPrefix, genDeclStandaloneDerivingInfix]

genDeclStandaloneDerivingPrefix :: Gen Decl
genDeclStandaloneDerivingPrefix = do
  className <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent
  n <- chooseInt (0, 2)
  types <- vectorOf n genInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  viaType <- frequency [(3, pure Nothing), (1, Just <$> genDerivingViaType)]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = viaType,
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
  className <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent
  lhs <- genInfixInstanceHeadType
  rhs <- genInfixInstanceHeadType
  strategy <- elements [Nothing, Just DerivingStock, Just DerivingNewtype, Just DerivingAnyclass]
  viaType <- frequency [(3, pure Nothing), (1, Just <$> genDerivingViaType)]
  ctx <- genSimpleContext
  pure $
    DeclStandaloneDeriving $
      StandaloneDerivingDecl
        { standaloneDerivingStrategy = strategy,
          standaloneDerivingViaType = viaType,
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
genInstanceHeadType = suchThat (sized (genType . min 4)) isValidInstanceHeadType

isValidInstanceHeadType :: Type -> Bool
isValidInstanceHeadType ty =
  case ty of
    TStar -> False
    TForall {} -> False
    TContext {} -> False
    TImplicitParam {} -> False
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
    TParen inner -> isInfixInstanceHeadType inner
    _ -> False

genInfixInstanceHeadType :: Gen Type
genInfixInstanceHeadType = suchThat genInstanceHeadType isInfixInstanceHeadType

genDeclDefault :: Gen Decl
genDeclDefault = do
  n <- chooseInt (0, 3)
  types <- vectorOf n (sized (genType . min 4))
  pure $ DeclDefault types

genDeclSplice :: Gen Decl
genDeclSplice = do
  name <- qualifyName Nothing . mkUnqualifiedName NameVarId <$> genIdent
  pure $ DeclSplice (EVar name)

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
  name <- genConIdent
  let headType = TCon (qualifyName Nothing (mkUnqualifiedName NameConId name)) Unpromoted
  params <- genSimpleTyVarBinders
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadPrefix,
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
  (nameType, nameText) <- elements [(NameConSym, genConSym), (NameConId, genConIdent)]
  name <- nameText
  lhsName <- genIdent
  rhsName <- genIdent
  let lhs = TyVarBinder [] lhsName Nothing TyVarBSpecified TyVarBVisible
      rhs = TyVarBinder [] rhsName Nothing TyVarBSpecified TyVarBVisible
      lhsType = TVar (mkUnqualifiedName NameVarId lhsName)
      rhsType = TVar (mkUnqualifiedName NameVarId rhsName)
      headType = TApp (TApp (TCon (qualifyName Nothing (mkUnqualifiedName nameType name)) Unpromoted) lhsType) rhsType
  pure $
    DeclTypeFamilyDecl $
      TypeFamilyDecl
        { typeFamilyDeclHeadForm = TypeHeadInfix,
          typeFamilyDeclHead = headType,
          typeFamilyDeclParams = [lhs, rhs],
          typeFamilyDeclResultSig = Nothing,
          typeFamilyDeclEquations = Nothing
        }

genDeclDataFamilyDecl :: Gen Decl
genDeclDataFamilyDecl = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  params <- genSimpleTyVarBinders
  pure $
    DeclDataFamilyDecl $
      DataFamilyDecl
        { dataFamilyDeclName = name,
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
          typeFamilyInstLhs = TApp (TApp (TCon op Unpromoted) lhsArg) rhsArg,
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
  kind <- genOptionalDataFamilyInstKind
  ctors <- genSimpleDataCons
  pure $
    DeclDataFamilyInst $
      DataFamilyInst
        { dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
          dataFamilyInstConstructors = ctors,
          dataFamilyInstDeriving = []
        }

genDeclDataFamilyInstGadt :: Gen Decl
genDeclDataFamilyInstGadt = do
  head' <- genFamilyLhsType
  kind <- genOptionalDataFamilyInstKind
  ctors <- genGadtDataCons
  pure $
    DeclDataFamilyInst $
      DataFamilyInst
        { dataFamilyInstIsNewtype = False,
          dataFamilyInstForall = [],
          dataFamilyInstHead = head',
          dataFamilyInstKind = kind,
          dataFamilyInstConstructors = ctors,
          dataFamilyInstDeriving = []
        }

genOptionalDataFamilyInstKind :: Gen (Maybe Type)
genOptionalDataFamilyInstKind =
  frequency
    [ (3, pure Nothing),
      (1, Just <$> genDataFamilyInstKind)
    ]

genDataFamilyInstKind :: Gen Type
genDataFamilyInstKind =
  sized (genType . min 6)

-- | Generate a type family LHS: a type constructor applied to an arbitrary type argument.
genFamilyLhsType :: Gen Type
genFamilyLhsType = do
  familyName <- genConIdent
  let familyCon = TCon (qualifyName Nothing (mkUnqualifiedName NameConId familyName)) Unpromoted
  TApp familyCon <$> genFamilyLhsArg

genTypeFamilyInstOperator :: Gen Name
genTypeFamilyInstOperator =
  oneof
    [ qualifyName Nothing . mkUnqualifiedName NameVarSym <$> genFamilyInstVarOperator,
      qualifyName Nothing . mkUnqualifiedName NameConSym <$> genFamilyInstConOperator,
      qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent
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
genFamilyLhsArg = suchThat (sized (genType . min 4)) (not . isStarType)

isStarType :: Type -> Bool
isStarType TStar = True
isStarType _ = False

genDeclPragma :: Gen Decl
genDeclPragma = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  DeclPragma . PragmaInline kind <$> genIdent

genDeclPatSyn :: Gen Decl
genDeclPatSyn = do
  synName <- mkUnqualifiedName NameConId <$> genConIdent
  argName <- genIdent
  conName <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genConIdent
  let args = PatSynPrefixArgs [argName]
      pat = PCon conName [] [PVar (mkUnqualifiedName NameVarId argName)]
  dir <- elements [PatSynBidirectional, PatSynUnidirectional]
  pure $ DeclPatSyn (PatSynDecl synName args pat dir)

genDeclPatSynSig :: Gen Decl
genDeclPatSynSig = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  DeclPatSynSig [name] <$> genSimpleType

genDeclStandaloneKindSig :: Gen Decl
genDeclStandaloneKindSig = do
  name <- mkUnqualifiedName NameConId <$> genConIdent
  kind <- sized (genType . min 6)
  pure $ DeclStandaloneKindSig name kind

-- | Generate simple type variable binders (0-2 params).
genSimpleTyVarBinders :: Gen [TyVarBinder]
genSimpleTyVarBinders = do
  n <- chooseInt (0, 2)
  vectorOf n (TyVarBinder [] <$> genIdent <*> pure Nothing <*> pure TyVarBSpecified <*> pure TyVarBVisible)

-- | Generate a simple type for use in declaration contexts.
genSimpleType :: Gen Type
genSimpleType =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent,
      ( TFun . TVar . mkUnqualifiedName NameVarId
          <$> genIdent
      )
        <*> (TVar . mkUnqualifiedName NameVarId <$> genIdent)
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
  classes <- vectorOf n genDerivingType
  viaType <- frequency [(4, pure Nothing), (1, Just <$> genDerivingViaType)]
  let paren = case (n, viaType) of
        (1, Nothing) -> False
        _ -> True
  pure $
    DerivingClause
      { derivingStrategy = strategy,
        derivingClasses = classes,
        derivingViaType = viaType,
        derivingParenthesized = paren
      }

-- | Generate a type suitable for use in 'deriving via'.
-- Avoids types that the parser wraps in parentheses (implicit params, contexts,
-- foralls, kind sigs) to ensure round-trip fidelity.
genDerivingViaType :: Gen Type
genDerivingViaType =
  frequency
    [ (3, genDerivingTypeAtom),
      (2, genDerivingViaTypeApp),
      (1, TParen <$> genDerivingViaType)
    ]

genDerivingViaTypeApp :: Gen Type
genDerivingViaTypeApp = do
  f <- genSimpleConType
  argCount <- chooseInt (1, 2)
  args <- vectorOf argCount genDerivingTypeAtom
  pure (foldl TApp f args)

-- | Generate a type for use in deriving clauses.
-- Avoids forall/context at top level since deriving clauses don't support those.
-- Also avoids TParen at the top level since the parser strips it when it's
-- the sole deriving class (treating the parens as clause parentheses).
genDerivingType :: Gen Type
genDerivingType =
  frequency
    [ (3, genSimpleConType),
      (2, genTypeAppForDeriving)
    ]

genTypeAppForDeriving :: Gen Type
genTypeAppForDeriving = do
  f <- genSimpleConType
  argCount <- chooseInt (1, 2)
  args <- vectorOf argCount genDerivingTypeAtom
  pure (foldl TApp f args)

genDerivingTypeAtom :: Gen Type
genDerivingTypeAtom =
  oneof
    [ TVar . mkUnqualifiedName NameVarId <$> genIdent,
      genSimpleConType
    ]

-- | Generate a simple constructor type (used in deriving/context).
genSimpleConType :: Gen Type
genSimpleConType =
  (\n -> TCon (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genConIdent

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
    <*> (TVar . mkUnqualifiedName NameVarId <$> genIdent)

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
      -- Collapse to unguarded using the first guard's body
      [UnguardedRhs [] (guardedRhsBody firstGrhs) Nothing | firstGrhs : _ <- [grhss]]
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
    <> [grhs {guardedRhsGuards = gs'} | gs' <- shrinkList (const []) (guardedRhsGuards grhs), not (null gs')]

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
  [ts {typeSynBody = ty'} | ty' <- shrinkType (typeSynBody ts)]
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
  [dc {derivingClasses = cs', derivingParenthesized = length cs' /= 1} | cs' <- shrinkList shrinkType (derivingClasses dc), not (null cs')]

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

shrinkDataFamilyDecl :: DataFamilyDecl -> [DataFamilyDecl]
shrinkDataFamilyDecl df =
  [df {dataFamilyDeclParams = ps'} | ps' <- shrinkTyVarBinders (dataFamilyDeclParams df)]

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
