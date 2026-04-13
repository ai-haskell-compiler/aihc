{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Decl
  ( genDecl,
    genFunctionDecl,
    shrinkDecl,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Expr (genExpr, genOperator, isValidGeneratedOperator, shrinkExpr, span0)
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
import Test.Properties.Arb.Pattern (genPattern)
import Test.Properties.Arb.Type (canonicalFunLeft, canonicalTopLevelType, genType)
import Test.QuickCheck

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
      genDeclDataFamilyDecl,
      genDeclTypeFamilyInst,
      genDeclDataFamilyInst,
      genDeclPragma,
      genDeclPatSyn,
      genDeclPatSynSig,
      genDeclStandaloneKindSig
    ]

genDeclValue :: Int -> Gen Decl
genDeclValue n = do
  name <- genVarBinderName
  expr <- resize n genExpr
  genFunctionDecl (name, expr)

genFunctionDecl :: (UnqualifiedName, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  headForm <- elements [MatchHeadPrefix, MatchHeadInfix]
  case headForm of
    MatchHeadPrefix ->
      do
        patCount <- chooseInt (0, 3)
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
        lhsPat <- sized (genPattern . min 3)
        rhsPat <- sized (genPattern . min 3)
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
  name <- genTypeConName
  n <- chooseInt (0, 3)
  roles <- vectorOf n (elements [RoleNominal, RoleRepresentational, RolePhantom, RoleInfer])
  pure $ DeclRoleAnnotation span0 (RoleAnnotation span0 name roles)

genDeclTypeSyn :: Gen Decl
genDeclTypeSyn = do
  name <- genTypeConName
  params <- genSimpleTyVarBinders
  DeclTypeSyn span0 . TypeSynDecl span0 TypeHeadPrefix name params <$> genSimpleType

-- | Generate an infix type synonym, covering both symbolic operators
-- (e.g. @type a :+: b = (a, b)@) and backtick-wrapped identifiers
-- (e.g. @type a \`Plus\` b = (a, b)@).
genDeclTypeSynInfix :: Gen Decl
genDeclTypeSynInfix = do
  name <- oneof [genConSymName, genTypeConName]
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
  name <- mkUnqualifiedName NameConId <$> genTypeConName
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
  name <- mkUnqualifiedName NameConId <$> genTypeConName
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
      conName <- mkUnqualifiedName NameConId <$> genTypeConName
      n <- chooseInt (0, 3)
      fields <- vectorOf n genSimpleBangType
      pure $ PrefixCon span0 [] [] conName fields

genSimpleDataDecl :: Gen DataDecl
genSimpleDataDecl = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
  params <- genSimpleTyVarBinders
  ctors <- genSimpleDataCons
  pure $
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
      [ mkUnqualifiedName NameConId <$> genTypeConName,
        mkUnqualifiedName NameConSym <$> genConSymName
      ]
  n <- chooseInt (0, 2)
  fields <- vectorOf n genSimpleBangType
  pure $ PrefixCon span0 [] [] name fields

genInfixCon :: Gen DataConDecl
genInfixCon = do
  -- Infix constructors can be symbolic (:+) or alphabetic (`Cons`)
  opName <-
    oneof
      [ mkUnqualifiedName NameConSym <$> genConSymName,
        mkUnqualifiedName NameConId <$> genTypeConName
      ]
  lhs <- genSimpleBangTypeWithoutFun
  InfixCon span0 [] [] lhs opName <$> genSimpleBangTypeWithoutFun

-- | Generate constructor symbol names: colon followed by 1–3 symbol characters.
-- Valid for both type-level and value-level constructor operators.
-- Examples: @:+@, @:*@, @:==@, @:+:@
genConSymName :: Gen Text
genConSymName = do
  symLen <- chooseInt (1, 3)
  syms <- vectorOf symLen (elements ['+', '*', '-', '=', '!', '<', '>', '&', '|'])
  pure (T.pack (':' : syms))

genRecordCon :: Gen DataConDecl
genRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genTypeConName
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
  names <- vectorOf n (mkUnqualifiedName NameConId <$> genTypeConName)
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
genGadtBangType :: Gen BangType
genGadtBangType = do
  ty <- canonicalFunLeft . canonicalTopLevelType <$> sized (genType . min 6)
  pure $ BangType span0 NoSourceUnpackedness False ty

-- | Generate a BangType without function types at the top level.
genSimpleBangTypeWithoutFun :: Gen BangType
genSimpleBangTypeWithoutFun = do
  ty <- genSimpleTypeWithoutFun
  pure $
    BangType
      { bangSpan = span0,
        bangSourceUnpackedness = NoSourceUnpackedness,
        bangStrict = False,
        bangType = ty
      }

-- | Generate a simple type without function types at the top level.
genSimpleTypeWithoutFun :: Gen Type
genSimpleTypeWithoutFun =
  oneof
    [ TVar span0 . mkUnqualifiedName NameVarId <$> genIdent,
      (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genTypeConName
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
  pure $ FieldDecl span0 [fieldName] (BangType span0 NoSourceUnpackedness False ty)

genSimpleBangType :: Gen BangType
genSimpleBangType = do
  ty <- genSimpleType
  pure $
    BangType
      { bangSpan = span0,
        bangSourceUnpackedness = NoSourceUnpackedness,
        bangStrict = False,
        bangType = ty
      }

genDeclNewtype :: Gen Decl
genDeclNewtype = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
  params <- genSimpleTyVarBinders
  ctor <- genNewtypeCon
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
          newtypeDeclDeriving = []
        }

genNewtypeCon :: Gen DataConDecl
genNewtypeCon =
  oneof
    [ genNewtypePrefixCon,
      genNewtypeRecordCon
    ]

genNewtypePrefixCon :: Gen DataConDecl
genNewtypePrefixCon = do
  conName <- mkUnqualifiedName NameConId <$> genTypeConName
  ty <- genSimpleType
  pure $ PrefixCon span0 [] [] conName [BangType span0 NoSourceUnpackedness False ty]

genNewtypeRecordCon :: Gen DataConDecl
genNewtypeRecordCon = do
  conName <- mkUnqualifiedName NameConId <$> genTypeConName
  fieldName <- genVarBinderName
  ty <- genSimpleType
  pure $ RecordCon span0 [] [] conName [FieldDecl span0 [fieldName] (BangType span0 NoSourceUnpackedness False ty)]

genDeclClass :: Gen Decl
genDeclClass = do
  name <- genTypeConName
  params <- genSimpleTyVarBinders
  pure $
    DeclClass span0 $
      ClassDecl
        { classDeclSpan = span0,
          classDeclContext = Nothing,
          classDeclHeadForm = TypeHeadPrefix,
          classDeclName = name,
          classDeclParams = params,
          classDeclFundeps = [],
          classDeclItems = []
        }

genDeclInstance :: Gen Decl
genDeclInstance = do
  className <- genTypeConName
  n <- chooseInt (0, 2)
  types <- vectorOf n genSimpleType
  pure $
    DeclInstance span0 $
      InstanceDecl
        { instanceDeclSpan = span0,
          instanceDeclOverlapPragma = Nothing,
          instanceDeclWarning = Nothing,
          instanceDeclForall = [],
          instanceDeclContext = [],
          instanceDeclParenthesizedHead = False,
          instanceDeclClassName = className,
          instanceDeclTypes = types,
          instanceDeclItems = []
        }

genDeclStandaloneDeriving :: Gen Decl
genDeclStandaloneDeriving = do
  className <- mkUnqualifiedName NameConId <$> genTypeConName
  n <- chooseInt (0, 2)
  types <- vectorOf n genSimpleType
  pure $
    DeclStandaloneDeriving span0 $
      StandaloneDerivingDecl
        { standaloneDerivingSpan = span0,
          standaloneDerivingStrategy = Nothing,
          standaloneDerivingViaType = Nothing,
          standaloneDerivingOverlapPragma = Nothing,
          standaloneDerivingWarning = Nothing,
          standaloneDerivingForall = [],
          standaloneDerivingContext = [],
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
  callConv <- elements [CCall, StdCall]
  name <- genIdent
  ty <- genSimpleType
  pure $
    DeclForeign span0 $
      ForeignDecl
        { foreignDeclSpan = span0,
          foreignDirection = ForeignImport,
          foreignCallConv = callConv,
          foreignSafety = Nothing,
          foreignEntity = ForeignEntityOmitted,
          foreignName = name,
          foreignType = ty
        }

genDeclTypeFamilyDecl :: Gen Decl
genDeclTypeFamilyDecl = do
  name <- genTypeConName
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

genDeclDataFamilyDecl :: Gen Decl
genDeclDataFamilyDecl = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
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
  familyName <- genTypeConName
  argName <- genTypeConName
  let familyCon = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId familyName)) Unpromoted
      argCon = TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId argName)) Unpromoted
  pure $ TApp span0 familyCon argCon

genDeclPragma :: Gen Decl
genDeclPragma = do
  kind <- elements ["INLINE", "NOINLINE", "INLINABLE"]
  DeclPragma span0 . PragmaInline kind <$> genIdent

genDeclPatSyn :: Gen Decl
genDeclPatSyn = do
  synName <- mkUnqualifiedName NameConId <$> genTypeConName
  argName <- genIdent
  conName <- qualifyName Nothing . mkUnqualifiedName NameConId <$> genTypeConName
  let args = PatSynPrefixArgs [argName]
      pat = PCon span0 conName [PVar span0 (mkUnqualifiedName NameVarId argName)]
  dir <- elements [PatSynBidirectional, PatSynUnidirectional]
  pure $ DeclPatSyn span0 (PatSynDecl span0 synName args pat dir)

genDeclPatSynSig :: Gen Decl
genDeclPatSynSig = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
  DeclPatSynSig span0 [name] <$> genSimpleType

genDeclStandaloneKindSig :: Gen Decl
genDeclStandaloneKindSig = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
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
      (\n -> TCon span0 (qualifyName Nothing (mkUnqualifiedName NameConId n)) Unpromoted) <$> genTypeConName,
      ( TFun span0 . TVar span0 . mkUnqualifiedName NameVarId
          <$> genIdent
      )
        <*> (TVar span0 . mkUnqualifiedName NameVarId <$> genIdent)
    ]

genTypeConName :: Gen Text
genTypeConName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 4)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
  pure (T.pack (first : rest))

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr _)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr' Nothing)) | expr' <- shrinkExpr expr]
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
        <> [DeclValue span0 (FunctionBind span0 name' [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr Nothing}]) | name' <- shrinkUnqualifiedVarName name]
    DeclTypeSig _ names ty ->
      [DeclTypeSig span0 names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
    _ -> []

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
