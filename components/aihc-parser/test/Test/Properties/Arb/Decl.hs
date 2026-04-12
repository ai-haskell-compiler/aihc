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
import Test.Properties.Arb.Expr (genExpr, shrinkExpr, span0)
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
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
  name <- mkUnqualifiedName NameVarId <$> genIdent
  expr <- resize n genExpr
  genFunctionDecl (name, expr)

genFunctionDecl :: (UnqualifiedName, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  headForm <- elements [MatchHeadPrefix, MatchHeadInfix]
  case headForm of
    MatchHeadPrefix ->
      pure $
        DeclValue
          span0
          ( FunctionBind
              span0
              name
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [],
                    matchRhs = UnguardedRhs span0 expr
                  }
              ]
          )
    MatchHeadInfix -> do
      -- For infix bindings, generate an operator name and two PVar patterns.
      -- Symbolic operators: x + y = ..., backtick identifiers: x `f` y = ...
      opName <-
        oneof
          [ mkUnqualifiedName NameVarSym <$> genSymbolicOp,
            mkUnqualifiedName NameVarId <$> genIdent
          ]
      lhsPat <- PVar span0 . mkUnqualifiedName NameVarId <$> genIdent
      rhsPat <- PVar span0 . mkUnqualifiedName NameVarId <$> genIdent
      pure $
        DeclValue
          span0
          ( FunctionBind
              span0
              opName
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadInfix,
                    matchPats = [lhsPat, rhsPat],
                    matchRhs = UnguardedRhs span0 expr
                  }
              ]
          )

genDeclTypeSig :: Gen Decl
genDeclTypeSig = do
  name <- mkUnqualifiedName NameVarId <$> genIdent
  DeclTypeSig span0 [name] <$> genSimpleType

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
  DeclTypeSyn span0 . TypeSynDecl span0 name params <$> genSimpleType

genDeclData :: Gen Decl
genDeclData = DeclData span0 <$> genSimpleDataDecl

genDeclTypeData :: Gen Decl
genDeclTypeData = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
  params <- genSimpleTyVarBinders
  ctors <- genTypeDataCons
  pure $
    DeclTypeData span0 $
      DataDecl
        { dataDeclSpan = span0,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = params,
          dataDeclKind = Nothing,
          dataDeclConstructors = ctors,
          dataDeclDeriving = []
        }
  where
    genTypeDataCons = do
      n <- chooseInt (1, 3)
      vectorOf n genTypeDataCon
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
        dataDeclContext = [],
        dataDeclName = name,
        dataDeclParams = params,
        dataDeclKind = Nothing,
        dataDeclConstructors = ctors,
        dataDeclDeriving = []
      }

genSimpleDataCons :: Gen [DataConDecl]
genSimpleDataCons = do
  n <- chooseInt (1, 3)
  vectorOf n genSimpleDataCon

genSimpleDataCon :: Gen DataConDecl
genSimpleDataCon = do
  name <- mkUnqualifiedName NameConId <$> genTypeConName
  n <- chooseInt (0, 2)
  fields <- vectorOf n genSimpleBangType
  pure $ PrefixCon span0 [] [] name fields

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
  conName <- mkUnqualifiedName NameConId <$> genTypeConName
  ty <- genSimpleType
  let ctor = PrefixCon span0 [] [] conName [BangType span0 NoSourceUnpackedness False ty]
  pure $
    DeclNewtype span0 $
      NewtypeDecl
        { newtypeDeclSpan = span0,
          newtypeDeclContext = [],
          newtypeDeclName = name,
          newtypeDeclParams = params,
          newtypeDeclKind = Nothing,
          newtypeDeclConstructor = Just ctor,
          newtypeDeclDeriving = []
        }

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
genDeclDataFamilyInst = do
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
  DeclStandaloneKindSig span0 name <$> genSimpleType

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
    DeclValue _ (PatternBind _ pat (UnguardedRhs _ expr)) ->
      [DeclValue span0 (PatternBind span0 pat (UnguardedRhs span0 expr')) | expr' <- shrinkExpr expr]
    DeclValue _ (FunctionBind _ name [match@Match {matchRhs = UnguardedRhs _ expr}]) ->
      [ DeclValue
          span0
          ( FunctionBind
              span0
              name
              [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr'}]
          )
      | expr' <- shrinkExpr expr
      ]
        <> [DeclValue span0 (FunctionBind span0 name' [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr}]) | name' <- shrinkUnqualifiedVarName name]
    DeclTypeSig _ names ty ->
      [DeclTypeSig span0 names' ty | names' <- shrinkList shrinkBinderName names, not (null names')]
    _ -> []

shrinkUnqualifiedVarName :: UnqualifiedName -> [UnqualifiedName]
shrinkUnqualifiedVarName name =
  [mkUnqualifiedName NameVarId candidate | candidate <- shrinkIdent (renderUnqualifiedName name)]

shrinkBinderName :: BinderName -> [BinderName]
shrinkBinderName = shrinkUnqualifiedVarName
