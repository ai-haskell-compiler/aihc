{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Ast
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.ExprHelpers (genExpr, genType, normalizeExpr, normalizeType, shrinkExpr, span0)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
   in counterexample (T.unpack source) $
        case parseModule defaultConfig source of
          ParseOk reparsed ->
            let expected = normalizeModule modu
                actual = normalizeModule reparsed
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
          ParseErr err ->
            counterexample (errorBundlePretty err) False

instance Arbitrary Module where
  arbitrary = do
    size <- getSize
    n <- chooseInt (0, min 6 size)
    decls <- vectorOf n (scaleDecl arbitrary)
    imports <- genImportDecls
    mHead <- genMaybeModuleHead
    pure $
      Module
        { moduleSpan = span0,
          moduleHead = mHead,
          moduleLanguagePragmas = [],
          moduleImports = imports,
          moduleDecls = decls
        }

  shrink modu =
    [ modu {moduleDecls = shrunk}
    | shrunk <- shrinkList shrinkDecl (moduleDecls modu),
      not (null shrunk)
    ]
      <> [ modu {moduleImports = shrunk}
         | shrunk <- shrinkList shrinkImportDecl (moduleImports modu)
         ]
      <> [ modu {moduleHead = shrunk}
         | shrunk <- shrinkMaybeModuleHead (moduleHead modu)
         ]

-- | Generate an optional module head.
-- Most modules have explicit headers, but implicit modules (Nothing) are also valid.
genMaybeModuleHead :: Gen (Maybe ModuleHead)
genMaybeModuleHead =
  frequency
    [ (9, Just <$> genModuleHead), -- 90% explicit module header
      (1, pure Nothing) -- 10% implicit module (no module declaration)
    ]

genModuleHead :: Gen ModuleHead
genModuleHead = do
  name <- genModuleName
  exports <- genMaybeExportSpecs
  pure $
    ModuleHead
      { moduleHeadSpan = span0,
        moduleHeadName = name,
        moduleHeadWarningText = Nothing,
        moduleHeadExports = exports
      }

-- | Shrink an optional module head.
shrinkMaybeModuleHead :: Maybe ModuleHead -> [Maybe ModuleHead]
shrinkMaybeModuleHead mHead =
  case mHead of
    Nothing -> []
    Just head' ->
      Nothing : [Just shrunk | shrunk <- shrinkModuleHead head']

shrinkModuleHead :: ModuleHead -> [ModuleHead]
shrinkModuleHead head' =
  [ head' {moduleHeadName = shrunk}
  | shrunk <- shrinkModuleName (moduleHeadName head')
  ]
    <> [ head' {moduleHeadExports = shrunk}
       | shrunk <- shrinkMaybeExportSpecs (moduleHeadExports head')
       ]

genMaybeExportSpecs :: Gen (Maybe [ExportSpec])
genMaybeExportSpecs =
  frequency
    [ (3, pure Nothing),
      (2, Just <$> genExportSpecs)
    ]

shrinkMaybeExportSpecs :: Maybe [ExportSpec] -> [Maybe [ExportSpec]]
shrinkMaybeExportSpecs mSpecs =
  case mSpecs of
    Nothing -> []
    Just specs ->
      Nothing : [Just shrunk | shrunk <- shrinkList shrink specs]

genExportSpecs :: Gen [ExportSpec]
genExportSpecs = do
  n <- chooseInt (0, 3)
  vectorOf n arbitrary

shrinkDecl :: Decl -> [Decl]
shrinkDecl decl =
  case decl of
    DeclValue _ (FunctionBind _ name [match]) ->
      case matchRhs match of
        UnguardedRhs _ expr ->
          [ DeclValue span0 (FunctionBind span0 name' [match {matchRhs = UnguardedRhs span0 expr}])
          | name' <- shrinkIdent name
          ]
            <> [ DeclValue span0 (FunctionBind span0 name [match {matchRhs = UnguardedRhs span0 expr'}])
               | expr' <- shrinkExpr expr
               ]
        _ -> []
    DeclValue _ _ -> []
    DeclTypeSig _ names ty ->
      [DeclTypeSig span0 names' ty | names' <- shrinkBinderNames names]
        <> [DeclTypeSig span0 names ty' | ty' <- shrinkDeclType ty]
    DeclStandaloneKindSig _ name kind ->
      [DeclStandaloneKindSig span0 name' kind | name' <- shrinkTypeName name]
        <> [DeclStandaloneKindSig span0 name kind' | kind' <- shrinkDeclType kind]
    DeclFixity _ assoc mPrec ops ->
      [DeclFixity span0 assoc mPrec' ops | mPrec' <- shrink mPrec]
        <> [DeclFixity span0 assoc mPrec ops' | ops' <- shrinkFixityOps ops]
    DeclTypeSyn _ syn ->
      [DeclTypeSyn span0 syn' | syn' <- shrinkTypeSynDecl syn]
    DeclData _ dataDecl ->
      [DeclData span0 dataDecl' | dataDecl' <- shrinkDataDecl dataDecl]
    DeclNewtype _ newtypeDecl ->
      [DeclNewtype span0 newtypeDecl' | newtypeDecl' <- shrinkNewtypeDecl newtypeDecl]
    DeclClass _ classDecl ->
      [DeclClass span0 classDecl' | classDecl' <- shrinkClassDecl classDecl]
    DeclInstance _ instanceDecl ->
      [DeclInstance span0 instanceDecl' | instanceDecl' <- shrinkInstanceDecl instanceDecl]
    DeclStandaloneDeriving _ derivingDecl ->
      [DeclStandaloneDeriving span0 derivingDecl' | derivingDecl' <- shrinkStandaloneDerivingDecl derivingDecl]
    DeclDefault _ tys ->
      [DeclDefault span0 tys' | tys' <- shrinkDeclTypes tys]
    DeclForeign _ foreignDecl ->
      [DeclForeign span0 foreignDecl' | foreignDecl' <- shrinkForeignDecl foreignDecl]

instance Arbitrary Decl where
  arbitrary =
    sized genDecl

  shrink = shrinkDecl

genDecl :: Int -> Gen Decl
genDecl n
  | n <= 0 =
      oneof
        [ genValueDecl,
          genTypeSigDecl,
          genStandaloneKindSigDecl,
          genFixityDecl,
          genTypeSynDecl,
          genDataDecl 0,
          genNewtypeDecl 0,
          genClassDecl 0,
          genInstanceDecl 0,
          genStandaloneDerivingDecl 0,
          genDefaultDecl,
          genForeignDecl 0
        ]
  | otherwise =
      frequency
        [ (5, genValueDecl),
          (3, genTypeSigDecl),
          (2, genStandaloneKindSigDecl),
          (2, genFixityDecl),
          (3, genTypeSynDecl),
          (3, genDataDecl n),
          (3, genNewtypeDecl n),
          (2, genClassDecl n),
          (2, genInstanceDecl n),
          (2, genStandaloneDerivingDecl n),
          (2, genDefaultDecl),
          (2, genForeignDecl n)
        ]

genValueDecl :: Gen Decl
genValueDecl = do
  name <- genIdent
  expr <- scaleExpr genExpr
  pure $
    DeclValue
      span0
      ( FunctionBind
          span0
          name
          [ Match
              { matchSpan = span0,
                matchPats = [],
                matchRhs = UnguardedRhs span0 expr
              }
          ]
      )

genTypeSigDecl :: Gen Decl
genTypeSigDecl = do
  names <- genBinderNames
  ty <- scaleTypeFromSize
  pure (DeclTypeSig span0 names ty)

genStandaloneKindSigDecl :: Gen Decl
genStandaloneKindSigDecl = do
  name <- genTypeName
  kind <- scaleTypeFromSize
  pure (DeclStandaloneKindSig span0 name kind)

genFixityDecl :: Gen Decl
genFixityDecl = do
  assoc <- elements [Infix, InfixL, InfixR]
  mPrec <- frequency [(1, pure Nothing), (4, Just <$> chooseInt (0, 9))]
  DeclFixity span0 assoc mPrec <$> genFixityOps

genTypeSynDecl :: Gen Decl
genTypeSynDecl = DeclTypeSyn span0 <$> genTypeSynDeclBody

genTypeSynDeclBody :: Gen TypeSynDecl
genTypeSynDeclBody = do
  name <- genTypeName
  params <- genTyVarBinders
  body <- scaleTypeFromSize
  pure $
    TypeSynDecl
      { typeSynSpan = span0,
        typeSynName = name,
        typeSynParams = params,
        typeSynBody = body
      }

genDataDecl :: Int -> Gen Decl
genDataDecl n = do
  name <- genTypeName
  params <- genTyVarBinders
  context <- genConstraints
  constructors <- genDataConstructors (childDeclSize n)
  derivingClauses <- genDerivingClauses
  pure $
    DeclData
      span0
      DataDecl
        { dataDeclSpan = span0,
          dataDeclContext = context,
          dataDeclName = name,
          dataDeclParams = params,
          dataDeclConstructors = constructors,
          dataDeclDeriving = derivingClauses
        }

genNewtypeDecl :: Int -> Gen Decl
genNewtypeDecl n = do
  name <- genTypeName
  params <- genTyVarBinders
  context <- genConstraints
  constructor <- genSingleConstructor (childDeclSize n)
  derivingClauses <- genDerivingClauses
  pure $
    DeclNewtype
      span0
      NewtypeDecl
        { newtypeDeclSpan = span0,
          newtypeDeclContext = context,
          newtypeDeclName = name,
          newtypeDeclParams = params,
          newtypeDeclConstructor = Just constructor,
          newtypeDeclDeriving = derivingClauses
        }

genClassDecl :: Int -> Gen Decl
genClassDecl n = do
  name <- genTypeName
  params <- genNonEmptyTyVarBinders
  context <- frequency [(2, pure Nothing), (3, Just <$> genNonEmptyConstraints)]
  items <- genClassDeclItems (childDeclSize n)
  pure $
    DeclClass
      span0
      ClassDecl
        { classDeclSpan = span0,
          classDeclContext = context,
          classDeclName = name,
          classDeclParams = params,
          classDeclItems = items
        }

genInstanceDecl :: Int -> Gen Decl
genInstanceDecl n = do
  className <- genTypeName
  context <- genConstraints
  types <- genNonEmptyDeclTypes
  items <- genInstanceDeclItems (childDeclSize n)
  pure $
    DeclInstance
      span0
      InstanceDecl
        { instanceDeclSpan = span0,
          instanceDeclContext = context,
          instanceDeclClassName = className,
          instanceDeclTypes = types,
          instanceDeclItems = items
        }

genStandaloneDerivingDecl :: Int -> Gen Decl
genStandaloneDerivingDecl _ = do
  strategy <- genMaybeDerivingStrategy
  context <- genConstraints
  className <- genTypeName
  types <- genNonEmptyDeclTypes
  pure $
    DeclStandaloneDeriving
      span0
      StandaloneDerivingDecl
        { standaloneDerivingSpan = span0,
          standaloneDerivingStrategy = strategy,
          standaloneDerivingContext = context,
          standaloneDerivingClassName = className,
          standaloneDerivingTypes = types
        }

genDefaultDecl :: Gen Decl
genDefaultDecl = DeclDefault span0 <$> genNonEmptyDeclTypes

genForeignDecl :: Int -> Gen Decl
genForeignDecl _ = do
  direction <- elements [ForeignImport, ForeignExport]
  callConv <- elements [CCall, StdCall]
  safety <- frequency [(2, pure Nothing), (3, Just <$> elements [Safe, Unsafe])]
  entity <- genForeignEntitySpec
  name <- genIdent
  ty <- scaleTypeFromSize
  pure $
    DeclForeign
      span0
      ForeignDecl
        { foreignDeclSpan = span0,
          foreignDirection = direction,
          foreignCallConv = callConv,
          foreignSafety = safety,
          foreignEntity = entity,
          foreignName = name,
          foreignType = ty
        }

genBinderNames :: Gen [Text]
genBinderNames = do
  n <- chooseInt (1, 3)
  candidateNames <- vectorOf (n * 2) genIdent
  pure (take n (nub candidateNames))

shrinkBinderNames :: [Text] -> [[Text]]
shrinkBinderNames names =
  [ shrunk
  | shrunk <- shrinkList shrinkIdent names,
    not (null shrunk)
  ]

genFixityOps :: Gen [Text]
genFixityOps = do
  n <- chooseInt (1, 3)
  vectorOf n genFixityOp

shrinkFixityOps :: [Text] -> [[Text]]
shrinkFixityOps ops =
  [ shrunk
  | shrunk <- shrinkList shrinkFixityOp ops,
    not (null shrunk)
  ]

genFixityOp :: Gen Text
genFixityOp =
  oneof
    [ elements ["+", "-", "*", "/", "<>", "<++>", "&&", "||", "++"],
      genIdent,
      genTypeName
    ]

shrinkFixityOp :: Text -> [Text]
shrinkFixityOp op =
  [candidate | candidate <- ["+", "-", "*", "op", "Infix"], candidate /= op]

genTyVarBinders :: Gen [TyVarBinder]
genTyVarBinders = do
  n <- chooseInt (0, 2)
  vectorOf n genTyVarBinder

genTyVarBinder :: Gen TyVarBinder
genTyVarBinder = do
  name <- genTypeVarName
  kind <- frequency [(4, pure Nothing), (1, Just <$> scaleTypeFromSize)]
  pure $
    TyVarBinder
      { tyVarBinderSpan = span0,
        tyVarBinderName = name,
        tyVarBinderKind = kind
      }

shrinkTyVarBinder :: TyVarBinder -> [TyVarBinder]
shrinkTyVarBinder binder =
  [binder {tyVarBinderName = name'} | name' <- shrinkTypeVarName (tyVarBinderName binder)]
    <> [binder {tyVarBinderKind = kind'} | kind' <- shrinkMaybeDeclType (tyVarBinderKind binder)]

shrinkTyVarBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkTyVarBinders = shrinkList shrinkTyVarBinder

genDataConstructors :: Int -> Gen [DataConDecl]
genDataConstructors n = do
  isEmpty <- frequency [(1, pure True), (4, pure False)]
  if isEmpty
    then pure []
    else do
      count <- chooseInt (1, 3)
      vectorOf count (genDataConDecl (childDeclSize n))

genSingleConstructor :: Int -> Gen DataConDecl
genSingleConstructor n = genDataConDecl (childDeclSize n)

genPrefixConstructor :: Int -> Gen DataConDecl
genPrefixConstructor n = do
  name <- genTypeName
  forallVars <- genForallVars
  constraints <- genConstraints
  fields <- genBangTypes (childDeclSize n)
  pure (PrefixCon span0 forallVars constraints name fields)

genInfixConstructor :: Int -> Gen DataConDecl
genInfixConstructor n = do
  forallVars <- genForallVars
  constraints <- genConstraints
  lhs <- genBangType (childDeclSize n)
  op <- genTypeName
  rhs <- genBangType (childDeclSize n)
  pure (InfixCon span0 forallVars constraints lhs op rhs)

genRecordConstructor :: Int -> Gen DataConDecl
genRecordConstructor n = do
  forallVars <- genForallVars
  constraints <- genConstraints
  name <- genTypeName
  fieldCount <- chooseInt (1, 3)
  fields <- vectorOf fieldCount (genFieldDecl (childDeclSize n))
  pure (RecordCon span0 forallVars constraints name fields)

genDataConDecl :: Int -> Gen DataConDecl
genDataConDecl n =
  frequency
    [ (4, genPrefixConstructor n),
      (2, genInfixConstructor n),
      (2, genRecordConstructor n)
    ]

genBangTypes :: Int -> Gen [BangType]
genBangTypes n = do
  count <- chooseInt (0, 3)
  vectorOf count (genBangType n)

genBangType :: Int -> Gen BangType
genBangType n = do
  strict <- arbitrary
  ty <- genType (childDeclSize n)
  pure $
    BangType
      { bangSpan = span0,
        bangStrict = strict,
        bangType = ty
      }

shrinkBangType :: BangType -> [BangType]
shrinkBangType bang =
  [bang {bangStrict = False} | bangStrict bang]
    <> [bang {bangType = ty'} | ty' <- shrinkDeclType (bangType bang)]

shrinkBangTypes :: [BangType] -> [[BangType]]
shrinkBangTypes = shrinkList shrinkBangType

shrinkDataConDecl :: DataConDecl -> [DataConDecl]
shrinkDataConDecl dataCon =
  case dataCon of
    PrefixCon _ forallVars constraints name fields ->
      [PrefixCon span0 forallVars constraints name' fields | name' <- shrinkTypeName name]
        <> [PrefixCon span0 forallVars constraints name fields' | fields' <- shrinkBangTypes fields]
    InfixCon _ forallVars constraints lhs name rhs ->
      [InfixCon span0 forallVars constraints lhs name' rhs | name' <- shrinkTypeName name]
        <> [InfixCon span0 forallVars constraints lhs' name rhs | lhs' <- shrinkBangType lhs]
        <> [InfixCon span0 forallVars constraints lhs name rhs' | rhs' <- shrinkBangType rhs]
    RecordCon _ forallVars constraints name fields ->
      [RecordCon span0 forallVars constraints name' fields | name' <- shrinkTypeName name]
        <> [RecordCon span0 forallVars constraints name fields' | fields' <- shrinkFieldDecls fields]
    GadtCon {} -> []

shrinkDataConstructors :: [DataConDecl] -> [[DataConDecl]]
shrinkDataConstructors = shrinkList shrinkDataConDecl

shrinkTypeSynDecl :: TypeSynDecl -> [TypeSynDecl]
shrinkTypeSynDecl syn =
  [syn {typeSynName = name'} | name' <- shrinkTypeName (typeSynName syn)]
    <> [syn {typeSynParams = params'} | params' <- shrinkTyVarBinders (typeSynParams syn)]
    <> [syn {typeSynBody = body'} | body' <- shrinkDeclType (typeSynBody syn)]

shrinkDataDecl :: DataDecl -> [DataDecl]
shrinkDataDecl dataDecl =
  [dataDecl {dataDeclName = name'} | name' <- shrinkTypeName (dataDeclName dataDecl)]
    <> [dataDecl {dataDeclContext = ctx'} | ctx' <- shrinkConstraints (dataDeclContext dataDecl)]
    <> [dataDecl {dataDeclParams = params'} | params' <- shrinkTyVarBinders (dataDeclParams dataDecl)]
    <> [dataDecl {dataDeclConstructors = ctors'} | ctors' <- shrinkDataConstructors (dataDeclConstructors dataDecl)]
    <> [dataDecl {dataDeclDeriving = deriving'} | deriving' <- shrinkDerivingClauses (dataDeclDeriving dataDecl)]

shrinkNewtypeDecl :: NewtypeDecl -> [NewtypeDecl]
shrinkNewtypeDecl newtypeDecl =
  [newtypeDecl {newtypeDeclName = name'} | name' <- shrinkTypeName (newtypeDeclName newtypeDecl)]
    <> [newtypeDecl {newtypeDeclContext = ctx'} | ctx' <- shrinkConstraints (newtypeDeclContext newtypeDecl)]
    <> [newtypeDecl {newtypeDeclParams = params'} | params' <- shrinkTyVarBinders (newtypeDeclParams newtypeDecl)]
    <> [ newtypeDecl {newtypeDeclConstructor = ctor'}
       | ctor' <- shrinkMaybeDataConDecl (newtypeDeclConstructor newtypeDecl)
       ]
    <> [ newtypeDecl {newtypeDeclDeriving = deriving'}
       | deriving' <- shrinkDerivingClauses (newtypeDeclDeriving newtypeDecl)
       ]

shrinkMaybeDataConDecl :: Maybe DataConDecl -> [Maybe DataConDecl]
shrinkMaybeDataConDecl mCtor =
  case mCtor of
    Nothing -> []
    Just ctor -> [Just ctor' | ctor' <- shrinkDataConDecl ctor]

genTypeVarName :: Gen Text
genTypeVarName = do
  first <- elements ['a' .. 'z']
  restLen <- chooseInt (0, 3)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['0' .. '9']))
  pure (T.pack (first : rest))

shrinkTypeVarName :: Text -> [Text]
shrinkTypeVarName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidTypeVarName candidate
  ]

isValidTypeVarName :: Text -> Bool
isValidTypeVarName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` ['a' .. 'z'])
        && T.all (`elem` (['a' .. 'z'] <> ['0' .. '9'])) rest
    Nothing -> False

genNonEmptyDeclTypes :: Gen [Type]
genNonEmptyDeclTypes = do
  n <- chooseInt (1, 3)
  vectorOf n scaleTypeFromSize

genConstraints :: Gen [Constraint]
genConstraints = do
  n <- chooseInt (0, 2)
  vectorOf n genConstraint

genConstraint :: Gen Constraint
genConstraint = do
  className <- genTypeName
  argCount <- chooseInt (0, 2)
  args <- vectorOf argCount scaleTypeAtomish
  paren <- arbitrary
  pure $
    Constraint
      { constraintSpan = span0,
        constraintClass = className,
        constraintArgs = args,
        constraintParen = paren
      }

genNonEmptyConstraints :: Gen [Constraint]
genNonEmptyConstraints = do
  n <- chooseInt (1, 2)
  vectorOf n genConstraint

genForallVars :: Gen [Text]
genForallVars = do
  n <- chooseInt (0, 2)
  vectorOf n genTypeVarName

genFieldDecl :: Int -> Gen FieldDecl
genFieldDecl n = do
  names <- genFieldNames
  fieldTy <- genBangType n
  pure $
    FieldDecl
      { fieldSpan = span0,
        fieldNames = names,
        fieldType = fieldTy
      }

genFieldNames :: Gen [Text]
genFieldNames = do
  n <- chooseInt (1, 3)
  candidateNames <- vectorOf (n * 2) genIdent
  pure (take n (nub candidateNames))

shrinkFieldDecl :: FieldDecl -> [FieldDecl]
shrinkFieldDecl fieldDecl =
  [fieldDecl {fieldNames = names'} | names' <- shrinkBinderNames (fieldNames fieldDecl)]
    <> [fieldDecl {fieldType = ty'} | ty' <- shrinkBangType (fieldType fieldDecl)]

shrinkFieldDecls :: [FieldDecl] -> [[FieldDecl]]
shrinkFieldDecls =
  shrinkList shrinkFieldDecl

genDerivingClauses :: Gen [DerivingClause]
genDerivingClauses = do
  n <- chooseInt (0, 2)
  vectorOf n genDerivingClause

genDerivingClause :: Gen DerivingClause
genDerivingClause = do
  strategy <- genMaybeDerivingStrategy
  classCount <- chooseInt (1, 3)
  derivingClassNames <- vectorOf classCount genTypeName
  pure (DerivingClause strategy derivingClassNames)

genMaybeDerivingStrategy :: Gen (Maybe DerivingStrategy)
genMaybeDerivingStrategy =
  frequency
    [ (2, pure Nothing),
      (1, Just <$> elements [DerivingStock, DerivingNewtype, DerivingAnyclass])
    ]

shrinkDerivingClauses :: [DerivingClause] -> [[DerivingClause]]
shrinkDerivingClauses =
  shrinkList shrinkDerivingClause

shrinkDerivingClause :: DerivingClause -> [DerivingClause]
shrinkDerivingClause (DerivingClause strategy derivingClassNames) =
  [DerivingClause strategy' derivingClassNames | strategy' <- shrinkMaybeDerivingStrategy strategy]
    <> [ DerivingClause strategy derivingClassNames'
       | derivingClassNames' <- shrinkList shrinkTypeName derivingClassNames,
         not (null derivingClassNames')
       ]

shrinkMaybeDerivingStrategy :: Maybe DerivingStrategy -> [Maybe DerivingStrategy]
shrinkMaybeDerivingStrategy strategy =
  case strategy of
    Nothing -> []
    Just strategy' -> Nothing : [Just strategy'' | strategy'' <- shrinkDerivingStrategy strategy']

shrinkDerivingStrategy :: DerivingStrategy -> [DerivingStrategy]
shrinkDerivingStrategy strategy =
  case strategy of
    DerivingAnyclass -> [DerivingStock, DerivingNewtype]
    DerivingNewtype -> [DerivingStock]
    DerivingStock -> []

genClassDeclItems :: Int -> Gen [ClassDeclItem]
genClassDeclItems n = do
  count <- chooseInt (0, 3)
  vectorOf count (genClassDeclItem n)

genClassDeclItem :: Int -> Gen ClassDeclItem
genClassDeclItem n =
  frequency
    [ (3, do
          names <- genBinderNames
          ty <- genType (childDeclSize n)
          pure (ClassItemTypeSig span0 names ty)
      ),
      (2, do
          assoc <- elements [Infix, InfixL, InfixR]
          mPrec <- frequency [(1, pure Nothing), (4, Just <$> chooseInt (0, 9))]
          ops <- genFixityOps
          pure (ClassItemFixity span0 assoc mPrec ops)
      ),
      (2, do
          valueDecl <- genSimpleValueDecl
          pure (ClassItemDefault span0 valueDecl)
      )
    ]

genInstanceDeclItems :: Int -> Gen [InstanceDeclItem]
genInstanceDeclItems n = do
  count <- chooseInt (0, 3)
  vectorOf count (genInstanceDeclItem n)

genInstanceDeclItem :: Int -> Gen InstanceDeclItem
genInstanceDeclItem n =
  frequency
    [ (3, InstanceItemBind span0 <$> genSimpleValueDecl),
      (2, do
          names <- genBinderNames
          ty <- genType (childDeclSize n)
          pure (InstanceItemTypeSig span0 names ty)
      ),
      (2, do
          assoc <- elements [Infix, InfixL, InfixR]
          mPrec <- frequency [(1, pure Nothing), (4, Just <$> chooseInt (0, 9))]
          ops <- genFixityOps
          pure (InstanceItemFixity span0 assoc mPrec ops)
      )
    ]

genSimpleValueDecl :: Gen ValueDecl
genSimpleValueDecl = do
  name <- genIdent
  expr <- scaleExpr genExpr
  pure $
    FunctionBind
      span0
      name
      [ Match
          { matchSpan = span0,
            matchPats = [],
            matchRhs = UnguardedRhs span0 expr
          }
      ]

genForeignEntitySpec :: Gen ForeignEntitySpec
genForeignEntitySpec =
  frequency
    [ (2, pure ForeignEntityOmitted),
      (1, pure ForeignEntityDynamic),
      (1, pure ForeignEntityWrapper),
      (2, ForeignEntityStatic <$> genMaybeForeignEntityName),
      (2, ForeignEntityAddress <$> genMaybeForeignEntityName),
      (2, ForeignEntityNamed <$> genForeignEntityName)
    ]

genMaybeForeignEntityName :: Gen (Maybe Text)
genMaybeForeignEntityName =
  frequency
    [ (1, pure Nothing),
      (3, Just <$> genForeignEntityName)
    ]

genForeignEntityName :: Gen Text
genForeignEntityName =
  oneof [genIdent, genTypeName]

genNonEmptyTyVarBinders :: Gen [TyVarBinder]
genNonEmptyTyVarBinders = do
  n <- chooseInt (1, 2)
  vectorOf n genTyVarBinder

childDeclSize :: Int -> Int
childDeclSize n =
  max 0 (n `div` 2)

scaleDecl :: Gen a -> Gen a
scaleDecl =
  scale (\n -> max 0 (n `div` 2))

scaleExpr :: Gen a -> Gen a
scaleExpr =
  scale (\n -> max 0 (n `div` 2))

scaleTypeFromSize :: Gen Type
scaleTypeFromSize =
  sized (\n -> genType (max 0 (n `div` 2)))

scaleTypeAtomish :: Gen Type
scaleTypeAtomish =
  sized (\n -> genType (max 0 (n `div` 2)))

shrinkDeclTypes :: [Type] -> [[Type]]
shrinkDeclTypes tys =
  [ shrunk
  | shrunk <- shrinkList shrinkDeclType tys,
    not (null shrunk)
  ]

shrinkDeclType :: Type -> [Type]
shrinkDeclType ty =
  case normalizeType ty of
    TVar _ name -> [TVar span0 name' | name' <- shrinkTypeVarName name]
    TCon _ name -> [TCon span0 name' | name' <- shrinkTypeName name]
    TStar _ -> []
    TQuasiQuote {} -> []
    TForall _ binders inner ->
      [inner]
        <> [TForall span0 binders' inner | binders' <- shrinkList shrinkTypeVarName binders, not (null binders')]
        <> [TForall span0 binders inner' | inner' <- shrinkDeclType inner]
    TApp _ fn arg ->
      [fn, arg]
        <> [TApp span0 fn' arg | fn' <- shrinkDeclType fn]
        <> [TApp span0 fn arg' | arg' <- shrinkDeclType arg]
    TFun _ lhs rhs ->
      [lhs, rhs]
        <> [TFun span0 lhs' rhs | lhs' <- shrinkDeclType lhs]
        <> [TFun span0 lhs rhs' | rhs' <- shrinkDeclType rhs]
    TTuple _ elems ->
      [ candidate
      | shrunk <- shrinkList shrinkDeclType elems,
        candidate <- case shrunk of
          [] -> [TTuple span0 []]
          [_] -> []
          _ -> [TTuple span0 shrunk]
      ]
    TList _ inner ->
      [inner] <> [TList span0 inner' | inner' <- shrinkDeclType inner]
    TParen _ inner ->
      inner : [TParen span0 inner' | inner' <- shrinkDeclType inner]
    TContext _ constraints inner ->
      [inner]
        <> [TContext span0 constraints' inner | constraints' <- shrinkConstraints constraints]
        <> [TContext span0 constraints inner' | inner' <- shrinkDeclType inner]

shrinkMaybeDeclType :: Maybe Type -> [Maybe Type]
shrinkMaybeDeclType mTy =
  case mTy of
    Nothing -> []
    Just ty -> Nothing : [Just ty' | ty' <- shrinkDeclType ty]

shrinkConstraints :: [Constraint] -> [[Constraint]]
shrinkConstraints = shrinkList shrinkConstraint

shrinkConstraint :: Constraint -> [Constraint]
shrinkConstraint constraint =
  [constraint {constraintClass = name'} | name' <- shrinkTypeName (constraintClass constraint)]
    <> [constraint {constraintArgs = args'} | args' <- shrinkDeclTypes (constraintArgs constraint)]

shrinkClassDecl :: ClassDecl -> [ClassDecl]
shrinkClassDecl classDecl =
  [classDecl {classDeclContext = context'} | context' <- shrinkMaybeConstraints (classDeclContext classDecl)]
    <> [classDecl {classDeclName = name'} | name' <- shrinkTypeName (classDeclName classDecl)]
    <> [classDecl {classDeclParams = params'} | params' <- shrinkNonEmptyTyVarBinders (classDeclParams classDecl)]
    <> [classDecl {classDeclItems = items'} | items' <- shrinkClassDeclItems (classDeclItems classDecl)]

shrinkMaybeConstraints :: Maybe [Constraint] -> [Maybe [Constraint]]
shrinkMaybeConstraints maybeConstraints =
  case maybeConstraints of
    Nothing -> []
    Just constraints ->
      Nothing : [Just constraints' | constraints' <- shrinkConstraints constraints, not (null constraints')]

shrinkNonEmptyTyVarBinders :: [TyVarBinder] -> [[TyVarBinder]]
shrinkNonEmptyTyVarBinders binders =
  [ binders'
  | binders' <- shrinkTyVarBinders binders,
    not (null binders')
  ]

shrinkClassDeclItems :: [ClassDeclItem] -> [[ClassDeclItem]]
shrinkClassDeclItems =
  shrinkList shrinkClassDeclItem

shrinkClassDeclItem :: ClassDeclItem -> [ClassDeclItem]
shrinkClassDeclItem item =
  case item of
    ClassItemTypeSig _ names ty ->
      [ClassItemTypeSig span0 names' ty | names' <- shrinkBinderNames names]
        <> [ClassItemTypeSig span0 names ty' | ty' <- shrinkDeclType ty]
    ClassItemFixity _ assoc mPrec ops ->
      [ClassItemFixity span0 assoc mPrec' ops | mPrec' <- shrink mPrec]
        <> [ClassItemFixity span0 assoc mPrec ops' | ops' <- shrinkFixityOps ops]
    ClassItemDefault _ valueDecl ->
      [ClassItemDefault span0 valueDecl' | valueDecl' <- shrinkValueDecl valueDecl]

shrinkInstanceDecl :: InstanceDecl -> [InstanceDecl]
shrinkInstanceDecl instanceDecl =
  [instanceDecl {instanceDeclContext = ctx'} | ctx' <- shrinkConstraints (instanceDeclContext instanceDecl)]
    <> [instanceDecl {instanceDeclClassName = name'} | name' <- shrinkTypeName (instanceDeclClassName instanceDecl)]
    <> [ instanceDecl {instanceDeclTypes = tys'}
       | tys' <- shrinkDeclTypes (instanceDeclTypes instanceDecl),
         not (null tys')
       ]
    <> [instanceDecl {instanceDeclItems = items'} | items' <- shrinkInstanceDeclItems (instanceDeclItems instanceDecl)]

shrinkInstanceDeclItems :: [InstanceDeclItem] -> [[InstanceDeclItem]]
shrinkInstanceDeclItems =
  shrinkList shrinkInstanceDeclItem

shrinkInstanceDeclItem :: InstanceDeclItem -> [InstanceDeclItem]
shrinkInstanceDeclItem item =
  case item of
    InstanceItemBind _ valueDecl ->
      [InstanceItemBind span0 valueDecl' | valueDecl' <- shrinkValueDecl valueDecl]
    InstanceItemTypeSig _ names ty ->
      [InstanceItemTypeSig span0 names' ty | names' <- shrinkBinderNames names]
        <> [InstanceItemTypeSig span0 names ty' | ty' <- shrinkDeclType ty]
    InstanceItemFixity _ assoc mPrec ops ->
      [InstanceItemFixity span0 assoc mPrec' ops | mPrec' <- shrink mPrec]
        <> [InstanceItemFixity span0 assoc mPrec ops' | ops' <- shrinkFixityOps ops]

shrinkStandaloneDerivingDecl :: StandaloneDerivingDecl -> [StandaloneDerivingDecl]
shrinkStandaloneDerivingDecl derivingDecl =
  [derivingDecl {standaloneDerivingStrategy = strategy'} | strategy' <- shrinkMaybeDerivingStrategy (standaloneDerivingStrategy derivingDecl)]
    <> [derivingDecl {standaloneDerivingContext = ctx'} | ctx' <- shrinkConstraints (standaloneDerivingContext derivingDecl)]
    <> [derivingDecl {standaloneDerivingClassName = name'} | name' <- shrinkTypeName (standaloneDerivingClassName derivingDecl)]
    <> [ derivingDecl {standaloneDerivingTypes = tys'}
       | tys' <- shrinkDeclTypes (standaloneDerivingTypes derivingDecl),
         not (null tys')
       ]

shrinkForeignDecl :: ForeignDecl -> [ForeignDecl]
shrinkForeignDecl foreignDecl =
  [foreignDecl {foreignSafety = safety'} | safety' <- shrinkMaybeForeignSafety (foreignSafety foreignDecl)]
    <> [foreignDecl {foreignEntity = entity'} | entity' <- shrinkForeignEntitySpec (foreignEntity foreignDecl)]
    <> [foreignDecl {foreignName = name'} | name' <- shrinkIdent (foreignName foreignDecl)]
    <> [foreignDecl {foreignType = ty'} | ty' <- shrinkDeclType (foreignType foreignDecl)]

shrinkMaybeForeignSafety :: Maybe ForeignSafety -> [Maybe ForeignSafety]
shrinkMaybeForeignSafety safety =
  case safety of
    Nothing -> []
    Just safety' -> Nothing : [Just safety'' | safety'' <- shrinkForeignSafety safety']

shrinkForeignSafety :: ForeignSafety -> [ForeignSafety]
shrinkForeignSafety safety =
  case safety of
    Unsafe -> [Safe]
    Safe -> []

shrinkForeignEntitySpec :: ForeignEntitySpec -> [ForeignEntitySpec]
shrinkForeignEntitySpec spec =
  case spec of
    ForeignEntityDynamic -> [ForeignEntityOmitted]
    ForeignEntityWrapper -> [ForeignEntityOmitted]
    ForeignEntityStatic mName -> ForeignEntityOmitted : [ForeignEntityStatic mName' | mName' <- shrinkMaybeText shrinkIdent mName]
    ForeignEntityAddress mName -> ForeignEntityOmitted : [ForeignEntityAddress mName' | mName' <- shrinkMaybeText shrinkIdent mName]
    ForeignEntityNamed name -> ForeignEntityOmitted : [ForeignEntityNamed name' | name' <- shrinkIdent name]
    ForeignEntityOmitted -> []

shrinkMaybeText :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybeText shrinkFn mValue =
  case mValue of
    Nothing -> []
    Just value -> Nothing : [Just value' | value' <- shrinkFn value]

shrinkValueDecl :: ValueDecl -> [ValueDecl]
shrinkValueDecl valueDecl =
  case valueDecl of
    FunctionBind _ name [match] ->
      case matchRhs match of
        UnguardedRhs _ expr ->
          [FunctionBind span0 name' [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr}] | name' <- shrinkIdent name]
            <> [FunctionBind span0 name [match {matchSpan = span0, matchRhs = UnguardedRhs span0 expr'}] | expr' <- shrinkExpr expr]
        _ -> []
    _ -> []

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule span0 <$> genModuleName,
        ExportVar span0 Nothing <$> genIdent,
        ExportAbs span0 <$> genTypeNamespace <*> genTypeName,
        ExportAll span0 <$> genTypeNamespace <*> genTypeName,
        ExportWith span0 <$> genTypeNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink spec =
    case spec of
      ExportModule _ modName ->
        [ExportModule span0 shrunk | shrunk <- shrinkModuleName modName]
      ExportVar _ namespace name ->
        [ExportVar span0 namespace shrunk | shrunk <- shrinkIdent name]
      ExportAbs _ namespace name ->
        [ExportAbs span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ExportAll _ namespace name ->
        [ExportAbs span0 namespace name]
          <> [ExportAll span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ExportWith _ namespace name members ->
        [ExportAbs span0 namespace name | not (null members)]
          <> [ExportWith span0 namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ExportWith span0 namespace name shrunk | shrunk <- shrinkList shrinkIdent members, not (null shrunk)]

instance Arbitrary ImportSpec where
  arbitrary =
    ImportSpec span0
      <$> arbitrary
      <*> genImportItems

  shrink spec =
    [spec {importSpecHiding = shrunk} | shrunk <- shrink (importSpecHiding spec)]
      <> [spec {importSpecItems = shrunk} | shrunk <- shrinkList shrink (importSpecItems spec)]

instance Arbitrary ImportItem where
  arbitrary =
    oneof
      [ ImportItemVar span0 Nothing <$> genIdent,
        ImportItemAbs span0 <$> genTypeNamespace <*> genTypeName,
        ImportItemAll span0 <$> genTypeNamespace <*> genTypeName,
        ImportItemWith span0 <$> genTypeNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink item =
    case item of
      ImportItemVar _ namespace name ->
        [ImportItemVar span0 namespace shrunk | shrunk <- shrinkIdent name]
      ImportItemAbs _ namespace name ->
        [ImportItemAbs span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemAll _ namespace name ->
        [ImportItemAbs span0 namespace name]
          <> [ImportItemAll span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemWith _ namespace name members ->
        [ImportItemAbs span0 namespace name | not (null members)]
          <> [ImportItemWith span0 namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemWith span0 namespace name shrunk | shrunk <- shrinkList shrinkIdent members, not (null shrunk)]

instance Arbitrary ImportDecl where
  arbitrary = do
    modName <- genModuleName
    spec <- genMaybeImportSpec
    pure $
      ImportDecl
        { importDeclSpan = span0,
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclQualified = False,
          importDeclQualifiedPost = False,
          importDeclModule = modName,
          importDeclAs = Nothing,
          importDeclSpec = spec
        }

  shrink decl =
    [ decl {importDeclModule = shrunk}
    | shrunk <- shrinkModuleName (importDeclModule decl)
    ]
      <> [ decl {importDeclSpec = shrunk}
         | shrunk <- shrinkMaybeImportSpec (importDeclSpec decl)
         ]

genImportDecls :: Gen [ImportDecl]
genImportDecls = do
  n <- chooseInt (0, 3)
  vectorOf n arbitrary

shrinkImportDecl :: ImportDecl -> [ImportDecl]
shrinkImportDecl = shrink

genModuleName :: Gen Text
genModuleName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

shrinkModuleName :: Text -> [Text]
shrinkModuleName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidModuleName candidate
  ]

isValidModuleName :: Text -> Bool
isValidModuleName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` ['A' .. 'Z'])
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
    Nothing -> False

genTypeName :: Gen Text
genTypeName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (T.pack (first : rest))

shrinkTypeName :: Text -> [Text]
shrinkTypeName name =
  [ candidate
  | candidate <- map T.pack (shrink (T.unpack name)),
    isValidTypeName candidate
  ]

isValidTypeName :: Text -> Bool
isValidTypeName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` ['A' .. 'Z'])
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
    Nothing -> False

genExportMembers :: Gen [Text]
genExportMembers = do
  n <- chooseInt (1, 3)
  vectorOf n genMemberName

genTypeNamespace :: Gen (Maybe Text)
genTypeNamespace =
  frequency
    [ (3, pure Nothing),
      (1, pure (Just "type"))
    ]

genMemberName :: Gen Text
genMemberName =
  oneof
    [genIdent, genTypeName]

genImportItems :: Gen [ImportItem]
genImportItems = do
  n <- chooseInt (0, 3)
  vectorOf n arbitrary

genMaybeImportSpec :: Gen (Maybe ImportSpec)
genMaybeImportSpec =
  frequency
    [ (3, pure Nothing),
      (2, Just <$> arbitrary)
    ]

shrinkMaybeImportSpec :: Maybe ImportSpec -> [Maybe ImportSpec]
shrinkMaybeImportSpec mSpec =
  case mSpec of
    Nothing -> []
    Just spec -> Nothing : [Just shrunk | shrunk <- shrink spec]

-- Module normalization
normalizeModule :: Module -> Module
normalizeModule modu =
  Module
    { moduleSpan = span0,
      moduleHead = fmap normalizeModuleHead (moduleHead modu),
      moduleLanguagePragmas = [],
      moduleImports = map normalizeImportDecl (moduleImports modu),
      moduleDecls = map normalizeDecl (moduleDecls modu)
    }

normalizeModuleHead :: ModuleHead -> ModuleHead
normalizeModuleHead head' =
  ModuleHead
    { moduleHeadSpan = span0,
      moduleHeadName = moduleHeadName head',
      moduleHeadWarningText = Nothing,
      moduleHeadExports = fmap (map normalizeExportSpec) (moduleHeadExports head')
    }

normalizeExportSpec :: ExportSpec -> ExportSpec
normalizeExportSpec spec =
  case spec of
    ExportModule _ modName -> ExportModule span0 modName
    ExportVar _ namespace name -> ExportVar span0 namespace name
    ExportAbs _ namespace name -> ExportAbs span0 namespace name
    ExportAll _ namespace name -> ExportAll span0 namespace name
    ExportWith _ namespace name members -> ExportWith span0 namespace name members

normalizeImportDecl :: ImportDecl -> ImportDecl
normalizeImportDecl decl =
  ImportDecl
    { importDeclSpan = span0,
      importDeclLevel = importDeclLevel decl,
      importDeclPackage = importDeclPackage decl,
      importDeclQualified = importDeclQualified decl,
      importDeclQualifiedPost = importDeclQualifiedPost decl,
      importDeclModule = importDeclModule decl,
      importDeclAs = importDeclAs decl,
      importDeclSpec = fmap normalizeImportSpec (importDeclSpec decl)
    }

normalizeImportSpec :: ImportSpec -> ImportSpec
normalizeImportSpec spec =
  ImportSpec
    { importSpecSpan = span0,
      importSpecHiding = importSpecHiding spec,
      importSpecItems = map normalizeImportItem (importSpecItems spec)
    }

normalizeImportItem :: ImportItem -> ImportItem
normalizeImportItem item =
  case item of
    ImportItemVar _ namespace name -> ImportItemVar span0 namespace name
    ImportItemAbs _ namespace name -> ImportItemAbs span0 namespace name
    ImportItemAll _ namespace name -> ImportItemAll span0 namespace name
    ImportItemWith _ namespace name members -> ImportItemWith span0 namespace name members

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclValue _ valueDecl -> DeclValue span0 (normalizeValueDecl valueDecl)
    DeclTypeSig _ names ty -> DeclTypeSig span0 names (normalizeType ty)
    DeclStandaloneKindSig _ name kind -> DeclStandaloneKindSig span0 name (normalizeType kind)
    DeclFixity _ assoc mPrec ops -> DeclFixity span0 assoc mPrec ops
    DeclTypeSyn _ syn -> DeclTypeSyn span0 (normalizeTypeSynDecl syn)
    DeclData _ dataDecl -> DeclData span0 (normalizeDataDecl dataDecl)
    DeclNewtype _ newtypeDecl -> DeclNewtype span0 (normalizeNewtypeDecl newtypeDecl)
    DeclClass _ classDecl -> DeclClass span0 (normalizeClassDecl classDecl)
    DeclInstance _ instanceDecl -> DeclInstance span0 (normalizeInstanceDecl instanceDecl)
    DeclStandaloneDeriving _ derivingDecl -> DeclStandaloneDeriving span0 (normalizeStandaloneDerivingDecl derivingDecl)
    DeclDefault _ tys -> DeclDefault span0 (map normalizeType tys)
    DeclForeign _ foreignDecl -> DeclForeign span0 (normalizeForeignDecl foreignDecl)

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl valueDecl =
  case valueDecl of
    PatternBind _ pat rhs -> PatternBind span0 pat (normalizeRhs rhs)
    FunctionBind _ name matches -> FunctionBind span0 name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch match =
  Match
    { matchSpan = span0,
      matchPats = matchPats match,
      matchRhs = normalizeRhs (matchRhs match)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> UnguardedRhs span0 (normalizeExpr expr)
    GuardedRhss _ guards -> GuardedRhss span0 guards

normalizeTypeSynDecl :: TypeSynDecl -> TypeSynDecl
normalizeTypeSynDecl syn =
  TypeSynDecl
    { typeSynSpan = span0,
      typeSynName = typeSynName syn,
      typeSynParams = map normalizeTyVarBinder (typeSynParams syn),
      typeSynBody = normalizeType (typeSynBody syn)
    }

normalizeDataDecl :: DataDecl -> DataDecl
normalizeDataDecl dataDecl =
  DataDecl
    { dataDeclSpan = span0,
      dataDeclContext = map normalizeConstraint (dataDeclContext dataDecl),
      dataDeclName = dataDeclName dataDecl,
      dataDeclParams = map normalizeTyVarBinder (dataDeclParams dataDecl),
      dataDeclConstructors = map normalizeDataConDecl (dataDeclConstructors dataDecl),
      dataDeclDeriving = dataDeclDeriving dataDecl
    }

normalizeNewtypeDecl :: NewtypeDecl -> NewtypeDecl
normalizeNewtypeDecl newtypeDecl =
  NewtypeDecl
    { newtypeDeclSpan = span0,
      newtypeDeclContext = map normalizeConstraint (newtypeDeclContext newtypeDecl),
      newtypeDeclName = newtypeDeclName newtypeDecl,
      newtypeDeclParams = map normalizeTyVarBinder (newtypeDeclParams newtypeDecl),
      newtypeDeclConstructor = fmap normalizeDataConDecl (newtypeDeclConstructor newtypeDecl),
      newtypeDeclDeriving = newtypeDeclDeriving newtypeDecl
    }

normalizeClassDecl :: ClassDecl -> ClassDecl
normalizeClassDecl classDecl =
  ClassDecl
    { classDeclSpan = span0,
      classDeclContext = fmap (map normalizeConstraint) (classDeclContext classDecl),
      classDeclName = classDeclName classDecl,
      classDeclParams = map normalizeTyVarBinder (classDeclParams classDecl),
      classDeclItems = map normalizeClassDeclItem (classDeclItems classDecl)
    }

normalizeClassDeclItem :: ClassDeclItem -> ClassDeclItem
normalizeClassDeclItem item =
  case item of
    ClassItemTypeSig _ names ty -> ClassItemTypeSig span0 names (normalizeType ty)
    ClassItemFixity _ assoc mPrec ops -> ClassItemFixity span0 assoc mPrec ops
    ClassItemDefault _ valueDecl -> ClassItemDefault span0 (normalizeValueDecl valueDecl)

normalizeInstanceDecl :: InstanceDecl -> InstanceDecl
normalizeInstanceDecl instanceDecl =
  InstanceDecl
    { instanceDeclSpan = span0,
      instanceDeclContext = map normalizeConstraint (instanceDeclContext instanceDecl),
      instanceDeclClassName = instanceDeclClassName instanceDecl,
      instanceDeclTypes = map normalizeType (instanceDeclTypes instanceDecl),
      instanceDeclItems = map normalizeInstanceDeclItem (instanceDeclItems instanceDecl)
    }

normalizeInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem
normalizeInstanceDeclItem item =
  case item of
    InstanceItemBind _ valueDecl -> InstanceItemBind span0 (normalizeValueDecl valueDecl)
    InstanceItemTypeSig _ names ty -> InstanceItemTypeSig span0 names (normalizeType ty)
    InstanceItemFixity _ assoc mPrec ops -> InstanceItemFixity span0 assoc mPrec ops

normalizeStandaloneDerivingDecl :: StandaloneDerivingDecl -> StandaloneDerivingDecl
normalizeStandaloneDerivingDecl derivingDecl =
  StandaloneDerivingDecl
    { standaloneDerivingSpan = span0,
      standaloneDerivingStrategy = standaloneDerivingStrategy derivingDecl,
      standaloneDerivingContext = map normalizeConstraint (standaloneDerivingContext derivingDecl),
      standaloneDerivingClassName = standaloneDerivingClassName derivingDecl,
      standaloneDerivingTypes = map normalizeType (standaloneDerivingTypes derivingDecl)
    }

normalizeForeignDecl :: ForeignDecl -> ForeignDecl
normalizeForeignDecl foreignDecl =
  ForeignDecl
    { foreignDeclSpan = span0,
      foreignDirection = foreignDirection foreignDecl,
      foreignCallConv = foreignCallConv foreignDecl,
      foreignSafety = foreignSafety foreignDecl,
      foreignEntity = foreignEntity foreignDecl,
      foreignName = foreignName foreignDecl,
      foreignType = normalizeType (foreignType foreignDecl)
    }

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder binder =
  TyVarBinder
    { tyVarBinderSpan = span0,
      tyVarBinderName = tyVarBinderName binder,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind binder)
    }

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint constraint =
  Constraint
    { constraintSpan = span0,
      constraintClass = constraintClass constraint,
      constraintArgs = map normalizeType (constraintArgs constraint),
      constraintParen = constraintParen constraint
    }

normalizeDataConDecl :: DataConDecl -> DataConDecl
normalizeDataConDecl dataCon =
  case dataCon of
    PrefixCon _ forallVars constraints name bangTypes ->
      PrefixCon span0 forallVars (map normalizeConstraint constraints) name (map normalizeBangType bangTypes)
    InfixCon _ forallVars constraints lhs name rhs ->
      InfixCon span0 forallVars (map normalizeConstraint constraints) (normalizeBangType lhs) name (normalizeBangType rhs)
    RecordCon _ forallVars constraints name fields ->
      RecordCon span0 forallVars (map normalizeConstraint constraints) name (map normalizeFieldDecl fields)
    GadtCon _ binders constraints names body ->
      GadtCon span0 (map normalizeTyVarBinder binders) (map normalizeConstraint constraints) names (normalizeGadtBody body)

normalizeBangType :: BangType -> BangType
normalizeBangType bang =
  BangType
    { bangSpan = span0,
      bangStrict = bangStrict bang,
      bangType = normalizeType (bangType bang)
    }

normalizeFieldDecl :: FieldDecl -> FieldDecl
normalizeFieldDecl fieldDecl =
  FieldDecl
    { fieldSpan = span0,
      fieldNames = fieldNames fieldDecl,
      fieldType = normalizeBangType (fieldType fieldDecl)
    }

normalizeGadtBody :: GadtBody -> GadtBody
normalizeGadtBody body =
  case body of
    GadtPrefixBody bangTypes ty -> GadtPrefixBody (map normalizeBangType bangTypes) (normalizeType ty)
    GadtRecordBody fields ty -> GadtRecordBody (map normalizeFieldDecl fields) (normalizeType ty)
