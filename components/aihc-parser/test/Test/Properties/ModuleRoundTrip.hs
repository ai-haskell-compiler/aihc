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
    n <- chooseInt (1, 6)
    decls <- vectorOf n (resize 4 arbitrary)
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
    DeclDefault _ tys ->
      [DeclDefault span0 tys' | tys' <- shrinkDeclTypes tys]
    _ -> []

instance Arbitrary Decl where
  arbitrary =
    sized (genDecl . min 4)

  shrink = shrinkDecl

genDecl :: Int -> Gen Decl
genDecl n
  | n <= 0 =
      oneof
        [ genValueDecl,
          genTypeSigDecl,
          genFixityDecl,
          genDefaultDecl
        ]
  | otherwise =
      frequency
        [ (5, genValueDecl),
          (3, genTypeSigDecl),
          (2, genStandaloneKindSigDecl),
          (2, genFixityDecl),
          (3, genTypeSynDecl),
          (3, genDataDecl (n - 1)),
          (3, genNewtypeDecl (n - 1)),
          (2, genDefaultDecl)
        ]

genValueDecl :: Gen Decl
genValueDecl = do
  name <- genIdent
  expr <- resize 4 genExpr
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
  ty <- resize 4 (genType 4)
  pure (DeclTypeSig span0 names ty)

genStandaloneKindSigDecl :: Gen Decl
genStandaloneKindSigDecl = do
  name <- genTypeName
  kind <- resize 3 (genType 3)
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
  body <- resize 4 (genType 4)
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
  constructors <- genDataConstructors n
  pure $
    DeclData
      span0
      DataDecl
        { dataDeclSpan = span0,
          dataDeclContext = [],
          dataDeclName = name,
          dataDeclParams = params,
          dataDeclConstructors = constructors,
          dataDeclDeriving = []
        }

genNewtypeDecl :: Int -> Gen Decl
genNewtypeDecl n = do
  name <- genTypeName
  params <- genTyVarBinders
  constructor <- genSingleConstructor n
  pure $
    DeclNewtype
      span0
      NewtypeDecl
        { newtypeDeclSpan = span0,
          newtypeDeclContext = [],
          newtypeDeclName = name,
          newtypeDeclParams = params,
          newtypeDeclConstructor = Just constructor,
          newtypeDeclDeriving = []
        }

genDefaultDecl :: Gen Decl
genDefaultDecl = DeclDefault span0 <$> genNonEmptyDeclTypes

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
  elements ["+", "-", "*", "/", "<>", "<++>", "&&", "||", "++"]

shrinkFixityOp :: Text -> [Text]
shrinkFixityOp op =
  [candidate | candidate <- ["+", "-", "*"], candidate /= op]

genTyVarBinders :: Gen [TyVarBinder]
genTyVarBinders = do
  n <- chooseInt (0, 2)
  vectorOf n genTyVarBinder

genTyVarBinder :: Gen TyVarBinder
genTyVarBinder = do
  name <- genTypeVarName
  kind <- frequency [(4, pure Nothing), (1, Just <$> resize 3 (genType 3))]
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
      vectorOf count (genPrefixConstructor n)

genSingleConstructor :: Int -> Gen DataConDecl
genSingleConstructor = genPrefixConstructor

genPrefixConstructor :: Int -> Gen DataConDecl
genPrefixConstructor n = do
  name <- genTypeName
  fields <- genBangTypes n
  pure (PrefixCon span0 [] [] name fields)

genBangTypes :: Int -> Gen [BangType]
genBangTypes n = do
  count <- chooseInt (0, 3)
  vectorOf count (genBangType n)

genBangType :: Int -> Gen BangType
genBangType n = do
  strict <- arbitrary
  ty <- resize 4 (genType (min 4 n))
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
    PrefixCon _ _ _ name fields ->
      [PrefixCon span0 [] [] name' fields | name' <- shrinkTypeName name]
        <> [PrefixCon span0 [] [] name fields' | fields' <- shrinkBangTypes fields]
    _ -> []

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
    <> [dataDecl {dataDeclParams = params'} | params' <- shrinkTyVarBinders (dataDeclParams dataDecl)]
    <> [dataDecl {dataDeclConstructors = ctors'} | ctors' <- shrinkDataConstructors (dataDeclConstructors dataDecl)]

shrinkNewtypeDecl :: NewtypeDecl -> [NewtypeDecl]
shrinkNewtypeDecl newtypeDecl =
  [newtypeDecl {newtypeDeclName = name'} | name' <- shrinkTypeName (newtypeDeclName newtypeDecl)]
    <> [newtypeDecl {newtypeDeclParams = params'} | params' <- shrinkTyVarBinders (newtypeDeclParams newtypeDecl)]
    <> [ newtypeDecl {newtypeDeclConstructor = ctor'}
       | ctor' <- shrinkMaybeDataConDecl (newtypeDeclConstructor newtypeDecl)
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
  vectorOf n (resize 4 (genType 4))

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
    DeclDefault _ tys -> DeclDefault span0 (map normalizeType tys)
    _ -> decl

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
