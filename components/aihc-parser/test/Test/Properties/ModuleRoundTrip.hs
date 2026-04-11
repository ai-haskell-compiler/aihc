{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import Data.Char (isUpper)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr, span0)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (errs, reparsed) = parseModule moduleConfig source
   in counterexample (T.unpack source) $
        case errs of
          [] ->
            let expected = normalizeModule modu
                actual = normalizeModule reparsed
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
          _ ->
            counterexample (formatParseErrors "<quickcheck>" (Just source) errs) False

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, Arrows, UnboxedTuples, UnboxedSums, TemplateHaskell, ExplicitNamespaces, PatternSynonyms]
    }

instance Arbitrary Module where
  arbitrary = do
    n <- chooseInt (1, 6)
    -- Generate unique names by generating more than needed and deduplicating
    candidateNames <- vectorOf (n * 2) genIdent
    let names = take n (nub candidateNames)
    exprs <- vectorOf (length names) (resize 4 genExpr)
    imports <- genImportDecls
    mHead <- genMaybeModuleHead
    decls <- mapM genFunctionDecl (zip (map (mkUnqualifiedName NameVarId) names) exprs)
    pure $
      Module
        { moduleSpan = span0,
          moduleHead = mHead,
          moduleLanguagePragmas = baseModuleLanguagePragmas,
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

genFunctionDecl :: (UnqualifiedName, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  infixHead <- arbitrary
  if infixHead
    then do
      lhs <- genIdent
      rhs <- genIdent
      pure $
        DeclValue
          span0
          ( FunctionBind
              span0
              name
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadInfix,
                    matchPats =
                      [ PVar span0 (mkUnqualifiedName NameVarId lhs),
                        PVar span0 (mkUnqualifiedName NameVarId rhs)
                      ],
                    matchRhs = UnguardedRhs span0 expr
                  }
              ]
          )
    else
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

baseModuleLanguagePragmas :: [ExtensionSetting]
baseModuleLanguagePragmas =
  [ EnableExtension BlockArguments,
    EnableExtension Arrows,
    EnableExtension UnboxedTuples,
    EnableExtension UnboxedSums,
    EnableExtension TemplateHaskell,
    EnableExtension ExplicitNamespaces,
    EnableExtension PatternSynonyms
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
          | name' <- shrinkUnqualifiedVarName name
          ]
            <> [ DeclValue span0 (FunctionBind span0 name [match {matchRhs = UnguardedRhs span0 expr'}])
               | expr' <- shrinkExpr expr
               ]
        _ -> []
    _ -> []

genExportVarName :: Gen Name
genExportVarName = qualifyName Nothing <$> genUnqualifiedVarName

genExportTypeName :: Gen Name
genExportTypeName = qualifyName Nothing <$> genTypeName

shrinkExportVarName :: Name -> [Name]
shrinkExportVarName name =
  [qualifyName Nothing n | n <- shrinkUnqualifiedVarName (mkUnqualifiedName (nameType name) (nameText name))]

shrinkExportTypeName :: Name -> [Name]
shrinkExportTypeName name =
  [qualifyName Nothing n | n <- shrinkTypeName (mkUnqualifiedName (nameType name) (nameText name))]

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule span0 Nothing <$> genModuleName,
        ExportVar span0 Nothing Nothing <$> genExportVarName,
        ExportAbs span0 Nothing <$> genTypeNamespace <*> genExportTypeName,
        ExportAll span0 Nothing <$> genTypeNamespace <*> genExportTypeName,
        ExportWith span0 Nothing <$> genBundledNamespace <*> genExportTypeName <*> genExportMembers
      ]

  shrink spec =
    case spec of
      ExportModule _ _ modName ->
        [ExportModule span0 Nothing shrunk | shrunk <- shrinkModuleName modName]
      ExportVar _ mWarning namespace name ->
        [ExportVar span0 Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportVar span0 mWarning namespace shrunk | shrunk <- shrinkExportVarName name]
      ExportAbs _ mWarning namespace name ->
        [ExportAbs span0 Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAbs span0 mWarning namespace shrunk | shrunk <- shrinkExportTypeName name]
      ExportAll _ mWarning namespace name ->
        [ExportAbs span0 mWarning namespace name]
          <> [ExportAll span0 Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAll span0 mWarning namespace shrunk | shrunk <- shrinkExportTypeName name]
      ExportWith _ mWarning namespace name members ->
        [ExportAbs span0 mWarning namespace name | not (null members)]
          <> [ExportWith span0 Nothing namespace name members | Just _ <- [mWarning]]
          <> [ExportWith span0 mWarning namespace shrunk members | shrunk <- shrinkExportTypeName name]
          <> [ExportWith span0 mWarning namespace name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]

instance Arbitrary IEEntityNamespace where
  arbitrary = elements [IEEntityNamespaceType, IEEntityNamespacePattern, IEEntityNamespaceData]

  shrink namespace =
    case namespace of
      IEEntityNamespaceType -> []
      IEEntityNamespacePattern -> [IEEntityNamespaceType]
      IEEntityNamespaceData -> [IEEntityNamespaceType]

instance Arbitrary IEBundledNamespace where
  arbitrary = pure IEBundledNamespaceData

  shrink namespace =
    case namespace of
      IEBundledNamespaceData -> []

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
      [ ImportItemVar span0 Nothing <$> genUnqualifiedVarName,
        ImportItemAbs span0 <$> genTypeNamespace <*> genTypeName,
        ImportItemAll span0 <$> genTypeNamespace <*> genTypeName,
        ImportItemWith span0 <$> genBundledNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink item =
    case item of
      ImportItemVar _ namespace name ->
        [ImportItemVar span0 namespace shrunk | shrunk <- shrinkUnqualifiedVarName name]
      ImportItemAbs _ namespace name ->
        [ImportItemAbs span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemAll _ namespace name ->
        [ImportItemAbs span0 namespace name]
          <> [ImportItemAll span0 namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemWith _ namespace name members ->
        [ImportItemAbs span0 namespace name | not (null members)]
          <> [ImportItemWith span0 namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemWith span0 namespace name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]

instance Arbitrary IEBundledMember where
  arbitrary = do
    namespace <- genMemberNamespace
    name <- genMemberNameFor namespace
    pure (IEBundledMember namespace name)

  shrink (IEBundledMember namespace name) =
    [IEBundledMember shrunkNamespace name | shrunkNamespace <- shrink namespace]
      <> [IEBundledMember namespace shrunkName | shrunkName <- shrinkMemberNameFor namespace name]

genMemberName :: Gen Name
genMemberName =
  frequency
    [ (3, qualifyName Nothing <$> genUnqualifiedMemberName),
      (1, genQualifiedMemberName)
    ]

genUnqualifiedMemberName :: Gen UnqualifiedName
genUnqualifiedMemberName =
  oneof [genUnqualifiedVarName, genTypeName]

genQualifiedMemberName :: Gen Name
genQualifiedMemberName = do
  modName <- genModuleName
  qualifyName (Just modName) <$> genTypeName

genMemberNameFor :: Maybe IEBundledNamespace -> Gen Name
genMemberNameFor namespace =
  case namespace of
    Nothing -> genMemberName
    Just _ -> qualifyName Nothing <$> genTypeName

shrinkMemberNameFor :: Maybe IEBundledNamespace -> Name -> [Name]
shrinkMemberNameFor namespace name =
  case namespace of
    Nothing -> shrinkUnqualifiedVarNameFor name <> shrinkTypeNameFor name
    Just _ -> shrinkTypeNameFor name
  where
    shrinkUnqualifiedVarNameFor n =
      [ qualifyName (nameQualifier n) (mkUnqualifiedName NameVarId candidate)
      | nameType n == NameVarId,
        candidate <- shrinkIdent (nameText n)
      ]
    shrinkTypeNameFor n =
      [ qualifyName (nameQualifier n) (mkUnqualifiedName NameConId candidate)
      | nameType n == NameConId,
        candidate <- map T.pack (shrink (T.unpack (nameText n))),
        not (T.null candidate),
        isUpper (T.head candidate)
      ]

instance Arbitrary ImportDecl where
  arbitrary = do
    modName <- genModuleName
    spec <- genMaybeImportSpec
    pure $
      ImportDecl
        { importDeclSpan = span0,
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclSource = False,
          importDeclSafe = False,
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

genTypeName :: Gen UnqualifiedName
genTypeName = do
  first <- elements ['A' .. 'Z']
  restLen <- chooseInt (0, 5)
  rest <- vectorOf restLen (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
  pure (mkUnqualifiedName NameConId (T.pack (first : rest)))

shrinkTypeName :: UnqualifiedName -> [UnqualifiedName]
shrinkTypeName name =
  [ mkUnqualifiedName NameConId candidate
  | candidate <- map T.pack (shrink (T.unpack (renderUnqualifiedName name))),
    isValidTypeName candidate
  ]

isValidTypeName :: Text -> Bool
isValidTypeName name =
  case T.uncons name of
    Just (first, rest) ->
      (first `elem` ['A' .. 'Z'])
        && T.all (`elem` (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'")) rest
    Nothing -> False

genExportMembers :: Gen [IEBundledMember]
genExportMembers = do
  n <- chooseInt (1, 3)
  vectorOf n arbitrary

genTypeNamespace :: Gen (Maybe IEEntityNamespace)
genTypeNamespace =
  frequency
    [ (3, pure Nothing),
      (1, pure (Just IEEntityNamespaceType)),
      (1, pure (Just IEEntityNamespaceData))
    ]

genBundledNamespace :: Gen (Maybe IEEntityNamespace)
genBundledNamespace =
  frequency
    [ (5, pure Nothing),
      (1, pure (Just IEEntityNamespacePattern))
    ]

genMemberNamespace :: Gen (Maybe IEBundledNamespace)
genMemberNamespace =
  frequency
    [ (4, pure Nothing),
      (1, pure (Just IEBundledNamespaceData))
    ]

genUnqualifiedVarName :: Gen UnqualifiedName
genUnqualifiedVarName = mkUnqualifiedName NameVarId <$> genIdent

shrinkUnqualifiedVarName :: UnqualifiedName -> [UnqualifiedName]
shrinkUnqualifiedVarName name =
  [ mkUnqualifiedName NameVarId candidate
  | candidate <- shrinkIdent (renderUnqualifiedName name)
  ]

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
      moduleLanguagePragmas = moduleLanguagePragmas modu,
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
    ExportModule _ mWarning modName -> ExportModule span0 (normalizeWarningText <$> mWarning) modName
    ExportVar _ mWarning namespace name -> ExportVar span0 (normalizeWarningText <$> mWarning) namespace name
    ExportAbs _ mWarning namespace name -> ExportAbs span0 (normalizeWarningText <$> mWarning) namespace name
    ExportAll _ mWarning namespace name -> ExportAll span0 (normalizeWarningText <$> mWarning) namespace name
    ExportWith _ mWarning namespace name members -> ExportWith span0 (normalizeWarningText <$> mWarning) namespace name (map normalizeExportMember members)

normalizeExportMember :: IEBundledMember -> IEBundledMember
normalizeExportMember (IEBundledMember namespace name) = IEBundledMember namespace name

normalizeWarningText :: WarningText -> WarningText
normalizeWarningText warningText =
  case warningText of
    DeprText _ msg -> DeprText span0 msg
    WarnText _ msg -> WarnText span0 msg

normalizeImportDecl :: ImportDecl -> ImportDecl
normalizeImportDecl decl =
  ImportDecl
    { importDeclSpan = span0,
      importDeclLevel = importDeclLevel decl,
      importDeclPackage = importDeclPackage decl,
      importDeclSource = importDeclSource decl,
      importDeclSafe = importDeclSafe decl,
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
    ImportItemWith _ namespace name members -> ImportItemWith span0 namespace name (map normalizeExportMember members)

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclValue _ valueDecl -> DeclValue span0 (normalizeValueDecl valueDecl)
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
      matchHeadForm = matchHeadForm match,
      matchPats = map normalizeMatchPattern (matchPats match),
      matchRhs = normalizeRhs (matchRhs match)
    }

normalizeMatchPattern :: Pattern -> Pattern
normalizeMatchPattern pat =
  case pat of
    PVar _ name -> PVar span0 name
    _ -> pat

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> UnguardedRhs span0 (normalizeExpr expr)
    GuardedRhss _ guards -> GuardedRhss span0 guards
