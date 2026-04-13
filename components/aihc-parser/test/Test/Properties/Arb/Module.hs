{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Module
  ( genModuleName,
    shrinkModuleName,
    genUnqualifiedVarName,
    shrinkUnqualifiedVarName,
    genTypeName,
    shrinkTypeName,
    baseModuleLanguagePragmas,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Decl ()
import Test.Properties.Arb.Expr (genOperator, isValidGeneratedOperator, span0)
import Test.Properties.Arb.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

instance Arbitrary Module where
  arbitrary = do
    n <- chooseInt (0, 6)
    imports <- genImportDecls
    mHead <- genMaybeModuleHead
    decls <- vectorOf n arbitrary
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
    | shrunk <- shrinkList shrink (moduleDecls modu),
      not (null shrunk)
    ]
      <> [ modu {moduleImports = shrunk}
         | shrunk <- shrinkList shrinkImportDecl (moduleImports modu)
         ]
      <> [ modu {moduleHead = shrunk}
         | shrunk <- shrinkMaybeModuleHead (moduleHead modu)
         ]

baseModuleLanguagePragmas :: [ExtensionSetting]
baseModuleLanguagePragmas =
  [ EnableExtension BlockArguments,
    EnableExtension Arrows,
    EnableExtension UnboxedTuples,
    EnableExtension UnboxedSums,
    EnableExtension TemplateHaskell,
    EnableExtension UnicodeSyntax,
    EnableExtension LambdaCase,
    EnableExtension QuasiQuotes,
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
  warningText <- frequency [(4, pure Nothing), (1, Just <$> arbitrary)]
  pure $
    ModuleHead
      { moduleHeadSpan = span0,
        moduleHeadName = name,
        moduleHeadWarningText = warningText,
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
    <> [ head' {moduleHeadWarningText = shrunk}
       | shrunk <- shrink (moduleHeadWarningText head')
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

genWarningMessage :: Gen Text
genWarningMessage = do
  n <- chooseInt (1, 8)
  chars <- vectorOf n (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> [' ']))
  pure (T.pack chars)

shrinkWarningMessage :: Text -> [Text]
shrinkWarningMessage msg =
  [T.pack candidate | candidate <- shrink (T.unpack msg), not (null candidate)]

instance Arbitrary WarningText where
  arbitrary =
    oneof
      [ DeprText span0 <$> genWarningMessage,
        WarnText span0 <$> genWarningMessage
      ]

  shrink wt =
    case wt of
      DeprText _ msg -> [DeprText span0 msg' | msg' <- shrinkWarningMessage msg]
      WarnText _ msg -> [WarnText span0 msg' | msg' <- shrinkWarningMessage msg]

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule span0 <$> arbitrary <*> genModuleName,
        ExportVar span0 <$> arbitrary <*> pure Nothing <*> genExportVarName,
        ExportAbs span0 <$> arbitrary <*> arbitrary <*> genExportTypeName,
        ExportAll span0 <$> arbitrary <*> arbitrary <*> genExportTypeName,
        ExportWith span0 <$> arbitrary <*> arbitrary <*> genExportTypeName <*> genExportMembers,
        ExportWithAll span0 <$> arbitrary <*> arbitrary <*> genExportTypeName <*> genExportMembers
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
      ExportWithAll _ mWarning namespace name members ->
        [ExportWith span0 mWarning namespace name members]
          <> [ExportWithAll span0 Nothing namespace name members | Just _ <- [mWarning]]
          <> [ExportWithAll span0 mWarning namespace shrunk members | shrunk <- shrinkExportTypeName name]
          <> [ExportWithAll span0 mWarning namespace name shrunk | shrunk <- shrinkList shrink members]

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
        ImportItemWith span0 <$> genBundledNamespace <*> genTypeName <*> genExportMembers,
        ImportItemAllWith span0 <$> genBundledNamespace <*> genTypeName <*> genExportMembers
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
      ImportItemAllWith _ namespace name members ->
        [ImportItemWith span0 namespace name members]
          <> [ImportItemAllWith span0 namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemAllWith span0 namespace name shrunk | shrunk <- shrinkList shrink members]

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
  qualifyName (Just modName) <$> oneof [genUnqualifiedVarName, genTypeName]

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
      [ qualifyName (nameQualifier n) (mkUnqualifiedName (nameType n) candidate)
      | nameType n `elem` [NameVarId, NameVarSym],
        candidate <- shrinkVarText n
      ]
    shrinkTypeNameFor n =
      [ qualifyName (nameQualifier n) (mkUnqualifiedName NameConId candidate)
      | nameType n == NameConId,
        candidate <- map T.pack (shrink (T.unpack (nameText n))),
        not (T.null candidate),
        isUpper (T.head candidate)
      ]
    shrinkVarText n =
      case nameType n of
        NameVarId -> shrinkIdent (nameText n)
        NameVarSym -> shrinkSymbolicName (nameText n)
        _ -> []

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
genUnqualifiedVarName =
  oneof
    [ mkUnqualifiedName NameVarId <$> genIdent,
      mkUnqualifiedName NameVarSym <$> genOperator
    ]

shrinkUnqualifiedVarName :: UnqualifiedName -> [UnqualifiedName]
shrinkUnqualifiedVarName name =
  [ mkUnqualifiedName (unqualifiedNameType name) candidate
  | candidate <- shrinkUnqualifiedVarText name
  ]

shrinkUnqualifiedVarText :: UnqualifiedName -> [Text]
shrinkUnqualifiedVarText name =
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
