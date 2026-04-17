{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Module
  ( genModuleName,
    shrinkModuleName,
    genUnqualifiedVarName,
    shrinkUnqualifiedVarName,
    genTypeName,
    shrinkTypeName,
  )
where

import Aihc.Parser.Syntax
import Data.Char (isUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Decl ()
import Test.Properties.Arb.Expr (genOperator, isValidGeneratedOperator)
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
        { moduleAnns = [],
          moduleHead = mHead,
          moduleLanguagePragmas = [],
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
      { moduleHeadAnns = [],
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
      [ DeprText <$> genWarningMessage,
        WarnText <$> genWarningMessage,
        WarningTextAnn (mkAnnotation ()) <$> leafWarningText
      ]
    where
      leafWarningText =
        oneof
          [ DeprText <$> genWarningMessage,
            WarnText <$> genWarningMessage
          ]

  shrink wt =
    case wt of
      DeprText msg -> [DeprText msg' | msg' <- shrinkWarningMessage msg]
      WarnText msg -> [WarnText msg' | msg' <- shrinkWarningMessage msg]
      WarningTextAnn _ sub -> sub : shrink sub

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule <$> arbitrary <*> genModuleName,
        ExportVar <$> arbitrary <*> pure Nothing <*> genExportVarName,
        ExportAbs <$> arbitrary <*> arbitrary <*> genExportTypeName,
        ExportAll <$> arbitrary <*> arbitrary <*> genExportTypeName,
        ExportWith <$> arbitrary <*> arbitrary <*> genExportTypeName <*> genExportMembers,
        ExportWithAll <$> arbitrary <*> arbitrary <*> genExportTypeName <*> genExportMembers
      ]

  shrink spec =
    case spec of
      ExportAnn _ sub -> sub : shrink sub
      ExportModule _ modName ->
        [ExportModule Nothing shrunk | shrunk <- shrinkModuleName modName]
      ExportVar mWarning namespace name ->
        [ExportVar Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportVar mWarning namespace shrunk | shrunk <- shrinkExportVarName name]
      ExportAbs mWarning namespace name ->
        [ExportAbs Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAbs mWarning namespace shrunk | shrunk <- shrinkExportTypeName name]
      ExportAll mWarning namespace name ->
        [ExportAbs mWarning namespace name]
          <> [ExportAll Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAll mWarning namespace shrunk | shrunk <- shrinkExportTypeName name]
      ExportWith mWarning namespace name members ->
        [ExportAbs mWarning namespace name | not (null members)]
          <> [ExportWith Nothing namespace name members | Just _ <- [mWarning]]
          <> [ExportWith mWarning namespace shrunk members | shrunk <- shrinkExportTypeName name]
          <> [ExportWith mWarning namespace name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]
      ExportWithAll mWarning namespace name members ->
        [ExportWith mWarning namespace name members]
          <> [ExportWithAll Nothing namespace name members | Just _ <- [mWarning]]
          <> [ExportWithAll mWarning namespace shrunk members | shrunk <- shrinkExportTypeName name]
          <> [ExportWithAll mWarning namespace name shrunk | shrunk <- shrinkList shrink members]

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
    ImportSpec []
      <$> arbitrary
      <*> genImportItems

  shrink spec =
    [spec {importSpecHiding = shrunk} | shrunk <- shrink (importSpecHiding spec)]
      <> [spec {importSpecItems = shrunk} | shrunk <- shrinkList shrink (importSpecItems spec)]

instance Arbitrary ImportItem where
  arbitrary =
    oneof
      [ ImportItemVar Nothing <$> genUnqualifiedVarName,
        ImportItemAbs <$> genTypeNamespace <*> genTypeName,
        ImportItemAll <$> genTypeNamespace <*> genTypeName,
        ImportItemWith <$> genBundledNamespace <*> genTypeName <*> genExportMembers,
        ImportItemAllWith <$> genBundledNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink item =
    case item of
      ImportAnn _ sub -> sub : shrink sub
      ImportItemVar namespace name ->
        [ImportItemVar namespace shrunk | shrunk <- shrinkUnqualifiedVarName name]
      ImportItemAbs namespace name ->
        [ImportItemAbs namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemAll namespace name ->
        [ImportItemAbs namespace name]
          <> [ImportItemAll namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemWith namespace name members ->
        [ImportItemAbs namespace name | not (null members)]
          <> [ImportItemWith namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemWith namespace name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]
      ImportItemAllWith namespace name members ->
        [ImportItemWith namespace name members]
          <> [ImportItemAllWith namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemAllWith namespace name shrunk | shrunk <- shrinkList shrink members]

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
        { importDeclAnns = [],
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
