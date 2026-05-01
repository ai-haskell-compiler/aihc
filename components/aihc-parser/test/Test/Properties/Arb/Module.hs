{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.Arb.Module
  ( genModuleName,
    shrinkModuleName,
    genUnqualifiedVarName,
    genTypeName,
    shrinkTypeName,
  )
where

import Aihc.Parser.Syntax
import Data.Text (Text)
import Data.Text qualified as T
import Test.Properties.Arb.Decl ()
import Test.Properties.Arb.Identifiers (genVarId, genVarSym, shrinkName, shrinkUnqualifiedName)
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
  warningText <- frequency [(4, pure Nothing), (1, Just <$> genWarningPragma)]
  pure $
    ModuleHead
      { moduleHeadAnns = [],
        moduleHeadName = name,
        moduleHeadWarningPragma = warningText,
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
    <> [ head' {moduleHeadWarningPragma = shrunk}
       | shrunk <- (Nothing :) . map Just . maybe [] shrinkWarningPragma $ moduleHeadWarningPragma head'
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

genWarningMessage :: Gen Text
genWarningMessage = do
  n <- chooseInt (1, 8)
  chars <- vectorOf n (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> [' ']))
  pure (T.pack chars)

shrinkWarningMessage :: Text -> [Text]
shrinkWarningMessage msg =
  [T.pack candidate | candidate <- shrink (T.unpack msg), not (null candidate)]

genMaybeWarningPragma :: Gen (Maybe Pragma)
genMaybeWarningPragma = frequency [(4, pure Nothing), (1, Just <$> genWarningPragma)]

genWarningPragma :: Gen Pragma
genWarningPragma = do
  msg <- genWarningMessage
  pt <- elements [PragmaWarning msg, PragmaDeprecated msg]
  pure Pragma {pragmaType = pt, pragmaRawText = ""}

shrinkWarningPragma :: Pragma -> [Pragma]
shrinkWarningPragma p = case pragmaType p of
  PragmaWarning msg -> [p {pragmaType = PragmaWarning msg'} | msg' <- shrinkWarningMessage msg]
  PragmaDeprecated msg -> [p {pragmaType = PragmaDeprecated msg'} | msg' <- shrinkWarningMessage msg]
  _ -> []

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule <$> genMaybeWarningPragma <*> genModuleName,
        ExportVar <$> genMaybeWarningPragma <*> pure Nothing <*> genExportVarName,
        ExportAbs <$> genMaybeWarningPragma <*> arbitrary <*> genExportTypeName,
        -- Namespace keywords (pattern/type/data) before T(..) are not valid GHC syntax
        ExportAll <$> genMaybeWarningPragma <*> pure Nothing <*> genExportTypeName,
        -- Namespace keywords (pattern/type/data) are not valid in ExportWith/ExportWithAll
        ExportWith <$> genMaybeWarningPragma <*> pure Nothing <*> genExportTypeName <*> genExportMembers,
        genExportWithAll
      ]

  shrink spec =
    case spec of
      ExportAnn _ sub -> sub : shrink sub
      ExportModule _ modName ->
        [ExportModule Nothing shrunk | shrunk <- shrinkModuleName modName]
      ExportVar mWarning namespace name ->
        [ExportVar Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportVar mWarning namespace shrunk | shrunk <- shrinkName name]
      ExportAbs mWarning namespace name ->
        [ExportAbs Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAbs mWarning namespace shrunk | shrunk <- shrinkName name]
      ExportAll mWarning namespace name ->
        [ExportAbs mWarning namespace name]
          <> [ExportAll Nothing namespace name | Just _ <- [mWarning]]
          <> [ExportAll mWarning namespace shrunk | shrunk <- shrinkName name]
      ExportWith mWarning _namespace name members ->
        -- Always use Nothing namespace (pattern/type/data not valid in ExportWith)
        [ExportAbs mWarning Nothing name | not (null members)]
          <> [ExportWith Nothing Nothing name members | Just _ <- [mWarning]]
          <> [ExportWith mWarning Nothing shrunk members | shrunk <- shrinkName name]
          <> [ExportWith mWarning Nothing name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]
      ExportWithAll mWarning _namespace name wildcardIndex members ->
        -- Always use Nothing namespace (pattern/type/data not valid in ExportWithAll)
        [ExportWith mWarning Nothing name members]
          <> [ExportWithAll Nothing Nothing name wildcardIndex members | Just _ <- [mWarning]]
          <> [ExportWithAll mWarning Nothing shrunk wildcardIndex members | shrunk <- shrinkName name]
          <> [ExportWithAll mWarning Nothing name shrunkIndex members | shrunkIndex <- shrinkWildcardIndex wildcardIndex members]
          <> [ExportWithAll mWarning Nothing name (min wildcardIndex (length shrunk)) shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]

instance Arbitrary IEEntityNamespace where
  arbitrary = elements [IEEntityNamespaceType, IEEntityNamespacePattern, IEEntityNamespaceData]

  shrink namespace =
    case namespace of
      IEEntityNamespaceType -> []
      IEEntityNamespacePattern -> [IEEntityNamespaceType]
      IEEntityNamespaceData -> [IEEntityNamespaceType]

instance Arbitrary IEBundledNamespace where
  arbitrary = elements [IEBundledNamespaceType, IEBundledNamespaceData]

  shrink namespace =
    case namespace of
      IEBundledNamespaceType -> []
      IEBundledNamespaceData -> [IEBundledNamespaceType]

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
        ImportItemWith <$> genBundledNamespace <*> genTypeName <*> genImportMembers
      ]

  shrink item =
    case item of
      ImportAnn _ sub -> sub : shrink sub
      ImportItemVar namespace name ->
        [ImportItemVar namespace shrunk | shrunk <- shrinkUnqualifiedName name]
      ImportItemAbs namespace name ->
        [ImportItemAbs namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemAll namespace name ->
        [ImportItemAbs namespace name]
          <> [ImportItemAll namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemWith namespace name members ->
        [ImportItemAbs namespace name | not (null members)]
          <> [ImportItemWith namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemWith namespace name shrunk | shrunk <- shrinkList shrink members, not (null shrunk)]
      ImportItemAllWith namespace name _wildcardIndex members ->
        [ImportItemWith namespace name members]

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
  oneof [mkUnqualifiedName NameVarId <$> genVarId, genTypeName]

genQualifiedMemberName :: Gen Name
genQualifiedMemberName = do
  modName <- genModuleName
  qualifyName (Just modName) <$> oneof [mkUnqualifiedName NameVarId <$> genVarId, genTypeName]

genMemberNameFor :: Maybe IEBundledNamespace -> Gen Name
genMemberNameFor namespace =
  case namespace of
    Nothing -> genMemberName
    Just _ -> qualifyName Nothing <$> genTypeName

genExportWithAll :: Gen ExportSpec
genExportWithAll = do
  mWarning <- genMaybeWarningPragma
  -- Namespace keywords (pattern/type/data) are not valid before T(..) in exports
  let namespace = Nothing
  name <- genExportTypeName
  members <- listOf1 arbitrary
  wildcardIndex <- chooseInt (0, length members)
  pure (ExportWithAll mWarning namespace name wildcardIndex members)

genImportMembers :: Gen [IEBundledMember]
genImportMembers =
  frequency
    [ (1, pure []),
      (4, genExportMembers)
    ]

shrinkWildcardIndex :: Int -> [a] -> [Int]
shrinkWildcardIndex wildcardIndex members =
  [shrunk | shrunk <- shrink wildcardIndex, shrunk >= 0, shrunk <= length members]

shrinkMemberNameFor :: Maybe IEBundledNamespace -> Name -> [Name]
shrinkMemberNameFor _namespace = shrinkName

instance Arbitrary ImportDecl where
  arbitrary = do
    modName <- genModuleName
    spec <- genMaybeImportSpec
    pure $
      ImportDecl
        { importDeclAnns = [],
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclSourcePragma = Nothing,
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
  n <- chooseInt (0, 3)
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
      (1, pure (Just IEEntityNamespaceData))
    ]

genMemberNamespace :: Gen (Maybe IEBundledNamespace)
genMemberNamespace =
  frequency
    [ (4, pure Nothing),
      (1, pure (Just IEBundledNamespaceData)),
      (1, pure (Just IEBundledNamespaceType))
    ]

genUnqualifiedVarName :: Gen UnqualifiedName
genUnqualifiedVarName =
  oneof
    [ mkUnqualifiedName NameVarId <$> genVarId,
      mkUnqualifiedName NameVarSym <$> genVarSym
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
