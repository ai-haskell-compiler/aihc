{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr, span0)
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
            counterexample (errorBundlePretty (Just source) err) False

instance Arbitrary Module where
  arbitrary = do
    n <- chooseInt (1, 6)
    -- Generate unique names by generating more than needed and deduplicating
    candidateNames <- vectorOf (n * 2) genIdent
    let names = take n (nub candidateNames)
    exprs <- vectorOf (length names) (resize 4 genExpr)
    imports <- genImportDecls
    mHead <- genMaybeModuleHead
    decls <- mapM genFunctionDecl (zip names exprs)
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

genFunctionDecl :: (Text, Expr) -> Gen Decl
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
                    matchPats = [PVar span0 lhs, PVar span0 rhs],
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
