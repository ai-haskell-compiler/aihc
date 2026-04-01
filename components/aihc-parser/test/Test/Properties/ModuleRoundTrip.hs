{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax (Extension (TemplateHaskell, UnboxedSums, UnboxedTuples))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.BareSyntax
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

moduleConfig :: ParserConfig
moduleConfig =
  defaultConfig
    { parserExtensions = [UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty (toSyntaxModule modu)))
   in counterexample (T.unpack source) $
        case parseModule moduleConfig source of
          ParseOk reparsed ->
            let expected = normalizeModule modu
                actual = normalizeModule (eraseModule reparsed)
             in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)
          ParseErr err ->
            counterexample (errorBundlePretty (Just source) err) False

instance Arbitrary Module where
  arbitrary = do
    n <- chooseInt (1, 6)
    candidateNames <- vectorOf (n * 2) genIdent
    let names = take n (nub candidateNames)
    exprs <- vectorOf (length names) (resize 4 genExpr)
    imports <- genImportDecls
    mHead <- genMaybeModuleHead
    decls <- mapM genFunctionDecl (zip names exprs)
    pure $
      Module
        { moduleHead = mHead,
          moduleLanguagePragmas = [],
          moduleImports = imports,
          moduleDecls = decls
        }

  shrink modu =
    [modu {moduleDecls = shrunk} | shrunk <- shrinkList shrinkDecl (moduleDecls modu), not (null shrunk)]
      <> [modu {moduleImports = shrunk} | shrunk <- shrinkList shrinkImportDecl (moduleImports modu)]
      <> [modu {moduleHead = shrunk} | shrunk <- shrinkMaybeModuleHead (moduleHead modu)]

genFunctionDecl :: (Text, Expr) -> Gen Decl
genFunctionDecl (name, expr) = do
  infixHead <- arbitrary
  if infixHead
    then do
      lhs <- genIdent
      rhs <- genIdent
      pure $
        DeclValue
          ( FunctionBind
              name
              [ Match
                  { matchHeadForm = MatchHeadInfix,
                    matchPats = [PVar lhs, PVar rhs],
                    matchRhs = UnguardedRhs expr
                  }
              ]
          )
    else
      pure $
        DeclValue
          ( FunctionBind
              name
              [ Match
                  { matchHeadForm = MatchHeadPrefix,
                    matchPats = [],
                    matchRhs = UnguardedRhs expr
                  }
              ]
          )

genMaybeModuleHead :: Gen (Maybe ModuleHead)
genMaybeModuleHead =
  frequency
    [ (9, Just <$> genModuleHead),
      (1, pure Nothing)
    ]

genModuleHead :: Gen ModuleHead
genModuleHead = do
  name <- genModuleName
  exports <- genMaybeExportSpecs
  pure $
    ModuleHead
      { moduleHeadName = name,
        moduleHeadWarningText = Nothing,
        moduleHeadExports = exports
      }

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
    DeclValue (FunctionBind name [match]) ->
      case matchRhs match of
        UnguardedRhs expr ->
          [DeclValue (FunctionBind name' [match {matchRhs = UnguardedRhs expr}]) | name' <- shrinkIdent name]
            <> [DeclValue (FunctionBind name [match {matchRhs = UnguardedRhs expr'}]) | expr' <- shrinkExpr expr]
        _ -> []
    _ -> []

instance Arbitrary ExportSpec where
  arbitrary =
    oneof
      [ ExportModule <$> genModuleName,
        ExportVar Nothing <$> genIdent,
        ExportAbs <$> genTypeNamespace <*> genTypeName,
        ExportAll <$> genTypeNamespace <*> genTypeName,
        ExportWith <$> genTypeNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink spec =
    case spec of
      ExportModule modName ->
        [ExportModule shrunk | shrunk <- shrinkModuleName modName]
      ExportVar namespace name ->
        [ExportVar namespace shrunk | shrunk <- shrinkIdent name]
      ExportAbs namespace name ->
        [ExportAbs namespace shrunk | shrunk <- shrinkTypeName name]
      ExportAll namespace name ->
        [ExportAbs namespace name]
          <> [ExportAll namespace shrunk | shrunk <- shrinkTypeName name]
      ExportWith namespace name members ->
        [ExportAbs namespace name | not (null members)]
          <> [ExportWith namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ExportWith namespace name shrunk | shrunk <- shrinkList shrinkIdent members, not (null shrunk)]

instance Arbitrary ImportSpec where
  arbitrary =
    ImportSpec
      <$> arbitrary
      <*> genImportItems

  shrink spec =
    [spec {importSpecHiding = shrunk} | shrunk <- shrink (importSpecHiding spec)]
      <> [spec {importSpecItems = shrunk} | shrunk <- shrinkList shrink (importSpecItems spec)]

instance Arbitrary ImportItem where
  arbitrary =
    oneof
      [ ImportItemVar Nothing <$> genIdent,
        ImportItemAbs <$> genTypeNamespace <*> genTypeName,
        ImportItemAll <$> genTypeNamespace <*> genTypeName,
        ImportItemWith <$> genTypeNamespace <*> genTypeName <*> genExportMembers
      ]

  shrink item =
    case item of
      ImportItemVar namespace name ->
        [ImportItemVar namespace shrunk | shrunk <- shrinkIdent name]
      ImportItemAbs namespace name ->
        [ImportItemAbs namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemAll namespace name ->
        [ImportItemAbs namespace name]
          <> [ImportItemAll namespace shrunk | shrunk <- shrinkTypeName name]
      ImportItemWith namespace name members ->
        [ImportItemAbs namespace name | not (null members)]
          <> [ImportItemWith namespace shrunk members | shrunk <- shrinkTypeName name]
          <> [ImportItemWith namespace name shrunk | shrunk <- shrinkList shrinkIdent members, not (null shrunk)]

instance Arbitrary ImportDecl where
  arbitrary = do
    modName <- genModuleName
    spec <- genMaybeImportSpec
    pure $
      ImportDecl
        { importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclQualified = False,
          importDeclQualifiedPost = False,
          importDeclModule = modName,
          importDeclAs = Nothing,
          importDeclSpec = spec
        }

  shrink decl =
    [decl {importDeclModule = shrunk} | shrunk <- shrinkModuleName (importDeclModule decl)]
      <> [decl {importDeclSpec = shrunk} | shrunk <- shrinkMaybeImportSpec (importDeclSpec decl)]

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
  oneof [genIdent, genTypeName]

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

normalizeModule :: Module -> Module
normalizeModule modu =
  Module
    { moduleHead = fmap normalizeModuleHead (moduleHead modu),
      moduleLanguagePragmas = [],
      moduleImports = map normalizeImportDecl (moduleImports modu),
      moduleDecls = map normalizeDecl (moduleDecls modu)
    }

normalizeModuleHead :: ModuleHead -> ModuleHead
normalizeModuleHead head' =
  ModuleHead
    { moduleHeadName = moduleHeadName head',
      moduleHeadWarningText = Nothing,
      moduleHeadExports = fmap (map normalizeExportSpec) (moduleHeadExports head')
    }

normalizeExportSpec :: ExportSpec -> ExportSpec
normalizeExportSpec spec =
  case spec of
    ExportModule modName -> ExportModule modName
    ExportVar namespace name -> ExportVar namespace name
    ExportAbs namespace name -> ExportAbs namespace name
    ExportAll namespace name -> ExportAll namespace name
    ExportWith namespace name members -> ExportWith namespace name members

normalizeImportDecl :: ImportDecl -> ImportDecl
normalizeImportDecl decl =
  ImportDecl
    { importDeclLevel = importDeclLevel decl,
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
    { importSpecHiding = importSpecHiding spec,
      importSpecItems = map normalizeImportItem (importSpecItems spec)
    }

normalizeImportItem :: ImportItem -> ImportItem
normalizeImportItem item =
  case item of
    ImportItemVar namespace name -> ImportItemVar namespace name
    ImportItemAbs namespace name -> ImportItemAbs namespace name
    ImportItemAll namespace name -> ImportItemAll namespace name
    ImportItemWith namespace name members -> ImportItemWith namespace name members

normalizeDecl :: Decl -> Decl
normalizeDecl decl =
  case decl of
    DeclValue valueDecl -> DeclValue (normalizeValueDecl valueDecl)
    DeclTypeSig names ty -> DeclTypeSig names ty

normalizeValueDecl :: ValueDecl -> ValueDecl
normalizeValueDecl valueDecl =
  case valueDecl of
    PatternBind pat rhs -> PatternBind pat (normalizeRhs rhs)
    FunctionBind name matches -> FunctionBind name (map normalizeMatch matches)

normalizeMatch :: Match -> Match
normalizeMatch match =
  Match
    { matchHeadForm = matchHeadForm match,
      matchPats = map normalizeMatchPattern (matchPats match),
      matchRhs = normalizeRhs (matchRhs match)
    }

normalizeMatchPattern :: Pattern -> Pattern
normalizeMatchPattern pat =
  case pat of
    PVar name -> PVar name
    _ -> pat

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs expr -> UnguardedRhs (normalizeExpr expr)
    GuardedRhss guards -> GuardedRhss guards
