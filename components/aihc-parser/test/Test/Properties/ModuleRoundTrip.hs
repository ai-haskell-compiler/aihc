{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Properties.ModuleRoundTrip
  ( prop_modulePrettyRoundTrip,
  )
where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Parser
import Parser.Ast
import Parser.Pretty (prettyModule)
import Parser.Types (ParseResult (..))
import Test.Properties.ExprHelpers (genExpr, normalizeExpr, shrinkExpr, span0)
import Test.Properties.Identifiers (genIdent, shrinkIdent)
import Test.QuickCheck

prop_modulePrettyRoundTrip :: Module -> Property
prop_modulePrettyRoundTrip modu =
  let source = prettyModule modu
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
    -- Generate unique names by generating more than needed and deduplicating
    candidateNames <- vectorOf (n * 2) genIdent
    let names = take n (nub candidateNames)
    exprs <- vectorOf (length names) (genExpr 4)
    imports <- genImportDecls
    pure $
      Module
        { moduleSpan = span0,
          moduleName = Just "Generated",
          moduleLanguagePragmas = [],
          moduleWarningText = Nothing,
          -- NOTE: moduleExports = Nothing to work around layout lexer issue
          -- with parenthesized import/export specs. See xfail golden test:
          -- Test/Fixtures/golden/module/import-export-spec-layout-bug.yaml
          moduleExports = Nothing,
          moduleImports = imports,
          moduleDecls =
            [ DeclValue
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
            | (name, expr) <- zip names exprs
            ]
        }

  shrink modu =
    [ modu {moduleDecls = shrunk}
    | shrunk <- shrinkList shrinkDecl (moduleDecls modu),
      not (null shrunk)
    ]
      <> [ modu {moduleImports = shrunk}
         | shrunk <- shrinkList shrinkImportDecl (moduleImports modu)
         ]

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
        ExportAbs span0 Nothing <$> genTypeName,
        ExportAll span0 Nothing <$> genTypeName,
        ExportWith span0 Nothing <$> genTypeName <*> genExportMembers
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

instance Arbitrary ImportDecl where
  arbitrary = do
    modName <- genModuleName
    pure $
      ImportDecl
        { importDeclSpan = span0,
          importDeclLevel = Nothing,
          importDeclPackage = Nothing,
          importDeclQualified = False,
          importDeclQualifiedPost = False,
          importDeclModule = modName,
          importDeclAs = Nothing,
          -- NOTE: importDeclSpec = Nothing to work around layout lexer issue
          -- with parenthesized import/export specs. See xfail golden test:
          -- Test/Fixtures/golden/module/import-export-spec-layout-bug.yaml
          importDeclSpec = Nothing
        }

  shrink decl =
    [ decl {importDeclModule = shrunk}
    | shrunk <- shrinkModuleName (importDeclModule decl)
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
  vectorOf n genIdent

-- Module normalization
normalizeModule :: Module -> Module
normalizeModule modu =
  Module
    { moduleSpan = span0,
      moduleName = moduleName modu,
      moduleLanguagePragmas = [],
      moduleWarningText = Nothing,
      moduleExports = Nothing,
      moduleImports = map normalizeImportDecl (moduleImports modu),
      moduleDecls = map normalizeDecl (moduleDecls modu)
    }

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
      importDeclSpec = Nothing
    }

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
      matchPats = matchPats match,
      matchRhs = normalizeRhs (matchRhs match)
    }

normalizeRhs :: Rhs -> Rhs
normalizeRhs rhs =
  case rhs of
    UnguardedRhs _ expr -> UnguardedRhs span0 (normalizeExpr expr)
    GuardedRhss _ guards -> GuardedRhss span0 guards
