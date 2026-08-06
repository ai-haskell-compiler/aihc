{-# LANGUAGE OverloadedStrings #-}

module Test.Resolver.Suite
  ( resolverGoldenTests,
    resolverUnitTests,
  )
where

import Aihc.Parser (defaultConfig, parseModule)
import Aihc.Resolve (DependencyHash (..), GlobalName (..), ModuleId (..), OccName (..), PackageId (..), PackageName (..), PackageVersion (..), ResolveResult (..), ResolvedName (..), Scope (..), collectModuleExportsForPackage, extractInterface, renderPackageId, resolve, resolvePackage, resolvePackageWithDeps, resolveWithDeps, resolvedGlobals)
import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified ResolverGolden as RG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, testCaseInfo)

resolverUnitTests :: TestTree
resolverUnitTests =
  testGroup
    "resolver-unit"
    [ testCase "dependency-backed GHC.Num supplies built-in fromInteger" testDependencyBackedGhcNum,
      testCase "same module and spelling remain distinct across package variants" testPackageVariantIdentity,
      testCase "package-qualified imports select the requested package" testPackageQualifiedImports,
      testCase "full package IDs select dependency-hash variants" testPackageVariantImports
    ]

testPackageVariantIdentity :: Assertion
testPackageVariantIdentity =
  case parseModule defaultConfig "module Shared where\nvalue = 1\n" of
    ([], modu) -> do
      let left = resolvePackage packageA [modu]
          right = resolvePackage packageB [modu]
          exports = collectModuleExportsForPackage packageA (resolvedModules left) <> collectModuleExportsForPackage packageB (resolvedModules right)
          identities = [name | scope <- Map.elems exports, ResolvedTopLevel name <- Map.elems (scopeTerms scope), globalOccName name == OccName "value"]
      assertEqual "both package variants survive interface union" 2 (length identities)
      assertBool "identities differ" (case identities of [first, second] -> first /= second; _ -> False)
    (errors, _) -> assertFailure (show errors)

testPackageQualifiedImports :: Assertion
testPackageQualifiedImports =
  case (parseModule defaultConfig providerSource, parseModule defaultConfig consumerSource) of
    (([], provider), ([], consumer)) -> do
      let left = resolvePackage packageA [provider]
          right = resolvePackage packageB [provider]
          dependencies =
            collectModuleExportsForPackage packageA (resolvedModules left)
              <> collectModuleExportsForPackage packageB (resolvedModules right)
          result = resolvePackageWithDeps consumerPackage dependencies [consumer]
          importedValues =
            [ globalModule name
            | resolvedModule <- resolvedModules result,
              name <- resolvedGlobals resolvedModule,
              globalOccName name == OccName "value"
            ]
      assertEqual "resolver diagnostics" [] (resolveErrors result)
      assertEqual "both selected variants occur" [packageA, packageB] (List.sort (map modulePackage importedValues))
    ((errors@(_ : _), _), _) -> assertFailure (show errors)
    (_, (errors@(_ : _), _)) -> assertFailure (show errors)
  where
    providerSource = "module Shared (value) where\nvalue = 1\n"
    consumerSource =
      "{-# LANGUAGE PackageImports #-}\n\
      \module Consumer where\n\
      \import qualified \"pkg-a\" Shared as A\n\
      \import qualified \"pkg-b\" Shared as B\n\
      \pair = (A.value, B.value)\n"

testPackageVariantImports :: Assertion
testPackageVariantImports =
  case (parseModule defaultConfig providerSource, parseModule defaultConfig consumerSource) of
    (([], provider), ([], consumer)) -> do
      let dependencies =
            collectModuleExportsForPackage variantA (resolvedModules (resolvePackage variantA [provider]))
              <> collectModuleExportsForPackage variantB (resolvedModules (resolvePackage variantB [provider]))
          result = resolvePackageWithDeps consumerPackage dependencies [consumer]
          importedPackages =
            [ modulePackage (globalModule name)
            | resolvedModule <- resolvedModules result,
              name <- resolvedGlobals resolvedModule,
              globalOccName name == OccName "value"
            ]
      assertEqual "resolver diagnostics" [] (resolveErrors result)
      assertEqual "both dependency-hash variants occur" [variantA, variantB] (List.sort importedPackages)
    ((errors@(_ : _), _), _) -> assertFailure (show errors)
    (_, (errors@(_ : _), _)) -> assertFailure (show errors)
  where
    providerSource = "module Shared (value) where\nvalue = 1\n"
    consumerSource =
      "{-# LANGUAGE PackageImports #-}\n\
      \module Consumer where\n\
      \import qualified \""
        <> renderPackageId variantA
        <> "\" Shared as A\n\
           \import qualified \""
        <> renderPackageId variantB
        <> "\" Shared as B\n\
           \pair = (A.value, B.value)\n"

packageA, packageB, consumerPackage :: PackageId
packageA = PackageId (PackageName "pkg-a") (PackageVersion "1.0") (DependencyHash "deps-a")
packageB = PackageId (PackageName "pkg-b") (PackageVersion "1.0") (DependencyHash "deps-b")
consumerPackage = PackageId (PackageName "consumer") (PackageVersion "1.0") (DependencyHash "deps")

variantA, variantB :: PackageId
variantA = PackageId (PackageName "shared") (PackageVersion "1.0") (DependencyHash "deps-a")
variantB = PackageId (PackageName "shared") (PackageVersion "1.0") (DependencyHash "deps-b")

testDependencyBackedGhcNum :: Assertion
testDependencyBackedGhcNum =
  case (parse "GHC.Num" numSource, parse "Prelude" preludeSource) of
    (Right numModule, Right preludeModule) -> do
      let dependencyResult = resolve [numModule]
          result = resolveWithDeps (extractInterface dependencyResult) [preludeModule]
      case resolveErrors dependencyResult of
        [] ->
          case resolveErrors result of
            [] -> pure ()
            errors -> assertFailure ("failed to resolve built-in syntax through dependency exports: " <> show errors)
        errors -> assertFailure ("failed to resolve dependency module: " <> show errors)
    (Left errors, _) -> assertFailure errors
    (_, Left errors) -> assertFailure errors
  where
    parse sourceName source =
      case parseModule defaultConfig source of
        ([], modu) -> Right modu
        (errors, _) -> Left (sourceName <> " parse failure: " <> show errors)
    numSource :: Text
    numSource =
      "module GHC.Num (Num (..)) where\n\
      \data Integer = Integer\n\
      \class Num a where\n\
      \  fromInteger :: Integer -> a\n"
    preludeSource :: Text
    preludeSource =
      "module Prelude (Num (..)) where\n\
      \import GHC.Num (Num (..))\n\
      \one = 1\n"

resolverGoldenTests :: IO TestTree
resolverGoldenTests = do
  cases <- RG.loadResolverCases
  checks <- mapM mkResolverCaseTest cases
  summary <- mkSummaryTest cases
  pure
    ( testGroup
        "resolver-golden"
        (checks <> [summary])
    )

mkResolverCaseTest :: RG.ResolverCase -> IO TestTree
mkResolverCaseTest meta = pure $ case RG.caseStatus meta of
  RG.StatusXFail -> testCaseInfo (RG.caseId meta) (assertXFailResolverCase meta >> pure "Known failure - to be fixed")
  RG.StatusXPass -> testCaseInfo (RG.caseId meta) (assertResolverCase meta >> pure "Known bug - to be fixed")
  _ -> testCase (RG.caseId meta) (assertResolverCase meta)

assertXFailResolverCase :: RG.ResolverCase -> Assertion
assertXFailResolverCase meta =
  case RG.evaluateResolverCase meta of
    (RG.OutcomeXFail, _details) -> pure ()
    (RG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xfail resolver case "
            <> RG.caseId meta
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertResolverCase :: RG.ResolverCase -> Assertion
assertResolverCase meta =
  case RG.evaluateResolverCase meta of
    (RG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in resolver case "
            <> RG.caseId meta
            <> " ("
            <> RG.caseCategory meta
            <> ") expected "
            <> show (RG.caseStatus meta)
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    (RG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xpass resolver case "
            <> RG.caseId meta
            <> " reason="
            <> RG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

mkSummaryTest :: [RG.ResolverCase] -> IO TestTree
mkSummaryTest cases = do
  let outcomes = map evaluate cases
  pure $ testCase "summary" (assertNoRegressions outcomes)

evaluate :: RG.ResolverCase -> (RG.ResolverCase, RG.Outcome, String)
evaluate meta =
  let (outcome, details) = RG.evaluateResolverCase meta
   in (meta, outcome, details)

assertNoRegressions :: [(RG.ResolverCase, RG.Outcome, String)] -> Assertion
assertNoRegressions outcomes = do
  let (passN, xfailN, xpassN, failN) = RG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( "resolver golden regressions found. "
          <> "pass="
          <> show passN
          <> " xfail="
          <> show xfailN
          <> " xpass="
          <> show xpassN
          <> " fail="
          <> show failN
          <> " completion="
          <> show completion
          <> "%"
      )

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0
