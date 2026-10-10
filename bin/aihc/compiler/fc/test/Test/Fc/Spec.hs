{-# LANGUAGE OverloadedStrings #-}

module Test.Fc.Spec (tests) where

import Aihc.Fc (FcDesugarResult (..), Program (..), allPublicDesugarConfig, desugarModuleFc, scopeEntries)
import Aihc.Parser (defaultConfig, parseModule)
import Aihc.Prim.Wiring (primTcConfig, primTcWiring)
import Aihc.Resolve (Package (..), PackageId (..), ResolvedUnit (..), builtins, collectModuleExportsWithDeps, modulesInPackage, resolveUnit)
import Aihc.Tc (emptyTcInterface, mkTcKinds, typecheckModuleSccWithInterface, typecheckModulesWithInterface)
import Test.Fc.Fold (fcFoldTests)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcFixtureTests, fcGoldenTests, fcLintTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: IO TestTree
tests = do
  fc <- fcFixtureTests
  fcLint <- fcLintTests
  fcGolden <- fcGoldenTests
  pure (testGroup "aihc-fc" [fc, fcLint, fcGolden, fcPropertyTests, fcFoldTests, moduleIdentityTests])

moduleIdentityTests :: TestTree
moduleIdentityTests =
  testGroup
    "module identity"
    [ identityTest "dependency order" typecheckModulesWithInterface,
      identityTest "module component" typecheckModuleSccWithInterface
    ]
  where
    identityTest label check = testCase label $ do
      let sources =
            [ "module Provider where",
              "module Reexport (module Provider) where\nimport Provider"
            ]
          parsed = map (parseModule defaultConfig) sources
          package = Package "aihc-base" (PackageId "aihc-base-test")
          units = modulesInPackage package [(ast, []) | (_, ast) <- parsed]
          exports = collectModuleExportsWithDeps mempty units
          prim = PackageId "aihc-prim"
          config = allPublicDesugarConfig (mkTcKinds (primTcWiring prim)) prim
      mapM_ (assertEqual "parse errors" [] . fst) parsed
      case resolveUnit (builtins package exports []) exports units of
        Left failure -> assertFailure (show failure)
        Right unit -> do
          let (checked, interface) = check (primTcConfig prim) emptyTcInterface (resolvedModules unit)
              results = map (desugarModuleFc config [] interface) checked
          assertEqual "module count" 2 (length results)
          mapM_ (assertEqual "FC diagnostics" [] . dsErrors) results
          mapM_
            ( \(name, result) -> do
                assertBool "FC success" (dsSuccess result)
                assertEqual
                  "module scope"
                  [(1, PackageId "aihc-base-test", name), (2, prim, "GHC.Types")]
                  (scopeEntries (programScopes (dsProgram result)))
            )
            (zip ["Provider", "Reexport"] results)
