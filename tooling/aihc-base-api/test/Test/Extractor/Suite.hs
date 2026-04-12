{-# LANGUAGE OverloadedStrings #-}

module Test.Extractor.Suite
  ( tests,
  )
where

import Aihc.BaseApi.Extractor
import Aihc.BaseApi.Extractor.PackageConf
import Data.List (dropWhileEnd, find)
import Data.Text qualified as T
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-base-api"
    [ testCase "package conf parser handles pkgroot" test_parsePackageConf,
      testCase "local extraction finds Prelude.id" test_extractLocalPreludeId
    ]

test_parsePackageConf :: IO ()
test_parsePackageConf = do
  let pkgroot = "/tmp/pkgdb"
      conf =
        T.unlines
          [ "name: base",
            "version: 4.20.2.0",
            "id: base-4.20.2.0-754f",
            "exposed-modules:",
            "  Prelude, Data.Maybe, GHC.Num.Integer from ghc-bignum-1.3-8849:GHC.Num.Integer",
            "import-dirs: ${pkgroot}/../lib/example-base"
          ]
  parsed <- either fail pure (parseBasePackageConf pkgroot conf)
  assertEqual "version" "4.20.2.0" (basePackageVersion parsed)
  assertEqual "modules" ["Prelude", "Data.Maybe", "GHC.Num.Integer"] (baseExposedModules parsed)
  assertEqual "import dir" ["/tmp/pkgdb/../lib/example-base"] (baseImportDirs parsed)

test_extractLocalPreludeId :: IO ()
test_extractLocalPreludeId = do
  ghcVersion <- T.strip . T.pack <$> readProcess "ghc" ["--numeric-version"] ""
  libdir <- dropWhileEnd (== '\n') <$> readProcess "ghc" ["--print-libdir"] ""
  packageDb <- dropWhileEnd (== '\n') <$> readProcess "ghc" ["--print-global-package-db"] ""
  snapshot <- extractBaseApiFromPaths ghcVersion "local-test" libdir packageDb
  let preludeModule = find ((== "Prelude") . apiModuleName) (snapshotModules snapshot)
      idExport = preludeModule >>= find ((== "id") . exportName) . apiModuleExports
  assertBool "Prelude module exists" (maybe False (const True) preludeModule)
  assertBool "Prelude.id exists" (maybe False (const True) idExport)
