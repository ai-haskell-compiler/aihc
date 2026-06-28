{-# LANGUAGE OverloadedStrings #-}

module Test.ExtractHiCompare
  ( extractHiCompareTests,
  )
where

import Aihc.Dev.ExtractHi (extractPackage, extractSourcePackage)
import Aihc.Dev.ExtractHi.Compare
  ( CompatibilityReport (..),
    CoreLibProgressReport (..),
    comparePackageCompatibility,
    comparePackageSubset,
    renderCoreLibProgressReports,
  )
import Aihc.Dev.ExtractHi.Types
import Control.Exception (bracket)
import Data.Text qualified as T
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

extractHiCompareTests :: TestTree
extractHiCompareTests =
  testGroup
    "extract-hi compare"
    [ testCase "accepts empty candidate module exports" test_acceptsEmptyCandidateModuleExports,
      testCase "rejects candidate module missing from oracle" test_rejectsMissingModule,
      testCase "rejects changed value type" test_rejectsChangedValueType,
      testGroup
        "core-libs-progress"
        [ testCase "counts matching exported interface facts" test_coreLibProgressCountsMatches,
          testCase "extracts source package exports" test_coreLibProgressExtractsSourcePackage,
          testCase "counts missing candidate exports as failures" test_coreLibProgressCountsMissingExports,
          testCase "rejects changed value types and type kinds" test_coreLibProgressRejectsChangedSignatures,
          testCase "requires exports to come from the same module" test_coreLibProgressRequiresSameModule,
          testCase "counts candidate-only exports separately" test_coreLibProgressCountsExtrasSeparately,
          testCase "renders stable command output" test_coreLibProgressRendersStableOutput
        ],
      testCase "aihc-prim is a subset of ghc-prim" test_aihcPrimSubset,
      testCase "aihc-internal is a subset of ghc-internal" test_aihcInternalSubset
    ]

test_acceptsEmptyCandidateModuleExports :: Assertion
test_acceptsEmptyCandidateModuleExports =
  assertEqual
    "mismatches"
    []
    (comparePackageSubset (pkg [emptyModule "GHC.Tuple"]) (pkg [moduleWithValue "GHC.Tuple" "Solo" "Solo a"]))

test_rejectsMissingModule :: Assertion
test_rejectsMissingModule =
  assertBool "expected missing module mismatch" $
    not (null (comparePackageSubset (pkg [emptyModule "Missing"]) (pkg [emptyModule "GHC.Tuple"])))

test_rejectsChangedValueType :: Assertion
test_rejectsChangedValueType =
  assertBool "expected changed type mismatch" $
    not (null (comparePackageSubset (pkg [moduleWithValue "GHC.Tuple" "Solo" "Int"]) (pkg [moduleWithValue "GHC.Tuple" "Solo" "Solo a"])))

test_coreLibProgressCountsMatches :: Assertion
test_coreLibProgressCountsMatches =
  assertEqual
    "report"
    (CompatibilityReport 6 6 0 [])
    (comparePackageCompatibility (pkg [fullModule "A"]) (pkg [fullModule "A"]))

test_coreLibProgressExtractsSourcePackage :: Assertion
test_coreLibProgressExtractsSourcePackage =
  withTempDir "core-libs-progress-source" $ \root -> do
    let srcDir = root </> "src" </> "Data"
    createDirectoryIfMissing True srcDir
    writeFile (root </> "demo-base.cabal") demoBaseCabal
    writeFile (srcDir </> "Bool.hs") demoBoolSource
    iface <- extractSourcePackage root "demo-base"
    case piModules iface of
      [modIface] -> do
        assertEqual "module" "Data.Bool" (miModule modIface)
        assertEqual "values" [ExportedValue "&&" "Bool -> Bool -> Bool", ExportedValue "not" "Bool -> Bool"] (miValues modIface)
        assertEqual "types" [ExportedType "Bool" "<unspecified-source-kind>" ["False", "True"]] (miTypes modIface)
        assertEqual "fixities" [FixityInfo "&&" InfixR 3] (miFixities modIface)
      _ -> assertFailure ("expected one source module, got " <> show (piModules iface))

test_coreLibProgressCountsMissingExports :: Assertion
test_coreLibProgressCountsMissingExports = do
  let report = comparePackageCompatibility (pkg [emptyModule "A"]) (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 0 (crExtra report)
  assertEqual "mismatches" 1 (length (crMismatches report))

test_coreLibProgressRejectsChangedSignatures :: Assertion
test_coreLibProgressRejectsChangedSignatures = do
  let candidate =
        (emptyModule "A")
          { miValues = [ExportedValue "f" "Bool"],
            miTypes = [ExportedType "T" "Bool" []]
          }
      oracle =
        (emptyModule "A")
          { miValues = [ExportedValue "f" "Int"],
            miTypes = [ExportedType "T" "Type" []]
          }
      report = comparePackageCompatibility (pkg [candidate]) (pkg [oracle])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 2 (crTotal report)
  assertEqual "mismatches" 2 (length (crMismatches report))

test_coreLibProgressRequiresSameModule :: Assertion
test_coreLibProgressRequiresSameModule = do
  let report =
        comparePackageCompatibility
          (pkg [moduleWithValue "B" "f" "Int"])
          (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 0 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 1 (crExtra report)

test_coreLibProgressCountsExtrasSeparately :: Assertion
test_coreLibProgressCountsExtrasSeparately = do
  let candidate =
        (moduleWithValue "A" "f" "Int")
          { miValues =
              [ ExportedValue "f" "Int",
                ExportedValue "extra" "Int"
              ]
          }
      report = comparePackageCompatibility (pkg [candidate]) (pkg [moduleWithValue "A" "f" "Int"])
  assertEqual "matched" 1 (crMatched report)
  assertEqual "total" 1 (crTotal report)
  assertEqual "extras" 1 (crExtra report)
  assertEqual "mismatches" 0 (length (crMismatches report))

test_coreLibProgressRendersStableOutput :: Assertion
test_coreLibProgressRendersStableOutput =
  assertEqual
    "output"
    "GHC_PRIM 1 4 25.00\nBASE 2 5 40.00\nEXTRA ghc-prim 2\nEXTRA base 3\n"
    ( renderCoreLibProgressReports
        [ CoreLibProgressReport "GHC_PRIM" "ghc-prim" (CompatibilityReport 1 4 2 []),
          CoreLibProgressReport "BASE" "base" (CompatibilityReport 2 5 3 [])
        ]
    )

test_aihcPrimSubset :: Assertion
test_aihcPrimSubset = do
  candidate <- extractPackage "aihc-prim"
  oracle <- extractPackage "ghc-prim"
  assertEqual "aihc-prim mismatches" [] (comparePackageSubset candidate oracle)

test_aihcInternalSubset :: Assertion
test_aihcInternalSubset = do
  candidate <- extractPackage "aihc-internal"
  oracle <- extractPackage "ghc-internal"
  assertEqual "aihc-internal mismatches" [] (comparePackageSubset candidate oracle)

pkg :: [ModuleInterface] -> PackageInterface
pkg modules =
  PackageInterface
    { piPackage = "pkg-0",
      piModules = modules
    }

emptyModule :: String -> ModuleInterface
emptyModule name =
  ModuleInterface
    { miModule = fromString name,
      miTypes = [],
      miValues = [],
      miClasses = [],
      miFixities = []
    }

moduleWithValue :: String -> String -> String -> ModuleInterface
moduleWithValue moduleName valueName valueType =
  (emptyModule moduleName)
    { miValues =
        [ ExportedValue
            { evName = fromString valueName,
              evType = fromString valueType
            }
        ]
    }

fullModule :: String -> ModuleInterface
fullModule name =
  (emptyModule name)
    { miValues = [ExportedValue "f" "Int"],
      miTypes = [ExportedType "T" "Type" ["MkT"]],
      miClasses = [ExportedClass "C" [ClassMethod "method" "Int"]],
      miFixities = [FixityInfo "+" InfixL 6]
    }

demoBaseCabal :: String
demoBaseCabal =
  unlines
    [ "cabal-version: 3.8",
      "name: demo-base",
      "version: 0.1.0.0",
      "build-type: Simple",
      "library",
      "  exposed-modules: Data.Bool",
      "  hs-source-dirs: src",
      "  default-language: GHC2021"
    ]

demoBoolSource :: String
demoBoolSource =
  unlines
    [ "module Data.Bool",
      "  ( Bool(False, True),",
      "    not,",
      "    (&&),",
      "  )",
      "where",
      "data Bool = False | True",
      "infixr 3 &&",
      "not :: Bool -> Bool",
      "not False = True",
      "not True = False",
      "(&&) :: Bool -> Bool -> Bool",
      "False && _ = False",
      "True && x = x"
    ]

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action

fromString :: String -> T.Text
fromString = T.pack
