{-# LANGUAGE OverloadedStrings #-}

module Test.ExtractHiCompare
  ( extractHiCompareTests,
  )
where

import Aihc.Dev.ExtractHi (extractPackage)
import Aihc.Dev.ExtractHi.Compare (comparePackageSubset)
import Aihc.Dev.ExtractHi.Types
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

extractHiCompareTests :: TestTree
extractHiCompareTests =
  testGroup
    "extract-hi compare"
    [ testCase "accepts empty candidate module exports" test_acceptsEmptyCandidateModuleExports,
      testCase "rejects candidate module missing from oracle" test_rejectsMissingModule,
      testCase "rejects changed value type" test_rejectsChangedValueType,
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

fromString :: String -> T.Text
fromString = T.pack
