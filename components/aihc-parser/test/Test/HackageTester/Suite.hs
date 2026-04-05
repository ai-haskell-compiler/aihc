{-# LANGUAGE OverloadedStrings #-}

module Test.HackageTester.Suite
  ( hackageTesterTests,
  )
where

import Aihc.Cpp (IncludeKind (..), IncludeRequest (..), Result (..))
import qualified Aihc.Parser.Syntax as Syntax
import Control.Exception (bracket)
import CppSupport (preprocessForParserIfEnabled)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GhcOracle (oracleModuleAstFingerprint)
import HackageSupport (fileInfoPath, findTargetFilesFromCabal, resolveIncludeBestEffort)
import HackageTester.CLI (Options (..), parseOptionsPure)
import HackageTester.Model (FileResult (..), Outcome (..), Summary (..), shouldFailSummary, summarizeResults)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

hackageTesterTests :: TestTree
hackageTesterTests =
  testGroup
    "hackage-tester"
    [ testGroup
        "cli"
        [ testCase "parses required package argument" test_cliParsesPackage,
          testCase "parses optional flags" test_cliParsesOptionalFlags,
          testCase "rejects missing package" test_cliRejectsMissingPackage,
          testCase "rejects invalid jobs" test_cliRejectsInvalidJobs
        ],
      testGroup
        "summary"
        [ testCase "counts outcomes correctly" test_summaryCountsOutcomes,
          testCase "fails when no files were processed" test_zeroFilesFails
        ],
      testGroup
        "oracle"
        [ testCase "accepts No-prefixed LANGUAGE pragmas" test_oracleAcceptsNoPrefixedLanguagePragma,
          testCase "accepts LANGUAGE Haskell2010 pragmas" test_oracleAcceptsHaskell2010LanguagePragma,
          testCase "accepts mixed-case LANGUAGE pragmas" test_oracleAcceptsMixedCaseLanguagePragma,
          testCase "accepts NondecreasingIndentation pragmas" test_oracleAcceptsNondecreasingIndentationPragma,
          testCase "applies implied extensions" test_oracleAppliesImpliedExtensions,
          testCase "defaults to Haskell2010 when language is omitted" test_oracleDefaultsToHaskell2010,
          testCase "uses Haskell2010 language defaults" test_oracleUsesHaskell2010Defaults,
          testCase "uses Haskell98 fallback defaults" test_oracleUsesHaskell98FallbackDefaults,
          testCase "handles CPP-defined LANGUAGE pragmas" test_oracleHandlesCppDefinedLanguagePragmas
        ],
      testGroup
        "package-selection"
        [ testCase "skips files from non-buildable components by default flags" test_skipsNonBuildableComponents,
          testCase "prefers top-level cabal when multiple cabal files exist" test_prefersTopLevelCabalWhenMultipleExist
        ],
      testGroup
        "cpp-macros"
        [ testCase "MIN_VERSION_base branch is taken" test_cppMinVersionBaseTrue,
          testCase "MIN_VERSION_ghc branch is taken" test_cppMinVersionGhcTrue,
          testCase "negated MIN_VERSION_ghc branch is not taken" test_cppNegatedMinVersionGhcFalse,
          testCase "MIN_VERSION_GLASGOW_HASKELL branch is taken" test_cppMinVersionGlasgowHaskellTrue,
          testCase "unknown MIN_VERSION package in include branch is taken" test_cppUnknownMinVersionFromIncludeTrue,
          testCase "cpp-options macros do not enable preprocessing without CPP extension" test_cppOptionsWithoutCppExtensionDoNotPreprocess
        ],
      testGroup
        "io"
        [ testCase "include resolution decodes invalid utf8 leniently" test_resolveIncludeLenientDecode
        ]
    ]

test_cliParsesPackage :: Assertion
test_cliParsesPackage =
  assertEqual
    "expected defaults with required package"
    (Right (Options "transformers" Nothing Nothing False False))
    (parseOptionsPure ["transformers"])

test_cliParsesOptionalFlags :: Assertion
test_cliParsesOptionalFlags =
  assertEqual
    "expected all optional flags to parse"
    (Right (Options "text" (Just "2.0.2") (Just 4) True True))
    (parseOptionsPure ["text", "--version", "2.0.2", "--jobs", "4", "--json", "--only-ghc-errors"])

test_cliRejectsMissingPackage :: Assertion
test_cliRejectsMissingPackage =
  assertLeftContaining "Missing: PACKAGE" (parseOptionsPure [])

test_cliRejectsInvalidJobs :: Assertion
test_cliRejectsInvalidJobs =
  assertLeftContaining "must be a positive integer" (parseOptionsPure ["bytestring", "--jobs", "0"])

test_summaryCountsOutcomes :: Assertion
test_summaryCountsOutcomes = do
  let results =
        [ FileResult "A.hs" OutcomeSuccess [] Nothing,
          FileResult "B.hs" OutcomeGhcError [] Nothing,
          FileResult "C.hs" OutcomeParseError [] Nothing,
          FileResult "D.hs" OutcomeRoundtripFail [] Nothing
        ]
      summary = summarizeResults results
  assertEqual "total files" 4 (totalFiles summary)
  assertEqual "successes" 1 (successCount summary)
  assertEqual "failures" 3 (failureCount summary)
  assertEqual "ghc errors" 1 (ghcErrors summary)
  assertEqual "parse errors" 1 (parseErrors summary)
  assertEqual "roundtrip fails" 1 (roundtripFails summary)

test_zeroFilesFails :: Assertion
test_zeroFilesFails =
  assertBool "expected empty run to fail" (shouldFailSummary (summarizeResults []))

test_oracleAcceptsNoPrefixedLanguagePragma :: Assertion
test_oracleAcceptsNoPrefixedLanguagePragma =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected NoMonomorphismRestriction pragma to be accepted, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE NoMonomorphismRestriction #-}",
          "module A where",
          "x = 1"
        ]

test_oracleAcceptsHaskell2010LanguagePragma :: Assertion
test_oracleAcceptsHaskell2010LanguagePragma =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected Haskell2010 language pragma to be accepted, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE Haskell2010 #-}",
          "module A where",
          "x = 1"
        ]

test_oracleAcceptsMixedCaseLanguagePragma :: Assertion
test_oracleAcceptsMixedCaseLanguagePragma =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected mixed-case BlockArguments pragma to be accepted, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# Language BlockArguments #-}",
          "module A where",
          "x = unsafePerformIO do",
          "  pure ()"
        ]

test_oracleAcceptsNondecreasingIndentationPragma :: Assertion
test_oracleAcceptsNondecreasingIndentationPragma =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected NondecreasingIndentation pragma to be accepted, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE NondecreasingIndentation #-}",
          "module A where",
          "foo = case True of",
          "  True -> do",
          "  x <- pure ()",
          "  pure x"
        ]

test_oracleAppliesImpliedExtensions :: Assertion
test_oracleAppliesImpliedExtensions =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected ScopedTypeVariables to imply ExplicitForAll, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE ScopedTypeVariables #-}",
          "module A where",
          "f :: forall a. a -> a",
          "f x = x"
        ]

test_oracleDefaultsToHaskell2010 :: Assertion
test_oracleDefaultsToHaskell2010 =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected omitted language to default to Haskell2010 record syntax, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "module A where",
          "data R = R { field :: Int }"
        ]

test_oracleUsesHaskell2010Defaults :: Assertion
test_oracleUsesHaskell2010Defaults =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected Haskell2010 defaults to enable traditional record syntax, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "module A where",
          "data R = R { field :: Int }"
        ]

test_oracleUsesHaskell98FallbackDefaults :: Assertion
test_oracleUsesHaskell98FallbackDefaults =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell98Edition [] source of
    Left err ->
      assertBool
        ("expected Haskell98 fallback defaults to allow nondecreasing indentation, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "module A where",
          "foo bs = do",
          "  let fn offset x = id $ \\(a, b) -> do",
          "      pure (offset + b)",
          "  pure bs"
        ]

test_oracleHandlesCppDefinedLanguagePragmas :: Assertion
test_oracleHandlesCppDefinedLanguagePragmas =
  case oracleModuleAstFingerprint "hackage-tester" Syntax.Haskell2010Edition [] source of
    Left err ->
      assertBool
        ("expected oracle to honor CPP-defined LANGUAGE pragmas, got: " <> T.unpack err)
        False
    Right {} -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#if 1",
          "{-# LANGUAGE LambdaCase #-}",
          "#endif",
          "module Test where",
          "x = \\case _ -> ()"
        ]

test_skipsNonBuildableComponents :: Assertion
test_skipsNonBuildableComponents =
  withTempDir "hackage-tester" $ \root -> do
    let cabalFile = root </> "demo.cabal"
        readmeFile = root </> "README.lhs"
        srcDir = root </> "src"
        mainFile = srcDir </> "Main.hs"

    createDirectory srcDir
    writeFile readmeFile "This is not buildable by default.\n"
    writeFile mainFile "main :: IO ()\nmain = pure ()\n"
    writeFile cabalFile sampleCabal

    files <- findTargetFilesFromCabal root
    let selected = map fileInfoPath files
    assertBool "expected Main.hs to be selected" (any ("src/Main.hs" `isSuffixOf`) selected)
    assertBool "expected README.lhs from disabled component to be skipped" (not (any ("README.lhs" `isSuffixOf`) selected))

test_prefersTopLevelCabalWhenMultipleExist :: Assertion
test_prefersTopLevelCabalWhenMultipleExist =
  withTempDir "hackage-tester" $ \root -> do
    let rootCabal = root </> "demo.cabal"
        nestedDir = root </> "testdata"
        nestedCabal = nestedDir </> "fixture.cabal"
        srcDir = root </> "src"
        mainFile = srcDir </> "Main.hs"

    createDirectory nestedDir
    createDirectory srcDir
    writeFile mainFile "module Main where\nmain :: IO ()\nmain = pure ()\n"
    writeFile rootCabal sampleCabal
    writeFile nestedCabal nestedFixtureCabal

    files <- findTargetFilesFromCabal root
    let selected = map fileInfoPath files
    assertBool "expected Main.hs from top-level package to be selected" (any ("src/Main.hs" `isSuffixOf`) selected)

test_resolveIncludeLenientDecode :: Assertion
test_resolveIncludeLenientDecode =
  withTempDir "hackage-tester" $ \root -> do
    let srcDir = root </> "src"
        incDir = root </> "include"
        current = srcDir </> "Main.hs"
        includeFile = incDir </> "bad.inc"
    createDirectory srcDir
    createDirectory incDir
    writeFile current "module Main where\n"
    BS.writeFile includeFile (BS.pack [65, 10, 255, 66, 10]) -- A\n<invalid>B\n
    mText <-
      resolveIncludeBestEffort
        root
        current
        IncludeRequest
          { includePath = "bad.inc",
            includeKind = IncludeSystem,
            includeFrom = current,
            includeLine = 1
          }
    case mText of
      Nothing -> assertBool "expected include bytes to resolve" False
      Just bs ->
        assertBool "expected raw bytes to be preserved" (bs == BS.pack [65, 10, 255, 66, 10])

test_cppMinVersionBaseTrue :: Assertion
test_cppMinVersionBaseTrue = do
  out <- preprocessWithIncludes "A.hs" mempty source
  assertBool "expected true branch for MIN_VERSION_base" ("hit\n" `T.isInfixOf` out)
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#if MIN_VERSION_base(4,12,0)",
          "hit",
          "#else",
          "miss",
          "#endif"
        ]

test_cppMinVersionGhcTrue :: Assertion
test_cppMinVersionGhcTrue = do
  out <- preprocessWithIncludes "A.hs" mempty source
  assertBool "expected true branch for MIN_VERSION_ghc" ("hit\n" `T.isInfixOf` out)
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#if MIN_VERSION_ghc(8,6,0)",
          "hit",
          "#else",
          "miss",
          "#endif"
        ]

test_cppNegatedMinVersionGhcFalse :: Assertion
test_cppNegatedMinVersionGhcFalse = do
  out <- preprocessWithIncludes "A.hs" mempty source
  assertBool "expected negated branch to be false for MIN_VERSION_ghc" ("miss\n" `T.isInfixOf` out)
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#if !MIN_VERSION_ghc(8,8,0)",
          "hit",
          "#else",
          "miss",
          "#endif"
        ]

test_cppMinVersionGlasgowHaskellTrue :: Assertion
test_cppMinVersionGlasgowHaskellTrue = do
  out <- preprocessWithIncludes "A.hs" mempty source
  assertBool "expected true branch for MIN_VERSION_GLASGOW_HASKELL" ("hit\n" `T.isInfixOf` out)
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#if MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)",
          "hit",
          "#else",
          "miss",
          "#endif"
        ]

test_cppUnknownMinVersionFromIncludeTrue :: Assertion
test_cppUnknownMinVersionFromIncludeTrue = do
  out <- preprocessWithIncludes "A.hs" includes source
  assertBool "expected true branch for unknown MIN_VERSION in include" ("hit\n" `T.isInfixOf` out)
  where
    source =
      T.unlines
        [ "{-# LANGUAGE CPP #-}",
          "#include \"defs.h\""
        ]
    includes =
      [ ("defs.h", T.unlines ["#if MIN_VERSION_custompkg(1,2,3)", "hit", "#else", "miss", "#endif"])
      ]

test_cppOptionsWithoutCppExtensionDoNotPreprocess :: Assertion
test_cppOptionsWithoutCppExtensionDoNotPreprocess = do
  Result {resultOutput = out} <-
    preprocessForParserIfEnabled [] ["-DTEST=1"] "A.hs" (\_ -> pure Nothing) source
  assertEqual "expected source to remain unchanged when CPP extension is not enabled" source out
  where
    source =
      T.unlines
        [ "module A where",
          "#if TEST",
          "x = 1",
          "#else",
          "x = 2",
          "#endif"
        ]

preprocessWithIncludes :: FilePath -> [(FilePath, T.Text)] -> T.Text -> IO T.Text
preprocessWithIncludes inputFile includeFiles source = do
  Result {resultOutput = out} <-
    preprocessForParserIfEnabled [] [] inputFile resolve source
  pure out
  where
    resolve req = pure (TE.encodeUtf8 <$> lookup (includePath req) includeFiles)

sampleCabal :: String
sampleCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0.1.0.0",
      "",
      "flag build-readme",
      "  default: False",
      "  manual: True",
      "",
      "executable readme",
      "  if !flag(build-readme)",
      "    buildable: False",
      "  main-is: README.lhs",
      "  build-depends: base",
      "  default-language: Haskell2010",
      "",
      "executable cli",
      "  main-is: Main.hs",
      "  hs-source-dirs: src",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

nestedFixtureCabal :: String
nestedFixtureCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: fixture",
      "version: 0.1.0.0",
      "executable fixture",
      "  main-is: Fixture.hs",
      "  build-depends: base",
      "  default-language: Haskell2010"
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

assertLeftContaining :: String -> Either String a -> Assertion
assertLeftContaining needle result =
  case result of
    Left err ->
      assertBool
        ("expected parse error to contain " ++ show needle ++ ", got: " ++ err)
        (needle `contains` err)
    Right _ ->
      assertBool "expected parse failure but got success" False

contains :: String -> String -> Bool
contains needle haystack = any (needle `prefixOf`) (tails haystack)

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x : xs) (y : ys) = x == y && prefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : rest) = xs : tails rest
