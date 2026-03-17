{-# LANGUAGE OverloadedStrings #-}

module Test.HackageTester.Suite
  ( hackageTesterTests,
  )
where

import qualified Data.Text as T
import GhcOracle (oracleDetailedParsesModuleWithNamesAt)
import HackageTester.CLI (Options (..), parseOptionsPure)
import HackageTester.Model (FileResult (..), Outcome (..), Summary (..), shouldFailSummary, summarizeResults)
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
          testCase "applies implied extensions" test_oracleAppliesImpliedExtensions,
          testCase "uses Haskell2010 language defaults" test_oracleUsesHaskell2010Defaults,
          testCase "uses Haskell98 fallback defaults" test_oracleUsesHaskell98FallbackDefaults,
          testCase "handles CPP-defined LANGUAGE pragmas" test_oracleHandlesCppDefinedLanguagePragmas
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
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] Nothing source of
    Left err ->
      assertBool
        ("expected NoMonomorphismRestriction pragma to be accepted, got: " <> T.unpack err)
        False
    Right () -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE NoMonomorphismRestriction #-}",
          "module A where",
          "x = 1"
        ]

test_oracleAcceptsHaskell2010LanguagePragma :: Assertion
test_oracleAcceptsHaskell2010LanguagePragma =
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] Nothing source of
    Left err ->
      assertBool
        ("expected Haskell2010 language pragma to be accepted, got: " <> T.unpack err)
        False
    Right () -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE Haskell2010 #-}",
          "module A where",
          "x = 1"
        ]

test_oracleAppliesImpliedExtensions :: Assertion
test_oracleAppliesImpliedExtensions =
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] Nothing source of
    Left err ->
      assertBool
        ("expected ScopedTypeVariables to imply ExplicitForAll, got: " <> T.unpack err)
        False
    Right () -> pure ()
  where
    source =
      T.unlines
        [ "{-# LANGUAGE ScopedTypeVariables #-}",
          "module A where",
          "f :: forall a. a -> a",
          "f x = x"
        ]

test_oracleUsesHaskell2010Defaults :: Assertion
test_oracleUsesHaskell2010Defaults =
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] (Just "Haskell2010") source of
    Left err ->
      assertBool
        ("expected Haskell2010 defaults to enable traditional record syntax, got: " <> T.unpack err)
        False
    Right () -> pure ()
  where
    source =
      T.unlines
        [ "module A where",
          "data R = R { field :: Int }"
        ]

test_oracleUsesHaskell98FallbackDefaults :: Assertion
test_oracleUsesHaskell98FallbackDefaults =
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] (Just "Haskell98") source of
    Left err ->
      assertBool
        ("expected Haskell98 fallback defaults to allow nondecreasing indentation, got: " <> T.unpack err)
        False
    Right () -> pure ()
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
  case oracleDetailedParsesModuleWithNamesAt "hackage-tester" [] Nothing source of
    Left err ->
      assertBool
        ("expected oracle to honor CPP-defined LANGUAGE pragmas, got: " <> T.unpack err)
        False
    Right () -> pure ()
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
