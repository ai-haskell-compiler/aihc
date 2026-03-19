{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import Parser
import Parser.Ast
import Parser.Types (ParseResult (..))
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.Extensions.Suite (extensionTests)
import Test.H2010.Suite (h2010Tests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.Properties.ExprModuleRoundTrip
  ( prop_exprPrettyRoundTrip,
    prop_modulePrettyRoundTrip,
  )
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  parserGolden <- parserGoldenTests
  h2010 <- h2010Tests
  extensions <- extensionTests
  lexer <- lexerTests
  let hackageTester = hackageTesterTests
  pure $
    testGroup
      "aihc-parser"
      [ parserGolden,
        lexer,
        testGroup
          "parser"
          [ testCase "module parses declaration list" test_moduleParsesDecls,
            testCase "reads header LANGUAGE pragmas" test_readsHeaderLanguagePragmas,
            testCase "reads header LANGUAGE pragmas starting with No" test_readsHeaderLanguagePragmasStartingWithNo,
            testCase "ignores unknown header pragmas" test_ignoresUnknownHeaderPragmas,
            testCase "ignores LANGUAGE pragmas inside comments" test_ignoresLanguagePragmasInsideComments,
            testCase "stops header scan at first module token" test_stopsHeaderScanAtFirstModuleToken
          ],
        testGroup
          "properties"
          [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
            QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
            QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
          ],
        h2010,
        extensions,
        extensionMappingTests,
        hackageTester
      ]

test_moduleParsesDecls :: Assertion
test_moduleParsesDecls =
  case parseModule defaultConfig "x = if y then z else w" of
    ParseErr err ->
      assertFailure ("expected module parse success, got parse error: " <> errorBundlePretty err)
    ParseOk modu ->
      case moduleDecls modu of
        [ DeclValue _ (FunctionBind _ "x" [Match {matchPats = [], matchRhs = UnguardedRhs _ (EIf _ (EVar _ "y") (EVar _ "z") (EVar _ "w"))}])
          ] ->
            pure ()
        other ->
          assertFailure ("unexpected parsed declarations: " <> show other)

test_readsHeaderLanguagePragmas :: Assertion
test_readsHeaderLanguagePragmas = do
  let source = T.unlines ["{-# LANGUAGE CPP #-}", "{-# LANGUAGE NoCPP #-}", "module M where", "x = 1"]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP, DisableExtension CPP]
  assertEqual "reads expected module header LANGUAGE settings" expected exts

test_readsHeaderLanguagePragmasStartingWithNo :: Assertion
test_readsHeaderLanguagePragmasStartingWithNo = do
  let source =
        T.unlines
          [ "{-# LANGUAGE NondecreasingIndentation #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension NondecreasingIndentation]
  assertEqual "reads LANGUAGE pragmas whose extension name starts with 'No'" expected exts

test_ignoresUnknownHeaderPragmas :: Assertion
test_ignoresUnknownHeaderPragmas = do
  let source =
        T.unlines
          [ "{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-name-shadowing #-}",
            "{-# OPTIONS_HADDOCK hide #-}",
            "{-# LANGUAGE CPP #-}"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "ignores unknown header pragmas and reads LANGUAGE" expected exts

test_ignoresLanguagePragmasInsideComments :: Assertion
test_ignoresLanguagePragmasInsideComments = do
  let source =
        T.unlines
          [ "-- line comment {-# LANGUAGE MagicHash #-}",
            "{- block comment {-# LANGUAGE EmptyCase #-} -}",
            "{-# LANGUAGE CPP #-}"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "ignores LANGUAGE pragmas in comments" expected exts

test_stopsHeaderScanAtFirstModuleToken :: Assertion
test_stopsHeaderScanAtFirstModuleToken = do
  let source =
        T.unlines
          [ "module M where",
            "{-# LANGUAGE CPP #-}",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
  assertEqual "stops before body pragmas" [] exts
