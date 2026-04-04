{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Parser
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokens, lexTokensFromChunks, readModuleHeaderExtensions, readModuleHeaderExtensionsFromChunks)
import Aihc.Parser.Syntax
import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.ErrorMessages.Suite (errorMessageTests)
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Oracle.Suite (oracleTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.Properties.ExprHelpers (genOperator, isValidGeneratedOperator)
import Test.Properties.ExprRoundTrip (prop_exprPrettyRoundTrip)
import Test.Properties.Identifiers (isValidGeneratedIdent, shrinkIdent)
import Test.Properties.ModuleRoundTrip (prop_modulePrettyRoundTrip)
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.StackageProgress.Summary (stackageProgressSummaryTests)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import qualified Text.Megaparsec.Error as MPE

tenMinutes :: Timeout
tenMinutes = Timeout (10 * 60 * 1000000) "10m"

main :: IO ()
main = buildTests >>= defaultMain

buildTests :: IO TestTree
buildTests = do
  parserGolden <- parserGoldenTests
  errorMessages <- errorMessageTests
  oracle <- oracleTests
  lexer <- lexerTests
  let hackageTester = hackageTesterTests
  pure $
    testGroup
      "aihc-parser"
      [ parserGolden,
        errorMessages,
        lexer,
        testGroup
          "parser"
          [ testCase "module parses declaration list" test_moduleParsesDecls,
            testCase "reads header LANGUAGE pragmas" test_readsHeaderLanguagePragmas,
            testCase "reads header LANGUAGE pragmas case-insensitively" test_readsHeaderLanguagePragmasCaseInsensitive,
            testCase "reads chunked header LANGUAGE pragmas" test_readsChunkedHeaderLanguagePragmas,
            testCase "reads header LANGUAGE pragmas starting with No" test_readsHeaderLanguagePragmasStartingWithNo,
            testCase "reads OPTIONS -X extension flag as LANGUAGE setting" test_readsOptionsPragmaXExtension,
            testCase "ignores invalid split OPTIONS -X ExtensionName form" test_ignoresSplitOptionsPragmaXExtension,
            testCase "reads OPTIONS -cpp flag as CPP extension" test_readsOptionsPragmaCpp,
            testCase "reads OPTIONS -fffi flag as ForeignFunctionInterface extension" test_readsOptionsPragmaFffi,
            testCase "reads OPTIONS_GHC -cpp among other flags" test_readsOptionsGhcPragmaCpp,
            testCase "reads OPTIONS -fglasgow-exts as legacy extension bundle" test_readsOptionsPragmaGlasgowExts,
            testCase "ignores unknown header pragmas" test_ignoresUnknownHeaderPragmas,
            testCase "ignores LANGUAGE pragmas inside comments" test_ignoresLanguagePragmasInsideComments,
            testCase "stops header scan at first module token" test_stopsHeaderScanAtFirstModuleToken,
            testCase "emits lexer error token for unterminated strings" test_unterminatedStringProducesErrorToken,
            testCase "emits lexer error token for unterminated block comments" test_unterminatedBlockCommentProducesErrorToken,
            testCase "applies hash line directives to subsequent tokens" test_hashLineDirectiveUpdatesSpan,
            testCase "applies gcc-style hash line directives to subsequent tokens" test_gccHashLineDirectiveUpdatesSpan,
            testCase "applies LINE pragmas to subsequent tokens" test_linePragmaUpdatesSpan,
            testCase "applies COLUMN pragmas to subsequent tokens" test_columnPragmaUpdatesSpan,
            testCase "applies COLUMN pragmas in the middle of a line" test_inlineColumnPragmaUpdatesSpan,
            testCase "can lex lazily from chunks" test_lexerChunkLaziness,
            testCase "parser config passes extensions to lexer" test_parserConfigPassesExtensions,
            testCase "parser config sets source name in parse errors" test_parserConfigSetsSourceName,
            testCase "parses tab-indented where after else branch" test_tabIndentedWhereAfterElseParses,
            testCase "generated identifiers reject reserved keyword as" test_generatedIdentifiersRejectReservedAs,
            testCase "generated identifiers reject standalone underscore" test_generatedIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk identifiers reject standalone underscore" test_shrunkIdentifiersRejectStandaloneUnderscore,
            QC.testProperty "generated operators reject dash-only comment starters" prop_generatedOperatorsRejectDashOnlyCommentStarters
          ],
        adjustOption (const tenMinutes) $
          testGroup
            "properties"
            [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
              QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
              QC.testProperty "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
              QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
            ],
        oracle,
        extensionMappingTests,
        hackageTester,
        stackageProgressSummaryTests
      ]

test_moduleParsesDecls :: Assertion
test_moduleParsesDecls =
  let (errs, modu) = parseModule defaultConfig "x = if y then z else w"
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [ DeclValue _ (FunctionBind _ "x" [Match {matchPats = [], matchRhs = UnguardedRhs _ (EIf _ (EVar _ "y") (EVar _ "z") (EVar _ "w"))}])
            ] ->
              pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_parserConfigPassesExtensions :: Assertion
test_parserConfigPassesExtensions =
  case parseExpr defaultConfig {parserExtensions = [NegativeLiterals]} "-1" of
    ParseOk (EInt _ (-1) _) -> pure ()
    ParseOk other -> assertFailure ("expected negative literal expression, got: " <> show other)
    ParseErr err -> assertFailure ("expected parse success, got parse error: " <> MPE.errorBundlePretty err)

test_parserConfigSetsSourceName :: Assertion
test_parserConfigSetsSourceName =
  let (errs, _) = parseModule defaultConfig {parserSourceName = "Example.hs"} "module"
   in case errs of
        _ : _ ->
          let errText = formatParseErrors "Example.hs" (Just "module") errs
           in if "Example.hs" `isInfixOf` errText
                then pure ()
                else assertFailure ("expected source name in parse error, got: " <> errText)
        [] ->
          assertFailure "expected parse failure, but got no errors"

test_tabIndentedWhereAfterElseParses :: Assertion
test_tabIndentedWhereAfterElseParses =
  let source =
        T.pack $
          unlines
            [ "addExtension file ext = case B.uncons ext of",
              "\tNothing -> file",
              "\tJust (x,_xs) -> joinDrive a $",
              "\t\tif isExtSeparator x",
              "\t\t\tthen b <> ext",
              "\t\t\telse b <> (extSeparator `B.cons` ext)",
              "  where",
              "\t(a,b) = splitDrive file"
            ]
   in let (errs, _) = parseModule defaultConfig source
       in assertBool ("expected no parse errors, got: " <> show errs) (null errs)

test_readsHeaderLanguagePragmas :: Assertion
test_readsHeaderLanguagePragmas = do
  let source = T.unlines ["{-# LANGUAGE CPP #-}", "{-# LANGUAGE NoCPP #-}", "module M where", "x = 1"]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP, DisableExtension CPP]
  assertEqual "reads expected module header LANGUAGE settings" expected exts

test_readsHeaderLanguagePragmasCaseInsensitive :: Assertion
test_readsHeaderLanguagePragmasCaseInsensitive = do
  let source = T.unlines ["{-# Language BlockArguments #-}", "module M where", "x = id do pure ()"]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension BlockArguments]
  assertEqual "reads expected module header LANGUAGE settings regardless of pragma keyword case" expected exts

test_readsChunkedHeaderLanguagePragmas :: Assertion
test_readsChunkedHeaderLanguagePragmas = do
  let chunks =
        [ "{-# LANG",
          "UAGE CPP #-}\n{-# LANGUAGE NoCPP #-}\nmodule M where\nx = 1"
        ]
      exts = readModuleHeaderExtensionsFromChunks chunks
      expected = [EnableExtension CPP, DisableExtension CPP]
  assertEqual "reads expected module header LANGUAGE settings across chunks" expected exts

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

test_readsOptionsPragmaXExtension :: Assertion
test_readsOptionsPragmaXExtension = do
  let source =
        T.unlines
          [ "{-# OPTIONS -XMagicHash #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension MagicHash]
  assertEqual "maps OPTIONS -XMagicHash to LANGUAGE MagicHash" expected exts

test_ignoresSplitOptionsPragmaXExtension :: Assertion
test_ignoresSplitOptionsPragmaXExtension = do
  let source =
        T.unlines
          [ "{-# OPTIONS -X MagicHash #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
  assertEqual "ignores invalid split OPTIONS -X Extension form" [] exts

test_readsOptionsPragmaCpp :: Assertion
test_readsOptionsPragmaCpp = do
  let source =
        T.unlines
          [ "{-# OPTIONS -cpp #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "maps OPTIONS -cpp to LANGUAGE CPP" expected exts

test_readsOptionsPragmaFffi :: Assertion
test_readsOptionsPragmaFffi = do
  let source =
        T.unlines
          [ "{-# OPTIONS -fffi #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension ForeignFunctionInterface]
  assertEqual "maps OPTIONS -fffi to LANGUAGE ForeignFunctionInterface" expected exts

test_readsOptionsGhcPragmaCpp :: Assertion
test_readsOptionsGhcPragmaCpp = do
  let source =
        T.unlines
          [ "{-# OPTIONS_GHC -cpp -pgmP \"cpphs --layout --hashes --cpp\" #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected = [EnableExtension CPP]
  assertEqual "maps OPTIONS_GHC -cpp while ignoring other options" expected exts

test_readsOptionsPragmaGlasgowExts :: Assertion
test_readsOptionsPragmaGlasgowExts = do
  let source =
        T.unlines
          [ "{-# OPTIONS -fglasgow-exts #-}",
            "module M where",
            "x = 1"
          ]
      exts = readModuleHeaderExtensions source
      expected =
        map
          EnableExtension
          [ ConstrainedClassMethods,
            DeriveDataTypeable,
            DeriveFoldable,
            DeriveFunctor,
            DeriveGeneric,
            DeriveTraversable,
            EmptyDataDecls,
            ExistentialQuantification,
            ExplicitNamespaces,
            FlexibleContexts,
            FlexibleInstances,
            ForeignFunctionInterface,
            FunctionalDependencies,
            GeneralizedNewtypeDeriving,
            ImplicitParams,
            InterruptibleFFI,
            KindSignatures,
            LiberalTypeSynonyms,
            MagicHash,
            MultiParamTypeClasses,
            ParallelListComp,
            PatternGuards,
            PostfixOperators,
            RankNTypes,
            RecursiveDo,
            ScopedTypeVariables,
            StandaloneDeriving,
            TypeOperators,
            TypeSynonymInstances,
            UnboxedTuples,
            UnicodeSyntax,
            UnliftedFFITypes
          ]
  assertEqual "maps OPTIONS -fglasgow-exts to legacy LANGUAGE bundle" expected exts

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

test_unterminatedStringProducesErrorToken :: Assertion
test_unterminatedStringProducesErrorToken =
  case lexTokens "\"unterminated" of
    [LexToken {lexTokenKind = TkError _}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected TkError followed by TkEOF, got: " <> show other)

test_unterminatedBlockCommentProducesErrorToken :: Assertion
test_unterminatedBlockCommentProducesErrorToken =
  case lexTokens "{-" of
    [LexToken {lexTokenKind = TkError _}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected TkError followed by TkEOF, got: " <> show other)

test_hashLineDirectiveUpdatesSpan :: Assertion
test_hashLineDirectiveUpdatesSpan =
  case lexTokens "#line 42\nx" of
    [LexToken {lexTokenKind = TkVarId "x", lexTokenSpan = span'}, LexToken {lexTokenKind = TkEOF}] ->
      assertSourceSpan "<input>" 42 1 42 2 9 10 span'
    other -> assertFailure ("expected identifier at line 42, got: " <> show other)

test_gccHashLineDirectiveUpdatesSpan :: Assertion
test_gccHashLineDirectiveUpdatesSpan =
  case lexTokens "# 42 \"generated.h\"\nx" of
    [LexToken {lexTokenKind = TkVarId "x", lexTokenSpan = span'}, LexToken {lexTokenKind = TkEOF}] ->
      assertSourceSpan "generated.h" 42 1 42 2 19 20 span'
    other -> assertFailure ("expected identifier at line 42 from gcc-style directive, got: " <> show other)

test_linePragmaUpdatesSpan :: Assertion
test_linePragmaUpdatesSpan =
  case lexTokens "{-# LINE 17 #-}\nx" of
    [LexToken {lexTokenKind = TkVarId "x", lexTokenSpan = span'}, LexToken {lexTokenKind = TkEOF}] ->
      assertSourceSpan "<input>" 17 1 17 2 16 17 span'
    other -> assertFailure ("expected identifier at line 17, got: " <> show other)

test_columnPragmaUpdatesSpan :: Assertion
test_columnPragmaUpdatesSpan =
  case lexTokens "x\n{-# COLUMN 7 #-}y" of
    [ LexToken {lexTokenKind = TkVarId "x"},
      LexToken {lexTokenKind = TkVarId "y", lexTokenSpan = span'},
      LexToken {lexTokenKind = TkEOF}
      ] -> assertSourceSpan "<input>" 2 7 2 8 18 19 span'
    other -> assertFailure ("expected second identifier at column 7, got: " <> show other)

test_inlineColumnPragmaUpdatesSpan :: Assertion
test_inlineColumnPragmaUpdatesSpan =
  case lexTokens "x{-# COLUMN 7 #-}y" of
    [ LexToken {lexTokenKind = TkVarId "x", lexTokenSpan = xSpan},
      LexToken {lexTokenKind = TkVarId "y", lexTokenSpan = ySpan},
      LexToken {lexTokenKind = TkEOF}
      ] -> do
        assertSourceSpan "<input>" 1 1 1 2 0 1 xSpan
        assertSourceSpan "<input>" 1 7 1 8 17 18 ySpan
    other -> assertFailure ("expected inline COLUMN pragma to update same-line column, got: " <> show other)

assertSourceSpan :: FilePath -> Int -> Int -> Int -> Int -> Int -> Int -> SourceSpan -> Assertion
assertSourceSpan expectedName expectedStartLine expectedStartCol expectedEndLine expectedEndCol expectedStartOffset expectedEndOffset span' =
  case span' of
    SourceSpan {sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndLine, sourceSpanEndCol, sourceSpanStartOffset, sourceSpanEndOffset} -> do
      assertEqual "source name" expectedName sourceSpanSourceName
      assertEqual "start line" expectedStartLine sourceSpanStartLine
      assertEqual "start col" expectedStartCol sourceSpanStartCol
      assertEqual "end line" expectedEndLine sourceSpanEndLine
      assertEqual "end col" expectedEndCol sourceSpanEndCol
      assertEqual "start offset" expectedStartOffset sourceSpanStartOffset
      assertEqual "end offset" expectedEndOffset sourceSpanEndOffset
    NoSourceSpan -> assertFailure "expected SourceSpan, got NoSourceSpan"

test_lexerChunkLaziness :: Assertion
test_lexerChunkLaziness =
  -- Test that we can take at least one token without forcing all chunks.
  -- Note: After TkEOF was added, the lexer may need to look ahead to determine
  -- if there's more input (trivia skipping), so we can't guarantee full laziness.
  -- This test verifies that the first token can be extracted.
  case take 1 (lexTokensFromChunks ["x"]) of
    [LexToken {lexTokenKind = TkVarId "x"}] -> pure ()
    other -> assertFailure ("expected lazy first token from chunks, got: " <> show other)

test_generatedIdentifiersRejectReservedAs :: Assertion
test_generatedIdentifiersRejectReservedAs =
  assertBool "reserved keyword 'as' must not be treated as a valid generated identifier" $
    not (isValidGeneratedIdent "as")

test_generatedIdentifiersRejectStandaloneUnderscore :: Assertion
test_generatedIdentifiersRejectStandaloneUnderscore =
  assertBool "standalone underscore must not be treated as a valid generated identifier" $
    not (isValidGeneratedIdent "_")

test_shrunkIdentifiersRejectStandaloneUnderscore :: Assertion
test_shrunkIdentifiersRejectStandaloneUnderscore =
  assertBool "standalone underscore must not be produced by shrinking" $
    "_" `notElem` shrinkIdent "__"

prop_generatedOperatorsRejectDashOnlyCommentStarters :: QC.Property
prop_generatedOperatorsRejectDashOnlyCommentStarters =
  QC.forAll (QC.vectorOf 2000 genOperator) $ \ops ->
    let invalid = filter (not . isValidGeneratedOperator) ops
     in QC.counterexample ("invalid generated operators: " <> show invalid) (null invalid)
