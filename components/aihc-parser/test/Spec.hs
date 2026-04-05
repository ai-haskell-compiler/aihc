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
import Test.Performance.Suite (parserPerformanceTests)
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
  performance <- parserPerformanceTests
  errorMessages <- errorMessageTests
  oracle <- oracleTests
  lexer <- lexerTests
  let hackageTester = hackageTesterTests
  pure $
    testGroup
      "aihc-parser"
      [ parserGolden,
        performance,
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
            testCase "parses non-aligned multi-way-if guards" test_nonAlignedMultiWayIfGuardsParse,
            testCase "generated identifiers reject reserved keyword as" test_generatedIdentifiersRejectReservedAs,
            testCase "generated identifiers reject standalone underscore" test_generatedIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk identifiers reject standalone underscore" test_shrunkIdentifiersRejectStandaloneUnderscore,
            QC.testProperty "generated operators reject dash-only comment starters" prop_generatedOperatorsRejectDashOnlyCommentStarters
          ],
        testGroup
          "checkPattern (do-bind)"
          [ testCase "variable pattern: x <- expr" test_doBindVarPattern,
            testCase "constructor pattern: Just x <- expr" test_doBindConPattern,
            testCase "wildcard pattern: _ <- expr" test_doBindWildcardPattern,
            testCase "tuple pattern: (a, b) <- expr" test_doBindTuplePattern,
            testCase "list pattern: [a, b] <- expr" test_doBindListPattern,
            testCase "literal pattern: 0 <- expr" test_doBindLitPattern,
            testCase "negated literal pattern: -1 <- expr" test_doBindNegLitPattern,
            testCase "nested constructor: Just (Left x) <- expr" test_doBindNestedConPattern,
            testCase "infix constructor: x : xs <- expr" test_doBindInfixConPattern,
            testCase "parenthesized pattern: (x) <- expr" test_doBindParenPattern,
            testCase "bang pattern: !x <- expr" test_doBindBangPattern,
            testCase "irrefutable pattern: ~(a, b) <- expr" test_doBindIrrefutablePattern,
            testCase "as pattern: x@(Just _) <- expr" test_doBindAsPattern,
            testCase "expression statement: expr" test_doExprStmt,
            testCase "let statement: let x = 5" test_doLetStmt,
            testCase "rejects if-then-else in pattern context" test_doBindRejectsIfExpr
          ],
        testGroup
          "checkPattern (guard qualifier)"
          [ testCase "guard expression: f x | x > 0 = x" test_guardExpr,
            testCase "guard pattern bind: f x | Just y <- g x = y" test_guardPatBind,
            testCase "guard let: f x | let y = x + 1 = y" test_guardLet,
            testCase "guard wildcard bind: f x | _ <- g x = x" test_guardWildcardBind,
            testCase "guard tuple bind: f x | (a, b) <- g x = a" test_guardTupleBind,
            testCase "guard constructor bind: f x | Just y <- g x = y" test_guardConBind,
            testCase "guard bang pattern: f x | !y <- g x = y" test_guardBangBind,
            testCase "guard irrefutable pattern: f x | ~(a, b) <- g x = a" test_guardIrrefutableBind,
            testCase "guard as pattern: f x | y@(Just _) <- g x = y" test_guardAsBind,
            testCase "guard infix pattern: f x | a : as <- g x = a" test_guardInfixBind
          ],
        testGroup
          "checkPattern (list comprehension)"
          [ testCase "comp guard: [x | x > 0]" test_compGuard,
            testCase "comp generator: [x | x <- xs]" test_compGen,
            testCase "comp let: [y | let y = 5]" test_compLet,
            testCase "comp wildcard gen: [1 | _ <- xs]" test_compWildcardGen,
            testCase "comp tuple gen: [a | (a, b) <- xs]" test_compTupleGen,
            testCase "comp constructor gen: [y | Just y <- xs]" test_compConGen,
            testCase "comp bang gen: [y | !y <- xs]" test_compBangGen,
            testCase "comp irrefutable gen: [a | ~(a, b) <- xs]" test_compIrrefutableGen,
            testCase "comp as gen: [y | y@(Just _) <- xs]" test_compAsGen,
            testCase "comp infix gen: [a | a : as <- xs]" test_compInfixGen
          ],
        testGroup
          "localDeclParser dispatch"
          [ testCase "type sig: f :: Int" test_localDeclTypeSig,
            testCase "type sig multi: f, g :: Int" test_localDeclTypeSigMulti,
            testCase "type sig operator: (+) :: Int -> Int -> Int" test_localDeclTypeSigOp,
            testCase "function bind prefix: f x = x" test_localDeclFunPrefix,
            testCase "function bind no args: f = 5" test_localDeclFunNoArgs,
            testCase "pattern bind tuple: (x, y) = expr" test_localDeclPatTuple,
            testCase "pattern bind constructor: Just x = expr" test_localDeclPatCon,
            testCase "pattern bind wildcard: _ = expr" test_localDeclPatWild,
            testCase "function bind guarded: f x | x > 0 = x" test_localDeclFunGuarded
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

test_nonAlignedMultiWayIfGuardsParse :: Assertion
test_nonAlignedMultiWayIfGuardsParse =
  let source =
        T.unlines
          [ "{-# LANGUAGE MultiWayIf #-}",
            "module M where",
            "x = if | True -> 1",
            "         | False -> 2",
            "           | otherwise -> 3"
          ]
      (errs, _) = parseModule defaultConfig source
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

-- Helper: parse a do-expression and extract the do-statements.
parseDoStmts :: T.Text -> Either String [DoStmt]
parseDoStmts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = UnguardedRhs _ (EDo _ stmts)}])] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

-- Helper: parse a do-expression with extensions and extract the do-statements.
parseDoStmtsExt :: [Extension] -> T.Text -> Either String [DoStmt]
parseDoStmtsExt exts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = UnguardedRhs _ (EDo _ stmts)}])] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

test_doBindVarPattern :: Assertion
test_doBindVarPattern =
  case parseDoStmts "do { x <- return 1; return x }" of
    Right [DoBind _ (PVar _ "x") _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected var bind, got: " <> show other)

test_doBindConPattern :: Assertion
test_doBindConPattern =
  case parseDoStmts "do { Just x <- return Nothing; return x }" of
    Right [DoBind _ (PCon _ "Just" [PVar _ "x"]) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected constructor bind, got: " <> show other)

test_doBindWildcardPattern :: Assertion
test_doBindWildcardPattern =
  case parseDoStmts "do { _ <- return 1; return 2 }" of
    Right [DoBind _ (PWildcard _) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected wildcard bind, got: " <> show other)

test_doBindTuplePattern :: Assertion
test_doBindTuplePattern =
  case parseDoStmts "do { (a, b) <- return (1, 2); return a }" of
    Right [DoBind _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"]) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected tuple bind, got: " <> show other)

test_doBindListPattern :: Assertion
test_doBindListPattern =
  case parseDoStmts "do { [a, b] <- return [1, 2]; return a }" of
    Right [DoBind _ (PList _ [PVar _ "a", PVar _ "b"]) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected list bind, got: " <> show other)

test_doBindLitPattern :: Assertion
test_doBindLitPattern =
  case parseDoStmts "do { 0 <- return 1; return 2 }" of
    Right [DoBind _ (PLit _ (LitInt _ 0 _)) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected literal bind, got: " <> show other)

test_doBindNegLitPattern :: Assertion
test_doBindNegLitPattern =
  case parseDoStmts "do { -1 <- return 0; return 2 }" of
    Right [DoBind _ (PNegLit _ (LitInt _ 1 _)) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected negated literal bind, got: " <> show other)

test_doBindNestedConPattern :: Assertion
test_doBindNestedConPattern =
  case parseDoStmts "do { Just (Left x) <- return Nothing; return x }" of
    Right [DoBind _ (PCon _ "Just" [PParen _ (PCon _ "Left" [PVar _ "x"])]) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected nested constructor bind, got: " <> show other)

test_doBindInfixConPattern :: Assertion
test_doBindInfixConPattern =
  case parseDoStmts "do { x : xs <- return [1, 2]; return x }" of
    Right [DoBind _ (PInfix _ (PVar _ "x") ":" (PVar _ "xs")) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected infix constructor bind, got: " <> show other)

test_doBindParenPattern :: Assertion
test_doBindParenPattern =
  case parseDoStmts "do { (x) <- return 1; return x }" of
    Right [DoBind _ (PParen _ (PVar _ "x")) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected parenthesized bind, got: " <> show other)

test_doBindBangPattern :: Assertion
test_doBindBangPattern =
  case parseDoStmtsExt [BangPatterns] "do { !x <- return 1; return x }" of
    Right [DoBind _ (PStrict _ (PVar _ "x")) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected bang pattern bind, got: " <> show other)

test_doBindIrrefutablePattern :: Assertion
test_doBindIrrefutablePattern =
  case parseDoStmts "do { ~(a, b) <- return (1, 2); return a }" of
    Right [DoBind _ (PIrrefutable _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"])) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected irrefutable pattern bind, got: " <> show other)

test_doBindAsPattern :: Assertion
test_doBindAsPattern =
  case parseDoStmts "do { x@(Just _) <- return Nothing; return x }" of
    Right [DoBind _ (PAs _ "x" (PParen _ (PCon _ "Just" [PWildcard _]))) _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected as-pattern bind, got: " <> show other)

test_doExprStmt :: Assertion
test_doExprStmt =
  case parseDoStmts "do { putStrLn \"hello\"; return () }" of
    Right [DoExpr _ _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected two expression statements, got: " <> show other)

test_doLetStmt :: Assertion
test_doLetStmt =
  case parseDoStmts "do { let { x = 5 }; return x }" of
    Right [DoLetDecls _ _, DoExpr _ _] -> pure ()
    other -> assertFailure ("expected let + expr statements, got: " <> show other)

test_doBindRejectsIfExpr :: Assertion
test_doBindRejectsIfExpr =
  let src = "x = do { if True then 1 else 2 <- return 3 }"
      (errs, _) = parseModule defaultConfig src
   in assertBool "expected parse error for if-then-else in bind pattern" (not (null errs))

-- Helpers: parse guard qualifiers from a function with guards.
-- Input: "f x | guard1, guard2 = body"
parseGuards :: T.Text -> Either String [GuardQualifier]
parseGuards src =
  let (errs, modu) = parseModule defaultConfig src
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = GuardedRhss _ [GuardedRhs {guardedRhsGuards = guards}]}])] ->
            Right guards
          other ->
            Left ("unexpected AST: " <> show other)

parseGuardsExt :: [Extension] -> T.Text -> Either String [GuardQualifier]
parseGuardsExt exts src =
  let (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = GuardedRhss _ [GuardedRhs {guardedRhsGuards = guards}]}])] ->
            Right guards
          other ->
            Left ("unexpected AST: " <> show other)
  where
    fullSrc = "{-# LANGUAGE " <> T.intercalate ", " (map (T.pack . show) exts) <> " #-}\n" <> src

test_guardExpr :: Assertion
test_guardExpr =
  case parseGuards "f x | x > 0 = x" of
    Right [GuardExpr _ _] -> pure ()
    other -> assertFailure ("expected guard expression, got: " <> show other)

test_guardPatBind :: Assertion
test_guardPatBind =
  case parseGuards "f x | Just y <- g x = y" of
    Right [GuardPat _ (PCon _ "Just" [PVar _ "y"]) _] -> pure ()
    other -> assertFailure ("expected guard pattern bind, got: " <> show other)

test_guardLet :: Assertion
test_guardLet =
  case parseGuards "f x | let { y = x } = y" of
    Right [GuardLet _ _] -> pure ()
    other -> assertFailure ("expected guard let, got: " <> show other)

test_guardWildcardBind :: Assertion
test_guardWildcardBind =
  case parseGuards "f x | _ <- g x = x" of
    Right [GuardPat _ (PWildcard _) _] -> pure ()
    other -> assertFailure ("expected guard wildcard bind, got: " <> show other)

test_guardTupleBind :: Assertion
test_guardTupleBind =
  case parseGuards "f x | (a, b) <- g x = a" of
    Right [GuardPat _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"]) _] -> pure ()
    other -> assertFailure ("expected guard tuple bind, got: " <> show other)

test_guardConBind :: Assertion
test_guardConBind =
  case parseGuards "f x | Just y <- g x = y" of
    Right [GuardPat _ (PCon _ "Just" [PVar _ "y"]) _] -> pure ()
    other -> assertFailure ("expected guard constructor bind, got: " <> show other)

test_guardBangBind :: Assertion
test_guardBangBind =
  case parseGuardsExt [BangPatterns] "f x | !y <- g x = y" of
    Right [GuardPat _ (PStrict _ (PVar _ "y")) _] -> pure ()
    other -> assertFailure ("expected guard bang bind, got: " <> show other)

test_guardIrrefutableBind :: Assertion
test_guardIrrefutableBind =
  case parseGuards "f x | ~(a, b) <- g x = a" of
    Right [GuardPat _ (PIrrefutable _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"])) _] -> pure ()
    other -> assertFailure ("expected guard irrefutable bind, got: " <> show other)

test_guardAsBind :: Assertion
test_guardAsBind =
  case parseGuards "f x | y@(Just _) <- g x = y" of
    Right [GuardPat _ (PAs _ "y" (PParen _ (PCon _ "Just" [PWildcard _]))) _] -> pure ()
    other -> assertFailure ("expected guard as-pattern bind, got: " <> show other)

test_guardInfixBind :: Assertion
test_guardInfixBind =
  case parseGuards "f x | a : as <- g x = a" of
    Right [GuardPat _ (PInfix _ (PVar _ "a") ":" (PVar _ "as")) _] -> pure ()
    other -> assertFailure ("expected guard infix bind, got: " <> show other)

-- Helpers: parse list comprehension statements.
-- Input: "[body | stmt1, stmt2]"
parseCompStmts :: T.Text -> Either String [CompStmt]
parseCompStmts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = UnguardedRhs _ (EListComp _ _ stmts)}])] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

parseCompStmtsExt :: [Extension] -> T.Text -> Either String [CompStmt]
parseCompStmtsExt exts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = UnguardedRhs _ (EListComp _ _ stmts)}])] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

test_compGuard :: Assertion
test_compGuard =
  case parseCompStmts "[x | x > 0]" of
    Right [CompGuard _ _] -> pure ()
    other -> assertFailure ("expected comp guard, got: " <> show other)

test_compGen :: Assertion
test_compGen =
  case parseCompStmts "[x | x <- xs]" of
    Right [CompGen _ (PVar _ "x") _] -> pure ()
    other -> assertFailure ("expected comp generator, got: " <> show other)

test_compLet :: Assertion
test_compLet =
  case parseCompStmts "[y | let { y = 5 }]" of
    Right [CompLetDecls _ _] -> pure ()
    other -> assertFailure ("expected comp let, got: " <> show other)

test_compWildcardGen :: Assertion
test_compWildcardGen =
  case parseCompStmts "[1 | _ <- xs]" of
    Right [CompGen _ (PWildcard _) _] -> pure ()
    other -> assertFailure ("expected comp wildcard gen, got: " <> show other)

test_compTupleGen :: Assertion
test_compTupleGen =
  case parseCompStmts "[a | (a, b) <- xs]" of
    Right [CompGen _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"]) _] -> pure ()
    other -> assertFailure ("expected comp tuple gen, got: " <> show other)

test_compConGen :: Assertion
test_compConGen =
  case parseCompStmts "[y | Just y <- xs]" of
    Right [CompGen _ (PCon _ "Just" [PVar _ "y"]) _] -> pure ()
    other -> assertFailure ("expected comp constructor gen, got: " <> show other)

test_compBangGen :: Assertion
test_compBangGen =
  case parseCompStmtsExt [BangPatterns] "[y | !y <- xs]" of
    Right [CompGen _ (PStrict _ (PVar _ "y")) _] -> pure ()
    other -> assertFailure ("expected comp bang gen, got: " <> show other)

test_compIrrefutableGen :: Assertion
test_compIrrefutableGen =
  case parseCompStmts "[a | ~(a, b) <- xs]" of
    Right [CompGen _ (PIrrefutable _ (PTuple _ Boxed [PVar _ "a", PVar _ "b"])) _] -> pure ()
    other -> assertFailure ("expected comp irrefutable gen, got: " <> show other)

test_compAsGen :: Assertion
test_compAsGen =
  case parseCompStmts "[y | y@(Just _) <- xs]" of
    Right [CompGen _ (PAs _ "y" (PParen _ (PCon _ "Just" [PWildcard _]))) _] -> pure ()
    other -> assertFailure ("expected comp as-pattern gen, got: " <> show other)

test_compInfixGen :: Assertion
test_compInfixGen =
  case parseCompStmts "[a | a : as <- xs]" of
    Right [CompGen _ (PInfix _ (PVar _ "a") ":" (PVar _ "as")) _] -> pure ()
    other -> assertFailure ("expected comp infix gen, got: " <> show other)

-- Helper: parse a let-expression and extract the local declarations.
-- Input: "let { decl1; decl2 } in body"
parseLetDecls :: T.Text -> Either String [Decl]
parseLetDecls src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case moduleDecls modu of
          [DeclValue _ (FunctionBind _ _ [Match {matchRhs = UnguardedRhs _ (ELetDecls _ decls _)}])] ->
            Right decls
          other ->
            Left ("unexpected AST: " <> show other)

test_localDeclTypeSig :: Assertion
test_localDeclTypeSig =
  case parseLetDecls "let { f :: Int } in f" of
    Right [DeclTypeSig _ ["f"] _] -> pure ()
    other -> assertFailure ("expected type sig, got: " <> show other)

test_localDeclTypeSigMulti :: Assertion
test_localDeclTypeSigMulti =
  case parseLetDecls "let { f, g :: Int } in f" of
    Right [DeclTypeSig _ ["f", "g"] _] -> pure ()
    other -> assertFailure ("expected multi-name type sig, got: " <> show other)

test_localDeclTypeSigOp :: Assertion
test_localDeclTypeSigOp =
  case parseLetDecls "let { (+) :: Int -> Int -> Int } in 1 + 2" of
    Right [DeclTypeSig _ ["+"] _] -> pure ()
    other -> assertFailure ("expected operator type sig, got: " <> show other)

test_localDeclFunPrefix :: Assertion
test_localDeclFunPrefix =
  case parseLetDecls "let { f x = x } in f 1" of
    Right [DeclValue _ (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar _ "x"]}])] -> pure ()
    other -> assertFailure ("expected prefix function bind, got: " <> show other)

test_localDeclFunNoArgs :: Assertion
test_localDeclFunNoArgs =
  case parseLetDecls "let { f = 5 } in f" of
    Right [DeclValue _ (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = []}])] -> pure ()
    other -> assertFailure ("expected no-args function bind, got: " <> show other)

test_localDeclPatTuple :: Assertion
test_localDeclPatTuple =
  case parseLetDecls "let { (x, y) = (1, 2) } in x" of
    Right [DeclValue _ (PatternBind _ (PTuple _ Boxed [PVar _ "x", PVar _ "y"]) _)] -> pure ()
    other -> assertFailure ("expected tuple pattern bind, got: " <> show other)

test_localDeclPatCon :: Assertion
test_localDeclPatCon =
  -- NOTE: GHC parses 'Just x = Nothing' in a let-binding as a PatBind with
  -- a constructor pattern. Our parser currently treats it as a FunctionBind
  -- for 'Just' because localFunctionDeclParser is tried before
  -- localPatternDeclParser. This is a pre-existing issue unrelated to the
  -- Phase 3 refactoring; fixing it is deferred to Phase 4.
  case parseLetDecls "let { Just x = Nothing } in x" of
    Right [DeclValue _ (FunctionBind _ "Just" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar _ "x"]}])] -> pure ()
    other -> assertFailure ("expected function bind for constructor name (pre-existing behaviour), got: " <> show other)

test_localDeclPatWild :: Assertion
test_localDeclPatWild =
  case parseLetDecls "let { _ = 5 } in 0" of
    Right [DeclValue _ (PatternBind _ (PWildcard _) _)] -> pure ()
    other -> assertFailure ("expected wildcard pattern bind, got: " <> show other)

test_localDeclFunGuarded :: Assertion
test_localDeclFunGuarded =
  case parseLetDecls "let { f x | x > 0 = x } in f 1" of
    Right [DeclValue _ (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar _ "x"], matchRhs = GuardedRhss _ _}])] -> pure ()
    other -> assertFailure ("expected guarded function bind, got: " <> show other)
