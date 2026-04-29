{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (resultOutput)
import Aihc.Parser
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokens, lexTokensFromChunks, lexTokensWithExtensions)
import Aihc.Parser.Parens (addDeclParens, addExprParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.Char (ord)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Numeric (showHex, showOct)
import ParserValidation (formatDiff, validateParser)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.ErrorMessages.Suite (errorMessageTests)
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Oracle.Suite (oracleTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.Performance.Suite (parserPerformanceTests)
import Test.Properties.Arb.Decl (genDeclClass, genDeclDataFamilyInst)
import Test.Properties.Arb.Module (genTypeName)
import Test.Properties.DeclRoundTrip (prop_declPrettyRoundTrip)
import Test.Properties.ExprRoundTrip (prop_exprPrettyRoundTrip)
import Test.Properties.Identifiers
  ( genConSym,
    genVarSym,
    isValidConIdent,
    isValidGeneratedConSym,
    isValidGeneratedIdent,
    isValidGeneratedVarSym,
    shrinkConIdent,
    shrinkIdent,
  )
import Test.Properties.ModuleRoundTrip (prop_modulePrettyRoundTrip, prop_moduleValidator)
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, counterexample)
import Test.QuickCheck.Gen qualified as QGen
import Test.QuickCheck.Random qualified as QRandom
import Test.StackageProgress.FileChecker (stackageProgressFileCheckerTests)
import Test.StackageProgress.FileCheckerTiming (stackageProgressFileCheckerTimingTests)
import Test.StackageProgress.Summary (stackageProgressSummaryTests)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Text.Megaparsec.Error qualified as MPE

tenMinutes :: Timeout
tenMinutes = Timeout (10 * 60 * 1000000) "10m"

sampleGen :: Int -> Gen a -> [a]
sampleGen count gen = QGen.unGen (QC.vectorOf count gen) (QRandom.mkQCGen 20260415) 5

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
          [ testCase "emits lexer error token for unterminated strings" test_unterminatedStringProducesErrorToken,
            testCase "emits lexer error token for unterminated block comments" test_unterminatedBlockCommentProducesErrorToken,
            testCase "applies hash line directives to subsequent tokens" test_hashLineDirectiveUpdatesSpan,
            testCase "applies gcc-style hash line directives to subsequent tokens" test_gccHashLineDirectiveUpdatesSpan,
            testCase "skips leading shebang lines as trivia" test_leadingShebangIsSkipped,
            testCase "skips space-prefixed shebang lines as trivia" test_spacedLeadingShebangIsSkipped,
            testCase "skips mid-stream shebang lines as trivia" test_midStreamShebangIsSkipped,
            testCase "does not misclassify line-start #) as a directive" test_lineStartHashTokenIsNotDirective,
            testCase "lexes overloaded labels as single tokens" test_overloadedLabelLexesAsSingleToken,
            testCase "preserves bundled export wildcard position" test_bundledExportWildcardPosition,
            testCase "parses associated data family operator names" test_associatedDataFamilyOperatorName,
            testCase "parses infix associated data family operator names" test_associatedDataFamilyInfixOperatorName,
            testCase "pretty-prints associated data family operator names" test_prettyAssocDataFamilyOperatorName,
            testCase "pretty-prints infix associated data family operator names" test_prettyAssocDataFamilyInfixOperatorName,
            testCase "lexes quoted overloaded labels" test_quotedOverloadedLabelLexes,
            testCase "lexes string gaps before a closing quote" test_stringGapBeforeClosingQuoteLexes,
            testCase "pretty-prints overloaded labels with delimiter spacing" test_overloadedLabelPrettyPrintsWithDelimiterSpacing,
            testCase "applies LINE pragmas to subsequent tokens" test_linePragmaUpdatesSpan,
            testCase "applies COLUMN pragmas to subsequent tokens" test_columnPragmaUpdatesSpan,
            testCase "applies COLUMN pragmas in the middle of a line" test_inlineColumnPragmaUpdatesSpan,
            testCase "sets lexTokenAtLineStart correctly" test_tokenAtLineStartWithoutDirective,
            testCase "hash line directive sets lexTokenAtLineStart" test_hashLineDirectiveSetsAtLineStart,
            testCase "hash line directive preserves layout" test_hashLineDirectivePreservesLayout,
            testCase "indented hash-line is operator, not directive" test_indentedHashLineIsOperator,
            testCase "can lex lazily from chunks" test_lexerChunkLaziness,
            testCase "lexes alternate valid character literal spellings" test_alternateCharLiteralSpellingsLexLikeGhc,
            testCase "lexes control-backslash character literal" test_controlBackslashCharLiteralLexes,
            testCase "parses character literals after escaped backslash cons patterns" test_escapedBackslashConsPatternCharLiteralParses,
            testCase "generated identifiers reject extension keyword rec" test_generatedIdentifiersRejectExtensionKeywordRec,
            testCase "generated identifiers reject standalone underscore" test_generatedIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk identifiers reject standalone underscore" test_shrunkIdentifiersRejectStandaloneUnderscore,
            testCase "generated identifiers accept unicode variable characters" test_generatedIdentifiersAcceptUnicodeVariableCharacters,
            testCase "generated identifiers accept MagicHash suffixes" test_generatedIdentifiersAcceptMagicHashSuffixes,
            testCase "generated constructor identifiers accept unicode uppercase and number tails" test_generatedConstructorIdentifiersAcceptUnicodeCharacters,
            testCase "data declaration result kinds parenthesize contexts" test_dataDeclResultKindContextRoundTrips,
            testCase "promoted qualified constructors avoid char literal ambiguity" test_promotedQualifiedConstructorAvoidsCharLiteralAmbiguity,
            testCase "boxed tuple infix constructor operands stay bare" test_boxedTupleInfixConOperandStaysBare,
            testCase "unboxed tuple infix constructor operands stay bare" test_unboxedTupleInfixConOperandStaysBare,
            testCase "data CTYPE pragmas round-trip" test_dataDeclCTypePragmaRoundTrips,
            testCase "newtype CTYPE pragmas round-trip" test_newtypeCTypePragmaRoundTrips,
            testCase "generated constructor identifiers accept MagicHash suffixes" test_generatedConstructorIdentifiersAcceptMagicHashSuffixes,
            testCase "shrinking constructor identifiers preserves the first character" test_shrunkConstructorIdentifiersPreserveFirstCharacter,
            testCase "lexes identifiers with repeated MagicHash suffixes" test_magicHashIdentifierLexes,
            testCase "parses repeated MagicHash suffixes in exports" test_magicHashExportParses,
            testCase "generated constructor symbols reject reserved spellings" test_generatedConstructorSymbolsRejectReservedSpellings,
            testCase "generated variable symbols reject reserved spellings" test_generatedVariableSymbolsRejectReservedSpellings,
            testCase "generated operators reject arrow tail spellings" test_generatedOperatorsRejectArrowTailSpellings,
            testCase "generated expressions can include mdo" test_generatedExpressionsCanIncludeMdo,
            testCase "pretty-prints infix RHS open-ended expressions inside sections" test_prettyInfixRhsOpenEndedInsideSection,
            testCase "pretty-prints negated open-ended type signature bodies" test_prettyNegatedOpenEndedTypeSigBody,
            testCase "formats roundtrip diffs minimally" test_roundtripDiffIsMinimal,
            testCase "bird-track unliteration preserves tab-sensitive layout columns" test_birdTrackUnlitPreservesTabColumns,
            localOption (QC.QuickCheckTests 2000) $
              QC.testProperty "generated valid char literal spellings lex like GHC" prop_validGeneratedCharLiteralSpellingsLexLikeGhc,
            QC.testProperty "generated operators reject dash-only comment starters" prop_generatedOperatorsRejectDashOnlyCommentStarters,
            localOption (QC.QuickCheckTests 25) $
              QC.testProperty "generated operators can produce unicode asterism" prop_generatedOperatorsCanProduceUnicodeAsterism,
            QC.testProperty "generated constructor symbols are valid" prop_generatedConstructorSymbolsAreValid,
            QC.testProperty "generated variable symbols are valid" prop_generatedVariableSymbolsAreValid
          ],
        testGroup
          "checkPattern (do-bind)"
          [ testCase "rejects if-then-else in pattern context" test_doBindRejectsIfExpr
          ],
        adjustOption (const tenMinutes) $
          testGroup
            "properties"
            [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
              QC.testProperty "generated decl AST pretty-printer round-trip" prop_declPrettyRoundTrip,
              QC.testProperty "generated data family instances can include inline result kinds" prop_generatedDataFamilyInstancesCanIncludeInlineResultKinds,
              QC.testProperty "generated class declarations can include associated data family operators" prop_generatedClassDeclsCanIncludeAssociatedDataFamilyOperators,
              QC.testProperty "generated class items include explicit associated type family syntax" prop_generatedAssociatedTypeFamiliesCanUseExplicitFamilyKeyword,
              QC.testProperty "generated class declarations cover all class item constructors" prop_generatedClassDeclsCoverAllClassItemConstructors,
              QC.testProperty "generated modules can include empty bundled imports" prop_generatedModulesCanIncludeEmptyBundledImports,
              QC.testProperty "generated type names can appear in empty bundled import syntax" prop_generatedTypeNamesSupportEmptyBundledImports,
              QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
              QC.testProperty "generated module AST validator" prop_moduleValidator,
              QC.testProperty "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
              QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
            ],
        oracle,
        extensionMappingTests,
        hackageTester,
        stackageProgressFileCheckerTests,
        stackageProgressFileCheckerTimingTests,
        stackageProgressSummaryTests
      ]

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

test_leadingShebangIsSkipped :: Assertion
test_leadingShebangIsSkipped =
  case lexTokens "#!/usr/bin/env runghc\nmain\n" of
    [LexToken {lexTokenKind = TkVarId "main", lexTokenSpan = span'}, LexToken {lexTokenKind = TkEOF}] ->
      assertSourceSpan "<input>" 2 1 2 5 22 26 span'
    other -> assertFailure ("expected leading shebang to be skipped, got: " <> show other)

test_spacedLeadingShebangIsSkipped :: Assertion
test_spacedLeadingShebangIsSkipped =
  case lexTokens " #!/usr/bin/env runghc\nmain\n" of
    [LexToken {lexTokenKind = TkVarId "main", lexTokenSpan = span'}, LexToken {lexTokenKind = TkEOF}] ->
      assertSourceSpan "<input>" 2 1 2 5 23 27 span'
    other -> assertFailure ("expected spaced leading shebang to be skipped, got: " <> show other)

test_midStreamShebangIsSkipped :: Assertion
test_midStreamShebangIsSkipped =
  case lexTokens "x\n#!/usr/bin/env runghc\ny" of
    [ LexToken {lexTokenKind = TkVarId "x", lexTokenSpan = xSpan},
      LexToken {lexTokenKind = TkVarId "y", lexTokenSpan = ySpan},
      LexToken {lexTokenKind = TkEOF}
      ] -> do
        assertSourceSpan "<input>" 1 1 1 2 0 1 xSpan
        assertSourceSpan "<input>" 3 1 3 2 24 25 ySpan
    other -> assertFailure ("expected mid-stream shebang to be skipped, got: " <> show other)

test_lineStartHashTokenIsNotDirective :: Assertion
test_lineStartHashTokenIsNotDirective =
  case lexTokensWithExtensions [UnboxedTuples] "(#\n  x\n  #)" of
    [ LexToken {lexTokenKind = TkSpecialUnboxedLParen},
      LexToken {lexTokenKind = TkVarId "x"},
      LexToken {lexTokenKind = TkSpecialUnboxedRParen},
      LexToken {lexTokenKind = TkEOF}
      ] -> pure ()
    other -> assertFailure ("expected line-start #) to lex as an unboxed tuple token, got: " <> show other)

test_overloadedLabelLexesAsSingleToken :: Assertion
test_overloadedLabelLexesAsSingleToken =
  case lexTokensWithExtensions [OverloadedLabels] "#typeUrl" of
    [LexToken {lexTokenKind = TkOverloadedLabel "typeUrl" "#typeUrl"}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected overloaded label token, got: " <> show other)

test_quotedOverloadedLabelLexes :: Assertion
test_quotedOverloadedLabelLexes =
  case lexTokensWithExtensions [OverloadedLabels] "#\"The quick brown fox\"" of
    [LexToken {lexTokenKind = TkOverloadedLabel "The quick brown fox" "#\"The quick brown fox\""}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected quoted overloaded label token, got: " <> show other)

test_stringGapBeforeClosingQuoteLexes :: Assertion
test_stringGapBeforeClosingQuoteLexes = do
  case lexTokens (T.pack "\"\\\n\\\"") of
    [LexToken {lexTokenKind = TkString ""}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected empty string token after string gap, got: " <> show other)
  case lexTokens (T.pack "\"\\\n\\c\"") of
    [LexToken {lexTokenKind = TkString "c"}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected string token with literal c after string gap, got: " <> show other)

test_overloadedLabelPrettyPrintsWithDelimiterSpacing :: Assertion
test_overloadedLabelPrettyPrintsWithDelimiterSpacing = do
  let config = defaultConfig {parserExtensions = [OverloadedLabels, UnboxedTuples]}
      exprs =
        [ ETuple Boxed [Just (EOverloadedLabel "a" "#a"), Nothing],
          EList [EOverloadedLabel "a" "#a"],
          EParen (EOverloadedLabel "a" "#a")
        ]
      rendered = map (renderStrict . layoutPretty defaultLayoutOptions . pretty) exprs
      expected = ["( #a, )", "[ #a]", "( #a)"]
  assertEqual "pretty-printed forms" expected rendered
  mapM_
    ( \source ->
        case parseExpr config source of
          ParseErr err -> assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> MPE.errorBundlePretty err)
          ParseOk _ -> pure ()
    )
    rendered

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

test_tokenAtLineStartWithoutDirective :: Assertion
test_tokenAtLineStartWithoutDirective =
  case lexTokens "x y" of
    [LexToken {lexTokenKind = TkVarId "x", lexTokenAtLineStart = True}, LexToken {lexTokenKind = TkVarId "y", lexTokenAtLineStart = False}, LexToken {lexTokenKind = TkEOF}] ->
      pure ()
    other -> assertFailure ("expected x at line start and y not at line start, got: " <> show other)

test_hashLineDirectiveSetsAtLineStart :: Assertion
test_hashLineDirectiveSetsAtLineStart =
  case lexTokens "x\n#line 1 \"foo.hs\"\ny" of
    [LexToken {lexTokenKind = TkVarId "x", lexTokenAtLineStart = True}, LexToken {lexTokenKind = TkVarId "y", lexTokenAtLineStart = True}, LexToken {lexTokenKind = TkEOF}] ->
      pure ()
    other -> assertFailure ("expected x and y both at line start, got: " <> show other)

test_hashLineDirectivePreservesLayout :: Assertion
test_hashLineDirectivePreservesLayout =
  let source = T.unlines ["module Test where", "x = 1", "#line 1 \"foo.hs\"", "y = 2"]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        assertEqual "expected two declarations" 2 (length (moduleDecls modu))

test_indentedHashLineIsOperator :: Assertion
test_indentedHashLineIsOperator =
  -- '# line' on an indented continuation line must lex as operator '#'
  -- plus identifier 'line', not as a CPP #line directive.
  case lexTokens "x\n  # line" of
    [ LexToken {lexTokenKind = TkVarId "x"},
      LexToken {lexTokenKind = TkVarSym "#"},
      LexToken {lexTokenKind = TkVarId "line"},
      LexToken {lexTokenKind = TkEOF}
      ] -> pure ()
    other -> assertFailure ("expected indented '# line' to lex as operator + identifier, got: " <> show other)

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

test_generatedIdentifiersRejectExtensionKeywordRec :: Assertion
test_generatedIdentifiersRejectExtensionKeywordRec =
  assertBool "extension keyword 'rec' must not be treated as a valid generated identifier" $
    not (isValidGeneratedIdent "rec")

test_generatedIdentifiersRejectStandaloneUnderscore :: Assertion
test_generatedIdentifiersRejectStandaloneUnderscore =
  assertBool "standalone underscore must not be treated as a valid generated identifier" $
    not (isValidGeneratedIdent "_")

test_shrunkIdentifiersRejectStandaloneUnderscore :: Assertion
test_shrunkIdentifiersRejectStandaloneUnderscore =
  assertBool "standalone underscore must not be produced by shrinking" $
    "_" `notElem` shrinkIdent "__"

test_generatedIdentifiersAcceptUnicodeVariableCharacters :: Assertion
test_generatedIdentifiersAcceptUnicodeVariableCharacters = do
  assertBool "unicode lowercase letters and unicode numbers should be accepted in generated identifiers" $
    isValidGeneratedIdent "a\x03b1\x00b2"
  assertBool "unicode lowercase letters should be accepted at the start of generated identifiers" $
    isValidGeneratedIdent "\x03bbx"

test_generatedIdentifiersAcceptMagicHashSuffixes :: Assertion
test_generatedIdentifiersAcceptMagicHashSuffixes = do
  assertBool "MagicHash should allow a single trailing hash on variable identifiers" $
    isValidGeneratedIdent "x#"
  assertBool "MagicHash should allow repeated trailing hashes on variable identifiers" $
    isValidGeneratedIdent "x####"

test_generatedConstructorIdentifiersAcceptUnicodeCharacters :: Assertion
test_generatedConstructorIdentifiersAcceptUnicodeCharacters = do
  assertBool "unicode titlecase letters should be accepted at the start of constructor identifiers" $
    isValidConIdent "\x01c5tail"
  assertBool "unicode uppercase letters and unicode numbers should be accepted in constructor identifiers" $
    isValidConIdent "\x0394\x0660"

test_dataDeclResultKindContextRoundTrips :: Assertion
test_dataDeclResultKindContextRoundTrips = do
  let decl =
        DeclData
          DataDecl
            { dataDeclCTypePragma = Nothing,
              dataDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "\66952") [],
              dataDeclContext = [],
              dataDeclKind =
                Just
                  ( TContext
                      [TCon (qualifyName Nothing (mkUnqualifiedName NameConId "C")) Unpromoted]
                      (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "C")) Unpromoted)
                  ),
              dataDeclConstructors = [],
              dataDeclDeriving = []
            }
      expected = stripAnnotations (addDeclParens decl)
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty (addDeclParens decl)))
  assertEqual "pretty-printed declaration" "data 𐖈 :: C => C" rendered
  case parseDecl defaultConfig rendered of
    ParseOk parsed -> assertEqual "round-tripped declaration" expected (stripAnnotations parsed)
    ParseErr err -> assertFailure ("expected parse success for " <> T.unpack rendered <> "\n" <> MPE.errorBundlePretty err)

test_promotedQualifiedConstructorAvoidsCharLiteralAmbiguity :: Assertion
test_promotedQualifiedConstructorAvoidsCharLiteralAmbiguity = do
  let promotedName = mkName (Just "A'.B") NameConId "C"
      decl = DeclStandaloneKindSig (mkUnqualifiedName NameConSym ":+") (TCon promotedName Promoted)
      expected = stripAnnotations (addDeclParens decl)
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty (addDeclParens decl)))
  assertEqual "pretty-printed declaration" "type (:+) :: ' A'.B.C" rendered
  case parseDecl defaultConfig rendered of
    ParseOk parsed -> assertEqual "round-tripped declaration" expected (stripAnnotations parsed)
    ParseErr err -> assertFailure ("expected parse success for " <> T.unpack rendered <> "\n" <> MPE.errorBundlePretty err)

test_boxedTupleInfixConOperandStaysBare :: Assertion
test_boxedTupleInfixConOperandStaysBare = do
  let decl =
        DeclData
          DataDecl
            { dataDeclCTypePragma = Nothing,
              dataDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "D") [],
              dataDeclContext = [],
              dataDeclKind = Nothing,
              dataDeclConstructors =
                [ InfixCon
                    []
                    []
                    (BangType [] [] False False (TTuple Boxed Unpromoted []))
                    (mkUnqualifiedName NameConSym ":.")
                    (BangType [] [] False False (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted))
                ],
              dataDeclDeriving = []
            }
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty (addDeclParens decl)))
  assertEqual "pretty-printed declaration" "data D = () :. T" rendered

test_unboxedTupleInfixConOperandStaysBare :: Assertion
test_unboxedTupleInfixConOperandStaysBare = do
  let decl =
        DeclData
          DataDecl
            { dataDeclCTypePragma = Nothing,
              dataDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "D") [],
              dataDeclContext = [],
              dataDeclKind = Nothing,
              dataDeclConstructors =
                [ InfixCon
                    []
                    []
                    (BangType [] [] False False (TTuple Unboxed Unpromoted [TVar (mkUnqualifiedName NameVarId "a")]))
                    (mkUnqualifiedName NameConSym ":.")
                    (BangType [] [] False False (TCon (qualifyName Nothing (mkUnqualifiedName NameConId "Int")) Unpromoted))
                ],
              dataDeclDeriving = []
            }
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty (addDeclParens decl)))
  assertEqual "pretty-printed declaration" "data D = (# a #) :. Int" rendered

test_dataDeclCTypePragmaRoundTrips :: Assertion
test_dataDeclCTypePragmaRoundTrips = do
  let source = T.unlines ["{-# LANGUAGE GHC2021 #-}", "{-# LANGUAGE CApiFFI #-}", "module M where", "data {-# CTYPE \"termbox.h\" \"struct tb_cell\" #-} Tb_cell = Tb_cell"]
  case parseModule defaultConfig source of
    ([], modu) ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
       in assertBool "expected rendered module to preserve data CTYPE pragma" ("CTYPE \"termbox.h\" \"struct tb_cell\"" `T.isInfixOf` rendered)
    (errs, _) -> assertFailure ("expected parse success, got: " <> show errs)

test_newtypeCTypePragmaRoundTrips :: Assertion
test_newtypeCTypePragmaRoundTrips = do
  let source = T.unlines ["{-# LANGUAGE GHC2021 #-}", "{-# LANGUAGE CApiFFI #-}", "module M where", "import Foreign.C.Types (CInt (..))", "newtype {-# CTYPE \"signed int\" #-} Fixed = Fixed CInt"]
  case parseModule defaultConfig source of
    ([], modu) ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
       in assertBool "expected rendered module to preserve newtype CTYPE pragma" ("CTYPE \"signed int\"" `T.isInfixOf` rendered)
    (errs, _) -> assertFailure ("expected parse success, got: " <> show errs)

test_generatedConstructorIdentifiersAcceptMagicHashSuffixes :: Assertion
test_generatedConstructorIdentifiersAcceptMagicHashSuffixes = do
  assertBool "MagicHash should allow a single trailing hash on constructor identifiers" $
    isValidConIdent "T#"
  assertBool "MagicHash should allow repeated trailing hashes on constructor identifiers" $
    isValidConIdent "T####"

test_shrunkConstructorIdentifiersPreserveFirstCharacter :: Assertion
test_shrunkConstructorIdentifiersPreserveFirstCharacter =
  assertBool "constructor identifier shrinking must preserve the first character" $
    all ((== Just '\x0394') . fmap fst . T.uncons) (shrinkConIdent "\x0394elta9")

test_magicHashIdentifierLexes :: Assertion
test_magicHashIdentifierLexes = do
  let varTokens = lexTokensWithExtensions [MagicHash] "x####"
      conTokens = lexTokensWithExtensions [MagicHash] "T####"
  case varTokens of
    [LexToken {lexTokenKind = TkVarId "x####"}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected MagicHash var identifier token, got: " <> show other)
  case conTokens of
    [LexToken {lexTokenKind = TkConId "T####"}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected MagicHash constructor identifier token, got: " <> show other)

test_magicHashExportParses :: Assertion
test_magicHashExportParses =
  let source = T.unlines ["{-# LANGUAGE MagicHash #-}", "module M (f##) where", "", "f## = undefined"]
      (errs, modu) = parseModule defaultConfig source
   in case errs of
        [] ->
          case moduleHead modu of
            Just ModuleHead {moduleHeadExports = Just [ExportAnn _ (ExportVar _ _ name)]} | name == qualifyName Nothing (mkUnqualifiedName NameVarId "f##") -> pure ()
            other -> assertFailure ("expected export of f##, got: " <> show other)
        _ -> assertFailure ("expected parse success for MagicHash export, got: " <> formatParseErrors "<quickcheck>" (Just source) errs)

test_generatedConstructorSymbolsRejectReservedSpellings :: Assertion
test_generatedConstructorSymbolsRejectReservedSpellings =
  assertBool "reserved constructor symbol spellings must be rejected" $
    not (any isValidGeneratedConSym [":", "::"])

test_generatedVariableSymbolsRejectReservedSpellings :: Assertion
test_generatedVariableSymbolsRejectReservedSpellings =
  assertBool "reserved variable symbol spellings and dash runs must be rejected" $
    not (any isValidGeneratedVarSym ["..", "=", "\\", "|", "|+", "<-", "->", "@", "~", "=>", "--", "---"])

test_generatedOperatorsRejectArrowTailSpellings :: Assertion
test_generatedOperatorsRejectArrowTailSpellings =
  assertBool "arrow-tail operators must not be treated as valid generated operators" $
    not (any isValidGeneratedVarSym ["-<", ">-", "-<<", ">>-"])

test_generatedExpressionsCanIncludeMdo :: Assertion
test_generatedExpressionsCanIncludeMdo =
  let samples = QGen.unGen (QC.vectorOf 4000 (QC.resize 5 (QC.arbitrary :: QC.Gen Expr))) (QRandom.mkQCGen 737) 5
   in assertBool "expected expression generator to include at least one mdo expression" $
        any isMdo samples
  where
    isMdo (EDo _ DoMdo) = True
    isMdo _ = False

test_alternateCharLiteralSpellingsLexLikeGhc :: Assertion
test_alternateCharLiteralSpellingsLexLikeGhc =
  mapM_ assertCharLiteralLexesLikeGhc finiteAlternateCharLiteralSpellings

test_controlBackslashCharLiteralLexes :: Assertion
test_controlBackslashCharLiteralLexes =
  assertCharLiteralLexesLikeGhc "'\\^\\'"

test_escapedBackslashConsPatternCharLiteralParses :: Assertion
test_escapedBackslashConsPatternCharLiteralParses =
  let source =
        T.unlines
          [ "module X where",
            "",
            "go xs = case xs of",
            "  '^' : '\\\\' : xs -> '\\^\\' : go xs",
            "  ys -> ys"
          ]
      (errs, _) = parseModule defaultConfig source
   in assertBool ("expected no parse errors, got: " <> show errs) (null errs)

prop_validGeneratedCharLiteralSpellingsLexLikeGhc :: QC.Property
prop_validGeneratedCharLiteralSpellingsLexLikeGhc =
  QC.forAll genValidCharLiteral $ \raw ->
    QC.counterexample ("literal: " <> T.unpack raw) $
      case ghcReadCharLiteral raw of
        Nothing -> QC.counterexample "generator produced an invalid literal" False
        Just expected ->
          case lexTokens raw of
            [LexToken {lexTokenKind = TkChar actual}, LexToken {lexTokenKind = TkEOF}] -> actual QC.=== expected
            other -> QC.counterexample ("unexpected tokens: " <> show other) False

prop_generatedOperatorsRejectDashOnlyCommentStarters :: QC.Property
prop_generatedOperatorsRejectDashOnlyCommentStarters =
  QC.forAll genVarSym $ \op ->
    QC.counterexample ("invalid generated operator: " <> show op) (isValidGeneratedVarSym op)

prop_generatedOperatorsCanProduceUnicodeAsterism :: QC.Property
prop_generatedOperatorsCanProduceUnicodeAsterism =
  QC.counterexample "expected ⁂ to be a valid generated operator" $
    isValidGeneratedVarSym "⁂"

prop_generatedConstructorSymbolsAreValid :: QC.Property
prop_generatedConstructorSymbolsAreValid =
  QC.forAll genConSym $ \op ->
    QC.counterexample ("invalid generated constructor symbol: " <> show op) (isValidGeneratedConSym op)

prop_generatedVariableSymbolsAreValid :: QC.Property
prop_generatedVariableSymbolsAreValid =
  QC.forAll genVarSym $ \op ->
    QC.counterexample ("invalid generated variable symbol: " <> show op) (isValidGeneratedVarSym op)

assertCharLiteralLexesLikeGhc :: T.Text -> Assertion
assertCharLiteralLexesLikeGhc raw =
  case ghcReadCharLiteral raw of
    Nothing -> assertFailure ("expected GHC to accept valid char literal: " <> show raw)
    Just expected ->
      case lexTokens raw of
        [LexToken {lexTokenKind = TkChar actual}, LexToken {lexTokenKind = TkEOF}] ->
          assertEqual ("character mismatch for literal " <> T.unpack raw) expected actual
        other ->
          assertFailure ("expected char token for literal " <> T.unpack raw <> ", got: " <> show other)

ghcReadCharLiteral :: T.Text -> Maybe Char
ghcReadCharLiteral raw =
  case reads (T.unpack raw) of
    [(c, "")] -> Just c
    _ -> Nothing

genValidCharLiteral :: QC.Gen T.Text
genValidCharLiteral =
  QC.oneof
    [ T.pack . show <$> (QC.arbitrary :: QC.Gen Char),
      QC.elements finiteAlternateCharLiteralSpellings,
      genNumericCharLiteral,
      genHexCharLiteral,
      genOctalCharLiteral
    ]

genNumericCharLiteral :: QC.Gen T.Text
genNumericCharLiteral = do
  c <- QC.arbitrary :: QC.Gen Char
  leadingZeros <- QC.chooseInt (0, 4)
  pure (mkCharLiteral ("\\" <> T.replicate leadingZeros "0" <> T.pack (show (ord c))))

genHexCharLiteral :: QC.Gen T.Text
genHexCharLiteral = do
  c <- QC.arbitrary :: QC.Gen Char
  leadingZeros <- QC.chooseInt (0, 4)
  uppercase <- QC.arbitrary
  let digits = showHex (ord c) ""
      rendered = if uppercase then map toUpperAscii digits else digits
  pure (mkCharLiteral ("\\x" <> T.replicate leadingZeros "0" <> T.pack rendered))

genOctalCharLiteral :: QC.Gen T.Text
genOctalCharLiteral = do
  c <- QC.arbitrary :: QC.Gen Char
  leadingZeros <- QC.chooseInt (0, 4)
  pure (mkCharLiteral ("\\o" <> T.replicate leadingZeros "0" <> T.pack (showOct (ord c) "")))

mkCharLiteral :: T.Text -> T.Text
mkCharLiteral body = "'" <> body <> "'"

finiteAlternateCharLiteralSpellings :: [T.Text]
finiteAlternateCharLiteralSpellings = map mkCharLiteral (simpleEscapeBodies <> controlEscapeBodies <> namedEscapeBodies)

simpleEscapeBodies :: [T.Text]
simpleEscapeBodies = ["\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v", "\\\\", "\\\"", "\\'"]

controlEscapeBodies :: [T.Text]
controlEscapeBodies = [T.pack ['\\', '^', c] | c <- ['@' .. '_']]

namedEscapeBodies :: [T.Text]
namedEscapeBodies =
  map
    ("\\" <>)
    [ "NUL",
      "SOH",
      "STX",
      "ETX",
      "EOT",
      "ENQ",
      "ACK",
      "BEL",
      "BS",
      "HT",
      "LF",
      "VT",
      "FF",
      "CR",
      "SO",
      "SI",
      "DLE",
      "DC1",
      "DC2",
      "DC3",
      "DC4",
      "NAK",
      "SYN",
      "ETB",
      "CAN",
      "EM",
      "SUB",
      "ESC",
      "FS",
      "GS",
      "RS",
      "US",
      "SP",
      "DEL"
    ]

toUpperAscii :: Char -> Char
toUpperAscii c
  | 'a' <= c && c <= 'f' = toEnum (fromEnum c - 32)
  | otherwise = c

test_doBindRejectsIfExpr :: Assertion
test_doBindRejectsIfExpr =
  let src = "x = do { if True then 1 else 2 <- return 3 }"
      (errs, _) = parseModule defaultConfig src
   in assertBool "expected parse error for if-then-else in bind pattern" (not (null errs))

test_bundledExportWildcardPosition :: Assertion
test_bundledExportWildcardPosition = do
  let source =
        T.unlines
          [ "{-# LANGUAGE PatternSynonyms #-}",
            "{-# LANGUAGE ExplicitNamespaces #-}",
            "module M (T (.., P, data Q)) where",
            "data T = A | B",
            "pattern P :: T",
            "pattern P = A",
            "pattern Q :: T",
            "pattern Q = B"
          ]
      (errs, modu) = parseModule defaultConfig source
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
      (reparseErrs, reparsed) = parseModule defaultConfig rendered
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        assertBool ("expected reparsed pretty output to succeed, got: " <> show reparseErrs) (null reparseErrs)
        case moduleExports modu of
          Just [ExportAnn _ (ExportWithAll _ Nothing "T" 0 [IEBundledMember Nothing "P", IEBundledMember (Just IEBundledNamespaceData) "Q"])] ->
            case moduleExports reparsed of
              Just [ExportAnn _ (ExportWithAll _ Nothing "T" 0 [IEBundledMember Nothing "P", IEBundledMember (Just IEBundledNamespaceData) "Q"])] ->
                pure ()
              other ->
                assertFailure ("unexpected reparsed export AST: " <> show other)
          other ->
            assertFailure ("unexpected export AST: " <> show other)

test_prettyInfixRhsOpenEndedInsideSection :: Assertion
test_prettyInfixRhsOpenEndedInsideSection = do
  let config = defaultConfig {parserExtensions = [LambdaCase]}
      op = qualifyName Nothing (mkUnqualifiedName NameVarId "a")
      expr =
        ESectionL
          ( EInfix
              (ELambdaCase [])
              op
              (ELambdaPats [PLit (LitInt 0 TInteger "0")] (EChar ' ' "' '"))
          )
          op
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
  case parseExpr config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed expression" (stripAnnotations (addExprParens expr)) (stripAnnotations reparsed)
    ParseErr bundle ->
      assertFailure ("expected pretty-printed expression to reparse, got:\n" <> show bundle)

test_prettyNegatedOpenEndedTypeSigBody :: Assertion
test_prettyNegatedOpenEndedTypeSigBody = do
  let config = defaultConfig {parserExtensions = [BlockArguments, MagicHash, UnboxedTuples]}
      plus = qualifyName Nothing (mkUnqualifiedName NameVarSym "+")
      tyCon = qualifyName Nothing (mkUnqualifiedName NameConId "C")
      expr =
        ETypeSig
          ( ENegate
              ( EApp
                  (ETuple Unboxed [])
                  (EIf (EStringHash "" "\"\"#") (EList []) (EVar plus))
              )
          )
          (TCon tyCon Unpromoted)
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
  case parseExpr config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed expression" (stripAnnotations (addExprParens expr)) (stripAnnotations reparsed)
    ParseErr bundle ->
      assertFailure ("expected pretty-printed expression to reparse, got:\n" <> show bundle)

test_roundtripDiffIsMinimal :: Assertion
test_roundtripDiffIsMinimal =
  let before =
        T.unlines
          [ "data CtOrigin",
            "  = forall (p :: Pass). (OutputableBndrId p) =>",
            "    ExpectedFunTySyntaxOp !CtOrigin !(HsExpr (GhcPass p)) |",
            "    ExpectedFunTyViewPat !(HsExpr GhcRn) |",
            "    forall (p :: Pass). Outputable (HsExpr (GhcPass p)) =>",
            "    Thing"
          ]
      rendered =
        T.unlines
          [ "data CtOrigin",
            "  = forall p. (OutputableBndrId p) =>",
            "    ExpectedFunTySyntaxOp !CtOrigin !(HsExpr (GhcPass p)) |",
            "    ExpectedFunTyViewPat !(HsExpr GhcRn) |",
            "    forall p. Outputable (HsExpr (GhcPass p)) =>",
            "    Thing"
          ]
      expected =
        Just
          ( T.unlines
              [ "@@ line 2 @@",
                "-   = forall (p :: Pass). (OutputableBndrId p) =>",
                "+   = forall p. (OutputableBndrId p) =>",
                "-     forall (p :: Pass). Outputable (HsExpr (GhcPass p)) =>",
                "+     forall p. Outputable (HsExpr (GhcPass p)) =>"
              ]
          )
   in assertEqual "minimal diff" expected (formatDiff before rendered)

-- | Regression test: bird-track unliteration must preserve column positions so
-- that tab-aligned case alternatives remain in the same layout context.
-- Before the fix, stripping "> " shifted columns by 2, causing tab-expanded
-- and space-only lines to land at different indentation columns.
test_birdTrackUnlitPreservesTabColumns :: Assertion
test_birdTrackUnlitPreservesTabColumns = do
  -- Literate Haskell with tab-indented case alternatives.
  -- The tab on the "Just" line and the spaces on the "Nothing" line
  -- are carefully chosen so they align at the same column in the original
  -- (with "> " prefix) but diverge after naively stripping "> ".
  let lhsSource =
        T.unlines
          [ "> module LitTest where",
            "> f x = case x of",
            ">          \t        Just y  -> y",
            ">                       Nothing -> 0"
          ]
      preprocessed = resultOutput (preprocessForParserWithoutIncludesIfEnabled [] [] "LitTest.lhs" [] lhsSource)
      (errs, _modu) = parseModule defaultConfig preprocessed
  assertBool
    ("expected no parse errors for bird-track .lhs with tabs, got:\n" <> formatParseErrors "LitTest.lhs" (Just preprocessed) errs)
    (null errs)

test_associatedDataFamilyOperatorName :: Assertion
test_associatedDataFamilyOperatorName = do
  let source = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "{-# LANGUAGE TypeOperators #-}", "class C a where", "  data (:*:) a"]
      expectedName = mkUnqualifiedName NameConSym ":*:"
  case parseModule defaultConfig source of
    ([], modu) ->
      case map peelDeclAnn (moduleDecls modu) of
        [ DeclClass ClassDecl {classDeclItems = [ClassItemAnn _ (ClassItemDataFamilyDecl dataFamilyDecl)]}
          ]
            | binderHeadName (dataFamilyDeclHead dataFamilyDecl) == expectedName,
              map tyVarBinderName (binderHeadParams (dataFamilyDeclHead dataFamilyDecl)) == ["a"],
              isNothing (dataFamilyDeclKind dataFamilyDecl) ->
                pure ()
        other ->
          assertFailure ("expected associated data family operator declaration, got: " <> show other)
    (errs, _) ->
      assertFailure ("expected associated data family operator declaration to parse, got: " <> show errs)

test_associatedDataFamilyInfixOperatorName :: Assertion
test_associatedDataFamilyInfixOperatorName = do
  let source = T.unlines ["{-# LANGUAGE TypeFamilies #-}", "{-# LANGUAGE TypeOperators #-}", "class C a where", "  data a :*: b"]
      expectedName = mkUnqualifiedName NameConSym ":*:"
  case parseModule defaultConfig source of
    ([], modu) ->
      case map peelDeclAnn (moduleDecls modu) of
        [ DeclClass ClassDecl {classDeclItems = [ClassItemAnn _ (ClassItemDataFamilyDecl dataFamilyDecl)]}
          ]
            | binderHeadForm (dataFamilyDeclHead dataFamilyDecl) == TypeHeadInfix,
              binderHeadName (dataFamilyDeclHead dataFamilyDecl) == expectedName,
              map tyVarBinderName (binderHeadParams (dataFamilyDeclHead dataFamilyDecl)) == ["a", "b"],
              isNothing (dataFamilyDeclKind dataFamilyDecl) ->
                pure ()
        other ->
          assertFailure ("expected infix associated data family operator declaration, got: " <> show other)
    (errs, _) ->
      assertFailure ("expected infix associated data family operator declaration to parse, got: " <> show errs)

test_prettyAssocDataFamilyOperatorName :: Assertion
test_prettyAssocDataFamilyOperatorName = do
  let decl =
        DeclClass
          ClassDecl
            { classDeclContext = Nothing,
              classDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "C") [TyVarBinder [] "a" Nothing TyVarBSpecified TyVarBVisible],
              classDeclFundeps = [],
              classDeclItems =
                [ ClassItemDataFamilyDecl
                    DataFamilyDecl
                      { dataFamilyDeclHead = PrefixBinderHead (mkUnqualifiedName NameConSym ":*:") [TyVarBinder [] "a" Nothing TyVarBSpecified TyVarBVisible],
                        dataFamilyDeclKind = Nothing
                      }
                ]
            }
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  rendered @?= "class C a where {data (:*:) a}"

test_prettyAssocDataFamilyInfixOperatorName :: Assertion
test_prettyAssocDataFamilyInfixOperatorName = do
  let decl =
        DeclClass
          ClassDecl
            { classDeclContext = Nothing,
              classDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "C") [TyVarBinder [] "x" Nothing TyVarBSpecified TyVarBVisible],
              classDeclFundeps = [],
              classDeclItems =
                [ ClassItemDataFamilyDecl
                    DataFamilyDecl
                      { dataFamilyDeclHead =
                          InfixBinderHead
                            (TyVarBinder [] "a" Nothing TyVarBSpecified TyVarBVisible)
                            (mkUnqualifiedName NameConSym ":*:")
                            (TyVarBinder [] "b" Nothing TyVarBSpecified TyVarBVisible)
                            [],
                        dataFamilyDeclKind = Just TStar
                      }
                ]
            }
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  rendered @?= "class C x where {data a :*: b :: *}"

prop_generatedDataFamilyInstancesCanIncludeInlineResultKinds :: Property
prop_generatedDataFamilyInstancesCanIncludeInlineResultKinds =
  let samples = sampleGen 6000 genDeclDataFamilyInst
      matching =
        [ decl
        | decl@(DeclDataFamilyInst DataFamilyInst {dataFamilyInstKind = Just _}) <- samples
        ]
   in counterexample ("expected at least one generated data family instance with inline result kind; sampled " <> show (length samples)) (not (null matching))

prop_generatedClassDeclsCanIncludeAssociatedDataFamilyOperators :: Property
prop_generatedClassDeclsCanIncludeAssociatedDataFamilyOperators =
  let samples = sampleGen 6000 genDeclClass
      prefixMatches =
        [ decl
        | decl@(DeclClass ClassDecl {classDeclItems}) <- samples,
          ClassItemDataFamilyDecl dataFamilyDecl <- map peelClassDeclItemAnn classDeclItems,
          binderHeadForm (dataFamilyDeclHead dataFamilyDecl) == TypeHeadPrefix,
          let name = binderHeadName (dataFamilyDeclHead dataFamilyDecl),
          unqualifiedNameType name == NameConSym
        ]
      infixMatches =
        [ decl
        | decl@(DeclClass ClassDecl {classDeclItems}) <- samples,
          ClassItemDataFamilyDecl dataFamilyDecl <- map peelClassDeclItemAnn classDeclItems,
          binderHeadForm (dataFamilyDeclHead dataFamilyDecl) == TypeHeadInfix,
          let name = binderHeadName (dataFamilyDeclHead dataFamilyDecl),
          let params = binderHeadParams (dataFamilyDeclHead dataFamilyDecl),
          unqualifiedNameType name == NameConSym
            && length params == 2
        ]
   in counterexample
        ( "expected generated class declarations to include prefix and infix associated data family operators; sampled "
            <> show (length samples)
            <> ", prefix matches="
            <> show (length prefixMatches)
            <> ", infix matches="
            <> show (length infixMatches)
        )
        (not (null prefixMatches) && not (null infixMatches))

prop_generatedAssociatedTypeFamiliesCanUseExplicitFamilyKeyword :: Property
prop_generatedAssociatedTypeFamiliesCanUseExplicitFamilyKeyword =
  let samples = sampleGen 6000 (arbitrary :: Gen Module)
      matching =
        [ tf
        | modu <- samples,
          DeclClass ClassDecl {classDeclItems} <- moduleDecls modu,
          ClassItemTypeFamilyDecl tf <- map peelClassDeclItemAnn classDeclItems,
          typeFamilyDeclExplicitFamilyKeyword tf
        ]
   in counterexample
        ( "expected generated modules to include explicit associated type family syntax; sampled "
            <> show (length samples)
            <> ", matches="
            <> show (length matching)
        )
        (not (null matching))

prop_generatedClassDeclsCoverAllClassItemConstructors :: Property
prop_generatedClassDeclsCoverAllClassItemConstructors =
  let samples = sampleGen 6000 genDeclClass
      seen =
        Set.fromList
          [ showConstr (toConstr item)
          | DeclClass ClassDecl {classDeclItems} <- samples,
            item <- map peelClassDeclItemAnn classDeclItems
          ]
      expected =
        Set.fromList
          [ ctor
          | ctor <- map showConstr (dataTypeConstrs (dataTypeOf (undefined :: ClassDeclItem))),
            ctor /= "ClassItemAnn"
          ]
      missing = Set.toList (expected Set.\\ seen)
   in counterexample
        ( "expected generated class declarations to cover all class item constructors; missing "
            <> show missing
            <> ", sampled "
            <> show (length samples)
            <> " declarations"
        )
        (Set.null (expected Set.\\ seen))

prop_generatedModulesCanIncludeEmptyBundledImports :: Property
prop_generatedModulesCanIncludeEmptyBundledImports =
  let samples = sampleGen 6000 (arbitrary :: Gen Module)
      matching =
        [ modu
        | modu <- samples,
          any hasEmptyBundledImport (moduleImports modu)
        ]
   in counterexample
        ( "expected generated modules to include empty bundled imports; sampled "
            <> show (length samples)
            <> ", matches="
            <> show (length matching)
        )
        (not (null matching))
  where
    hasEmptyBundledImport decl =
      case importDeclSpec decl of
        Just spec -> any isEmptyBundledImportItem (importSpecItems spec)
        Nothing -> False
    isEmptyBundledImportItem item =
      case item of
        ImportAnn _ sub -> isEmptyBundledImportItem sub
        ImportItemWith _ _ [] -> True
        _ -> False

prop_generatedTypeNamesSupportEmptyBundledImports :: Property
prop_generatedTypeNamesSupportEmptyBundledImports =
  let samples = sampleGen 512 genTypeName
      renderImport name = T.unlines ["module M where", "import A (" <> renderUnqualifiedName name <> "())"]
      failures =
        [ (name, err)
        | name <- samples,
          Just err <- [validateParser "GeneratedEmptyBundledImport.hs" Haskell2010Edition [] (renderImport name)]
        ]
   in counterexample
        ( unlines
            [ "expected generated type names to support empty bundled import syntax",
              "sample count: " <> show (length samples),
              "failure count: " <> show (length failures),
              unlines [T.unpack (renderUnqualifiedName name) <> ": " <> show err | (name, err) <- take 10 failures]
            ]
        )
        (null failures)
