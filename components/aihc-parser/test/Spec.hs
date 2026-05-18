{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (resultOutput)
import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    formatParseErrorBundle,
    formatParseErrors,
    parseDecl,
    parseExpr,
    parseModule,
    parsePattern,
    parseSignatureType,
    parseType,
  )
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokens, lexTokensFromChunks, lexTokensWithExtensions)
import Aihc.Parser.Parens (addExprParens, addSignatureTypeParens, addTypeParens)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
import CppSupport (preprocessForParserWithoutIncludesIfEnabled)
import Data.Char (ord)
import Data.Data (Data, dataTypeConstrs, dataTypeOf, gmapQl, isAlgType, showConstr, toConstr)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex, showOct)
import ParserValidation (formatDiff, stripParens, validateParser)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.ErrorMessages.Suite (errorMessageTests)
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Oracle.Suite (oracleTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.Performance.Suite (parserPerformanceTests)
import Test.Properties.Arb.Decl (genDeclClass, genDeclDataFamilyInst, shrinkDecl)
import Test.Properties.Arb.Expr (shrinkExpr)
import Test.Properties.Arb.Identifiers
  ( genConSym,
    genVarSym,
    isValidConIdent,
    isValidGeneratedConSym,
    isValidGeneratedIdent,
    isValidGeneratedVarSym,
    shrinkIdent,
  )
import Test.Properties.Arb.Module (genTypeName)
import Test.Properties.Arb.Pattern (shrinkPattern)
import Test.Properties.Arb.Utils (requiredExtensions)
import Test.Properties.DeclRoundTrip (prop_declPrettyRoundTrip)
import Test.Properties.ExprRoundTrip (prop_exprPrettyRoundTrip)
import Test.Properties.MinimalParentheses (prop_minimalParenthesesExpr, prop_minimalParenthesesPattern, prop_minimalParenthesesSignatureType, prop_minimalParenthesesType)
import Test.Properties.ModuleRoundTrip (prop_modulePrettyRoundTrip, prop_moduleValidator)
import Test.Properties.NoExceptions
  ( prop_declParserArbitraryTokensNoExceptions,
    prop_exprParserArbitraryTokensNoExceptions,
    prop_genLexTokenKindConstructorCoverage,
    prop_importDeclParserArbitraryTokensNoExceptions,
    prop_lexerArbitraryTextNoExceptions,
    prop_moduleHeaderParserArbitraryTokensNoExceptions,
    prop_moduleParserArbitraryTokensNoExceptions,
    prop_patternParserArbitraryTokensNoExceptions,
    prop_preprocessorArbitraryTextNoExceptions,
    prop_typeParserArbitraryTokensNoExceptions,
  )
import Test.Properties.ParensIdempotency
  ( prop_declParensIdempotent,
    prop_exprParensIdempotent,
    prop_moduleParensIdempotent,
    prop_patternParensIdempotent,
    prop_typeParensIdempotent,
  )
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.ShorthandSubset
  ( prop_shorthandDeclSubsetOfShow,
    prop_shorthandExprSubsetOfShow,
    prop_shorthandLexTokenSubsetOfShow,
    prop_shorthandModuleSubsetOfShow,
    prop_shorthandTypeSubsetOfShow,
  )
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, counterexample, shrink)
import Test.QuickCheck.Gen qualified as QGen
import Test.QuickCheck.Random qualified as QRandom
import Test.StackageProgress.Summary (stackageProgressSummaryTests)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

tenMinutes :: Timeout
tenMinutes = Timeout (10 * 60 * 1000000) "10m"

sampleGen :: Int -> Gen a -> [a]
sampleGen count gen = QGen.unGen (QC.vectorOf count gen) (QRandom.mkQCGen 20260415) 5

renderPretty :: (Pretty a) => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty

assertEqualShorthand :: (Shorthand a, Eq a) => String -> a -> a -> Assertion
assertEqualShorthand context expected actual
  | expected == actual = return ()
  | otherwise = assertFailure ("context: " <> context <> "\nexpected:\n" <> show (shorthand expected) <> "\nactual:\n" <> show (shorthand actual))

assertParsedStrippedExprShapeRoundTrip :: ParserConfig -> Text -> Assertion
assertParsedStrippedExprShapeRoundTrip config source =
  case parseExpr config source of
    ParseOk expr ->
      let stripped = stripParens (stripAnnotations expr)
          rendered = renderPretty stripped
       in case parseExpr config rendered of
            ParseOk reparsed -> do
              assertEqualShorthand (T.unpack rendered) (stripAnnotations stripped) (stripAnnotations (stripParens reparsed))
            ParseErr bundle ->
              assertFailure ("expected pretty-printed expression to reparse, got:\n" <> formatParseErrors "<test>" Nothing bundle)
    ParseErr bundle ->
      assertFailure ("expected parse success for:\n" <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

assertParsedStrippedPatternShapeRoundTrip :: ParserConfig -> Text -> Assertion
assertParsedStrippedPatternShapeRoundTrip config source =
  case parsePattern config source of
    ParseOk pat ->
      let stripped = stripParens pat
          rendered = renderPretty stripped
       in case parsePattern config rendered of
            ParseOk reparsed ->
              assertEqual "reparsed pattern" (stripAnnotations stripped) (stripAnnotations (stripParens reparsed))
            ParseErr bundle ->
              assertFailure ("expected pretty-printed pattern to reparse:\n" <> T.unpack rendered <> "\ngot:\n" <> formatParseErrors "<test>" Nothing bundle)
    ParseErr bundle ->
      assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

assertParsedStrippedDeclShapeRoundTrip :: ParserConfig -> Text -> Assertion
assertParsedStrippedDeclShapeRoundTrip config source =
  case parseDecl config source of
    ParseOk decl ->
      let stripped = stripParens decl
          rendered = renderPretty stripped
       in case parseDecl config rendered of
            ParseOk reparsed ->
              assertEqual "reparsed declaration" (stripAnnotations stripped) (stripAnnotations (stripParens reparsed))
            ParseErr bundle ->
              assertFailure ("expected pretty-printed declaration to reparse, got:\n" <> formatParseErrors "<test>" Nothing bundle)
    ParseErr bundle ->
      assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

assertParsedModulePrettyContains :: Text -> Text -> Assertion
assertParsedModulePrettyContains source expected =
  case parseModule defaultConfig source of
    ([], modu) ->
      let rendered = renderPretty modu
       in assertBool ("expected rendered module to contain " <> T.unpack expected) (expected `T.isInfixOf` rendered)
    (errs, _) ->
      assertFailure ("expected parse success, got: " <> show errs)

assertExprRenderingRoundTrip :: ParserConfig -> Expr -> Text -> Assertion
assertExprRenderingRoundTrip config expr rendered =
  case parseExpr config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed expression" (stripAnnotations (addExprParens expr)) (stripAnnotations reparsed)
    ParseErr bundle ->
      assertFailure ("expected pretty-printed expression to reparse, got:\n" <> formatParseErrors "<test>" Nothing bundle)

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
            testCase "lexes quoted overloaded labels" test_quotedOverloadedLabelLexes,
            testCase "does not lex symbolic unicode as overloaded labels" test_unicodeSymbolIsNotOverloadedLabel,
            testCase "lexes string gaps before a closing quote" test_stringGapBeforeClosingQuoteLexes,
            testCase "pretty-prints overloaded labels with delimiter spacing" test_overloadedLabelPrettyPrintsWithDelimiterSpacing,
            testCase "applies LINE pragmas to subsequent tokens" test_linePragmaUpdatesSpan,
            testCase "applies COLUMN pragmas to subsequent tokens" test_columnPragmaUpdatesSpan,
            testCase "applies COLUMN pragmas in the middle of a line" test_inlineColumnPragmaUpdatesSpan,
            testCase "sets lexTokenAtLineStart correctly" test_tokenAtLineStartWithoutDirective,
            testCase "hash line directive sets lexTokenAtLineStart" test_hashLineDirectiveSetsAtLineStart,
            testCase "hash line directive preserves layout" test_hashLineDirectivePreservesLayout,
            testCase "inserts empty case layout at EOF" test_emptyCaseLayoutAtEof,
            testCase "comments are ignored in module headers" test_commentsIgnoredInModuleHeaders,
            testCase "comments are ignored by layout" test_commentsIgnoredByLayout,
            testCase "comments preserve trivia for negative literals" test_commentsPreserveTriviaForNegativeLiterals,
            testCase "indented hash-line is operator, not directive" test_indentedHashLineIsOperator,
            testCase "can lex lazily from chunks" test_lexerChunkLaziness,
            testCase "lexes alternate valid character literal spellings" test_alternateCharLiteralSpellingsLexLikeGhc,
            testCase "lexes control-backslash character literal" test_controlBackslashCharLiteralLexes,
            testCase "parses character literals after escaped backslash cons patterns" test_escapedBackslashConsPatternCharLiteralParses,
            testCase "generated identifiers reject extension keyword rec" test_generatedIdentifiersRejectExtensionKeywordRec,
            testCase "generated identifiers reject standalone underscore" test_generatedIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk identifiers reject standalone underscore" test_shrunkIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk pattern type signatures shrink their types" test_shrunkPatternTypeSignaturesShrinkTypes,
            testCase "shrunk module headers without warnings make progress" test_shrunkModuleHeaderWithoutWarningMakesProgress,
            testCase "shrunk class default pattern binds make progress" test_shrunkClassDefaultPatternBindMakesProgress,
            testCase "shrunk arrow command infix lhs modules make progress" test_shrunkArrowCommandInfixLhsModuleMakesProgress,
            testCase "shrunk wildcard pattern binds do not cycle" test_shrunkWildcardPatternBindsDoNotCycle,
            testCase "shrunk infix expression left operands do not cycle" test_shrunkInfixExprLeftOperandsDoNotCycle,
            testCase "shrunk right sections do not keep prefix minus" test_shrunkRightSectionsDoNotKeepPrefixMinus,
            testCase "shrunk let expressions do not cycle through simple lets" test_shrunkLetExpressionsDoNotCycleThroughSimpleLets,
            testCase "shrunk wildcard let expressions do not cycle through simple lets" test_shrunkWildcardLetExpressionsDoNotCycleThroughSimpleLets,
            testCase "generated identifiers accept unicode variable characters" test_generatedIdentifiersAcceptUnicodeVariableCharacters,
            testCase "lexes unicode identifier continuation characters" test_unicodeIdentifierContinuationCharactersLex,
            testCase "generated identifiers accept MagicHash suffixes" test_generatedIdentifiersAcceptMagicHashSuffixes,
            testCase "generated constructor identifiers accept unicode uppercase and number tails" test_generatedConstructorIdentifiersAcceptUnicodeCharacters,
            testCase "data CTYPE pragmas round-trip" test_dataDeclCTypePragmaRoundTrips,
            testCase "newtype CTYPE pragmas round-trip" test_newtypeCTypePragmaRoundTrips,
            testCase "generated constructor identifiers accept MagicHash suffixes" test_generatedConstructorIdentifiersAcceptMagicHashSuffixes,
            testCase "lexes identifiers with repeated MagicHash suffixes" test_magicHashIdentifierLexes,
            testCase "parses repeated MagicHash suffixes in exports" test_magicHashExportParses,
            testCase "quasiquotes do not enable Template Haskell name quotes" test_quasiQuotesDoNotEnableTHNameQuotes,
            testCase "generated constructor symbols reject reserved spellings" test_generatedConstructorSymbolsRejectReservedSpellings,
            testCase "generated variable symbols reject reserved spellings" test_generatedVariableSymbolsRejectReservedSpellings,
            testCase "generated operators reject arrow tail spellings" test_generatedOperatorsRejectArrowTailSpellings,
            testCase "generated expressions can include mdo" test_generatedExpressionsCanIncludeMdo,
            testCase "generated expressions can include SCC pragmas" test_generatedExpressionsCanIncludeSccPragmas,
            testCase "generated expressions can include explicit type syntax" test_generatedExpressionsCanIncludeExplicitTypeSyntax,
            testCase "pretty-prints negated layout-ending list-comprehension bodies" test_prettyNegatedLayoutEndingListCompBody,
            testCase "pretty-prints layout let guards in multi-way if" test_prettyLayoutLetGuardInMultiWayIf,
            testCase "pretty-prints multi-way if left operands using layout" test_prettyMultiWayIfInfixLhs,
            testCase "pretty-prints multi-way if left operands inside do using layout" test_prettyMultiWayIfInfixLhsInsideDo,
            testCase "pretty-prints multi-way if left operands inside unboxed tuples with parentheses" test_prettyMultiWayIfInfixLhsInsideUnboxedTuple,
            testCase "pretty-prints multi-way if left operands inside unboxed sums with parentheses" test_prettyMultiWayIfInfixLhsInsideUnboxedSum,
            testCase "pretty-prints lambda-case applicative chains without extra parens" test_prettyLambdaCaseApplicativeChain,
            testCase "pretty-prints TH splices before record dots with parentheses" test_prettySpliceRecordDotBase,
            testCase "pretty-prints infix RHS open-ended expressions inside sections" test_prettyInfixRhsOpenEndedInsideSection,
            testCase "pretty-prints nested infix RHS expressions inside sections" test_prettyNestedInfixRhsInsideSection,
            testCase "pretty-prints right sections with bare infix operands" test_prettyRightSectionInfixOperand,
            testCase "pretty-prints infix RHS open-ended expressions before following infix operators" test_prettyInfixRhsOpenEndedBeforeFollowingInfix,
            testCase "parenthesizes transform-list-comp group-by infix RHS before using" test_transformListCompGroupByInfixRhsParens,
            testCase "parenthesizes lambda RHS before following infix operators" test_lambdaInfixRhsBeforeFollowingInfixParens,
            testCase "parenthesizes if RHS before following infix operators" test_ifInfixRhsBeforeFollowingInfixParens,
            testCase "parenthesizes infix RHS operands inside left sections" test_infixRhsInsideLeftSectionParens,
            testCase "pretty-prints reserved at right sections" test_prettyReservedAtRightSection,
            testCase "pretty-prints type applications after layout-ending functions" test_prettyTypeAppAfterLayoutEndingFunction,
            testCase "pretty-prints type signatures after layout-ending functions" test_prettyTypeSigAfterLayoutEndingFunction,
            testCase "pretty-prints operators after layout-rendered do blocks" test_prettyOperatorAfterLayoutDoBlock,
            testCase "pretty-prints negated open-ended expressions inside left sections" test_prettyNegatedOpenEndedSectionLhs,
            testCase "pretty-prints negated open-ended type signature bodies" test_prettyNegatedOpenEndedTypeSigBody,
            testCase "parenthesizes command lambda RHS before following infix operators" test_commandLambdaRhsBeforeFollowingInfixParens,
            testCase "pretty-prints record-dot TH splice bases" test_prettyRecordDotTHSpliceBase,
            testCase "inserts required parentheses" test_parenthesesInsertion,
            testCase "parses TH type quotes before constrained expression signatures" test_thTypeQuoteBeforeConstraintExprSig,
            testCase "parenthesizes view expressions ending with applied type signatures" test_viewExprAppliedTypeSigParens,
            testCase "parenthesizes multi-way if view expressions ending with type signatures in decls" test_viewExprMultiWayIfTypeSigParens,
            testCase "parenthesizes arrow-command lhs applications ending in lambda-case" test_arrowCommandLhsLambdaCaseParens,
            testCase "parenthesizes arrow-command lhs applications ending in mdo" test_arrowCommandLhsMdoParens,
            testCase "parenthesizes arrow-command lhs applications ending in lambda-cases" test_arrowCommandLhsLambdaCasesParens,
            testCase "parenthesizes infix view expressions inside lambda-cases" test_lambdaCasesViewExprInfixParens,
            testCase "parenthesizes typed arrow-command RHS inside view expressions" test_viewExprArrowCommandTypeSigRhsParens,
            testCase "parenthesizes negated typed view expressions" test_viewExprNegatedTypeSigParens,
            testCase "pretty-prints typed view expressions without invalid layout" test_typedViewExprPrettyLayout,
            testCase "rejects bare kind signatures as signature types" test_signatureTypeParserRejectsBareKindSignature,
            testCase "parses parenthesized kind signatures as signature types" test_signatureTypeParserParsesParenthesizedKindSignature,
            testCase "parses delimited kind signatures as signature types" test_signatureTypeParserParsesDelimitedKindSignature,
            testCase "rejects bare kind signatures in signature type applications" test_signatureTypeParserRejectsAppArgBareKindSignature,
            testCase "parses parenthesized kind signatures in signature type applications" test_signatureTypeParserParsesAppArgParenthesizedKindSignature,
            testCase "rejects bare kind signatures in signature implicit parameter bodies" test_signatureTypeParserRejectsImplicitParamBareKindSignature,
            testCase "rejects bare implicit parameter type arguments in contexts" test_typeParserRejectsBareImplicitParamContextAppArg,
            testCase "rejects bare kind signatures in value signatures" test_declParserRejectsBareKindSignature,
            testCase "parses bare kind signatures as types" test_typeParserParsesBareKindSignature,
            testCase "parenthesizes forall kind signatures as signature types" test_signatureTypeParensWrapsForallKindSignatures,
            testCase "keeps nested kind signatures bare in type contexts" test_typeParensKeepsNestedKindSignaturesBare,
            testCase "keeps forall kind signatures bare in type contexts" test_typeParensKeepsForallKindSignaturesBare,
            testCase "keeps splice kind signatures bare in type contexts" test_typeParensKeepsSpliceKindSignaturesBare,
            testCase "keeps nested kind signatures bare in signature tuples" test_signatureTypeParensKeepsNestedKindSignaturesBare,
            testCase "shrunk do expressions keep a final expression statement" test_shrunkDoExpressionsKeepFinalExpression,
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
            [ QC.testProperty "expr paren insertion is minimal" prop_minimalParenthesesExpr,
              QC.testProperty "pattern paren insertion is minimal" prop_minimalParenthesesPattern,
              QC.testProperty "signature type paren insertion is minimal" prop_minimalParenthesesSignatureType,
              QC.testProperty "type paren insertion is minimal" prop_minimalParenthesesType,
              QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
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
              QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip,
              QC.testProperty "module paren insertion is idempotent" prop_moduleParensIdempotent,
              QC.testProperty "decl paren insertion is idempotent" prop_declParensIdempotent,
              QC.testProperty "expr paren insertion is idempotent" prop_exprParensIdempotent,
              QC.testProperty "pattern paren insertion is idempotent" prop_patternParensIdempotent,
              QC.testProperty "type paren insertion is idempotent" prop_typeParensIdempotent,
              QC.testProperty "module shorthand is a subset of Show" prop_shorthandModuleSubsetOfShow,
              QC.testProperty "decl shorthand is a subset of Show" prop_shorthandDeclSubsetOfShow,
              QC.testProperty "expr shorthand is a subset of Show" prop_shorthandExprSubsetOfShow,
              QC.testProperty "type shorthand is a subset of Show" prop_shorthandTypeSubsetOfShow,
              QC.testProperty "lex token shorthand is a subset of Show" prop_shorthandLexTokenSubsetOfShow,
              QC.testProperty "lex token kind generator covers constructors" prop_genLexTokenKindConstructorCoverage,
              testGroup
                "no exceptions"
                [ QC.testProperty "preprocessor accepts arbitrary text" prop_preprocessorArbitraryTextNoExceptions,
                  QC.testProperty "lexer accepts arbitrary text" prop_lexerArbitraryTextNoExceptions,
                  QC.testProperty "module parser accepts arbitrary tokens" prop_moduleParserArbitraryTokensNoExceptions,
                  QC.testProperty "expr parser accepts arbitrary tokens" prop_exprParserArbitraryTokensNoExceptions,
                  QC.testProperty "type parser accepts arbitrary tokens" prop_typeParserArbitraryTokensNoExceptions,
                  QC.testProperty "pattern parser accepts arbitrary tokens" prop_patternParserArbitraryTokensNoExceptions,
                  QC.testProperty "decl parser accepts arbitrary tokens" prop_declParserArbitraryTokensNoExceptions,
                  QC.testProperty "import decl parser accepts arbitrary tokens" prop_importDeclParserArbitraryTokensNoExceptions,
                  QC.testProperty "module header parser accepts arbitrary tokens" prop_moduleHeaderParserArbitraryTokensNoExceptions
                ]
            ],
        oracle,
        extensionMappingTests,
        hackageTester,
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

test_unicodeSymbolIsNotOverloadedLabel :: Assertion
test_unicodeSymbolIsNotOverloadedLabel = do
  case lexTokensWithExtensions [OverloadedLabels] "#﹏" of
    [LexToken {lexTokenKind = TkVarSym "#﹏"}, LexToken {lexTokenKind = TkEOF}] -> pure ()
    other -> assertFailure ("expected symbolic operator tokens, got: " <> show other)
  let config = defaultConfig {parserExtensions = [Arrows, MagicHash, OverloadedLabels]}
      source = "0.0 = proc 0.0 -> 0 -<< 'w'# where { ( #﹏ ) = proc C -> [] -< [] }"
  case parseDecl config source of
    ParseOk _ -> pure ()
    ParseErr bundle ->
      assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

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
      expected = ["( #a,\n   )", "[ #a]", "( #a)"]
  assertEqual "pretty-printed forms" expected rendered
  mapM_
    ( \source ->
        case parseExpr config source of
          ParseErr err -> assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing err)
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

test_emptyCaseLayoutAtEof :: Assertion
test_emptyCaseLayoutAtEof =
  let source = "x = case () of"
      kinds = map lexTokenKind (lexTokens source)
   in assertEqual
        "token kinds"
        [ TkVarId "x",
          TkReservedEquals,
          TkKeywordCase,
          TkSpecialLParen,
          TkSpecialRParen,
          TkKeywordOf,
          TkSpecialLBrace,
          TkSpecialRBrace,
          TkEOF
        ]
        kinds

test_commentsIgnoredInModuleHeaders :: Assertion
test_commentsIgnoredInModuleHeaders =
  let source =
        T.unlines
          [ "{-# LANGUAGE OverloadedLabels #-}",
            "-- comment between pragma and module header",
            "module Test where",
            "x = #foo"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        assertEqual "expected one declaration" 1 (length (moduleDecls modu))

test_commentsIgnoredByLayout :: Assertion
test_commentsIgnoredByLayout =
  let source =
        T.unlines
          [ "module Test where",
            "main = do",
            "  -- comment inside layout block",
            "  pure ()",
            "  pure ()"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        assertEqual "expected one declaration" 1 (length (moduleDecls modu))

test_commentsPreserveTriviaForNegativeLiterals :: Assertion
test_commentsPreserveTriviaForNegativeLiterals =
  case lexTokensWithExtensions [NegativeLiterals] "x -- comment\n-1" of
    [ LexToken {lexTokenKind = TkVarId "x"},
      LexToken {lexTokenKind = TkLineComment},
      LexToken {lexTokenKind = TkInteger (-1) TInteger},
      LexToken {lexTokenKind = TkEOF}
      ] -> pure ()
    other -> assertFailure ("expected comment trivia before negative literal, got: " <> show other)

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

test_shrunkPatternTypeSignaturesShrinkTypes :: Assertion
test_shrunkPatternTypeSignaturesShrinkTypes =
  assertBool "pattern type signatures should shrink names in their types" $
    any shrinksTypeName (shrinkPattern pat)
  where
    pat = PTypeSig PWildcard (TCon originalName Unpromoted)
    originalName = mkName (Just "\120613\120044\42565") NameConSym ":\118928\8690\10289"
    shrinksTypeName (PTypeSig PWildcard (TCon name Unpromoted)) =
      isNothing (nameQualifier name) || nameText name == ":+"
    shrinksTypeName _ = False

test_shrunkModuleHeaderWithoutWarningMakesProgress :: Assertion
test_shrunkModuleHeaderWithoutWarningMakesProgress =
  assertBool "module shrinker must not return the original module" $
    modu `notElem` shrink modu
  where
    modu =
      Module
        { moduleAnns = [],
          moduleHead =
            Just
              ModuleHead
                { moduleHeadAnns = [],
                  moduleHeadName = "A",
                  moduleHeadWarningPragma = Nothing,
                  moduleHeadExports = Nothing
                },
          moduleLanguagePragmas = [],
          moduleImports = [],
          moduleDecls = []
        }

test_shrunkClassDefaultPatternBindMakesProgress :: Assertion
test_shrunkClassDefaultPatternBindMakesProgress =
  assertBool "class default pattern bind shrinker must not return the original declaration" $
    decl `notElem` shrinkDecl decl
  where
    decl =
      DeclClass
        ClassDecl
          { classDeclContext = Nothing,
            classDeclHead = PrefixBinderHead (mkUnqualifiedName NameConId "C") [],
            classDeclFundeps = [],
            classDeclItems =
              [ ClassItemDefault
                  ( PatternBind
                      NoMultiplicityTag
                      (PVar (mkUnqualifiedName NameVarId "x"))
                      (UnguardedRhs [] (EList []) Nothing)
                  )
              ]
          }

test_shrunkArrowCommandInfixLhsModuleMakesProgress :: Assertion
test_shrunkArrowCommandInfixLhsModuleMakesProgress =
  assertBool "module shrinker must not return the original arrow command module" $
    modu `notElem` shrink modu
  where
    modu =
      Module
        { moduleAnns = [],
          moduleHead = Nothing,
          moduleLanguagePragmas = [],
          moduleImports = [],
          moduleDecls =
            [ DeclValue
                ( PatternBind
                    NoMultiplicityTag
                    (PVar (mkUnqualifiedName NameVarId "a"))
                    (UnguardedRhs [] (EProc PWildcard command) Nothing)
                )
            ]
        }
    command =
      CmdArrApp
        (EInfix (EList []) (qualifyName Nothing (mkUnqualifiedName NameVarId "a")) (EList []))
        HsFirstOrderApp
        ( ELetDecls
            [ DeclValue
                ( PatternBind
                    NoMultiplicityTag
                    (PVar (mkUnqualifiedName NameVarId "x"))
                    (UnguardedRhs [] (ETuple Unboxed []) Nothing)
                )
            ]
            (EInt 846 TIntHash "846#")
        )

test_shrunkWildcardPatternBindsDoNotCycle :: Assertion
test_shrunkWildcardPatternBindsDoNotCycle =
  assertBool "pattern bind shrinker must not cycle between wildcard and simple variable patterns" $
    all (\shrunk -> decl `notElem` shrinkDecl shrunk) (shrinkDecl decl)
  where
    decl =
      DeclValue
        ( PatternBind
            NoMultiplicityTag
            PWildcard
            (UnguardedRhs [] (EList []) Nothing)
        )

test_shrunkInfixExprLeftOperandsDoNotCycle :: Assertion
test_shrunkInfixExprLeftOperandsDoNotCycle =
  assertBool "infix expression shrinker must not cycle between simple variables and empty lists on the lhs" $
    all (\shrunk -> expr `notElem` shrink shrunk) (shrink expr)
  where
    expr =
      EInfix
        (EVar (qualifyName Nothing (mkUnqualifiedName NameVarId "a")))
        (qualifyName Nothing (mkUnqualifiedName NameVarId "a"))
        (EList [])

test_shrunkRightSectionsDoNotKeepPrefixMinus :: Assertion
test_shrunkRightSectionsDoNotKeepPrefixMinus =
  assertBool "right section shrinker must not keep the unqualified minus operator" $
    not (any isUnqualifiedMinusRightSection (shrink expr))
  where
    expr =
      ESectionR
        (qualifyName Nothing (mkUnqualifiedName NameVarSym "-"))
        (EInt 1 TInteger "1")
    isUnqualifiedMinusRightSection (ESectionR name _) =
      isNothing (nameQualifier name)
        && nameType name == NameVarSym
        && nameText name == "-"
    isUnqualifiedMinusRightSection _ = False

test_shrunkLetExpressionsDoNotCycleThroughSimpleLets :: Assertion
test_shrunkLetExpressionsDoNotCycleThroughSimpleLets =
  assertBool "let expression shrinker must not cycle through its simple let target" $
    all (\shrunk -> expr `notElem` shrink shrunk) (shrink expr)
  where
    expr =
      ELetDecls
        [ DeclValue
            ( PatternBind
                NoMultiplicityTag
                (PVar (mkUnqualifiedName NameVarId "a"))
                (UnguardedRhs [] (ETuple Boxed []) Nothing)
            )
        ]
        (EList [])

test_shrunkWildcardLetExpressionsDoNotCycleThroughSimpleLets :: Assertion
test_shrunkWildcardLetExpressionsDoNotCycleThroughSimpleLets =
  assertBool "let expression shrinker must not cycle between wildcard and simple variable let targets" $
    all (\shrunk -> expr `notElem` shrink shrunk) (shrink expr)
  where
    expr =
      ELetDecls
        [ DeclValue
            ( PatternBind
                NoMultiplicityTag
                PWildcard
                (UnguardedRhs [] (EList []) Nothing)
            )
        ]
        (EList [])

test_generatedIdentifiersAcceptUnicodeVariableCharacters :: Assertion
test_generatedIdentifiersAcceptUnicodeVariableCharacters = do
  assertBool "unicode lowercase letters and unicode numbers should be accepted in generated identifiers" $
    isValidGeneratedIdent "a\x03b1\x00b2"
  assertBool "unicode lowercase letters should be accepted at the start of generated identifiers" $
    isValidGeneratedIdent "\x03bbx"

test_unicodeIdentifierContinuationCharactersLex :: Assertion
test_unicodeIdentifierContinuationCharactersLex =
  mapM_
    assertVarId
    [ "\x03b1\x209b", -- modifier letter: alpha + subscript s
      "a\x0301", -- non-spacing mark: a + combining acute
      "a\x2160" -- letter number: a + roman numeral one
    ]
  where
    assertVarId ident =
      case lexTokens ident of
        [LexToken {lexTokenKind = TkVarId actual}, LexToken {lexTokenKind = TkEOF}]
          | actual == ident -> pure ()
        other -> assertFailure ("expected variable identifier " <> T.unpack ident <> ", got: " <> show other)

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

test_dataDeclCTypePragmaRoundTrips :: Assertion
test_dataDeclCTypePragmaRoundTrips = do
  let source = T.unlines ["{-# LANGUAGE GHC2021 #-}", "{-# LANGUAGE CApiFFI #-}", "module M where", "data {-# CTYPE \"termbox.h\" \"struct tb_cell\" #-} Tb_cell = Tb_cell"]
  assertParsedModulePrettyContains source "CTYPE \"termbox.h\" \"struct tb_cell\""

test_newtypeCTypePragmaRoundTrips :: Assertion
test_newtypeCTypePragmaRoundTrips = do
  let source = T.unlines ["{-# LANGUAGE GHC2021 #-}", "{-# LANGUAGE CApiFFI #-}", "module M where", "import Foreign.C.Types (CInt (..))", "newtype {-# CTYPE \"signed int\" #-} Fixed = Fixed CInt"]
  assertParsedModulePrettyContains source "CTYPE \"signed int\""

test_generatedConstructorIdentifiersAcceptMagicHashSuffixes :: Assertion
test_generatedConstructorIdentifiersAcceptMagicHashSuffixes = do
  assertBool "MagicHash should allow a single trailing hash on constructor identifiers" $
    isValidConIdent "T#"
  assertBool "MagicHash should allow repeated trailing hashes on constructor identifiers" $
    isValidConIdent "T####"

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
    not (any isValidGeneratedVarSym ["..", "=", "\\", "|", "|+", "<-", "->", "~", "=>", "--", "---"])

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

test_generatedExpressionsCanIncludeSccPragmas :: Assertion
test_generatedExpressionsCanIncludeSccPragmas =
  let samples = QGen.unGen (QC.vectorOf 4000 (QC.resize 5 (QC.arbitrary :: QC.Gen Expr))) (QRandom.mkQCGen 738) 5
   in assertBool "expected expression generator to include at least one SCC pragma expression" $
        any (Set.member "EPragma" . usedCtors) samples

test_generatedExpressionsCanIncludeExplicitTypeSyntax :: Assertion
test_generatedExpressionsCanIncludeExplicitTypeSyntax =
  let samples = QGen.unGen (QC.vectorOf 4000 (QC.resize 5 (QC.arbitrary :: QC.Gen Expr))) (QRandom.mkQCGen 739) 5
   in assertBool "expected expression generator to include at least one explicit type syntax expression" $
        any (Set.member "ETypeSyntax" . usedCtors) samples

usedCtors :: (Data a) => a -> Set.Set String
usedCtors x =
  let here
        | isAlgType (dataTypeOf x) = Set.singleton (showConstr (toConstr x))
        | otherwise = Set.empty
   in here <> gmapQl (<>) Set.empty usedCtors x

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

test_quasiQuotesDoNotEnableTHNameQuotes :: Assertion
test_quasiQuotesDoNotEnableTHNameQuotes = do
  let config = defaultConfig {parserExtensions = [QuasiQuotes]}
      source = T.unlines ["module M where", "x = 'id", "y = ''T"]
      (errs, _modu) = parseModule config source
  assertBool "expected TH name quotes to require TemplateHaskellQuotes or TemplateHaskell" (not (null errs))

test_prettyNegatedLayoutEndingListCompBody :: Assertion
test_prettyNegatedLayoutEndingListCompBody = do
  let config = defaultConfig {parserExtensions = [BlockArguments]}
      source =
        """
        [-0.0
          case 0.0 of
            0
              | let {  }
               ->
                ""
         | let {  }]
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyLayoutLetGuardInMultiWayIf :: Assertion
test_prettyLayoutLetGuardInMultiWayIf = do
  let config = defaultConfig {parserExtensions = [BlockArguments, MultiWayIf]}
      source =
        """
        if | let
             x =
               ()
                :: a
            ->
             case '5' of
               (+) ->
                 0
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyMultiWayIfInfixLhs :: Assertion
test_prettyMultiWayIfInfixLhs = do
  let config = defaultConfig {parserExtensions = [MultiWayIf]}
      source =
        """
        (if | True
            ->
             ())
         `a` 'x'
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyMultiWayIfInfixLhsInsideDo :: Assertion
test_prettyMultiWayIfInfixLhsInsideDo = do
  let config = defaultConfig {parserExtensions = [MultiWayIf]}
      source =
        """
        do
          (if | True
              ->
               ())
            `a` 'x'
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyMultiWayIfInfixLhsInsideUnboxedTuple :: Assertion
test_prettyMultiWayIfInfixLhsInsideUnboxedTuple = do
  let config = defaultConfig {parserExtensions = [UnboxedTuples, MultiWayIf, OverloadedLabels]}
      source =
        """
        (# ((if | let {  }
                 ->
                  #foo)
            `a` [880
                 | 48.7]) #)
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyMultiWayIfInfixLhsInsideUnboxedSum :: Assertion
test_prettyMultiWayIfInfixLhsInsideUnboxedSum = do
  let config = defaultConfig {parserExtensions = [UnboxedSums, MultiWayIf]}
      source =
        """
        (# ((if | let {  }
                 ->
                  0)
             `a` ()) | #)
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyLambdaCaseApplicativeChain :: Assertion
test_prettyLambdaCaseApplicativeChain = do
  let config = defaultConfig {parserExtensions = [LambdaCase]}
      source =
        T.unlines
          [ "f originalParsedOptions =",
            "  (Options <$> __command",
            "   <*> \\case",
            "         f | __withFlake f -> Flake",
            "         _ | otherwise -> Traditional",
            "   <*> __prioritiseLocalPinnedSystem)",
            "    originalParsedOptions"
          ]
  assertParsedStrippedDeclShapeRoundTrip config source

test_prettyInfixRhsOpenEndedInsideSection :: Assertion
test_prettyInfixRhsOpenEndedInsideSection = do
  let config = defaultConfig {parserExtensions = [LambdaCase]}
      source =
        """
        ((\\case {  })
         `a` (\\ 0 -> ' ')
         `a`)
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyNestedInfixRhsInsideSection :: Assertion
test_prettyNestedInfixRhsInsideSection = do
  let source =
        """
        ([]
         + (""
            `a` ())
         +)
        """
  assertParsedStrippedExprShapeRoundTrip defaultConfig source

test_prettyRightSectionInfixOperand :: Assertion
test_prettyRightSectionInfixOperand = do
  let config = defaultConfig {parserExtensions = [OverloadedStrings]}
      sources =
        [ "(/ 10 ^ (3 :: Int))",
          "(<> msg <> \"\\n\")",
          "(++ \" seconds\" ++ dir f)",
          "(< sizeOf x * 8)",
          "(<= m + 1 / 2)",
          "(.+^ p ^. vel)",
          "(<?> prettyIndentation ref ++ \" (started at line \" ++ prettyLine ref ++ \")\")"
        ]
  mapM_ (assertParsedStrippedExprShapeRoundTrip config) sources

test_prettyInfixRhsOpenEndedBeforeFollowingInfix :: Assertion
test_prettyInfixRhsOpenEndedBeforeFollowingInfix = do
  let config = defaultConfig {parserExtensions = [TemplateHaskell, QuasiQuotes]}
      source =
        """
        [t| () |]
         + (if [t| _ |] then [] else [])
         `a` 0
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_lambdaInfixRhsBeforeFollowingInfixParens :: Assertion
test_lambdaInfixRhsBeforeFollowingInfixParens = do
  let config = defaultConfig {parserExtensions = [QuasiQuotes]}
      source =
        """
        (+) =
          []
           `a` (\\ [a||] -> 0)
           `a` ()
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_ifInfixRhsBeforeFollowingInfixParens :: Assertion
test_ifInfixRhsBeforeFollowingInfixParens = do
  let config = defaultConfig {parserExtensions = [OverloadedLabels, OverloadedRecordDot]}
      source =
        """
        (+) =
          []
           `a` (if () then #a else ())
           `a` (.a)
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_infixRhsInsideLeftSectionParens :: Assertion
test_infixRhsInsideLeftSectionParens = do
  let config = defaultConfig {parserExtensions = [TemplateHaskell]}
      source =
        """
        a =
          ('' ()
           + (0 `a` 0)
           `a`)
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_prettyReservedAtRightSection :: Assertion
test_prettyReservedAtRightSection = do
  let expr = ESectionR (qualifyName Nothing (mkUnqualifiedName NameVarSym "@")) (ETuple Boxed [])
      rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
  assertEqual "pretty-printed expression" "(@ ())" rendered
  assertExprRenderingRoundTrip defaultConfig expr rendered

test_prettyTypeAppAfterLayoutEndingFunction :: Assertion
test_prettyTypeAppAfterLayoutEndingFunction = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        (+) =
          0
            \\case
              0.0 ->
                ""#
           @(# _ | * #)
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_prettyTypeSigAfterLayoutEndingFunction :: Assertion
test_prettyTypeSigAfterLayoutEndingFunction = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        (:+) =
          [d|
            |]
            \\case
              _ ->
                '1'
           :: [a||]
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_prettyOperatorAfterLayoutDoBlock :: Assertion
test_prettyOperatorAfterLayoutDoBlock = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        (do
           let f x = \\case
                 -134 | (# #) <- 'n' -> 85.3)
        + (# | | | 0 #)
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettySpliceRecordDotBase :: Assertion
test_prettySpliceRecordDotBase = do
  let config = defaultConfig {parserExtensions = [TemplateHaskell, OverloadedRecordDot, MagicHash]}
  assertParsedStrippedExprShapeRoundTrip config "($x#).adpE"

test_prettyNegatedOpenEndedSectionLhs :: Assertion
test_prettyNegatedOpenEndedSectionLhs = do
  let config = defaultConfig {parserExtensions = [BlockArguments]}
      source =
        """
        ((-(""
          (if 'N' then () else ())))
         `a`)
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_prettyNegatedOpenEndedTypeSigBody :: Assertion
test_prettyNegatedOpenEndedTypeSigBody = do
  let config = defaultConfig {parserExtensions = [BlockArguments, MagicHash, UnboxedTuples]}
      source =
        """
        -((# #)
          (if ""# then [] else (+)))
         :: C
        """
  assertParsedStrippedExprShapeRoundTrip config source

test_commandLambdaRhsBeforeFollowingInfixParens :: Assertion
test_commandLambdaRhsBeforeFollowingInfixParens = do
  let plusName = qualifyName Nothing (mkUnqualifiedName NameVarSym "+")
      aName = qualifyName Nothing (mkUnqualifiedName NameVarId "a")
      arrApp op = CmdArrApp (EList []) op (EList [])
      command =
        CmdInfix
          ( CmdInfix
              (arrApp HsHigherOrderApp)
              plusName
              (CmdLam [PWildcard] (arrApp HsFirstOrderApp))
          )
          aName
          (arrApp HsFirstOrderApp)
      decl =
        DeclValue
          ( PatternBind
              NoMultiplicityTag
              PWildcard
              (UnguardedRhs [] (EProc PWildcard command) Nothing)
          )
      rendered = renderPretty decl
  case validateParser "<test>" GHC2024Edition (map EnableExtension requiredExtensions) rendered of
    Nothing -> pure ()
    Just err ->
      assertFailure ("expected pretty-printed command to validate, got:\n" <> show err <> "\nRendered:\n" <> T.unpack rendered)

test_prettyRecordDotTHSpliceBase :: Assertion
test_prettyRecordDotTHSpliceBase = do
  let config = defaultConfig {parserExtensions = [TemplateHaskell, MagicHash, OverloadedRecordDot]}
  assertParsedStrippedExprShapeRoundTrip config "($q#).j7Msfc"
  assertParsedStrippedExprShapeRoundTrip config "($$q#).j7Msfc"

test_parenthesesInsertion :: Assertion
test_parenthesesInsertion = do
  let config =
        defaultConfig
          { parserExtensions =
              [ TemplateHaskell,
                MagicHash,
                OverloadedRecordDot,
                Arrows,
                BlockArguments,
                QuasiQuotes,
                ViewPatterns,
                UnboxedSums,
                MultiWayIf,
                QualifiedDo,
                ParallelListComp,
                TransformListComp
              ]
          }
  assertParsedStrippedExprShapeRoundTrip config "- (- 10)"
  assertParsedStrippedExprShapeRoundTrip config "a + (b + c)"
  assertParsedStrippedExprShapeRoundTrip config "(' (+)).a"
  assertParsedStrippedExprShapeRoundTrip config "(A.+).a"
  assertParsedStrippedExprShapeRoundTrip config "[t| (_ :: _) |]"
  assertParsedStrippedExprShapeRoundTrip config "[p| _ |] (proc _ -> () -<< a)"
  assertParsedStrippedPatternShapeRoundTrip config "(# | proc _ -> [] -< [] -> _ #)"
  assertParsedStrippedPatternShapeRoundTrip config "(:+) {a = [let a = () in [] ..] -> _}"
  assertParsedStrippedPatternShapeRoundTrip config "C {a = proc _ -> do ([] -<< []) + case [] of _ -> [] -<< [] -> _}"
  assertParsedStrippedPatternShapeRoundTrip config "(proc _ -> (if [] then [] -<< [] else [] -<< []) + case [] of _ -> [] -< [] -> _)"
  assertParsedStrippedDeclShapeRoundTrip config "a = ((if | [] -> []) :: _,)"
  assertParsedStrippedPatternShapeRoundTrip config "((A.do (if | [] -> []) :: _) -> _)"
  assertParsedStrippedDeclShapeRoundTrip config "_ = ([] + - let _ = [] in []) :: _"
  assertParsedStrippedPatternShapeRoundTrip config "C {a = [[] | then [] by [] | then [] + [] by []] -> _}"
  assertParsedStrippedExprShapeRoundTrip config "let ((:+) :: _) = [] in []"

test_thTypeQuoteBeforeConstraintExprSig :: Assertion
test_thTypeQuoteBeforeConstraintExprSig = do
  let config = defaultConfig {parserExtensions = [TemplateHaskell, QuasiQuotes]}
      source :: Text
      source = "x = [t| C |] :: (:+) => ()"
  case parseDecl config source of
    ParseOk _ -> pure ()
    ParseErr bundle ->
      assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

test_viewExprAppliedTypeSigParens :: Assertion
test_viewExprAppliedTypeSigParens = do
  let config = defaultConfig {parserExtensions = [BlockArguments, DataKinds, ViewPatterns]}
      source =
        """
        (([]
          (if a then a else []
           :: 'C -> C))
         -> C)
        """
  assertParsedStrippedPatternShapeRoundTrip config source

test_viewExprMultiWayIfTypeSigParens :: Assertion
test_viewExprMultiWayIfTypeSigParens = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        (# (if | let {  }
                ->
                 'G'
                  :: *)
              -> 0
             | 
             |  #) =
          [(\\cases {  }) ..]
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_viewExprArrowCommandTypeSigRhsParens :: Assertion
test_viewExprArrowCommandTypeSigRhsParens = do
  let config = defaultConfig {parserExtensions = [Arrows, MagicHash, QuasiQuotes, ViewPatterns]}
      source =
        """
        ((proc a -> () -<< ('7'#
         :: [a||]))
         -> C {})
        """
  assertParsedStrippedPatternShapeRoundTrip config source

test_lambdaCasesViewExprInfixParens :: Assertion
test_lambdaCasesViewExprInfixParens = do
  let config = defaultConfig {parserExtensions = [LambdaCase, ViewPatterns]}
      source =
        """
        _ = \\cases
              (([]
              `a` [])
               -> _) ->
                []
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_viewExprNegatedTypeSigParens :: Assertion
test_viewExprNegatedTypeSigParens = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        _ = []
            where
              ((- \\ _ -> []
                    :: _
                 -> _)
               -> _) + _ = []
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_typedViewExprPrettyLayout :: Assertion
test_typedViewExprPrettyLayout = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  assertParsedStrippedExprShapeRoundTrip
    config
    """
    do
      (([]
        :: _)
       -> _) <- []
      []
    """
  assertParsedStrippedPatternShapeRoundTrip
    config
    """
    ((if | let _ = []
            ->
             []
       :: _)
     -> _)
    """

test_signatureTypeParserRejectsBareKindSignature :: Assertion
test_signatureTypeParserRejectsBareKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "_ :: _" of
    ParseErr {} -> pure ()
    ParseOk ty ->
      assertFailure ("expected parse failure, got: " <> show (shorthand (stripAnnotations ty)))

test_signatureTypeParserParsesParenthesizedKindSignature :: Assertion
test_signatureTypeParserParsesParenthesizedKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "(_ :: _)" of
    ParseOk ty ->
      assertEqual "parenthesized kind signature" (TParen (TKindSig TWildcard TWildcard)) (stripAnnotations ty)
    ParseErr bundle ->
      assertFailure ("expected parse success for parenthesized kind signature\n" <> formatParseErrors "<test>" Nothing bundle)

test_signatureTypeParserParsesDelimitedKindSignature :: Assertion
test_signatureTypeParserParsesDelimitedKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "[_ :: _]" of
    ParseOk ty ->
      assertEqual "delimited kind signature" (TList Unpromoted [TKindSig TWildcard TWildcard]) (stripAnnotations ty)
    ParseErr bundle ->
      assertFailure ("expected parse success for delimited kind signature\n" <> formatParseErrors "<test>" Nothing bundle)

test_signatureTypeParserRejectsAppArgBareKindSignature :: Assertion
test_signatureTypeParserRejectsAppArgBareKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "_ _ :: _" of
    ParseErr {} -> pure ()
    ParseOk ty ->
      assertFailure ("expected parse failure, got: " <> show (shorthand (stripAnnotations ty)))

test_signatureTypeParserParsesAppArgParenthesizedKindSignature :: Assertion
test_signatureTypeParserParsesAppArgParenthesizedKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "_ (_ :: _)" of
    ParseOk ty ->
      assertEqual "parenthesized app argument kind signature" (TApp TWildcard (TParen (TKindSig TWildcard TWildcard))) (stripAnnotations ty)
    ParseErr bundle ->
      assertFailure ("expected parse success for parenthesized app argument kind signature\n" <> formatParseErrors "<test>" Nothing bundle)

test_signatureTypeParserRejectsImplicitParamBareKindSignature :: Assertion
test_signatureTypeParserRejectsImplicitParamBareKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseSignatureType config "?a :: _ :: _" of
    ParseErr {} -> pure ()
    ParseOk ty ->
      assertFailure ("expected parse failure, got: " <> show (shorthand (stripAnnotations ty)))

test_typeParserRejectsBareImplicitParamContextAppArg :: Assertion
test_typeParserRejectsBareImplicitParamContextAppArg = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseType config "_ => (_, (:+) ?a :: _) => _" of
    ParseErr {} -> pure ()
    ParseOk ty ->
      assertFailure ("expected parse failure, got: " <> show (shorthand (stripAnnotations ty)))

test_declParserRejectsBareKindSignature :: Assertion
test_declParserRejectsBareKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseDecl config "x :: _ :: _" of
    ParseErr {} -> pure ()
    ParseOk decl ->
      assertFailure ("expected parse failure, got: " <> show (shorthand (stripAnnotations decl)))

test_typeParserParsesBareKindSignature :: Assertion
test_typeParserParsesBareKindSignature = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
  case parseType config "_ :: _" of
    ParseOk ty ->
      assertEqual "type kind signature" (TKindSig TWildcard TWildcard) (stripAnnotations ty)
    ParseErr bundle ->
      assertFailure ("expected parse success for type kind signature\n" <> formatParseErrors "<test>" Nothing bundle)

test_signatureTypeParensWrapsForallKindSignatures :: Assertion
test_signatureTypeParensWrapsForallKindSignatures = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      ty = TForall (ForallTelescope ForallInvisible [TyVarBinder [] "a" Nothing TyVarBSpecified TyVarBVisible]) (TKindSig TWildcard TWildcard)
      rendered = renderPretty (addSignatureTypeParens ty)
  assertEqual "signature forall kind signature rendering" "forall a. (_ :: _)" rendered
  case parseSignatureType config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed signature forall kind signature" ty (stripAnnotations (stripParens reparsed))
    ParseErr bundle ->
      assertFailure ("expected parse success for signature forall kind signature\n" <> formatParseErrorBundle "<test>" Nothing bundle)

test_typeParensKeepsNestedKindSignaturesBare :: Assertion
test_typeParensKeepsNestedKindSignaturesBare =
  assertEqual
    "nested kind signatures in contexts should not gain extra parens"
    (TContext [TWildcard] (TContext [TWildcard] (TKindSig TWildcard TWildcard)))
    ( addTypeParens
        (TContext [TWildcard] (TContext [TWildcard] (TKindSig TWildcard TWildcard)))
    )

test_typeParensKeepsForallKindSignaturesBare :: Assertion
test_typeParensKeepsForallKindSignaturesBare = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      ty = TContext [TWildcard] (TForall (ForallTelescope ForallInvisible [TyVarBinder [] "a" Nothing TyVarBSpecified TyVarBVisible]) (TKindSig TWildcard TWildcard))
      rendered = renderPretty (addTypeParens ty)
  assertEqual "constrained forall kind signature rendering" "_ => forall a. _ :: _" rendered
  case parseType config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed constrained forall kind signature" ty (stripAnnotations reparsed)
    ParseErr bundle ->
      assertFailure ("expected parse success for constrained forall kind signature\n" <> formatParseErrorBundle "<test>" Nothing bundle)

test_typeParensKeepsSpliceKindSignaturesBare :: Assertion
test_typeParensKeepsSpliceKindSignaturesBare = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      ty = TContext [TWildcard] (TKindSig (TSplice (EVar (qualifyName Nothing (mkUnqualifiedName NameVarId "a")))) TWildcard)
      rendered = renderPretty (addTypeParens ty)
  assertEqual "constrained splice kind signature rendering" "_ => $a :: _" rendered
  case parseType config rendered of
    ParseOk reparsed ->
      assertEqual "reparsed constrained splice kind signature" ty (stripAnnotations reparsed)
    ParseErr bundle ->
      assertFailure ("expected parse success for constrained splice kind signature\n" <> formatParseErrorBundle "<test>" Nothing bundle)

test_signatureTypeParensKeepsNestedKindSignaturesBare :: Assertion
test_signatureTypeParensKeepsNestedKindSignaturesBare =
  assertEqual
    "nested kind signatures in tuple elements should not gain extra parens"
    (TTuple Boxed Unpromoted [TWildcard, TKindSig TWildcard (TKindSig TWildcard TWildcard)])
    ( addSignatureTypeParens
        (TTuple Boxed Unpromoted [TWildcard, TKindSig TWildcard (TKindSig TWildcard TWildcard)])
    )

test_shrunkDoExpressionsKeepFinalExpression :: Assertion
test_shrunkDoExpressionsKeepFinalExpression = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        do
          (([] :: _) -> _) <- []
          []
        """
  case parseExpr config source of
    ParseOk expr ->
      let invalidShrinks = [stmts | EDo stmts _ <- shrinkExpr expr, not (testDoStmtsEndInExpr stmts)]
       in assertEqual "invalid do-statement shrinks" [] invalidShrinks
    ParseErr bundle ->
      assertFailure ("expected parse success for " <> T.unpack source <> "\n" <> formatParseErrors "<test>" Nothing bundle)

test_transformListCompGroupByInfixRhsParens :: Assertion
test_transformListCompGroupByInfixRhsParens = do
  let config = defaultConfig {parserExtensions = [TransformListComp]}
      source = "_ = [[] | then group by ([] `a` - []) using []]"
  assertParsedStrippedDeclShapeRoundTrip config source

testDoStmtsEndInExpr :: [DoStmt Expr] -> Bool
testDoStmtsEndInExpr stmts =
  case reverse stmts of
    stmt : _ -> testIsDoExprStmt stmt
    [] -> False

testIsDoExprStmt :: DoStmt Expr -> Bool
testIsDoExprStmt stmt =
  case peelDoStmtAnn stmt of
    DoExpr {} -> True
    _ -> False

test_arrowCommandLhsLambdaCaseParens :: Assertion
test_arrowCommandLhsLambdaCaseParens = do
  let config = defaultConfig {parserExtensions = [Arrows, BlockArguments, LambdaCase]}
      source =
        """
        0 =
          proc a -> ([]
            \\case
              C ->
                []) -< ()
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_arrowCommandLhsMdoParens :: Assertion
test_arrowCommandLhsMdoParens = do
  let config = defaultConfig {parserExtensions = requiredExtensions}
      source =
        """
        (:+) =
          proc a -> (()
            + mdo
                "nerEAot"#) -< ()
        """
  assertParsedStrippedDeclShapeRoundTrip config source

test_arrowCommandLhsLambdaCasesParens :: Assertion
test_arrowCommandLhsLambdaCasesParens = do
  let config = defaultConfig {parserExtensions = [Arrows, BlockArguments, LambdaCase]}
      source =
        """
        0 =
          proc a -> ([]
            \\cases
              C ->
                []) -< ()
        """
  assertParsedStrippedDeclShapeRoundTrip config source

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
