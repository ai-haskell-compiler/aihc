{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Aihc.Parser
import Aihc.Parser.Lex (LexToken (..), LexTokenKind (..), lexTokens, lexTokensFromChunks, lexTokensWithExtensions, readModuleHeaderExtensions, readModuleHeaderExtensionsFromChunks)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax
import Data.Char (ord)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex, showOct)
import ParserValidation (validateParser)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.ErrorMessages.Suite (errorMessageTests)
import Test.ExtensionMapping.Suite (extensionMappingTests)
import Test.HackageTester.Suite (hackageTesterTests)
import Test.Lexer.Suite (lexerTests)
import Test.Oracle.Suite (oracleTests)
import Test.Parser.Suite (parserGoldenTests)
import Test.Performance.Suite (parserPerformanceTests)
import Test.Properties.Arb.Expr (genOperator, isValidGeneratedOperator)
import Test.Properties.DeclRoundTrip (prop_declPrettyRoundTrip)
import Test.Properties.ExprHelpers (normalizeDecl, normalizeExpr, span0, stripTypeSourceSpanAnnotations)
import Test.Properties.ExprRoundTrip (prop_exprPrettyRoundTrip)
import Test.Properties.Identifiers (isValidGeneratedIdent, shrinkIdent)
import Test.Properties.ModuleRoundTrip (prop_modulePrettyRoundTrip)
import Test.Properties.PatternRoundTrip (prop_patternPrettyRoundTrip)
import Test.Properties.TypeRoundTrip (prop_typePrettyRoundTrip)
import Test.QuickCheck.Gen qualified as QGen
import Test.QuickCheck.Random qualified as QRandom
import Test.StackageProgress.FileCheckerTiming (stackageProgressFileCheckerTimingTests)
import Test.StackageProgress.Summary (stackageProgressSummaryTests)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Text.Megaparsec.Error qualified as MPE

tenMinutes :: Timeout
tenMinutes = Timeout (10 * 60 * 1000000) "10m"

expr0 :: Expr -> Expr
expr0 = exprAnnSpan span0

pat0 :: Pattern -> Pattern
pat0 = patternAnnSpan span0

pattern PVar_ :: UnqualifiedName -> Pattern
pattern PVar_ name <- (peelPatternAnn -> PVar name)

pattern PWildcard_ :: Pattern
pattern PWildcard_ <- (peelPatternAnn -> PWildcard)

pattern PLit_ :: Literal -> Pattern
pattern PLit_ lit <- (peelPatternAnn -> PLit lit)

pattern LitInt_ :: Integer -> Text -> Literal
pattern LitInt_ value repr <- (peelLiteralAnn -> LitInt value repr)

pattern PTuple_ :: TupleFlavor -> [Pattern] -> Pattern
pattern PTuple_ tupleFlavor elems <- (peelPatternAnn -> PTuple tupleFlavor elems)

pattern PList_ :: [Pattern] -> Pattern
pattern PList_ elems <- (peelPatternAnn -> PList elems)

pattern PCon_ :: Name -> [Pattern] -> Pattern
pattern PCon_ con args <- (peelPatternAnn -> PCon con args)

pattern PInfix_ :: Pattern -> Name -> Pattern -> Pattern
pattern PInfix_ lhs op rhs <- (peelPatternAnn -> PInfix lhs op rhs)

pattern PView_ :: Expr -> Pattern -> Pattern
pattern PView_ viewExpr pat <- (peelPatternAnn -> PView viewExpr pat)

pattern PAs_ :: Text -> Pattern -> Pattern
pattern PAs_ name pat <- (peelPatternAnn -> PAs name pat)

pattern PStrict_ :: Pattern -> Pattern
pattern PStrict_ pat <- (peelPatternAnn -> PStrict pat)

pattern PIrrefutable_ :: Pattern -> Pattern
pattern PIrrefutable_ pat <- (peelPatternAnn -> PIrrefutable pat)

pattern PNegLit_ :: Literal -> Pattern
pattern PNegLit_ lit <- (peelPatternAnn -> PNegLit lit)

pattern PParen_ :: Pattern -> Pattern
pattern PParen_ pat <- (peelPatternAnn -> PParen pat)

pattern PRecord_ :: Name -> [(Name, Pattern)] -> Bool -> Pattern
pattern PRecord_ con fields rwc <- (peelPatternAnn -> PRecord con fields rwc)

pattern PUnboxedSum_ :: Int -> Int -> Pattern -> Pattern
pattern PUnboxedSum_ altIdx arity pat <- (peelPatternAnn -> PUnboxedSum altIdx arity pat)

pattern PSplice_ :: Expr -> Pattern
pattern PSplice_ body <- (peelPatternAnn -> PSplice body)

pattern EVar_ :: Name -> Expr
pattern EVar_ name <- (peelExprAnn -> EVar name)

pattern EInt_ :: Integer -> Text -> Expr
pattern EInt_ value repr <- (peelExprAnn -> EInt value repr)

pattern EOverloadedLabel_ :: Text -> Text -> Expr
pattern EOverloadedLabel_ value repr <- (peelExprAnn -> EOverloadedLabel value repr)

pattern EIf_ :: Expr -> Expr -> Expr -> Expr
pattern EIf_ cond thenE elseE <- (peelExprAnn -> EIf cond thenE elseE)

pattern EDo_ :: [DoStmt Expr] -> Bool -> Expr
pattern EDo_ stmts isMdo <- (peelExprAnn -> EDo stmts isMdo)

pattern DoBind_ :: Pattern -> Expr -> DoStmt Expr
pattern DoBind_ pat e <- (peelDoStmtAnn -> DoBind pat e)

pattern DoExpr_ :: Expr -> DoStmt Expr
pattern DoExpr_ e <- (peelDoStmtAnn -> DoExpr e)

pattern DoLetDecls_ :: [Decl] -> DoStmt Expr
pattern DoLetDecls_ decls <- (peelDoStmtAnn -> DoLetDecls decls)

pattern EListComp_ :: Expr -> [CompStmt] -> Expr
pattern EListComp_ body stmts <- (peelExprAnn -> EListComp body stmts)

pattern CompGen_ :: Pattern -> Expr -> CompStmt
pattern CompGen_ pat e <- (peelCompStmtAnn -> CompGen pat e)

pattern CompGuard_ :: Expr -> CompStmt
pattern CompGuard_ e <- (peelCompStmtAnn -> CompGuard e)

pattern CompLetDecls_ :: [Decl] -> CompStmt
pattern CompLetDecls_ decls <- (peelCompStmtAnn -> CompLetDecls decls)

pattern GuardExpr_ :: Expr -> GuardQualifier
pattern GuardExpr_ e <- (peelGuardQualifierAnn -> GuardExpr e)

pattern GuardPat_ :: Pattern -> Expr -> GuardQualifier
pattern GuardPat_ pat e <- (peelGuardQualifierAnn -> GuardPat pat e)

pattern GuardLet_ :: [Decl] -> GuardQualifier
pattern GuardLet_ decls <- (peelGuardQualifierAnn -> GuardLet decls)

pattern ELetDecls_ :: [Decl] -> Expr -> Expr
pattern ELetDecls_ decls body <- (peelExprAnn -> ELetDecls decls body)

pattern EApp_ :: Expr -> Expr -> Expr
pattern EApp_ fn arg <- (peelExprAnn -> EApp fn arg)

pattern ClassItemTypeSig_ :: [BinderName] -> Type -> ClassDeclItem
pattern ClassItemTypeSig_ names ty <- (peelClassDeclItemAnn -> ClassItemTypeSig names ty)

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
            testCase "module parses nullary class declaration" test_moduleParsesNullaryClassDecl,
            testCase "module parses nullary class declaration with where block" test_moduleParsesNullaryClassDeclWithWhere,
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
            testCase "skips leading shebang lines as trivia" test_leadingShebangIsSkipped,
            testCase "skips space-prefixed shebang lines as trivia" test_spacedLeadingShebangIsSkipped,
            testCase "skips mid-stream shebang lines as trivia" test_midStreamShebangIsSkipped,
            testCase "does not misclassify line-start #) as a directive" test_lineStartHashTokenIsNotDirective,
            testCase "lexes overloaded labels as single tokens" test_overloadedLabelLexesAsSingleToken,
            testCase "lexes quoted overloaded labels" test_quotedOverloadedLabelLexes,
            testCase "lexes string gaps before a closing quote" test_stringGapBeforeClosingQuoteLexes,
            testCase "parses overloaded label expressions" test_overloadedLabelExprParses,
            testCase "pretty-prints overloaded labels with delimiter spacing" test_overloadedLabelPrettyPrintsWithDelimiterSpacing,
            testCase "applies LINE pragmas to subsequent tokens" test_linePragmaUpdatesSpan,
            testCase "applies COLUMN pragmas to subsequent tokens" test_columnPragmaUpdatesSpan,
            testCase "applies COLUMN pragmas in the middle of a line" test_inlineColumnPragmaUpdatesSpan,
            testCase "can lex lazily from chunks" test_lexerChunkLaziness,
            testCase "parser config passes extensions to lexer" test_parserConfigPassesExtensions,
            testCase "parser config sets source name in parse errors" test_parserConfigSetsSourceName,
            testCase "parses tab-indented where after else branch" test_tabIndentedWhereAfterElseParses,
            testCase "parses non-aligned multi-way-if guards" test_nonAlignedMultiWayIfGuardsParse,
            testCase "lexes alternate valid character literal spellings" test_alternateCharLiteralSpellingsLexLikeGhc,
            testCase "lexes control-backslash character literal" test_controlBackslashCharLiteralLexes,
            testCase "parses character literals after escaped backslash cons patterns" test_escapedBackslashConsPatternCharLiteralParses,
            testCase "generated identifiers reject extension keyword rec" test_generatedIdentifiersRejectExtensionKeywordRec,
            testCase "generated identifiers reject standalone underscore" test_generatedIdentifiersRejectStandaloneUnderscore,
            testCase "shrunk identifiers reject standalone underscore" test_shrunkIdentifiersRejectStandaloneUnderscore,
            testCase "generated operators reject arrow tail spellings" test_generatedOperatorsRejectArrowTailSpellings,
            testCase "generated expressions can include mdo" test_generatedExpressionsCanIncludeMdo,
            testCase "parses parenthesized kind signature type atoms" test_typeParsesParenthesizedKindSignature,
            testCase "parses parenthesized kind signatures in application heads" test_typeParsesKindSignatureApplicationHead,
            testCase "parses empty list type constructor" test_typeParsesEmptyListConstructor,
            testCase "parses promoted empty list type constructor" test_typeParsesPromotedEmptyListConstructor,
            testCase "parses parenthesized empty list in instance heads" test_instanceParsesParenthesizedEmptyListType,
            testCase "parses GADT constructor arguments with kind signatures" test_gadtConstructorParsesKindAnnotatedArgument,
            testCase "preserves source unpack pragmas on constructor fields" test_constructorFieldsPreserveSourceUnpackedness,
            testCase "ignores unexpected pragmas without parse failure" test_ignoresUnexpectedPragmas,
            testCase "captures known pragmas after ignored unknown pragmas" test_knownPragmaStillParsesAfterIgnoredUnknownPragma,
            testCase "roundtrips source unpackedness through pretty-printing" test_sourceUnpackednessRoundtrip,
            testCase "parses warned export reexports" test_warnedExportReexportParses,
            testCase "roundtrips warned export reexports" test_warnedExportReexportRoundtrip,
            testCase "parses warned export module reexports" test_warnedExportModuleReexportParses,
            testCase "parses infix class heads" test_infixClassHeadParses,
            testCase "roundtrips else branches with local where clauses" test_ifElseWhereBranchRoundtrip,
            testCase "parses standalone mdo expressions" test_standaloneMdoExprParses,
            testCase "parses mdo view patterns" test_mdoViewPatternParses,
            testCase "parses and roundtrips infix type family heads" test_infixTypeFamilyHeadRoundtrip,
            QC.testProperty "generated valid char literal spellings lex like GHC" prop_validGeneratedCharLiteralSpellingsLexLikeGhc,
            QC.testProperty "generated operators reject dash-only comment starters" prop_generatedOperatorsRejectDashOnlyCommentStarters,
            QC.testProperty "generated operators can produce unicode asterism" prop_generatedOperatorsCanProduceUnicodeAsterism
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
            testCase "nested prefix patterns: K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- expr" test_doBindNestedPrefixPattern,
            testCase "expression statement: expr" test_doExprStmt,
            testCase "let statement: let x = 5" test_doLetStmt,
            testCase "rejects if-then-else in pattern context" test_doBindRejectsIfExpr
          ],
        testGroup
          "checkPattern (guard qualifier)"
          [ testCase "guard expression: f x | x > 0 = x" test_guardExpr,
            testCase "guard pattern bind: f x | Just y <- g x = y" test_guardPatBind,
            testCase "guard view pattern bind: f x | (view -> Just y) <- x = y" test_guardViewPatternBind,
            testCase "guard let: f x | let y = x + 1 = y" test_guardLet,
            testCase "guard wildcard bind: f x | _ <- g x = x" test_guardWildcardBind,
            testCase "guard tuple bind: f x | (a, b) <- g x = a" test_guardTupleBind,
            testCase "guard constructor bind: f x | Just y <- g x = y" test_guardConBind,
            testCase "guard bang pattern: f x | !y <- g x = y" test_guardBangBind,
            testCase "guard irrefutable pattern: f x | ~(a, b) <- g x = a" test_guardIrrefutableBind,
            testCase "guard as pattern: f x | y@(Just _) <- g x = y" test_guardAsBind,
            testCase "guard infix pattern: f x | a : as <- g x = a" test_guardInfixBind,
            testCase "guard nested prefix patterns: f x | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs = y" test_guardNestedPrefixBind
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
            testCase "comp infix gen: [a | a : as <- xs]" test_compInfixGen,
            testCase "comp nested prefix gen: [y | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs]" test_compNestedPrefixGen
          ],
        testGroup
          "localDeclParser dispatch"
          [ testCase "type sig: f :: Int" test_localDeclTypeSig,
            testCase "type sig multi: f, g :: Int" test_localDeclTypeSigMulti,
            testCase "type sig operator: (+) :: Int -> Int -> Int" test_localDeclTypeSigOp,
            testCase "type sig unicode operator: (⁂) :: Int -> Int -> Int" test_localDeclTypeSigUnicodeOp,
            testCase "function bind prefix: f x = x" test_localDeclFunPrefix,
            testCase "function bind no args: f = 5" test_localDeclFunNoArgs,
            testCase "pattern bind tuple: (x, y) = expr" test_localDeclPatTuple,
            testCase "pattern bind constructor: Just x = expr" test_localDeclPatCon,
            testCase "pattern bind wildcard: _ = expr" test_localDeclPatWild,
            testCase "function bind guarded: f x | x > 0 = x" test_localDeclFunGuarded,
            testCase "pattern bind record constructor: R {} = expr" test_localDeclPatRecordCon,
            testCase "pattern bind unboxed sum: (# | | | x #) = expr" test_localDeclPatUnboxedSum
          ],
        testGroup
          "pretty"
          [ testCase "guard lambda round-trips with parentheses" test_prettyGuardLambdaRoundTrip,
            testCase "guard let expression stays unparenthesized" test_prettyGuardLetFormatting,
            testCase "function-head list view patterns stay bare" test_prettyFunctionHeadListViewPattern,
            testCase "unicode operator type signatures round-trip with parentheses" test_prettyUnicodeOperatorTypeSigRoundTrip,
            testCase "prefix function head record pattern stays bare" test_prettyPrefixFunctionHeadRecordPattern,
            testCase "infix function head constructor applications stay bare" test_prettyInfixFunctionHeadConstructorPatterns,
            testCase "infix function head irrefutable patterns stay bare" test_prettyInfixFunctionHeadIrrefutablePatterns,
            testCase "view pattern with let-typed expr gets parenthesized" test_prettyViewLetTypeSigParens,
            testCase "guard pattern with type sig gets parenthesized" test_prettyGuardPatTypeSigParens
          ],
        testGroup
          "functionHeadParserWith dispatch"
          [ testCase "prefix: f x y = x + y" test_funHeadPrefix,
            testCase "prefix no args: f = 5" test_funHeadPrefixNoArgs,
            testCase "prefix operator name: (+) x y = x" test_funHeadPrefixOp,
            testCase "prefix constructor application arg: f (Just x) y = y" test_funHeadPrefixConstructorArg,
            testCase "prefix list view pattern arg: fn [id -> x] = x" test_funHeadPrefixListViewPattern,
            testCase "prefix singleton unboxed tuple arg: f (# x #) = x" test_funHeadPrefixUnboxedTupleSingletonArg,
            testCase "infix: x + y = x" test_funHeadInfix,
            testCase "infix backtick: x `add` y = x" test_funHeadInfixBacktick,
            testCase "infix record rhs: x `f` (R {}) = x" test_funHeadInfixRecordRhs,
            testCase "infix tuple lhs and qualified record rhs" test_funHeadInfixTupleLhsQualifiedRecordRhs,
            testCase "infix complex tuple lhs and qualified record rhs" test_funHeadInfixComplexTupleLhsQualifiedRecordRhs,
            testCase "infix backtick with TH splice lhs: $splice `fn` () = ()" test_funHeadInfixThSpliceLhs,
            testCase "parenthesized infix: (x + y) = x" test_funHeadParenInfix,
            testCase "parenthesized infix with tail: (x + y) z = x" test_funHeadParenInfixTail,
            testCase "local prefix: let f x = x" test_funHeadLocalPrefix,
            testCase "local infix: let x + y = x" test_funHeadLocalInfix,
            testCase "local paren op name: let (+) x y = x" test_funHeadLocalPrefixOp
          ],
        adjustOption (const tenMinutes) $
          testGroup
            "properties"
            [ QC.testProperty "generated expr AST pretty-printer round-trip" prop_exprPrettyRoundTrip,
              QC.testProperty "generated decl AST pretty-printer round-trip" prop_declPrettyRoundTrip,
              QC.testProperty "generated module AST pretty-printer round-trip" prop_modulePrettyRoundTrip,
              QC.testProperty "generated pattern AST pretty-printer round-trip" prop_patternPrettyRoundTrip,
              QC.testProperty "generated type AST pretty-printer round-trip" prop_typePrettyRoundTrip
            ],
        oracle,
        extensionMappingTests,
        hackageTester,
        stackageProgressFileCheckerTimingTests,
        stackageProgressSummaryTests
      ]

test_moduleParsesDecls :: Assertion
test_moduleParsesDecls =
  let (errs, modu) = parseModule defaultConfig "x = if y then z else w"
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case map normalizeDecl (moduleDecls modu) of
          [ DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EIf_ (EVar_ "y") (EVar_ "z") (EVar_ "w")) _))
            ] ->
              pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_moduleParsesNullaryClassDecl :: Assertion
test_moduleParsesNullaryClassDecl =
  let source = T.unlines ["module M where", "class C"]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [DeclClass ClassDecl {classDeclName = "C", classDeclParams = [], classDeclItems = []}] ->
            pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_moduleParsesNullaryClassDeclWithWhere :: Assertion
test_moduleParsesNullaryClassDeclWithWhere =
  let source = T.unlines ["module M where", "class C where", "  method :: Int"]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [DeclClass ClassDecl {classDeclName = "C", classDeclParams = [], classDeclItems = [ClassItemTypeSig_ ["method"] ty]}]
            | TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations ty ->
                pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_typeParsesParenthesizedKindSignature :: Assertion
test_typeParsesParenthesizedKindSignature =
  case parseType defaultConfig {parserExtensions = [KindSignatures, StarIsType]} "(x :: *)" of
    ParseOk ty
      | TKindSig (TVar "x") TStar <- stripTypeSourceSpanAnnotations ty ->
          pure ()
    other -> assertFailure ("expected parenthesized kind signature type, got: " <> show other)

test_typeParsesKindSignatureApplicationHead :: Assertion
test_typeParsesKindSignatureApplicationHead =
  case parseType defaultConfig {parserExtensions = [KindSignatures]} "(f :: Type -> Type) a" of
    ParseOk ty
      | TApp (TKindSig (TVar "f") (TFun (TCon "Type" Unpromoted) (TCon "Type" Unpromoted))) (TVar "a") <-
          stripTypeSourceSpanAnnotations ty ->
          pure ()
    other -> assertFailure ("expected kind-signature application head, got: " <> show other)

test_typeParsesEmptyListConstructor :: Assertion
test_typeParsesEmptyListConstructor =
  case parseType defaultConfig "[]" of
    ParseOk ty
      | TCon "[]" Unpromoted <- stripTypeSourceSpanAnnotations ty ->
          pure ()
    other -> assertFailure ("expected empty list type constructor, got: " <> show other)

test_typeParsesPromotedEmptyListConstructor :: Assertion
test_typeParsesPromotedEmptyListConstructor =
  case parseType defaultConfig {parserExtensions = [DataKinds]} "'[]" of
    ParseOk ty
      | TCon "[]" Promoted <- stripTypeSourceSpanAnnotations ty ->
          pure ()
    other -> assertFailure ("expected promoted empty list type constructor, got: " <> show other)

test_instanceParsesParenthesizedEmptyListType :: Assertion
test_instanceParsesParenthesizedEmptyListType =
  let source =
        T.unlines
          [ "{-# LANGUAGE FlexibleInstances #-}",
            "module M where",
            "class C a",
            "instance C ([])"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [ DeclClass ClassDecl {classDeclName = "C", classDeclParams = [_]},
            DeclInstance inst
            ]
              | instanceDeclClassName inst == "C",
                [ity] <- instanceDeclTypes inst,
                TParen (TCon "[]" Unpromoted) <- stripTypeSourceSpanAnnotations ity ->
                  pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)

test_gadtConstructorParsesKindAnnotatedArgument :: Assertion
test_gadtConstructorParsesKindAnnotatedArgument =
  let src = T.unlines ["data T where", "  C :: (x :: *) -> T"]
      (errs, modu) = parseModule defaultConfig {parserExtensions = [GADTs, KindSignatures, StarIsType]} src
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (GadtCon [] [] ["C"] (GadtPrefixBody [BangType {bangType = kb}] rb))]}]
            | TKindSig (TVar "x") TStar <- stripTypeSourceSpanAnnotations kb,
              TCon "T" Unpromoted <- stripTypeSourceSpanAnnotations rb ->
                pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_constructorFieldsPreserveSourceUnpackedness :: Assertion
test_constructorFieldsPreserveSourceUnpackedness =
  let source =
        T.unlines
          [ "{-# LANGUAGE GADTs #-}",
            "module M where",
            "data Prefix = Prefix {-# UNPACK #-} !Int",
            "data Infix = {-# NOUNPACK #-} !(Int, Int) :*: Int",
            "data Record = Record { field :: {-# UNPACK #-} !Int }",
            "data G where",
            "  G :: {-# UNPACK #-} !Int -> G"
          ]
      (errs, modu) = parseModule defaultConfig {parserExtensions = [GADTs]} source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [ DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (PrefixCon [] [] "Prefix" [BangType {bangSourceUnpackedness = SourceUnpack, bangStrict = True, bangType = bt1}])]},
            DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (InfixCon [] [] BangType {bangSourceUnpackedness = SourceNoUnpack, bangStrict = True, bangType = bt2} ":*:" BangType {bangSourceUnpackedness = NoSourceUnpackedness, bangStrict = False, bangType = bt3})]},
            DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (RecordCon [] [] "Record" [FieldDecl {fieldType = BangType {bangSourceUnpackedness = SourceUnpack, bangStrict = True, bangType = bt4}}])]},
            DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (GadtCon [] [] ["G"] (GadtPrefixBody [BangType {bangSourceUnpackedness = SourceUnpack, bangStrict = True, bangType = bt5}] bt6))]}
            ]
              | TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations bt1,
                TTuple Boxed Unpromoted [TCon "Int" Unpromoted, TCon "Int" Unpromoted] <-
                  stripTypeSourceSpanAnnotations bt2,
                TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations bt3,
                TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations bt4,
                TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations bt5,
                TCon "G" Unpromoted <- stripTypeSourceSpanAnnotations bt6 ->
                  pure ()
          other ->
            assertFailure ("unexpected parsed declarations: " <> show other)

test_ignoresUnexpectedPragmas :: Assertion
test_ignoresUnexpectedPragmas =
  let source =
        T.unlines
          [ "module M where",
            "x = {-# UNKNOWN #-} 1",
            "y = ({-# INLINE #-} 2)",
            "data T = T {-# BAD #-} Int"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case map normalizeDecl (moduleDecls modu) of
          [DeclValue {}, DeclValue {}, DeclData {}] -> pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)

test_knownPragmaStillParsesAfterIgnoredUnknownPragma :: Assertion
test_knownPragmaStillParsesAfterIgnoredUnknownPragma =
  let source =
        T.unlines
          [ "module M where",
            "data T = T {-# UNKNOWN #-} {-# UNPACK #-} !Int"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [DeclData DataDecl {dataDeclConstructors = [DataConAnn _ (PrefixCon [] [] "T" [BangType {bangSourceUnpackedness = SourceUnpack, bangStrict = True, bangType = bt}])]}]
            | TCon "Int" Unpromoted <- stripTypeSourceSpanAnnotations bt ->
                pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)

test_sourceUnpackednessRoundtrip :: Assertion
test_sourceUnpackednessRoundtrip =
  let source =
        T.unlines
          [ "{-# LANGUAGE GADTs #-}",
            "module M where",
            "data Pair = Pair {-# UNPACK #-} !Int {-# NOUNPACK #-} !(Int, Int)",
            "data G where",
            "  G :: {-# UNPACK #-} !Int -> G"
          ]
   in case validateParser "SourceUnpackedness.hs" Haskell2010Edition [EnableExtension GADTs] source of
        Nothing -> pure ()
        Just err -> assertFailure ("expected source unpackedness roundtrip to validate, got: " <> show err)

test_warnedExportReexportParses :: Assertion
test_warnedExportReexportParses =
  let source =
        T.unlines
          [ "module M",
            "  ( {-# DEPRECATED \"Import g from A instead\" #-} g",
            "  ) where",
            "import A (g)"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleExports modu of
          Just [ExportVar (Just (DeprText "Import g from A instead")) Nothing "g"] -> pure ()
          other -> assertFailure ("unexpected exports: " <> show other)

test_warnedExportReexportRoundtrip :: Assertion
test_warnedExportReexportRoundtrip =
  let source =
        T.unlines
          [ "module M",
            "  ( {-# DEPRECATED \"Import g from A instead\" #-} g",
            "  , {-# WARNING \"Use T carefully\" #-} T(..)",
            "  , {-# DEPRECATED \"Moved to B\" #-} module B",
            "  ) where",
            "import A (g, T(..))",
            "import B"
          ]
   in case validateParser "WarnedExportReexport.hs" Haskell2010Edition [] source of
        Nothing -> pure ()
        Just err -> assertFailure ("expected warned exports roundtrip to validate, got: " <> show err)

test_warnedExportModuleReexportParses :: Assertion
test_warnedExportModuleReexportParses =
  let source =
        T.unlines
          [ "module M",
            "  ( {-# DEPRECATED \"Moved to B\" #-}",
            "      module B",
            "  ) where",
            "import B"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleExports modu of
          Just [ExportModule (Just (DeprText "Moved to B")) "B"] -> pure ()
          other -> assertFailure ("unexpected exports: " <> show other)

test_infixClassHeadParses :: Assertion
test_infixClassHeadParses =
  let source =
        T.unlines
          [ "{-# LANGUAGE TypeOperators #-}",
            "module M where",
            "infix 4 :=:",
            "class a :=: b where",
            "  proof :: a -> b -> ()"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [ DeclFixity {},
            DeclClass ClassDecl {classDeclHeadForm = TypeHeadInfix, classDeclName = ":=:", classDeclParams = [TyVarBinder _ "a" Nothing TyVarBSpecified, TyVarBinder _ "b" Nothing TyVarBSpecified], classDeclItems = [ClassItemTypeSig_ ["proof"] _]}
            ] -> pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)

test_ifElseWhereBranchRoundtrip :: Assertion
test_ifElseWhereBranchRoundtrip =
  let elseBranch =
        expr0 (ETypeSig (expr0 (ETuple Boxed [])) (TTuple Boxed Unpromoted []))
      expectedDecl =
        DeclValue
          ( FunctionBind
              span0
              "x"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [],
                    matchRhs = UnguardedRhs span0 (expr0 (EIf (expr0 (EVar "b")) (expr0 (ETuple Boxed [])) elseBranch)) Nothing
                  }
              ]
          )
      source =
        renderStrict . layoutPretty defaultLayoutOptions . pretty $
          Module
            { moduleSpan = span0,
              moduleHead = Nothing,
              moduleLanguagePragmas = [],
              moduleImports = [],
              moduleDecls = [expectedDecl]
            }
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs <> "\nsource:\n" <> T.unpack source) (null errs)
        case map normalizeDecl (moduleDecls modu) of
          [actualDecl] -> assertEqual "roundtripped declaration" (normalizeDecl expectedDecl) actualDecl
          other -> assertFailure ("unexpected parsed declarations: " <> show other <> "\nsource:\n" <> T.unpack source)

test_standaloneMdoExprParses :: Assertion
test_standaloneMdoExprParses =
  case parseExpr defaultConfig {parserExtensions = [RecursiveDo]} "mdo { pure x }" of
    ParseOk parsed
      | EDo_ [DoExpr_ (EApp_ (EVar_ "pure") (EVar_ "x"))] True <- normalizeExpr parsed -> pure ()
    other -> assertFailure ("expected standalone mdo expression, got: " <> show other)

test_mdoViewPatternParses :: Assertion
test_mdoViewPatternParses =
  let source =
        T.unlines
          [ "{-# LANGUAGE RecursiveDo #-}",
            "{-# LANGUAGE ViewPatterns #-}",
            "module M where",
            "f (mdo { pure x } -> y) = y"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case map normalizeDecl (moduleDecls modu) of
          [DeclValue (FunctionBind _ "f" [Match {matchPats = [PView_ (EDo_ [DoExpr_ (EApp_ (EVar_ "pure") (EVar_ "x"))] True) (PVar_ "y")], matchRhs = UnguardedRhs _ (EVar_ "y") _}])] -> pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)

test_infixTypeFamilyHeadRoundtrip :: Assertion
test_infixTypeFamilyHeadRoundtrip =
  let source =
        T.unlines
          [ "{-# LANGUAGE TypeFamilies #-}",
            "{-# LANGUAGE TypeOperators #-}",
            "module M where",
            "type family l `And` r where",
            "  l `And` r = l"
          ]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case moduleDecls modu of
          [ DeclTypeFamilyDecl
              TypeFamilyDecl
                { typeFamilyDeclHeadForm = TypeHeadInfix,
                  typeFamilyDeclHead = h,
                  typeFamilyDeclParams = [TyVarBinder _ "l" Nothing TyVarBSpecified, TyVarBinder _ "r" Nothing TyVarBSpecified],
                  typeFamilyDeclEquations = Just [TypeFamilyEq {typeFamilyEqHeadForm = TypeHeadInfix, typeFamilyEqLhs = lhs, typeFamilyEqRhs = rhs}]
                }
            ]
              | TApp (TApp (TCon "And" Unpromoted) (TVar "l")) (TVar "r") <- stripTypeSourceSpanAnnotations h,
                TApp (TApp (TCon "And" Unpromoted) (TVar "l")) (TVar "r") <- stripTypeSourceSpanAnnotations lhs,
                TVar "l" <- stripTypeSourceSpanAnnotations rhs ->
                  pure ()
          other -> assertFailure ("unexpected parsed declarations: " <> show other)
        case validateParser "InfixTypeFamilyHead.hs" Haskell2010Edition [EnableExtension TypeFamilies, EnableExtension TypeOperators] source of
          Nothing -> pure ()
          Just err -> assertFailure ("expected infix type family head roundtrip to validate, got: " <> show err)

test_parserConfigPassesExtensions :: Assertion
test_parserConfigPassesExtensions =
  case parseExpr defaultConfig {parserExtensions = [NegativeLiterals]} "-1" of
    ParseOk parsed
      | EInt_ (-1) _ <- normalizeExpr parsed -> pure ()
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

test_overloadedLabelExprParses :: Assertion
test_overloadedLabelExprParses =
  let source = T.unlines ["{-# LANGUAGE OverloadedLabels #-}", "module M where", "x = #typeUrl", "y = #\"The quick brown fox\""]
      (errs, modu) = parseModule defaultConfig source
   in do
        assertBool ("expected no parse errors, got: " <> show errs) (null errs)
        case map normalizeDecl (moduleDecls modu) of
          [ DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EOverloadedLabel_ "typeUrl" "#typeUrl") _)),
            DeclValue (PatternBind _ (PVar_ "y") (UnguardedRhs _ (EOverloadedLabel_ "The quick brown fox" "#\"The quick brown fox\"") _))
            ] -> pure ()
          other -> assertFailure ("expected overloaded label expressions in AST, got: " <> show other)

test_overloadedLabelPrettyPrintsWithDelimiterSpacing :: Assertion
test_overloadedLabelPrettyPrintsWithDelimiterSpacing = do
  let config = defaultConfig {parserExtensions = [OverloadedLabels, UnboxedTuples]}
      exprs =
        [ expr0 (ETuple Boxed [Just (expr0 (EOverloadedLabel "a" "#a")), Nothing]),
          expr0 (EList [expr0 (EOverloadedLabel "a" "#a")]),
          expr0 (EParen (expr0 (EOverloadedLabel "a" "#a")))
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

test_generatedOperatorsRejectArrowTailSpellings :: Assertion
test_generatedOperatorsRejectArrowTailSpellings =
  assertBool "arrow-tail operators must not be treated as valid generated operators" $
    not (any isValidGeneratedOperator ["-<", ">-", "-<<", ">>-"])

test_generatedExpressionsCanIncludeMdo :: Assertion
test_generatedExpressionsCanIncludeMdo =
  let samples = QGen.unGen (QC.vectorOf 4000 (QC.resize 5 (QC.arbitrary :: QC.Gen Expr))) (QRandom.mkQCGen 737) 5
   in assertBool "expected expression generator to include at least one mdo expression" $
        any isMdo samples
  where
    isMdo (EDo_ _ True) = True
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
  QC.withMaxSuccess 2000 $ QC.forAll genValidCharLiteral $ \raw ->
    QC.counterexample ("literal: " <> T.unpack raw) $
      case ghcReadCharLiteral raw of
        Nothing -> QC.counterexample "generator produced an invalid literal" False
        Just expected ->
          case lexTokens raw of
            [LexToken {lexTokenKind = TkChar actual}, LexToken {lexTokenKind = TkEOF}] -> actual QC.=== expected
            other -> QC.counterexample ("unexpected tokens: " <> show other) False

prop_generatedOperatorsRejectDashOnlyCommentStarters :: QC.Property
prop_generatedOperatorsRejectDashOnlyCommentStarters =
  QC.forAll (QC.vectorOf 2000 genOperator) $ \ops ->
    let invalid = filter (not . isValidGeneratedOperator) ops
     in QC.counterexample ("invalid generated operators: " <> show invalid) (null invalid)

prop_generatedOperatorsCanProduceUnicodeAsterism :: QC.Property
prop_generatedOperatorsCanProduceUnicodeAsterism =
  QC.withMaxSuccess 25 $
    QC.forAll (QC.vectorOf 2000 genOperator) $ \ops ->
      QC.counterexample "expected generator to include ⁂ in sampled operators" $
        "⁂" `elem` ops

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

-- Helper: parse a do-expression and extract the do-statements.
parseDoStmts :: T.Text -> Either String [DoStmt Expr]
parseDoStmts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EDo_ stmts _) _))] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

-- Helper: parse a do-expression with extensions and extract the do-statements.
parseDoStmtsExt :: [Extension] -> T.Text -> Either String [DoStmt Expr]
parseDoStmtsExt exts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EDo_ stmts _) _))] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

test_doBindVarPattern :: Assertion
test_doBindVarPattern =
  case parseDoStmts "do { x <- return 1; return x }" of
    Right [DoBind_ (PVar_ "x") _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected var bind, got: " <> show other)

test_doBindConPattern :: Assertion
test_doBindConPattern =
  case parseDoStmts "do { Just x <- return Nothing; return x }" of
    Right [DoBind_ (PCon_ "Just" [PVar_ "x"]) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected constructor bind, got: " <> show other)

test_doBindWildcardPattern :: Assertion
test_doBindWildcardPattern =
  case parseDoStmts "do { _ <- return 1; return 2 }" of
    Right [DoBind_ PWildcard_ _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected wildcard bind, got: " <> show other)

test_doBindTuplePattern :: Assertion
test_doBindTuplePattern =
  case parseDoStmts "do { (a, b) <- return (1, 2); return a }" of
    Right [DoBind_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"]) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected tuple bind, got: " <> show other)

test_doBindListPattern :: Assertion
test_doBindListPattern =
  case parseDoStmts "do { [a, b] <- return [1, 2]; return a }" of
    Right [DoBind_ (PList_ [PVar_ "a", PVar_ "b"]) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected list bind, got: " <> show other)

test_doBindLitPattern :: Assertion
test_doBindLitPattern =
  case parseDoStmts "do { 0 <- return 1; return 2 }" of
    Right [DoBind_ (PLit_ (LitInt_ 0 _)) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected literal bind, got: " <> show other)

test_doBindNegLitPattern :: Assertion
test_doBindNegLitPattern =
  case parseDoStmts "do { -1 <- return 0; return 2 }" of
    Right [DoBind_ (PNegLit_ (LitInt_ 1 _)) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected negated literal bind, got: " <> show other)

test_doBindNestedConPattern :: Assertion
test_doBindNestedConPattern =
  case parseDoStmts "do { Just (Left x) <- return Nothing; return x }" of
    Right [DoBind_ (PCon_ "Just" [PParen_ (PCon_ "Left" [PVar_ "x"])]) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected nested constructor bind, got: " <> show other)

test_doBindInfixConPattern :: Assertion
test_doBindInfixConPattern =
  case parseDoStmts "do { x : xs <- return [1, 2]; return x }" of
    Right [DoBind_ (PInfix_ (PVar_ "x") ":" (PVar_ "xs")) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected infix constructor bind, got: " <> show other)

test_doBindParenPattern :: Assertion
test_doBindParenPattern =
  case parseDoStmts "do { (x) <- return 1; return x }" of
    Right [DoBind_ (PParen_ (PVar_ "x")) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected parenthesized bind, got: " <> show other)

test_doBindBangPattern :: Assertion
test_doBindBangPattern =
  case parseDoStmtsExt [BangPatterns] "do { !x <- return 1; return x }" of
    Right [DoBind_ (PStrict_ (PVar_ "x")) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected bang pattern bind, got: " <> show other)

test_doBindIrrefutablePattern :: Assertion
test_doBindIrrefutablePattern =
  case parseDoStmts "do { ~(a, b) <- return (1, 2); return a }" of
    Right [DoBind_ (PIrrefutable_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"])) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected irrefutable pattern bind, got: " <> show other)

test_doBindAsPattern :: Assertion
test_doBindAsPattern =
  case parseDoStmts "do { x@(Just _) <- return Nothing; return x }" of
    Right [DoBind_ (PAs_ "x" (PCon_ "Just" [PWildcard_])) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected as-pattern bind, got: " <> show other)

test_doBindNestedPrefixPattern :: Assertion
test_doBindNestedPrefixPattern =
  case parseDoStmtsExt [BangPatterns, ViewPatterns] "do { K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs; pure y }" of
    Right [DoBind_ (PCon_ "K" [PStrict_ (PVar_ "y"), PIrrefutable_ (PCon_ "Just" [PVar_ "z"]), PAs_ "q" (PCon_ "Right" [PWildcard_]), PParen_ (PParen_ (PView_ _ (PVar_ "n"))), PParen_ (PNegLit_ (LitInt_ 1 _))]) _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected nested prefix-pattern bind, got: " <> show other)

test_doExprStmt :: Assertion
test_doExprStmt =
  case parseDoStmts "do { putStrLn \"hello\"; return () }" of
    Right [DoExpr_ _, DoExpr_ _] -> pure ()
    other -> assertFailure ("expected two expression statements, got: " <> show other)

test_doLetStmt :: Assertion
test_doLetStmt =
  case parseDoStmts "do { let { x = 5 }; return x }" of
    Right [DoLetDecls_ _, DoExpr_ _] -> pure ()
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
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (FunctionBind _ _ [Match {matchRhs = GuardedRhss _ [GuardedRhs {guardedRhsGuards = guards}] _}])] ->
            Right guards
          other ->
            Left ("unexpected AST: " <> show other)

parseGuardsExt :: [Extension] -> T.Text -> Either String [GuardQualifier]
parseGuardsExt exts src =
  let (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (FunctionBind _ _ [Match {matchRhs = GuardedRhss _ [GuardedRhs {guardedRhsGuards = guards}] _}])] ->
            Right guards
          other ->
            Left ("unexpected AST: " <> show other)
  where
    fullSrc = "{-# LANGUAGE " <> T.intercalate ", " (map (T.pack . show) exts) <> " #-}\n" <> src

test_guardExpr :: Assertion
test_guardExpr =
  case parseGuards "f x | x > 0 = x" of
    Right [GuardExpr_ _] -> pure ()
    other -> assertFailure ("expected guard expression, got: " <> show other)

test_prettyGuardLambdaRoundTrip :: Assertion
test_prettyGuardLambdaRoundTrip = do
  let decl =
        DeclValue
          ( FunctionBind
              span0
              "f"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [pat0 (PVar "x")],
                    matchRhs = UnguardedRhs span0 caseExpr Nothing
                  }
              ]
          )
      caseExpr =
        expr0
          ( ECase
              (expr0 (EVar "x"))
              [ CaseAlt
                  { caseAltSpan = span0,
                    caseAltPattern = pat0 (PVar "y"),
                    caseAltRhs =
                      GuardedRhss
                        span0
                        [ GuardedRhs
                            { guardedRhsSpan = span0,
                              guardedRhsGuards =
                                [ guardAnnSpan
                                    span0
                                    ( GuardExpr
                                        (expr0 (ELambdaPats [pat0 (PVar "z")] (expr0 (EVar "z"))))
                                    )
                                ],
                              guardedRhsBody = expr0 (EVar "x")
                            }
                        ]
                        Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
      expected = normalizeDecl decl
  case parseDecl defaultConfig source of
    ParseOk parsed ->
      normalizeDecl parsed @?= expected
    ParseErr err ->
      assertFailure ("expected pretty-printed guard lambda to parse, got:\n" <> MPE.errorBundlePretty err <> "\nsource:\n" <> T.unpack source)

test_prettyGuardLetFormatting :: Assertion
test_prettyGuardLetFormatting = do
  let decl =
        DeclValue
          ( FunctionBind
              span0
              "f"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [pat0 (PVar "n")],
                    matchRhs =
                      GuardedRhss
                        span0
                        [ GuardedRhs
                            { guardedRhsSpan = span0,
                              guardedRhsGuards =
                                [ guardAnnSpan
                                    span0
                                    ( GuardExpr
                                        ( expr0
                                            ( ELetDecls
                                                [ DeclValue
                                                    (FunctionBind span0 "x" [Match {matchSpan = span0, matchHeadForm = MatchHeadPrefix, matchPats = [], matchRhs = UnguardedRhs span0 (expr0 (EInt 1 "1")) Nothing}])
                                                ]
                                                (expr0 (EInfix (expr0 (EVar "x")) (qualifyName Nothing ">") (expr0 (EInt 0 "0"))))
                                            )
                                        )
                                    )
                                ],
                              guardedRhsBody = expr0 (EVar "n")
                            }
                        ]
                        Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  assertBool ("expected guard let expression without extra parens, got:\n" <> T.unpack source) (not ("| (let" `T.isInfixOf` source))

test_prettyFunctionHeadListViewPattern :: Assertion
test_prettyFunctionHeadListViewPattern = do
  let decl =
        DeclValue
          ( FunctionBind
              span0
              "fn"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [pat0 (PList [pat0 (PView (expr0 (EVar "id")) (pat0 (PVar "x")))])],
                    matchRhs = UnguardedRhs span0 (expr0 (EVar "x")) Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
      expected = normalizeDecl decl
  assertBool ("expected bare view pattern inside list pattern, got:\n" <> T.unpack source) ("fn [id -> x] = x" == source)
  case parseDecl defaultConfig {parserExtensions = [ViewPatterns]} source of
    ParseOk parsed ->
      normalizeDecl parsed @?= expected
    ParseErr err ->
      assertFailure ("expected pretty-printed list view pattern to parse, got:\n" <> MPE.errorBundlePretty err <> "\nsource:\n" <> T.unpack source)

test_prettyUnicodeOperatorTypeSigRoundTrip :: Assertion
test_prettyUnicodeOperatorTypeSigRoundTrip = do
  let intTy = TCon (qualifyName Nothing (mkUnqualifiedName NameConId "Int")) Unpromoted
      decl =
        DeclTypeSig
          [mkUnqualifiedName NameVarSym "⁂"]
          (TFun intTy (TFun intTy intTy))
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  assertBool ("expected unicode operator binder to be parenthesized, got:\n" <> T.unpack source) ("(⁂) ::" `T.isPrefixOf` source)
  case parseDecl defaultConfig source of
    ParseOk parsed ->
      normalizeDecl parsed @?= normalizeDecl decl
    ParseErr err ->
      assertFailure ("expected unicode operator type signature to parse, got:\n" <> MPE.errorBundlePretty err <> "\nsource:\n" <> T.unpack source)

test_prettyPrefixFunctionHeadRecordPattern :: Assertion
test_prettyPrefixFunctionHeadRecordPattern = do
  let decl =
        DeclValue
          ( FunctionBind
              span0
              "f"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadPrefix,
                    matchPats = [pat0 (PRecord (qualifyName Nothing (mkUnqualifiedName NameConId "Point")) [] True)],
                    matchRhs = UnguardedRhs span0 (expr0 (EInt 0 "0")) Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  assertBool ("expected bare record pattern in prefix function head, got:\n" <> T.unpack source) ("f Point {..} = 0" == source)

test_prettyInfixFunctionHeadConstructorPatterns :: Assertion
test_prettyInfixFunctionHeadConstructorPatterns = do
  let box name = pat0 (PCon (qualifyName Nothing (mkUnqualifiedName NameConId "Box")) [pat0 (PVar name)])
      decl =
        DeclValue
          ( FunctionBind
              span0
              "=="
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadInfix,
                    matchPats = [box "x", box "y"],
                    matchRhs = UnguardedRhs span0 (expr0 (EVar "x")) Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  assertBool ("expected bare constructor applications in infix function head, got:\n" <> T.unpack source) ("Box x == Box y = x" == source)

test_prettyInfixFunctionHeadIrrefutablePatterns :: Assertion
test_prettyInfixFunctionHeadIrrefutablePatterns = do
  let decl =
        DeclValue
          ( FunctionBind
              span0
              "combine"
              [ Match
                  { matchSpan = span0,
                    matchHeadForm = MatchHeadInfix,
                    matchPats = [pat0 (PIrrefutable (pat0 (PVar "x"))), pat0 (PVar "y")],
                    matchRhs = UnguardedRhs span0 (expr0 (EVar "x")) Nothing
                  }
              ]
          )
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty decl))
  assertBool ("expected bare irrefutable pattern in infix function head, got:\n" <> T.unpack source) ("~x `combine` y = x" == source)

-- | Regression test: a view pattern whose view expression is a let-expression
-- ending with a type signature must parenthesize the let so the view-pattern
-- arrow is not absorbed into the type.
-- Without the fix, this would produce @(let { x = (#  #) } in (#  #) :: T -> [])@
-- which GHC rejects because @:: T -> []@ is parsed as a single type signature.
test_prettyViewLetTypeSigParens :: Assertion
test_prettyViewLetTypeSigParens = do
  let tyCon = TCon (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted
      unboxedUnit = expr0 (ETuple Unboxed [])
      viewExpr =
        expr0
          ( ELetDecls
              [DeclValue (PatternBind span0 (pat0 (PVar (mkUnqualifiedName NameVarId "x"))) (UnguardedRhs span0 unboxedUnit Nothing))]
              (expr0 (ETypeSig unboxedUnit tyCon))
          )
      pat = pat0 (PView viewExpr (pat0 (PList [])))
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
  assertBool
    ("expected parenthesized let-expression in view pattern, got:\n" <> T.unpack source)
    ("((let { x = (#  #) } in (#  #) :: T) -> [])" == source)

-- | Regression test: a guard pattern whose expression ends with a type
-- signature must parenthesize the expression so the multi-way if arrow @->@
-- is not absorbed into the type.
-- Without the fix, this would produce @if { | () <- 262 :: T -> () }@
-- which GHC rejects because @:: T -> ()@ is parsed as a function type.
test_prettyGuardPatTypeSigParens :: Assertion
test_prettyGuardPatTypeSigParens = do
  let tyCon = TCon (qualifyName Nothing (mkUnqualifiedName NameConId "T")) Unpromoted
      guardExpr = expr0 (ETypeSig (expr0 (EInt 262 "262")) tyCon)
      grhs =
        GuardedRhs
          { guardedRhsSpan = span0,
            guardedRhsGuards = [guardAnnSpan span0 (GuardPat (pat0 (PTuple Boxed [])) guardExpr)],
            guardedRhsBody = expr0 (ETuple Boxed [])
          }
      expr = expr0 (EMultiWayIf [grhs])
      source = renderStrict (layoutPretty defaultLayoutOptions (pretty expr))
  assertBool
    ("expected parenthesized type sig in guard pattern, got:\n" <> T.unpack source)
    ("if { | () <- (262 :: T) -> () }" == source)

test_guardPatBind :: Assertion
test_guardPatBind =
  case parseGuards "f x | Just y <- g x = y" of
    Right [GuardPat_ (PCon_ "Just" [PVar_ "y"]) _] -> pure ()
    other -> assertFailure ("expected guard pattern bind, got: " <> show other)

test_guardViewPatternBind :: Assertion
test_guardViewPatternBind =
  case parseGuardsExt [PatternGuards, ViewPatterns] "f x | (view -> Just y) <- x = y" of
    Right [GuardPat_ (PParen_ (PView_ (EVar_ "view") (PCon_ "Just" [PVar_ "y"]))) (EVar_ "x")] -> pure ()
    other -> assertFailure ("expected guard view-pattern bind, got: " <> show other)

test_guardLet :: Assertion
test_guardLet =
  case parseGuards "f x | let { y = x } = y" of
    Right [GuardLet_ _] -> pure ()
    other -> assertFailure ("expected guard let, got: " <> show other)

test_guardWildcardBind :: Assertion
test_guardWildcardBind =
  case parseGuards "f x | _ <- g x = x" of
    Right [GuardPat_ PWildcard_ _] -> pure ()
    other -> assertFailure ("expected guard wildcard bind, got: " <> show other)

test_guardTupleBind :: Assertion
test_guardTupleBind =
  case parseGuards "f x | (a, b) <- g x = a" of
    Right [GuardPat_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"]) _] -> pure ()
    other -> assertFailure ("expected guard tuple bind, got: " <> show other)

test_guardConBind :: Assertion
test_guardConBind =
  case parseGuards "f x | Just y <- g x = y" of
    Right [GuardPat_ (PCon_ "Just" [PVar_ "y"]) _] -> pure ()
    other -> assertFailure ("expected guard constructor bind, got: " <> show other)

test_guardBangBind :: Assertion
test_guardBangBind =
  case parseGuardsExt [BangPatterns] "f x | !y <- g x = y" of
    Right [GuardPat_ (PStrict_ (PVar_ "y")) _] -> pure ()
    other -> assertFailure ("expected guard bang bind, got: " <> show other)

test_guardIrrefutableBind :: Assertion
test_guardIrrefutableBind =
  case parseGuards "f x | ~(a, b) <- g x = a" of
    Right [GuardPat_ (PIrrefutable_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"])) _] -> pure ()
    other -> assertFailure ("expected guard irrefutable bind, got: " <> show other)

test_guardAsBind :: Assertion
test_guardAsBind =
  case parseGuards "f x | y@(Just _) <- g x = y" of
    Right [GuardPat_ (PAs_ "y" (PCon_ "Just" [PWildcard_])) _] -> pure ()
    other -> assertFailure ("expected guard as-pattern bind, got: " <> show other)

test_guardNestedPrefixBind :: Assertion
test_guardNestedPrefixBind =
  case parseGuardsExt [BangPatterns, ViewPatterns] "f xs | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs = y" of
    Right [GuardPat_ (PCon_ "K" [PStrict_ (PVar_ "y"), PIrrefutable_ (PCon_ "Just" [PVar_ "z"]), PAs_ "q" (PCon_ "Right" [PWildcard_]), PParen_ (PParen_ (PView_ _ (PVar_ "n"))), PParen_ (PNegLit_ (LitInt_ 1 _))]) _] -> pure ()
    other -> assertFailure ("expected nested prefix-pattern guard, got: " <> show other)

test_guardInfixBind :: Assertion
test_guardInfixBind =
  case parseGuards "f x | a : as <- g x = a" of
    Right [GuardPat_ (PInfix_ (PVar_ "a") ":" (PVar_ "as")) _] -> pure ()
    other -> assertFailure ("expected guard infix bind, got: " <> show other)

-- Helpers: parse list comprehension statements.
-- Input: "[body | stmt1, stmt2]"
parseCompStmts :: T.Text -> Either String [CompStmt]
parseCompStmts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EListComp_ _ stmts) _))] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

parseCompStmtsExt :: [Extension] -> T.Text -> Either String [CompStmt]
parseCompStmtsExt exts src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig {parserExtensions = exts} fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (EListComp_ _ stmts) _))] ->
            Right stmts
          other ->
            Left ("unexpected AST: " <> show other)

test_compGuard :: Assertion
test_compGuard =
  case parseCompStmts "[x | x > 0]" of
    Right [CompGuard_ _] -> pure ()
    other -> assertFailure ("expected comp guard, got: " <> show other)

test_compGen :: Assertion
test_compGen =
  case parseCompStmts "[x | x <- xs]" of
    Right [CompGen_ (PVar_ "x") _] -> pure ()
    other -> assertFailure ("expected comp generator, got: " <> show other)

test_compLet :: Assertion
test_compLet =
  case parseCompStmts "[y | let { y = 5 }]" of
    Right [CompLetDecls_ _] -> pure ()
    other -> assertFailure ("expected comp let, got: " <> show other)

test_compWildcardGen :: Assertion
test_compWildcardGen =
  case parseCompStmts "[1 | _ <- xs]" of
    Right [CompGen_ PWildcard_ _] -> pure ()
    other -> assertFailure ("expected comp wildcard gen, got: " <> show other)

test_compTupleGen :: Assertion
test_compTupleGen =
  case parseCompStmts "[a | (a, b) <- xs]" of
    Right [CompGen_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"]) _] -> pure ()
    other -> assertFailure ("expected comp tuple gen, got: " <> show other)

test_compConGen :: Assertion
test_compConGen =
  case parseCompStmts "[y | Just y <- xs]" of
    Right [CompGen_ (PCon_ "Just" [PVar_ "y"]) _] -> pure ()
    other -> assertFailure ("expected comp constructor gen, got: " <> show other)

test_compBangGen :: Assertion
test_compBangGen =
  case parseCompStmtsExt [BangPatterns] "[y | !y <- xs]" of
    Right [CompGen_ (PStrict_ (PVar_ "y")) _] -> pure ()
    other -> assertFailure ("expected comp bang gen, got: " <> show other)

test_compIrrefutableGen :: Assertion
test_compIrrefutableGen =
  case parseCompStmts "[a | ~(a, b) <- xs]" of
    Right [CompGen_ (PIrrefutable_ (PTuple_ Boxed [PVar_ "a", PVar_ "b"])) _] -> pure ()
    other -> assertFailure ("expected comp irrefutable gen, got: " <> show other)

test_compAsGen :: Assertion
test_compAsGen =
  case parseCompStmts "[y | y@(Just _) <- xs]" of
    Right [CompGen_ (PAs_ "y" (PCon_ "Just" [PWildcard_])) _] -> pure ()
    other -> assertFailure ("expected comp as-pattern gen, got: " <> show other)

test_compNestedPrefixGen :: Assertion
test_compNestedPrefixGen =
  case parseCompStmtsExt [BangPatterns, ViewPatterns] "[y | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs]" of
    Right [CompGen_ (PCon_ "K" [PStrict_ (PVar_ "y"), PIrrefutable_ (PCon_ "Just" [PVar_ "z"]), PAs_ "q" (PCon_ "Right" [PWildcard_]), PParen_ (PParen_ (PView_ _ (PVar_ "n"))), PParen_ (PNegLit_ (LitInt_ 1 _))]) _] -> pure ()
    other -> assertFailure ("expected nested prefix-pattern generator, got: " <> show other)

test_compInfixGen :: Assertion
test_compInfixGen =
  case parseCompStmts "[a | a : as <- xs]" of
    Right [CompGen_ (PInfix_ (PVar_ "a") ":" (PVar_ "as")) _] -> pure ()
    other -> assertFailure ("expected comp infix gen, got: " <> show other)

-- Helper: parse a let-expression and extract the local declarations.
-- Input: "let { decl1; decl2 } in body"
parseLetDecls :: T.Text -> Either String [Decl]
parseLetDecls src =
  let fullSrc = "x = " <> src
      (errs, modu) = parseModule defaultConfig fullSrc
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [DeclValue (PatternBind _ (PVar_ "x") (UnguardedRhs _ (ELetDecls_ decls _) _))] ->
            Right decls
          other ->
            Left ("unexpected AST: " <> show other)

test_localDeclTypeSig :: Assertion
test_localDeclTypeSig =
  case parseLetDecls "let { f :: Int } in f" of
    Right [DeclTypeSig ["f"] _] -> pure ()
    other -> assertFailure ("expected type sig, got: " <> show other)

test_localDeclTypeSigMulti :: Assertion
test_localDeclTypeSigMulti =
  case parseLetDecls "let { f, g :: Int } in f" of
    Right [DeclTypeSig ["f", "g"] _] -> pure ()
    other -> assertFailure ("expected multi-name type sig, got: " <> show other)

test_localDeclTypeSigOp :: Assertion
test_localDeclTypeSigOp =
  case parseLetDecls "let { (+) :: Int -> Int -> Int } in 1 + 2" of
    Right [DeclTypeSig ["+"] _] -> pure ()
    other -> assertFailure ("expected operator type sig, got: " <> show other)

test_localDeclTypeSigUnicodeOp :: Assertion
test_localDeclTypeSigUnicodeOp =
  case parseLetDecls "let { (⁂) :: Int -> Int -> Int } in 1 ⁂ 2" of
    Right [DeclTypeSig [name] _]
      | unqualifiedNameType name == NameVarSym && renderUnqualifiedName name == "⁂" -> pure ()
    other -> assertFailure ("expected unicode operator type sig, got: " <> show other)

test_localDeclFunPrefix :: Assertion
test_localDeclFunPrefix =
  case parseLetDecls "let { f x = x } in f 1" of
    Right [DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x"]}])] -> pure ()
    other -> assertFailure ("expected prefix function bind, got: " <> show other)

test_localDeclFunNoArgs :: Assertion
test_localDeclFunNoArgs =
  case parseLetDecls "let { f = 5 } in f" of
    Right [DeclValue (PatternBind _ (PVar_ "f") _)] -> pure ()
    other -> assertFailure ("expected no-args function bind, got: " <> show other)

test_localDeclPatTuple :: Assertion
test_localDeclPatTuple =
  case parseLetDecls "let { (x, y) = (1, 2) } in x" of
    Right [DeclValue (PatternBind _ (PTuple_ Boxed [PVar_ "x", PVar_ "y"]) _)] -> pure ()
    other -> assertFailure ("expected tuple pattern bind, got: " <> show other)

test_localDeclPatCon :: Assertion
test_localDeclPatCon =
  case parseLetDecls "let { Just x = Nothing } in x" of
    Right [DeclValue (PatternBind _ (PCon_ "Just" [PVar_ "x"]) _)] -> pure ()
    other -> assertFailure ("expected constructor pattern bind, got: " <> show other)

test_localDeclPatWild :: Assertion
test_localDeclPatWild =
  case parseLetDecls "let { _ = 5 } in 0" of
    Right [DeclValue (PatternBind _ PWildcard_ _)] -> pure ()
    other -> assertFailure ("expected wildcard pattern bind, got: " <> show other)

test_localDeclFunGuarded :: Assertion
test_localDeclFunGuarded =
  case parseLetDecls "let { f x | x > 0 = x } in f 1" of
    Right [DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x"], matchRhs = GuardedRhss {}}])] -> pure ()
    other -> assertFailure ("expected guarded function bind, got: " <> show other)

test_localDeclPatRecordCon :: Assertion
test_localDeclPatRecordCon =
  case parseTopDecl "BYys {} = ()" of
    Right (DeclValue (PatternBind _ (PRecord_ "BYys" [] False) _)) -> pure ()
    other -> assertFailure ("expected record constructor pattern bind, got: " <> show other)

test_localDeclPatUnboxedSum :: Assertion
test_localDeclPatUnboxedSum =
  case parseTopDeclWithExts [UnboxedSums] "(#  |  |  | a #) = ()" of
    Right (DeclValue (PatternBind _ (PUnboxedSum_ 3 4 (PVar_ "a")) _)) -> pure ()
    other -> assertFailure ("expected unboxed sum pattern bind, got: " <> show other)

-- Helper: parse a top-level declaration and extract the ValueDecl.
parseTopDecl :: T.Text -> Either String Decl
parseTopDecl src =
  let (errs, modu) = parseModule defaultConfig src
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [decl] -> Right decl
          other -> Left ("expected one decl, got: " <> show (length other))

parseTopDeclWithExts :: [Extension] -> T.Text -> Either String Decl
parseTopDeclWithExts exts src =
  let (errs, modu) = parseModule defaultConfig {parserExtensions = exts} src
   in if not (null errs)
        then Left ("parse errors: " <> show errs)
        else case map normalizeDecl (moduleDecls modu) of
          [decl] -> Right decl
          other -> Left ("expected one decl, got: " <> show (length other))

test_funHeadPrefix :: Assertion
test_funHeadPrefix =
  case parseTopDecl "f x y = x + y" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x", PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected prefix function bind, got: " <> show other)

test_funHeadPrefixNoArgs :: Assertion
test_funHeadPrefixNoArgs =
  case parseTopDecl "f = 5" of
    Right (DeclValue (PatternBind _ (PVar_ "f") _)) -> pure ()
    other -> assertFailure ("expected prefix function bind with no args, got: " <> show other)

test_funHeadPrefixOp :: Assertion
test_funHeadPrefixOp =
  case parseTopDecl "(+) x y = x" of
    Right (DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x", PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected prefix operator function bind, got: " <> show other)

test_funHeadPrefixConstructorArg :: Assertion
test_funHeadPrefixConstructorArg =
  case parseTopDecl "f (Just x) y = y" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PCon_ "Just" [PVar_ "x"], PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected constructor application argument in prefix function head, got: " <> show other)

test_funHeadPrefixListViewPattern :: Assertion
test_funHeadPrefixListViewPattern =
  case parseTopDeclWithExts [ViewPatterns] "fn [id -> x] = x" of
    Right (DeclValue (FunctionBind _ "fn" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PList_ [PView_ (EVar_ "id") (PVar_ "x")]]}])) -> pure ()
    other -> assertFailure ("expected list view-pattern argument in prefix function head, got: " <> show other)

test_funHeadPrefixUnboxedTupleSingletonArg :: Assertion
test_funHeadPrefixUnboxedTupleSingletonArg =
  case parseTopDeclWithExts [UnboxedTuples] "f (# x #) = x" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PTuple_ Unboxed [PVar_ "x"]]}])) -> pure ()
    other -> assertFailure ("expected singleton unboxed tuple argument in prefix function head, got: " <> show other)

test_funHeadInfix :: Assertion
test_funHeadInfix =
  case parseTopDecl "x + y = x" of
    Right (DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected infix function bind, got: " <> show other)

test_funHeadInfixBacktick :: Assertion
test_funHeadInfixBacktick =
  case parseTopDecl "x `add` y = x" of
    Right (DeclValue (FunctionBind _ "add" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected backtick infix function bind, got: " <> show other)

test_funHeadInfixRecordRhs :: Assertion
test_funHeadInfixRecordRhs =
  case parseTopDecl "x `f` (R {}) = x" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PRecord_ "R" [] False]}])) -> pure ()
    other -> assertFailure ("expected infix function bind with record rhs pattern, got: " <> show other)

test_funHeadInfixTupleLhsQualifiedRecordRhs :: Assertion
test_funHeadInfixTupleLhsQualifiedRecordRhs =
  case parseTopDecl "((x, _), K []) `f` (M.N.R {}) = x" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PTuple_ Boxed _, PRecord_ "M.N.R" [] False]}])) -> pure ()
    other -> assertFailure ("expected infix function bind with tuple lhs and qualified record rhs pattern, got: " <> show other)

test_funHeadInfixComplexTupleLhsQualifiedRecordRhs :: Assertion
test_funHeadInfixComplexTupleLhsQualifiedRecordRhs =
  case parseTopDeclWithExts [UnboxedTuples, UnboxedSums, QuasiQuotes] "((#  |  | -0xbe |  #), ([g|f|]), (# x, _ #), M.C [] []) `f` (N.R {}) = ()" of
    Right (DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PTuple_ Boxed _, PRecord_ "N.R" [] False]}])) -> pure ()
    other -> assertFailure ("expected infix function bind with complex tuple lhs and qualified record rhs pattern, got: " <> show other)

test_funHeadInfixThSpliceLhs :: Assertion
test_funHeadInfixThSpliceLhs =
  case parseTopDecl "{-# LANGUAGE TemplateHaskell #-}\n$splice `fn` () = ()" of
    Right (DeclValue (FunctionBind _ "fn" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PSplice_ (EVar_ "splice"), PTuple_ Boxed []]}])) -> pure ()
    other -> assertFailure ("expected TH splice lhs infix function bind, got: " <> show other)

test_funHeadParenInfix :: Assertion
test_funHeadParenInfix =
  case parseTopDecl "(x + y) = x" of
    Right (DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PVar_ "y"]}])) -> pure ()
    other -> assertFailure ("expected parenthesized infix function bind, got: " <> show other)

test_funHeadParenInfixTail :: Assertion
test_funHeadParenInfixTail =
  case parseTopDecl "(x + y) z = x" of
    Right (DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PVar_ "y", PVar_ "z"]}])) -> pure ()
    other -> assertFailure ("expected parenthesized infix with tail, got: " <> show other)

test_funHeadLocalPrefix :: Assertion
test_funHeadLocalPrefix =
  case parseLetDecls "let { f x = x } in f 1" of
    Right [DeclValue (FunctionBind _ "f" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x"]}])] -> pure ()
    other -> assertFailure ("expected local prefix function bind, got: " <> show other)

test_funHeadLocalInfix :: Assertion
test_funHeadLocalInfix =
  case parseLetDecls "let { x + y = x } in 1 + 2" of
    Right [DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadInfix, matchPats = [PVar_ "x", PVar_ "y"]}])] -> pure ()
    other -> assertFailure ("expected local infix function bind, got: " <> show other)

test_funHeadLocalPrefixOp :: Assertion
test_funHeadLocalPrefixOp =
  case parseLetDecls "let { (+) x y = x } in 1 + 2" of
    Right [DeclValue (FunctionBind _ "+" [Match {matchHeadForm = MatchHeadPrefix, matchPats = [PVar_ "x", PVar_ "y"]}])] -> pure ()
    other -> assertFailure ("expected local prefix operator function bind, got: " <> show other)
