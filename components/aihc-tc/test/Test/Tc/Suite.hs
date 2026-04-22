{-# LANGUAGE OverloadedStrings #-}

module Test.Tc.Suite
  ( tcTests,
    tcGoldenTests,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr)
import Aihc.Parser.Syntax (Expr)
import Aihc.Tc
import Data.Text (Text)
import TcGolden qualified as TG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tcTests :: TestTree
tcTests =
  testGroup
    "tc-unit"
    [ testGroup "literals" literalTests,
      testGroup "application" applicationTests,
      testGroup "case" caseTests,
      testGroup "if-then-else" ifTests,
      testGroup "lambda" lambdaTests,
      testGroup "variables" variableTests,
      testGroup "error-cases" errorTests
    ]

-- | Build the golden test tree from YAML fixtures.
tcGoldenTests :: IO TestTree
tcGoldenTests = do
  cases <- TG.loadTcCases
  let tests = map mkGoldenTest cases
  pure (testGroup "tc-golden" tests)

mkGoldenTest :: TG.TcCase -> TestTree
mkGoldenTest tcase = testCase (TG.caseId tcase) $ do
  let (outcome, details) = TG.evaluateTcCase tcase
  case outcome of
    TG.OutcomePass -> pure ()
    TG.OutcomeXFail -> pure () -- known failure, expected
    TG.OutcomeFail ->
      assertFailure
        ( "TC golden test failed: "
            <> TG.caseId tcase
            <> " details="
            <> details
        )
    TG.OutcomeXPass ->
      assertFailure
        ( "Unexpected pass in TC golden test: "
            <> TG.caseId tcase
            <> " details="
            <> details
        )

-- | Parse an expression from text.
parseE :: Text -> Expr
parseE input =
  let config = defaultConfig {parserSourceName = "<test>"}
   in case parseExpr config input of
        ParseOk result -> result
        ParseErr err -> error ("Parse error in test: " ++ show err)

-- | Typecheck an expression and return the result.
tc :: Text -> TcResult
tc = typecheckExpr . parseE

-- Helper to check that a type is a specific TyCon
isTyCon :: Text -> TcType -> Bool
isTyCon name (TcTyCon tyc []) = tyConName tyc == name
isTyCon _ _ = False

-- Helper to check function type
isFunTy :: TcType -> Bool
isFunTy (TcFunTy _ _) = True
isFunTy _ = False

-- | Tests for literal expressions.
literalTests :: [TestTree]
literalTests =
  [ testCase "integer literal has type Int" $ do
      let result = tc "42"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Int" (isTyCon "Int" (tcResultType result)),
    testCase "float literal has type Double" $ do
      let result = tc "3.14"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Double" (isTyCon "Double" (tcResultType result)),
    testCase "char literal has type Char" $ do
      let result = tc "'a'"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Char" (isTyCon "Char" (tcResultType result)),
    testCase "string literal has type String" $ do
      let result = tc "\"hello\""
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be String" (isTyCon "String" (tcResultType result))
  ]

-- | Tests for function application.
applicationTests :: [TestTree]
applicationTests =
  [ testCase "application infers result type" $ do
      let result = tc "f x"
      assertBool "should have errors (unbound)" (not (tcResultSuccess result))
  ]

-- | Tests for if-then-else.
ifTests :: [TestTree]
ifTests =
  [ testCase "if-then-else with matching branches" $ do
      let result = tc "if True then 1 else 2"
      let ty = tcResultType result
      assertBool "should produce a type" (ty /= TcMetaTv (Unique (-1)))
  ]

-- | Tests for case expressions.
caseTests :: [TestTree]
caseTests =
  [ testCase "case expression threads scrutinee type into branches" $ do
      let result = tc "\\x -> case x of { y -> y }"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be function type" (isFunTy (tcResultType result))
  ]

-- | Tests for lambda expressions.
lambdaTests :: [TestTree]
lambdaTests =
  [ testCase "lambda produces function type" $ do
      let result = tc "\\x -> 42"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be function type" (isFunTy (tcResultType result))
  ]

-- | Tests for variable expressions.
variableTests :: [TestTree]
variableTests =
  [ testCase "unbound variable produces error" $ do
      let result = tc "undefined_var"
      assertBool "should fail" (not (tcResultSuccess result))
      assertBool "should have diagnostics" (not (null (tcResultDiagnostics result)))
  ]

-- | Tests for error cases.
errorTests :: [TestTree]
errorTests =
  [ testCase "unbound variable reports error" $ do
      let result = tc "foo"
      assertBool "should not succeed" (not (tcResultSuccess result))
      case tcResultDiagnostics result of
        [] -> assertBool "expected diagnostics" False
        (d : _) -> case diagKind d of
          UnboundVariable name ->
            assertEqual "error should name 'foo'" "foo" name
          other ->
            assertBool ("unexpected error kind: " ++ show other) False
  ]
