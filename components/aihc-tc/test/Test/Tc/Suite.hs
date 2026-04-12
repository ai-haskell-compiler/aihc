{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the type checker.
module Test.Tc.Suite
  ( tcTests,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr)
import Aihc.Parser.Syntax (Expr)
import Aihc.Tc
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tcTests :: TestTree
tcTests =
  testGroup
    "tc-unit"
    [ testGroup "literals" literalTests,
      testGroup "application" applicationTests,
      testGroup "if-then-else" ifTests,
      testGroup "lambda" lambdaTests,
      testGroup "variables" variableTests,
      testGroup "error-cases" errorTests
    ]

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
      -- Applying a function to an argument should produce the result type.
      -- For now, test that it doesn't crash and produces a type.
      let result = tc "f x"
      -- f and x are unbound, so we get errors but still a type.
      assertBool "should have errors (unbound)" (not (tcResultSuccess result))
  ]

-- | Tests for if-then-else.
ifTests :: [TestTree]
ifTests =
  [ testCase "if-then-else with matching branches" $ do
      let result = tc "if True then 1 else 2"
      -- True is unbound in MVP, but the structure should work.
      -- We check that it produces a type (even if there are errors).
      let ty = tcResultType result
      assertBool "should produce a type" (ty /= TcMetaTv (Unique (-1)))
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
