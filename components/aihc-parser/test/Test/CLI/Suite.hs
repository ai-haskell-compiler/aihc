{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Suite
  ( cliTests,
  )
where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Parser (defaultConfig, parseModule)
import Parser.Ast
import Parser.Lexer (LexTokenKind (..), lexTokensWithExtensions)
import Parser.PrettyAST (prettyASTModule, prettyASTToken, prettyASTTokenKind)
import Parser.Types (ParseResult (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

cliTests :: TestTree
cliTests =
  testGroup
    "CLI"
    [ testGroup "aihc-lexer" lexerCLITests,
      testGroup "aihc-parser" parserCLITests
    ]

lexerCLITests :: [TestTree]
lexerCLITests =
  [ testCase "lexes simple negative literal without extension" test_lexerNegativeLiteralNoExt,
    testCase "lexes simple negative literal with NegativeLiterals" test_lexerNegativeLiteralWithExt,
    testCase "lexes invalid char literal as error" test_lexerInvalidCharLiteral,
    testCase "token pretty-printing format" test_tokenPrettyFormat
  ]

parserCLITests :: [TestTree]
parserCLITests =
  [ testCase "parses simple module" test_parserSimpleModule,
    testCase "parses module without header" test_parserModuleWithoutHeader,
    testCase "parses empty module" test_parserEmptyModule,
    testCase "parses data with deriving" test_parserDataDeriving,
    testCase "parses module with warning" test_parserModuleWarning,
    testCase "reports parse error" test_parserError,
    testCase "module AST pretty-printing format" test_moduleASTFormat
  ]

-- Lexer CLI tests

test_lexerNegativeLiteralNoExt :: Assertion
test_lexerNegativeLiteralNoExt = do
  let tokens = lexTokensWithExtensions [] "-10"
      output = T.unlines (map prettyASTToken tokens)
  assertInfixOf "TkVarSym \"-\"" output
  assertInfixOf "TkInteger 10" output

test_lexerNegativeLiteralWithExt :: Assertion
test_lexerNegativeLiteralWithExt = do
  let tokens = lexTokensWithExtensions [NegativeLiterals] "-10"
      output = T.unlines (map prettyASTToken tokens)
  assertInfixOf "TkInteger -10" output
  assertNotInfixOf "TkVarSym" output

test_lexerInvalidCharLiteral :: Assertion
test_lexerInvalidCharLiteral = do
  let tokens = lexTokensWithExtensions [] "'ab'"
      output = T.unlines (map prettyASTToken tokens)
  assertInfixOf "TkError" output
  assertInfixOf "invalid char literal" output

test_tokenPrettyFormat :: Assertion
test_tokenPrettyFormat = do
  -- Verify token kind pretty printing
  assertEqual "TkInteger pretty" "TkInteger 42" (prettyASTTokenKind (TkInteger 42))
  assertEqual "TkVarSym pretty" "TkVarSym \"-\"" (prettyASTTokenKind (TkVarSym "-"))
  assertEqual "TkString pretty" "TkString \"hello\"" (prettyASTTokenKind (TkString "hello"))
  assertEqual "TkChar pretty" "TkChar 'x'" (prettyASTTokenKind (TkChar 'x'))

-- Parser CLI tests

test_parserSimpleModule :: Assertion
test_parserSimpleModule = do
  let result = parseModule defaultConfig "module Demo where\nx = 1"
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      assertInfixOf "Module" output
      assertInfixOf "name = \"Demo\"" output
      assertInfixOf "FunctionBind \"x\"" output
      assertInfixOf "EInt 1" output

test_parserModuleWithoutHeader :: Assertion
test_parserModuleWithoutHeader = do
  let result = parseModule defaultConfig "x = 1"
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      assertInfixOf "Module" output
      assertNotInfixOf "name =" output
      assertInfixOf "FunctionBind \"x\"" output

test_parserEmptyModule :: Assertion
test_parserEmptyModule = do
  let result = parseModule defaultConfig ""
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      assertEqual "empty module output" "Module {}" output

test_parserDataDeriving :: Assertion
test_parserDataDeriving = do
  let result = parseModule defaultConfig "data A deriving ()"
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      assertInfixOf "DeclData" output
      assertInfixOf "DataDecl" output
      assertInfixOf "name = \"A\"" output
      assertInfixOf "DerivingClause" output

test_parserModuleWarning :: Assertion
test_parserModuleWarning = do
  let result = parseModule defaultConfig "module Demo {-# WARNING \"test warning\" #-} where"
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      assertInfixOf "name = \"Demo\"" output
      assertInfixOf "WarnText \"test warning\"" output

test_parserError :: Assertion
test_parserError = do
  let result = parseModule defaultConfig "module where"
  case result of
    ParseOk _ -> assertFailure "Expected parse failure"
    ParseErr _ -> pure () -- Expected

test_moduleASTFormat :: Assertion
test_moduleASTFormat = do
  -- Verify output format matches expected structure
  let result = parseModule defaultConfig "x = 1"
  case result of
    ParseErr _ -> assertFailure "Expected parse success"
    ParseOk modu -> do
      let output = prettyASTModule modu
      -- Should be on one line with proper formatting
      assertNotInfixOf "\n" output
      assertInfixOf "Module {" output
      assertInfixOf "decls = [" output

-- Test helpers

assertInfixOf :: Text -> Text -> Assertion
assertInfixOf needle haystack =
  unless (needle `T.isInfixOf` haystack) $
    assertFailure ("Expected to find \"" <> T.unpack needle <> "\" in:\n" <> T.unpack haystack)

assertNotInfixOf :: Text -> Text -> Assertion
assertNotInfixOf needle haystack =
  when (needle `T.isInfixOf` haystack) $
    assertFailure ("Did not expect to find \"" <> T.unpack needle <> "\" in:\n" <> T.unpack haystack)
