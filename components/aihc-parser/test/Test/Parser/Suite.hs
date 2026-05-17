{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Suite
  ( parserGoldenTests,
  )
where

import Control.Monad (unless, when)
import Data.Text qualified as T
import ParserEquivalent qualified as PE
import ParserGolden qualified as PG
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

parserGoldenTests :: IO TestTree
parserGoldenTests = do
  goldenTests <- parserGoldenGroup
  equivalentTests <- parserEquivalentGroup
  pure
    ( testGroup
        "parser-fixtures"
        [ goldenTests,
          equivalentTests
        ]
    )

parserGoldenGroup :: IO TestTree
parserGoldenGroup = do
  exprCases <- PG.loadExprCases
  importCases <- PG.loadImportCases
  moduleCases <- PG.loadModuleCases
  patternCases <- PG.loadPatternCases
  pragmaCases <- PG.loadPragmaCases

  exprChecks <- mapM mkExprCaseTest exprCases
  importChecks <- mapM mkModuleCaseTest importCases
  moduleChecks <- mapM mkModuleCaseTest moduleCases
  patternChecks <- mapM mkPatternCaseTest patternCases
  pragmaChecks <- mapM mkModuleCaseTest pragmaCases

  exprSummary <- mkSummaryTest "expr" PG.evaluateExprCase exprCases
  importSummary <- mkSummaryTest "import" PG.evaluateModuleCase importCases
  moduleSummary <- mkSummaryTest "module" PG.evaluateModuleCase moduleCases
  patternSummary <- mkSummaryTest "pattern" PG.evaluatePatternCase patternCases
  pragmaSummary <- mkSummaryTest "pragma" PG.evaluateModuleCase pragmaCases
  combinedSummary <- mkCombinedSummary exprCases importCases moduleCases patternCases pragmaCases

  pure
    ( testGroup
        "parser-golden"
        [ fixtureValidationTests,
          testGroup "expr" (exprChecks <> [exprSummary]),
          testGroup "import" (importChecks <> [importSummary]),
          testGroup "module" (moduleChecks <> [moduleSummary]),
          testGroup "pattern" (patternChecks <> [patternSummary]),
          testGroup "pragma" (pragmaChecks <> [pragmaSummary]),
          combinedSummary
        ]
    )

parserEquivalentGroup :: IO TestTree
parserEquivalentGroup = do
  exprCases <- PE.loadExprCases
  moduleCases <- PE.loadModuleCases
  declCases <- PE.loadDeclCases
  patternCases <- PE.loadPatternCases

  exprChecks <- mapM mkEquivalentExprCaseTest exprCases
  moduleChecks <- mapM mkEquivalentModuleCaseTest moduleCases
  declChecks <- mapM mkEquivalentDeclCaseTest declCases
  patternChecks <- mapM mkEquivalentPatternCaseTest patternCases

  exprSummary <- mkEquivalentSummaryTest "expr" PE.evaluateExprCase exprCases
  moduleSummary <- mkEquivalentSummaryTest "module" PE.evaluateModuleCase moduleCases
  declSummary <- mkEquivalentSummaryTest "decl" PE.evaluateDeclCase declCases
  patternSummary <- mkEquivalentSummaryTest "pattern" PE.evaluatePatternCase patternCases
  combinedSummary <- mkEquivalentCombinedSummary exprCases moduleCases declCases patternCases

  pure
    ( testGroup
        "parser-equivalent"
        [ equivalenceFixtureValidationTests,
          testGroup "expr" (exprChecks <> [exprSummary]),
          testGroup "module" (moduleChecks <> [moduleSummary]),
          testGroup "decl" (declChecks <> [declSummary]),
          testGroup "pattern" (patternChecks <> [patternSummary]),
          combinedSummary
        ]
    )

mkExprCaseTest :: PG.ParserCase -> IO TestTree
mkExprCaseTest meta = pure $ case PG.caseStatus meta of
  PG.StatusXFail -> testCaseInfo (PG.caseId meta) (xfailDetails (PG.evaluateExprCase meta) <* assertExprCase meta)
  _ -> testCase (PG.caseId meta) (assertExprCase meta)

mkModuleCaseTest :: PG.ParserCase -> IO TestTree
mkModuleCaseTest meta = pure $ case PG.caseStatus meta of
  PG.StatusXFail -> testCaseInfo (PG.caseId meta) (xfailDetails (PG.evaluateModuleCase meta) <* assertModuleCase meta)
  _ -> testCase (PG.caseId meta) (assertModuleCase meta)

mkPatternCaseTest :: PG.ParserCase -> IO TestTree
mkPatternCaseTest meta = pure $ case PG.caseStatus meta of
  PG.StatusXFail -> testCaseInfo (PG.caseId meta) (xfailDetails (PG.evaluatePatternCase meta) <* assertPatternCase meta)
  _ -> testCase (PG.caseId meta) (assertPatternCase meta)

xfailDetails :: (PG.Outcome, String) -> IO String
xfailDetails (outcome, details) = do
  case outcome of
    PG.OutcomeXFail -> pure ()
    _ -> assertFailure ("expected xfail outcome, got: " <> show outcome)
  pure details

mkSummaryTest :: String -> (PG.ParserCase -> (PG.Outcome, String)) -> [PG.ParserCase] -> IO TestTree
mkSummaryTest label evaluateCase cases = do
  let outcomes = map (evaluate evaluateCase) cases
  pure $ testCase (label <> " summary") (assertNoRegressions label outcomes)

mkCombinedSummary :: [PG.ParserCase] -> [PG.ParserCase] -> [PG.ParserCase] -> [PG.ParserCase] -> [PG.ParserCase] -> IO TestTree
mkCombinedSummary exprCases importCases moduleCases patternCases pragmaCases = do
  let exprOutcomes = map (evaluate PG.evaluateExprCase) exprCases
      importOutcomes = map (evaluate PG.evaluateModuleCase) importCases
      moduleOutcomes = map (evaluate PG.evaluateModuleCase) moduleCases
      patternOutcomes = map (evaluate PG.evaluatePatternCase) patternCases
      pragmaOutcomes = map (evaluate PG.evaluateModuleCase) pragmaCases
      outcomes = exprOutcomes <> importOutcomes <> moduleOutcomes <> patternOutcomes <> pragmaOutcomes
  pure $ testCase "summary" (assertNoRegressions "parser golden" outcomes)

assertExprCase :: PG.ParserCase -> Assertion
assertExprCase = assertCaseWith PG.evaluateExprCase

assertModuleCase :: PG.ParserCase -> Assertion
assertModuleCase = assertCaseWith PG.evaluateModuleCase

assertPatternCase :: PG.ParserCase -> Assertion
assertPatternCase = assertCaseWith PG.evaluatePatternCase

mkEquivalentExprCaseTest :: PE.EquivalentCase -> IO TestTree
mkEquivalentExprCaseTest meta = pure $ case PE.caseStatus meta of
  PE.StatusXFail -> testCaseInfo (PE.caseId meta) (equivalentXFailDetails (PE.evaluateExprCase meta) <* assertEquivalentExprCase meta)
  _ -> testCase (PE.caseId meta) (assertEquivalentExprCase meta)

mkEquivalentModuleCaseTest :: PE.EquivalentCase -> IO TestTree
mkEquivalentModuleCaseTest meta = pure $ case PE.caseStatus meta of
  PE.StatusXFail -> testCaseInfo (PE.caseId meta) (equivalentXFailDetails (PE.evaluateModuleCase meta) <* assertEquivalentModuleCase meta)
  _ -> testCase (PE.caseId meta) (assertEquivalentModuleCase meta)

mkEquivalentDeclCaseTest :: PE.EquivalentCase -> IO TestTree
mkEquivalentDeclCaseTest meta = pure $ case PE.caseStatus meta of
  PE.StatusXFail -> testCaseInfo (PE.caseId meta) (equivalentXFailDetails (PE.evaluateDeclCase meta) <* assertEquivalentDeclCase meta)
  _ -> testCase (PE.caseId meta) (assertEquivalentDeclCase meta)

mkEquivalentPatternCaseTest :: PE.EquivalentCase -> IO TestTree
mkEquivalentPatternCaseTest meta = pure $ case PE.caseStatus meta of
  PE.StatusXFail -> testCaseInfo (PE.caseId meta) (equivalentXFailDetails (PE.evaluatePatternCase meta) <* assertEquivalentPatternCase meta)
  _ -> testCase (PE.caseId meta) (assertEquivalentPatternCase meta)

equivalentXFailDetails :: (PE.Outcome, String) -> IO String
equivalentXFailDetails (outcome, details) = do
  case outcome of
    PE.OutcomeXFail -> pure ()
    _ -> assertFailure ("expected xfail outcome, got: " <> show outcome)
  pure details

mkEquivalentSummaryTest :: String -> (PE.EquivalentCase -> (PE.Outcome, String)) -> [PE.EquivalentCase] -> IO TestTree
mkEquivalentSummaryTest label evaluateCase cases = do
  let outcomes = map (evaluateEquivalent evaluateCase) cases
  pure $ testCase (label <> " summary") (assertNoEquivalentRegressions label outcomes)

mkEquivalentCombinedSummary :: [PE.EquivalentCase] -> [PE.EquivalentCase] -> [PE.EquivalentCase] -> [PE.EquivalentCase] -> IO TestTree
mkEquivalentCombinedSummary exprCases moduleCases declCases patternCases = do
  let exprOutcomes = map (evaluateEquivalent PE.evaluateExprCase) exprCases
      moduleOutcomes = map (evaluateEquivalent PE.evaluateModuleCase) moduleCases
      declOutcomes = map (evaluateEquivalent PE.evaluateDeclCase) declCases
      patternOutcomes = map (evaluateEquivalent PE.evaluatePatternCase) patternCases
      outcomes = exprOutcomes <> moduleOutcomes <> declOutcomes <> patternOutcomes
  pure $ testCase "summary" (assertNoEquivalentRegressions "parser equivalence" outcomes)

assertEquivalentExprCase :: PE.EquivalentCase -> Assertion
assertEquivalentExprCase = assertEquivalentCaseWith PE.evaluateExprCase

assertEquivalentModuleCase :: PE.EquivalentCase -> Assertion
assertEquivalentModuleCase = assertEquivalentCaseWith PE.evaluateModuleCase

assertEquivalentDeclCase :: PE.EquivalentCase -> Assertion
assertEquivalentDeclCase = assertEquivalentCaseWith PE.evaluateDeclCase

assertEquivalentPatternCase :: PE.EquivalentCase -> Assertion
assertEquivalentPatternCase = assertEquivalentCaseWith PE.evaluatePatternCase

assertCaseWith :: (PG.ParserCase -> (PG.Outcome, String)) -> PG.ParserCase -> Assertion
assertCaseWith evaluateCase meta =
  case evaluateCase meta of
    (PG.OutcomeFail, details) ->
      assertFailure
        ( "Regression in parser case "
            <> PG.caseId meta
            <> " ("
            <> PG.caseCategory meta
            <> ") expected "
            <> show (PG.caseStatus meta)
            <> " reason="
            <> PG.caseReason meta
            <> " details="
            <> details
        )
    (PG.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xfail parser case "
            <> PG.caseId meta
            <> " reason="
            <> PG.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertEquivalentCaseWith :: (PE.EquivalentCase -> (PE.Outcome, String)) -> PE.EquivalentCase -> Assertion
assertEquivalentCaseWith evaluateCase meta =
  case evaluateCase meta of
    (PE.OutcomeFail, details) ->
      assertFailure
        ( "Regression in parser equivalence case "
            <> PE.caseId meta
            <> " ("
            <> PE.caseCategory meta
            <> ") expected "
            <> show (PE.caseStatus meta)
            <> " reason="
            <> PE.caseReason meta
            <> " details="
            <> details
        )
    (PE.OutcomeXPass, details) ->
      assertFailure
        ( "Unexpected pass in xfail parser equivalence case "
            <> PE.caseId meta
            <> " reason="
            <> PE.caseReason meta
            <> " details="
            <> details
        )
    _ -> pure ()

assertNoRegressions :: String -> [(PG.ParserCase, PG.Outcome, String)] -> Assertion
assertNoRegressions label outcomes = do
  let (passN, xfailN, xpassN, failN) = PG.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( label
          <> " regressions found. "
          <> "pass="
          <> show passN
          <> " xfail="
          <> show xfailN
          <> " xpass="
          <> show xpassN
          <> " fail="
          <> show failN
          <> " completion="
          <> show completion
          <> "%"
      )

evaluate :: (PG.ParserCase -> (PG.Outcome, String)) -> PG.ParserCase -> (PG.ParserCase, PG.Outcome, String)
evaluate evaluateCase meta =
  let (outcome, details) = evaluateCase meta
   in (meta, outcome, details)

assertNoEquivalentRegressions :: String -> [(PE.EquivalentCase, PE.Outcome, String)] -> Assertion
assertNoEquivalentRegressions label outcomes = do
  let (passN, xfailN, xpassN, failN) = PE.progressSummary outcomes
      totalN = passN + xfailN + xpassN + failN
      completion = pct passN totalN
  when (failN > 0 || xpassN > 0) $
    assertFailure
      ( label
          <> " regressions found. "
          <> "pass="
          <> show passN
          <> " xfail="
          <> show xfailN
          <> " xpass="
          <> show xpassN
          <> " fail="
          <> show failN
          <> " completion="
          <> show completion
          <> "%"
      )

evaluateEquivalent :: (PE.EquivalentCase -> (PE.Outcome, String)) -> PE.EquivalentCase -> (PE.EquivalentCase, PE.Outcome, String)
evaluateEquivalent evaluateCase meta =
  let (outcome, details) = evaluateCase meta
   in (meta, outcome, details)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

fixtureValidationTests :: TestTree
fixtureValidationTests =
  testGroup
    "fixture-parse"
    [ testCase "rejects missing required keys" $
        case PG.parseParserCaseText PG.CaseExpr "missing.yaml" "extensions: []\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for missing required YAML keys",
      testCase "requires reason for xfail" $
        case PG.parseParserCaseText PG.CaseExpr "xfail.yaml" validXFailMissingReason of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when xfail reason is missing",
      testCase "requires ast for pass" $
        case PG.parseParserCaseText PG.CaseExpr "pass.yaml" validPassMissingAst of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when pass ast is missing",
      testCase "rejects xpass status" $
        case PG.parseParserCaseText PG.CaseExpr "xpass.yaml" validXPassFixture of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for xpass status",
      testCase "accepts xfail without ast" $
        case PG.parseParserCaseText PG.CaseExpr "xfail-no-ast.yaml" validXFailNoAst of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            if PG.caseStatus parsed == PG.StatusXFail && null (PG.caseAst parsed)
              then pure ()
              else assertFailure "expected xfail status with empty ast",
      testCase "xfail parse failures retain details" $
        case PG.parseParserCaseText PG.CaseExpr "xfail-details.yaml" validXFailWithParseFailure of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            case PG.evaluateExprCase parsed of
              (PG.OutcomeXFail, details)
                | null details -> assertFailure "expected xfail details to be non-empty"
                | otherwise -> pure ()
              other -> assertFailure ("expected xfail outcome with details, got: " <> show other),
      testCase "only YAML fixtures are loaded" $ do
        exprCases <- PG.loadExprCases
        importCases <- PG.loadImportCases
        moduleCases <- PG.loadModuleCases
        patternCases <- PG.loadPatternCases
        pragmaCases <- PG.loadPragmaCases
        let cases = exprCases <> importCases <> moduleCases <> patternCases <> pragmaCases
        mapM_
          ( \meta ->
              unless (takeExtension (PG.casePath meta) `elem` [".yaml", ".yml"]) $
                assertFailure ("unexpected non-parser fixture loaded: " <> PG.casePath meta)
          )
          cases
    ]

validXFailMissingReason :: T.Text
validXFailMissingReason =
  T.unlines
    [ "extensions: []",
      "input: bad",
      "status: xfail"
    ]

validXFailNoAst :: T.Text
validXFailNoAst =
  T.unlines
    [ "extensions: []",
      "input: bad",
      "status: xfail",
      "reason: known bug"
    ]

validXFailWithParseFailure :: T.Text
validXFailWithParseFailure =
  T.unlines
    [ "extensions: []",
      "input: \"(\"",
      "status: xfail",
      "reason: known bug"
    ]

validPassMissingAst :: T.Text
validPassMissingAst =
  T.unlines
    [ "extensions: []",
      "input: x",
      "status: pass"
    ]

validXPassFixture :: T.Text
validXPassFixture =
  T.unlines
    [ "extensions: []",
      "input: x",
      "ast: EVar \"x\"",
      "status: xpass",
      "reason: known bug"
    ]

equivalenceFixtureValidationTests :: TestTree
equivalenceFixtureValidationTests =
  testGroup
    "equivalence-fixture-parse"
    [ testCase "rejects missing required keys" $
        case PE.parseEquivalentCaseText PE.CaseExpr "missing.yaml" "extensions: []\n" of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for missing required YAML keys",
      testCase "requires two equivalent inputs" $
        case PE.parseEquivalentCaseText PE.CaseExpr "one-input.yaml" validEquivalentSingleInput of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for single equivalent input",
      testCase "requires reason for xfail" $
        case PE.parseEquivalentCaseText PE.CaseExpr "xfail.yaml" validEquivalentXFailMissingReason of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure when xfail reason is missing",
      testCase "rejects xpass status" $
        case PE.parseEquivalentCaseText PE.CaseExpr "xpass.yaml" validEquivalentXPassFixture of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure for xpass status",
      testCase "accepts pass fixture" $
        case PE.parseEquivalentCaseText PE.CaseExpr "pass.yaml" validEquivalentPassFixture of
          Left err -> assertFailure ("expected parse success, got: " <> err)
          Right parsed ->
            if PE.caseStatus parsed == PE.StatusPass && length (PE.caseInputs parsed) == 2
              then pure ()
              else assertFailure "expected pass status with two inputs",
      testCase "only YAML fixtures are loaded" $ do
        exprCases <- PE.loadExprCases
        moduleCases <- PE.loadModuleCases
        declCases <- PE.loadDeclCases
        patternCases <- PE.loadPatternCases
        let cases = exprCases <> moduleCases <> declCases <> patternCases
        mapM_
          ( \meta ->
              unless (takeExtension (PE.casePath meta) `elem` [".yaml", ".yml"]) $
                assertFailure ("unexpected non-parser equivalence fixture loaded: " <> PE.casePath meta)
          )
          cases
    ]

validEquivalentSingleInput :: T.Text
validEquivalentSingleInput =
  T.unlines
    [ "extensions: []",
      "equivalent:",
      "  - x",
      "status: pass"
    ]

validEquivalentXFailMissingReason :: T.Text
validEquivalentXFailMissingReason =
  T.unlines
    [ "extensions: []",
      "equivalent:",
      "  - x",
      "  - (x)",
      "status: xfail"
    ]

validEquivalentXPassFixture :: T.Text
validEquivalentXPassFixture =
  T.unlines
    [ "extensions: []",
      "equivalent:",
      "  - x",
      "  - (x)",
      "status: xpass",
      "reason: known bug"
    ]

validEquivalentPassFixture :: T.Text
validEquivalentPassFixture =
  T.unlines
    [ "extensions: []",
      "equivalent:",
      "  - x",
      "  - (x)",
      "status: pass"
    ]
