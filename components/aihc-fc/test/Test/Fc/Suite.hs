{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcEvalTests,
    fcEvalFixtureTests,
  )
where

import Aihc.Fc
import Aihc.Tc (TcType (..), TyCon (..), Unique (..))
import Data.Text (Text)
import FcEvalGolden qualified as EvalGolden
import FcGolden
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

-- | Build the golden test tree from fixtures.
fcGoldenTests :: IO TestTree
fcGoldenTests = do
  cases <- loadFcCases
  let tests = map mkTest cases
  pure (testGroup "FC golden tests" tests)

mkTest :: FcCase -> TestTree
mkTest tc = testCase (caseId tc) $ do
  let (outcome, details) = evaluateFcCase tc
  case outcome of
    OutcomePass -> pure ()
    OutcomeXFail -> pure ()
    OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    OutcomeFail -> assertFailure details

fcEvalTests :: TestTree
fcEvalTests =
  testGroup
    "FC evaluator"
    [ testCase "renders string literals" $
        assertEvalExpr "\"hello world\"" (FcLit (LitString "hello world")),
      testCase "renders char literals" $
        assertEvalExpr "'x'#" (FcLit (LitChar 'x')),
      testCase "renders int literals" $
        assertEvalExpr "42" (FcLit (LitInt 42)),
      testCase "applies lambdas" $
        assertEvalExpr
          "\"ok\""
          (FcApp (FcLam (var "x" stringTy) (FcVar (var "x" stringTy))) (FcLit (LitString "ok"))),
      testCase "evaluates top-level bindings" $
        let program =
              FcProgram
                [ FcTopBind
                    (FcNonRec (var "answer" stringTy) (FcLit (LitString "top")))
                ]
         in assertEqual "result" (Right "\"top\"") (evalProgramBinding "answer" program >>= renderValue),
      testCase "renders raw constructor values" $
        assertEqual
          "raw result"
          (Right ": 'x' []")
          (renderRawValue (VConstructor ":" [VConstructor "C#" [VLit (LitChar 'x')], VConstructor "[]" []]))
    ]

fcEvalFixtureTests :: IO TestTree
fcEvalFixtureTests = do
  cases <- EvalGolden.loadFcEvalCases
  let tests = map mkEvalFixtureTest cases
  pure (testGroup "FC evaluation fixtures" tests)

mkEvalFixtureTest :: EvalGolden.FcEvalCase -> TestTree
mkEvalFixtureTest tc = testCase (EvalGolden.evalCaseId tc) $ do
  (outcome, details) <- EvalGolden.evaluateFcEvalCase tc
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

assertEvalExpr :: Text -> FcExpr -> IO ()
assertEvalExpr expected expr =
  assertEqual "result" (Right expected) (evalExpr expr >>= renderValue)

var :: Text -> TcType -> Var
var name = Var name (Unique 0)

stringTy :: TcType
stringTy = TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []]
