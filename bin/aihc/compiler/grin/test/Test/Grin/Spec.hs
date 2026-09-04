{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Spec (tests) where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as FcType
import Aihc.Grin (GrinProgram (..), InterpretError (..), interpretProgramBinding, interpretProgramIoBinding, lintProgram, lowerProgram)
import Aihc.Resolve (PackageId (..))
import Aihc.Testing.EvalFixture qualified as EvalFixture
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GrinGolden qualified
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Grin.Srt qualified as Srt
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: IO TestTree
tests = do
  fixtures <- GrinGolden.loadGrinCases
  evalFixtures <- filter (("grin" `elem`) . EvalFixture.evalCaseEvaluators) <$> EvalFixture.loadEvalCases
  pure
    ( testGroup
        "aihc-grin"
        [ testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip,
          Srt.tests,
          testGroup "GRIN golden tests" (map fixtureTest fixtures),
          withResource EvalFixture.loadEvalEnvironment (const (pure ())) $ \getEnvironment ->
            testGroup "shared evaluation fixtures via GRIN" (map (evalFixtureTest getEnvironment) evalFixtures)
        ]
    )

fixtureTest :: GrinGolden.GrinCase -> TestTree
fixtureTest fixture = testCase (GrinGolden.caseId fixture) $
  case GrinGolden.evaluateGrinCase fixture of
    (GrinGolden.OutcomePass, _) -> pure ()
    (GrinGolden.OutcomeXFail, _) -> pure ()
    (GrinGolden.OutcomeXPass, details) -> assertFailure ("unexpected pass: " <> details)
    (GrinGolden.OutcomeFail, details) -> assertFailure details

evalFixtureTest :: IO EvalFixture.EvalEnvironment -> EvalFixture.EvalCase -> TestTree
evalFixtureTest getEnvironment fixture = testCase (EvalFixture.evalCaseId fixture) $ do
  environment <- getEnvironment
  (outcome, details) <- EvalFixture.evaluateEvalCase environment evaluateGrin fixture
  case outcome of
    EvalFixture.OutcomePass -> pure ()
    EvalFixture.OutcomeXFail -> pure ()
    EvalFixture.OutcomeXPass -> assertFailure ("unexpected pass: " <> details)
    EvalFixture.OutcomeFail -> assertFailure details

evaluateGrin :: EvalFixture.ProgramEvaluator
evaluateGrin name program =
  case prepareEvalProgram name program of
    Left problem -> pure (Left (EvalFixture.EvaluationError problem))
    Right (prepared, unwrapResult) -> evaluatePrepared prepared unwrapResult
  where
    evaluatePrepared prepared unwrapResult =
      case lowerProgram prepared of
        Left problem -> pure (Left (EvalFixture.EvaluationError problem))
        Right grinProgram ->
          case lintProgram grinProgram of
            [] -> fmap unwrapResult . classifyResult <$> interpreter (bindingName name grinProgram) grinProgram
            problems -> pure (Left (EvalFixture.EvaluationError ("GRIN lint error: " <> show problems)))
    interpreter
      | evalBindingIsIo name program = interpretProgramIoBinding
      | otherwise = interpretProgramBinding

prepareEvalProgram :: Text -> Fc.Program -> Either String (Fc.Program, Text -> Text)
prepareEvalProgram sourceName program =
  case break isEvalDeclaration (Fc.programDecls program) of
    (_, []) -> Left ("missing evaluation binding " <> T.unpack sourceName)
    (before, Fc.DeclVal declaration : after) ->
      case FcType.repOf typeEnvironment (Fc.valType declaration) of
        Nothing -> Left ("missing evaluation binding representation for " <> T.unpack sourceName)
        Just representation
          | isLiftedRepresentation representation -> Right (program, id)
          | otherwise -> Right (wrapDeclaration before declaration after representation)
    _ -> Left ("invalid evaluation binding " <> T.unpack sourceName)
  where
    typeEnvironment = FcType.typeEnvFromProgram (PackageId "aihc-prim") program
    isEvalDeclaration (Fc.DeclVal declaration) = Fc.nameText (Fc.valName declaration) == sourceName
    isEvalDeclaration _ = False
    isLiftedRepresentation (Fc.TyCon name) = Fc.nameText name == "LiftedRep"
    isLiftedRepresentation _ = False
    wrapDeclaration before declaration after representation =
      let valueName = Fc.valName declaration
          origin = Fc.nameOrigin valueName
          resultTypeName = Fc.Name "__AihcEvalResultType" Fc.SortTypeConstructor origin
          constructorName = Fc.Name "__AihcEvalResult" Fc.SortDataConstructor origin
          primOrigin = Fc.OriginTop (PackageId "aihc-prim") "GHC.Types"
          typeName = Fc.Name "TYPE" Fc.SortTypeConstructor primOrigin
          liftedName = Fc.Name "LiftedRep" Fc.SortDataConstructor primOrigin
          liftedRepresentation = Fc.TyCon liftedName
          resultType = Fc.TyCon resultTypeName
          resultDeclaration =
            Fc.DeclType
              Fc.TypeDecl
                { Fc.typeVis = Fc.Private,
                  Fc.typeName = resultTypeName,
                  Fc.typeBinders = [],
                  Fc.typeResult = Fc.TyApp (Fc.TyCon typeName) liftedRepresentation,
                  Fc.typeRoles = [],
                  Fc.typeCons =
                    [ Fc.ConDecl
                        { Fc.conVis = Fc.Private,
                          Fc.conName = constructorName,
                          Fc.conType = Fc.TyFun representation liftedRepresentation (Fc.valType declaration) resultType
                        }
                    ]
                }
          wrappedDeclaration =
            Fc.DeclVal
              declaration
                { Fc.valType = resultType,
                  Fc.valBody = Fc.ExApp (Fc.ExVar constructorName) (Fc.valBody declaration)
                }
          prefix = "__AihcEvalResult "
          unwrap rendered = fromMaybe rendered (T.stripPrefix prefix rendered)
       in (program {Fc.programDecls = [resultDeclaration] <> before <> [wrappedDeclaration] <> after}, unwrap)

bindingName :: Text -> GrinProgram -> Text
bindingName name program =
  fromMaybe name (listToMaybe [globalName | (globalName, _) <- grinGlobals program, ("\0" <> name) `T.isSuffixOf` globalName])

evalBindingIsIo :: Text -> Fc.Program -> Bool
evalBindingIsIo sourceName program =
  any isIoDeclaration (Fc.programDecls program)
  where
    isIoDeclaration (Fc.DeclVal declaration) =
      Fc.nameText (Fc.valName declaration) == sourceName && isIoType (Fc.valType declaration)
    isIoDeclaration _ = False
    isIoType (Fc.TyApp function _) = isIoType function
    isIoType (Fc.TyForAll _ body) = isIoType body
    isIoType (Fc.TyCon name) = Fc.nameText name == "IO"
    isIoType _ = False

classifyResult :: Either InterpretError Text -> Either EvalFixture.EvaluationFailure Text
classifyResult result =
  case result of
    Left (InterpretRaisedException exception) -> Left (EvalFixture.EvaluationRaised exception)
    Left problem -> Left (EvalFixture.EvaluationError (show problem))
    Right value -> Right value
