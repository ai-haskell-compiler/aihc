{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Spec (tests) where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as FcType
import Aihc.Grin (GrinGlobal (..), GrinLintError (..), GrinProgram (..), InterpretError (..), ProgramStreams (..), interpretProgramBinding, interpretProgramIoBinding, lintProgram, lowerProgram, normalizeGrinProgram, prettyProgram)
import Aihc.Grin.Cps (toCpsGrin)
import Aihc.Grin.Gc (gcGrinProgram, lowerGc)
import Aihc.Grin.Lint (lintGcProgram)
import Aihc.Grin.Parser qualified as GrinParser
import Aihc.Grin.Simplify (simplifyGrinProgram)
import Aihc.Grin.Syntax (GrinFunction (..))
import Aihc.Grin.Tidy (tidyGrinProgram)
import Aihc.Resolve (PackageId (..))
import Aihc.Testing.EvalFixture qualified as EvalFixture
import Control.Exception (evaluate)
import Data.Aeson ((.:))
import Data.Aeson.Types (parseEither, withObject)
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import GrinGolden qualified
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (listDirectory)
import System.Environment (getEnv)
import System.FilePath (takeExtension, (</>))
import System.IO (stderr, stdin)
import Test.Grin.Anf qualified as Anf
import Test.Grin.Arbitrary (prop_grinPrettyRoundTrip)
import Test.Grin.Heap qualified as Heap
import Test.Grin.Lint qualified as Lint
import Test.Grin.Srt qualified as Srt
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

data GrinEvalEnvironment = GrinEvalEnvironment
  { grinEvalFrontend :: !EvalFixture.EvalEnvironment,
    grinEvalCore :: !GrinProgram
  }

tests :: IO TestTree
tests = do
  lintFixtures <- loadLintFixtures
  simplifyFixtures <- loadSimplifyFixtures
  gcFixtures <- loadGcFixtures
  fixtures <- GrinGolden.loadGrinCases
  evalFixtures <- filter (("grin" `elem`) . EvalFixture.evalCaseEvaluators) <$> EvalFixture.loadEvalCases
  pure
    ( testGroup
        "aihc-grin"
        [ testProperty "generated GRIN pretty-printer round-trip" prop_grinPrettyRoundTrip,
          Anf.tests,
          Heap.tests,
          Lint.tests,
          testGroup "GRIN lint fixtures" lintFixtures,
          testGroup "GRIN simplify fixtures" simplifyFixtures,
          testGroup "GRIN GC fixtures" gcFixtures,
          Srt.tests,
          testGroup "GRIN golden tests" (map fixtureTest fixtures),
          withResource loadGrinEvalEnvironment (const (pure ())) $ \getEnvironment ->
            testGroup "shared evaluation fixtures via GRIN" (map (evalFixtureTest getEnvironment) evalFixtures)
        ]
    )

-- | Parse each textual GRIN fixture before the lint status check.
loadLintFixtures :: IO [TestTree]
loadLintFixtures = do
  root <- getEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/grin/test/Test/Fixtures/grin-lint"
  paths <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure [testCase path (checkLintFixture (directory </> path)) | path <- paths]

checkLintFixture :: FilePath -> IO ()
checkLintFixture path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> assertFailure (Y.prettyPrintParseException problem)
    Right value ->
      case parseEither parseFixture value of
        Left problem -> assertFailure problem
        Right (source, expected) ->
          case GrinParser.parseProgram source of
            Left problem -> assertFailure (GrinParser.renderParseError problem)
            Right program ->
              case (expected, lintProgram program) of
                ("none", []) -> pure ()
                ("result-layout", problems@(_ : _))
                  | all isResultLayout problems -> pure ()
                (_, problems) -> assertFailure ("expected " <> T.unpack expected <> ", got " <> show problems)
  where
    parseFixture = withObject "GRIN lint fixture" $ \object -> do
      source <- object .: "program"
      status <- object .: "status"
      expected <- object .: "error"
      reason <- object .: "reason"
      if status == ("pass" :: Text) && expected `elem` ["none", "result-layout"] && not (T.null reason)
        then pure (source, expected)
        else fail "invalid GRIN lint fixture status or error"
    isResultLayout GrinLintResultLayout {} = True
    isResultLayout _ = False

-- | Simplify each textual GRIN fixture and compare it with the expected
-- program. The simplifier leaves copy binds behind for the normalizer, so
-- the fixture shows the normalized result, and that result must lint.
loadSimplifyFixtures :: IO [TestTree]
loadSimplifyFixtures = do
  root <- getEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/grin/test/Test/Fixtures/grin-simplify"
  paths <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure [testCase path (checkSimplifyFixture (directory </> path)) | path <- paths]

checkSimplifyFixture :: FilePath -> IO ()
checkSimplifyFixture path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> assertFailure (Y.prettyPrintParseException problem)
    Right value ->
      case parseEither parseFixture value of
        Left problem -> assertFailure problem
        Right (source, expected) ->
          case GrinParser.parseProgram source of
            Left problem -> assertFailure (GrinParser.renderParseError problem)
            Right program -> do
              let simplified = normalizeGrinProgram (simplifyGrinProgram program)
                  actual = T.strip (T.pack (renderString (layoutPretty defaultLayoutOptions (prettyProgram simplified))))
              case lintProgram simplified of
                [] -> pure ()
                problems -> assertFailure ("the simplified program does not lint: " <> show problems)
              if actual == T.strip expected
                then pure ()
                else assertFailure ("output mismatch\nexpected:\n" <> T.unpack expected <> "\nactual:\n" <> T.unpack actual)
  where
    parseFixture = withObject "GRIN simplify fixture" $ \object -> do
      source <- object .: "program"
      expected <- object .: "expected"
      status <- object .: "status"
      reason <- object .: "reason"
      if status == ("pass" :: Text) && not (T.null reason)
        then pure (source, expected :: Text)
        else fail "invalid GRIN simplify fixture status"

-- | Check explicit roots and relocated results after the GC stage.
loadGcFixtures :: IO [TestTree]
loadGcFixtures = do
  root <- getEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/grin/test/Test/Fixtures/grin-gc"
  paths <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure [testCase path (checkGcFixture (directory </> path)) | path <- paths]

checkGcFixture :: FilePath -> IO ()
checkGcFixture path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> assertFailure (Y.prettyPrintParseException problem)
    Right value ->
      case parseEither parseFixture value of
        Left problem -> assertFailure problem
        Right (source, expected) ->
          case GrinParser.parseProgram source of
            Left problem -> assertFailure (GrinParser.renderParseError problem)
            Right program ->
              case toCpsGrin (normalizeGrinProgram program) of
                Left problem -> assertFailure (show problem)
                Right cps -> do
                  let gc = lowerGc cps
                      output = gcGrinProgram gc
                      names = map grinFunctionName (grinFunctions program)
                      selected = output {grinFunctions = filter ((`elem` names) . grinFunctionName) (grinFunctions output)}
                      actual = T.strip (T.pack (renderString (layoutPretty defaultLayoutOptions (prettyProgram (tidyGrinProgram selected)))))
                  case lintGcProgram gc of
                    [] -> pure ()
                    problems -> assertFailure ("the GC program does not lint: " <> show problems)
                  case GrinParser.parseProgram actual of
                    Left problem -> assertFailure (GrinParser.renderParseError problem)
                    Right parsed ->
                      if parsed == tidyGrinProgram selected
                        then pure ()
                        else assertFailure "the GC program does not survive a parser round trip"
                  if actual == T.strip expected
                    then pure ()
                    else assertFailure ("output mismatch\nexpected:\n" <> T.unpack expected <> "\nactual:\n" <> T.unpack actual)
  where
    parseFixture = withObject "GRIN GC fixture" $ \object -> do
      source <- object .: "program"
      expected <- object .: "expected"
      status <- object .: "status"
      reason <- object .: "reason"
      if status == ("pass" :: Text) && not (T.null reason)
        then pure (source, expected :: Text)
        else fail "invalid GRIN GC fixture status"

-- | Lower aihc-prim and aihc-base to GRIN one time.
loadGrinEvalEnvironment :: IO GrinEvalEnvironment
loadGrinEvalEnvironment = do
  frontend <- EvalFixture.loadEvalEnvironment
  case lowerProgram (EvalFixture.evalEnvironmentProgram frontend) of
    Left problem -> fail ("core library GRIN lower error: " <> problem)
    Right grinProgram ->
      case lintProgram grinProgram of
        [] -> do
          -- Force the core GRIN program one time.
          _ <- evaluate (length (grinFunctions grinProgram))
          _ <- evaluate (length (grinGlobals grinProgram))
          pure
            GrinEvalEnvironment
              { grinEvalFrontend = frontend,
                grinEvalCore = grinProgram
              }
        problems -> fail ("core library GRIN lint error: " <> show problems)

fixtureTest :: GrinGolden.GrinCase -> TestTree
fixtureTest fixture = testCase (GrinGolden.caseId fixture) $
  case GrinGolden.evaluateGrinCase fixture of
    (GrinGolden.OutcomePass, _) -> pure ()
    (GrinGolden.OutcomeXFail, _) -> pure ()
    (GrinGolden.OutcomeXPass, details) -> assertFailure ("unexpected pass: " <> details)
    (GrinGolden.OutcomeFail, details) -> assertFailure details

evalFixtureTest :: IO GrinEvalEnvironment -> EvalFixture.EvalCase -> TestTree
evalFixtureTest getEnvironment fixture = testCase (EvalFixture.evalCaseId fixture) $ do
  environment <- getEnvironment
  (outcome, details) <-
    EvalFixture.evaluateEvalCase
      (grinEvalFrontend environment)
      (evaluateGrin (grinEvalCore environment))
      fixture
  case outcome of
    EvalFixture.OutcomePass -> pure ()
    EvalFixture.OutcomeXFail -> pure ()
    EvalFixture.OutcomeXPass -> assertFailure ("unexpected pass: " <> details)
    EvalFixture.OutcomeFail -> assertFailure details

-- | The program writes its standard output to the given handle.
evaluateGrin :: GrinProgram -> EvalFixture.ProgramEvaluator
evaluateGrin coreProgram output name program =
  case prepareEvalProgram name program of
    Left problem -> pure (Left (EvalFixture.EvaluationError problem))
    Right (prepared, unwrapResult) -> evaluatePrepared prepared unwrapResult
  where
    evaluatePrepared prepared unwrapResult =
      case lowerProgram prepared of
        Left problem -> pure (Left (EvalFixture.EvaluationError problem))
        Right fixtureProgram ->
          case lintProgram fixtureProgram of
            [] ->
              fmap unwrapResult . classifyResult
                <$> interpreter streams (bindingName name fixtureProgram) (appendGrinProgram coreProgram fixtureProgram)
            problems -> pure (Left (EvalFixture.EvaluationError ("GRIN lint error: " <> show problems)))
    streams = ProgramStreams {programStdin = stdin, programStdout = output, programStderr = stderr}
    interpreter
      | evalBindingIsIo name program = interpretProgramIoBinding
      | otherwise = interpretProgramBinding

-- | Put the fixture GRIN after the core GRIN.
-- The fixture has a different package name and a different module name.
appendGrinProgram :: GrinProgram -> GrinProgram -> GrinProgram
appendGrinProgram core fixture =
  GrinProgram
    { grinConstructors = grinConstructors core <> grinConstructors fixture,
      grinPrimitives = grinPrimitives core <> grinPrimitives fixture,
      grinForeignCalls = grinForeignCalls core <> grinForeignCalls fixture,
      grinGlobals = grinGlobals core <> grinGlobals fixture,
      grinFunctions = grinFunctions core <> grinFunctions fixture
    }

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
    isLiftedRepresentation (Fc.TyApp (Fc.TyCon boxed) (Fc.TyCon levity)) =
      Fc.nameText boxed == "BoxedRep" && Fc.nameText levity == "Lifted"
    isLiftedRepresentation _ = False
    wrapDeclaration before declaration after representation =
      let valueName = Fc.valName declaration
          origin = Fc.nameOrigin valueName
          resultTypeName = Fc.Name "__AihcEvalResultType" Fc.SortTypeConstructor origin
          constructorName = Fc.Name "__AihcEvalResult" Fc.SortDataConstructor origin
          primOrigin = Fc.OriginTop (PackageId "aihc-prim") "GHC.Types"
          typeName = Fc.Name "TYPE" Fc.SortTypeConstructor primOrigin
          liftedName = Fc.Name "LiftedRep" Fc.SortSynonym primOrigin
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
                          Fc.conType = Fc.TyFun representation liftedRepresentation (Fc.valType declaration) resultType,
                          Fc.conRepresentation = Fc.HeapConstructor
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
  fromMaybe name (listToMaybe [globalName | global <- grinGlobals program, let globalName = grinGlobalName global, ("\0" <> name) `T.isSuffixOf` globalName])

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
