{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Spec (tests) where

import Aihc.Fc qualified as Fc
import Aihc.Fc.TypeOf qualified as FcType
import Aihc.Grin (GrinConstructorDecl (..), GrinGlobal (..), GrinLintError (..), GrinProgram (..), GrinVis (..), InterpretError (..), PointsToRewrites (..), ProgramStreams (..), analyzePointsTo, finishGrinProgram, interpretProgramBinding, interpretProgramIoBinding, lintProgram, lowerProgram, normalizeGrinProgram, prettyProgram, rewriteWithPointsTo)
import Aihc.Grin.Cps (toCpsGrin)
import Aihc.Grin.Dce (sweptGrinProgram)
import Aihc.Grin.Gc (gcGrinProgram, lowerGc)
import Aihc.Grin.Lint (lintGcProgram)
import Aihc.Grin.Parser qualified as GrinParser
import Aihc.Grin.Simplify (simplifyGrinProgram)
import Aihc.Grin.Syntax (GrinFunction (..))
import Aihc.Grin.Tidy (tidyGrinProgram)
import Aihc.Resolve (PackageId (..))
import Aihc.Testing.EvalFixture qualified as EvalFixture
import Control.Exception (evaluate)
import Data.Aeson ((.:), (.:?))
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
  pointsToFixtures <- loadPointsToFixtures
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
          testGroup "GRIN points-to fixtures" pointsToFixtures,
          testGroup "GRIN GC fixtures" gcFixtures,
          Srt.tests,
          testGroup "GRIN golden tests" (map fixtureTest fixtures),
          withResource loadGrinEvalEnvironment (const (pure ())) $ \getEnvironment ->
            testGroup
              "shared evaluation fixtures via GRIN"
              [ testGroup "separate core" (map (evalFixtureTest getEnvironment SeparateCore) evalFixtures),
                -- The whole program of each fixture, with the rewrites of the
                -- points-to analysis. The interpreter fails an evaluation of
                -- a thunk that a single-entry evaluation entered before, so
                -- these runs check the sharing analysis too.
                testGroup "whole program with points-to" (map (evalFixtureTest getEnvironment WholeProgramPointsTo) evalFixtures)
              ]
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
                ("invalid-forward", problems@(_ : _))
                  | all isInvalidForward problems -> pure ()
                (_, problems) -> assertFailure ("expected " <> T.unpack expected <> ", got " <> show problems)
  where
    parseFixture = withObject "GRIN lint fixture" $ \object -> do
      source <- object .: "program"
      status <- object .: "status"
      expected <- object .: "error"
      reason <- object .: "reason"
      if status == ("pass" :: Text) && expected `elem` ["none", "result-layout", "invalid-forward"] && not (T.null reason)
        then pure (source, expected)
        else fail "invalid GRIN lint fixture status or error"
    isResultLayout GrinLintResultLayout {} = True
    isResultLayout _ = False
    isInvalidForward GrinLintInvalidForward = True
    isInvalidForward GrinLintResultLayout {} = True
    isInvalidForward GrinLintForwardedResultPlaced {} = True
    isInvalidForward _ = False

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

-- | Analyze each textual GRIN fixture, apply the rewrites of the points-to
-- analysis, and compare the normalized program and the number of rewrites
-- of each kind with the fixture. The result must lint. A fixture with
-- @analysis: skipped@ holds a form that the analysis refuses.
loadPointsToFixtures :: IO [TestTree]
loadPointsToFixtures = do
  root <- getEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/grin/test/Test/Fixtures/grin-points-to"
  paths <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure [testCase path (checkPointsToFixture (directory </> path)) | path <- paths]

checkPointsToFixture :: FilePath -> IO ()
checkPointsToFixture path = do
  decoded <- Y.decodeFileEither path
  case decoded of
    Left problem -> assertFailure (Y.prettyPrintParseException problem)
    Right value ->
      case parseEither parseFixture value of
        Left problem -> assertFailure problem
        Right (source, expectation) ->
          case GrinParser.parseProgram source of
            Left problem -> assertFailure (GrinParser.renderParseError problem)
            Right program ->
              case (analyzePointsTo program, expectation) of
                (Nothing, Nothing) -> pure ()
                (Nothing, Just _) -> assertFailure "the analysis refused the program"
                (Just _, Nothing) -> assertFailure "the analysis accepted a program that the fixture expects it to refuse"
                (Just analysis, Just (expected, expectedRewrites)) -> do
                  let (rewritten, rewrites) = rewriteWithPointsTo analysis program
                      normalized = normalizeGrinProgram rewritten
                      actual = T.strip (T.pack (renderString (layoutPretty defaultLayoutOptions (prettyProgram normalized))))
                  case lintProgram normalized of
                    [] -> pure ()
                    problems -> assertFailure ("the rewritten program does not lint: " <> show problems)
                  if actual == T.strip expected
                    then pure ()
                    else assertFailure ("output mismatch\nexpected:\n" <> T.unpack expected <> "\nactual:\n" <> T.unpack actual)
                  let counts = [rewritesDeadAlternatives rewrites, rewritesEvaluatedEvals rewrites, rewritesDirectCalls rewrites, rewritesSingleEntryEvals rewrites]
                  if counts == expectedRewrites
                    then pure ()
                    else assertFailure ("rewrite counts: expected " <> show expectedRewrites <> ", actual " <> show counts)
  where
    parseFixture = withObject "GRIN points-to fixture" $ \object -> do
      source <- object .: "program"
      status <- object .: "status"
      reason <- object .: "reason"
      analysis <- object .:? "analysis"
      expectation <-
        case analysis of
          Just ("skipped" :: Text) -> pure Nothing
          Just _ -> fail "invalid GRIN points-to fixture analysis"
          Nothing -> do
            expected <- object .: "expected"
            rewrites <- object .: "rewrites"
            counts <-
              withObject
                "rewrites"
                ( \counts ->
                    sequence
                      [ counts .: "dead-alternatives",
                        counts .: "evals-of-values",
                        counts .: "direct-calls",
                        counts .: "single-entry-evals"
                      ]
                )
                rewrites
            pure (Just (expected :: Text, counts :: [Int]))
      if status == ("pass" :: Text) && not (T.null reason)
        then pure (source :: Text, expectation)
        else fail "invalid GRIN points-to fixture status"

-- | Make the binding the one public global of a whole program, drop what it
-- does not reach, and run the points-to analysis and its rewrites, as a
-- whole-program build does.
optimizeWholeProgram :: Text -> GrinProgram -> Either String GrinProgram
optimizeWholeProgram binding program = do
  swept <-
    sweptGrinProgram
      program
        { grinGlobals = [global {grinGlobalVis = if grinGlobalName global == binding then GrinPub else GrinPrivate} | global <- grinGlobals program],
          grinConstructors = [constructor {grinConstructorVis = GrinPrivate} | constructor <- grinConstructors program]
        }
  case analyzePointsTo swept of
    Nothing -> Left "the points-to analysis refused the program"
    Just analysis -> finishGrinProgram (fst (rewriteWithPointsTo analysis swept))

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

-- | How the program of an evaluation fixture gets its core library.
data GrinEvalMode
  = -- | Lower the fixture alone and put it after the core GRIN.
    SeparateCore
  | -- | Merge the fixture with the System FC of the core library, keep what
    -- the binding reaches, lower the result as one program, and run the
    -- points-to analysis on it.
    WholeProgramPointsTo

evalFixtureTest :: IO GrinEvalEnvironment -> GrinEvalMode -> EvalFixture.EvalCase -> TestTree
evalFixtureTest getEnvironment mode fixture = testCase (EvalFixture.evalCaseId fixture) $ do
  environment <- getEnvironment
  (outcome, details) <-
    EvalFixture.evaluateEvalCase
      (grinEvalFrontend environment)
      (evaluateGrin mode environment)
      fixture
  case outcome of
    EvalFixture.OutcomePass -> pure ()
    EvalFixture.OutcomeXFail -> pure ()
    EvalFixture.OutcomeXPass -> assertFailure ("unexpected pass: " <> details)
    EvalFixture.OutcomeFail -> assertFailure details

-- | The program writes its standard output to the given handle.
evaluateGrin :: GrinEvalMode -> GrinEvalEnvironment -> EvalFixture.ProgramEvaluator
evaluateGrin mode environment output name program =
  case prepareEvalProgram name program of
    Left problem -> pure (Left (EvalFixture.EvaluationError problem))
    Right (prepared, unwrapResult) ->
      case mode of
        SeparateCore -> evaluateSeparate prepared unwrapResult
        WholeProgramPointsTo -> evaluateWhole prepared unwrapResult
  where
    evaluateSeparate prepared unwrapResult =
      case lowerProgram prepared of
        Left problem -> pure (Left (EvalFixture.EvaluationError problem))
        Right fixtureProgram ->
          case lintProgram fixtureProgram of
            [] -> run unwrapResult fixtureProgram (appendGrinProgram (grinEvalCore environment) fixtureProgram)
            problems -> pure (Left (EvalFixture.EvaluationError ("GRIN lint error: " <> show problems)))
    -- The fixture must lower on its own first, as a module does in a build
    -- that is not whole-program. Pruning would otherwise hide an error in a
    -- declaration that the binding does not use.
    evaluateWhole prepared unwrapResult =
      case ([Fc.valName declaration | Fc.DeclVal declaration <- Fc.programDecls prepared, Fc.nameText (Fc.valName declaration) == name], lowerProgram prepared) of
        (_, Left problem) -> pure (Left (EvalFixture.EvaluationError problem))
        (root : _, Right _) -> do
          let merged = Fc.pruneProgram [root] (Fc.mergePrograms [EvalFixture.evalEnvironmentProgram (grinEvalFrontend environment), prepared])
          case lowerProgram merged of
            Left problem -> pure (Left (EvalFixture.EvaluationError problem))
            Right lowered -> do
              let binding = bindingName name lowered
              case optimizeWholeProgram binding lowered of
                Left problem -> pure (Left (EvalFixture.EvaluationError problem))
                Right optimized ->
                  case lintProgram optimized of
                    [] -> run unwrapResult optimized optimized
                    problems -> pure (Left (EvalFixture.EvaluationError ("GRIN lint error after the points-to rewrites: " <> show problems)))
        ([], Right _) -> pure (Left (EvalFixture.EvaluationError ("missing evaluation binding " <> T.unpack name)))
    run unwrapResult named whole =
      fmap unwrapResult . classifyResult <$> interpreter streams (bindingName name named) whole
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
