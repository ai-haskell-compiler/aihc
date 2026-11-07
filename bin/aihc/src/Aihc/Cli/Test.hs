-- | The @test@ command: a build of the test suites of a package, as
-- "Aihc.Cli.Build" builds executables, and then a run of each of them.
--
-- A test suite runs in the directory of its package, as @cabal test@ runs
-- it, with the arguments of @--test-option@. Its output goes to the
-- streams of the command. The command fails when a test suite fails.
module Aihc.Cli.Test
  ( TestSuiteResult (..),
    buildTests,
    runTestSuites,
    renderTestSummary,
    runTest,
    testBuildOptions,
  )
where

import Aihc.Cli.Build (BuiltProgram (..), buildTestSuites)
import Aihc.Cli.Options (BuildOptions (..), PlanOptions (..), TestOptions (..))
import Aihc.Cli.Progress (ProgressReporter, withProgress)
import Aihc.PackagePlan.Project (Project (..), ProjectPackage (..), selectProject)
import Control.Monad (forM, unless, when)
import Data.Map.Strict qualified as Map
import GHC.IO.Handle (hDuplicate)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, hFlush, hPutStrLn, stderr, stdout)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

-- | The exit code of one test suite.
data TestSuiteResult = TestSuiteResult
  { resultName :: !String,
    resultExitCode :: !ExitCode
  }
  deriving (Eq, Show)

-- | Build with the progress on stderr, then run each test suite with its
-- output on stdout. The command fails when a test suite fails.
runTest :: TestOptions -> IO ()
runTest options = do
  programs <- withProgress stderr (`buildTests` options)
  results <- runTestSuites stdout (testArguments options) programs
  putStrLn (renderTestSummary results)
  unless (all ((== ExitSuccess) . resultExitCode) results) exitFailure

-- | Build the test suites that the input names. @all@ builds the test
-- suites of every package of the project, one package after another, and
-- passes over a package without a test suite. Another input builds the
-- test suites of its package, and a package without a buildable test
-- suite is an error.
buildTests :: ProgressReporter -> TestOptions -> IO [BuiltProgram]
buildTests reporter options
  | testInput options == "all" = do
      project <- selectProject (planProject (testPlanOptions options))
      found <- maybe (ioError (userError "all names every package of a cabal.project, and there is no project")) pure project
      let withTests = [info | info <- Map.elems (projectPackages found), not (null (projectPackageTestSuites info))]
      concat <$> forM withTests (\info -> buildTestSuites reporter (testBuildOptions options) {buildInput = projectPackageDirectory info})
  | otherwise = do
      programs <- buildTestSuites reporter (testBuildOptions options)
      when (null programs) $
        ioError (userError (testInput options <> " has no buildable test suite"))
      pure programs

-- | Run each test suite in the directory of its package, with its output
-- on the handle, and give its exit code. The handle is flushed before each
-- run, so that the lines of the command and of the test suite keep their
-- order.
runTestSuites :: Handle -> [String] -> [BuiltProgram] -> IO [TestSuiteResult]
runTestSuites handle arguments programs =
  forM programs $ \program -> do
    let name = builtName program
    hPutStrLn handle ("Test suite " <> name <> ": RUNNING...")
    hFlush handle
    -- The process closes a handle it receives, so it receives a copy. The
    -- standard streams of the command are inherited instead.
    out <- stream
    err <- stream
    (_, _, _, process) <-
      createProcess
        (proc (builtOutput program) arguments)
          { cwd = Just (builtPackageRoot program),
            std_out = out,
            std_err = err
          }
    code <- waitForProcess process
    hPutStrLn handle ("Test suite " <> name <> ": " <> renderExitCode code)
    hFlush handle
    pure (TestSuiteResult name code)
  where
    stream
      | handle == stdout = pure Inherit
      | otherwise = UseHandle <$> hDuplicate handle

renderExitCode :: ExitCode -> String
renderExitCode code =
  case code of
    ExitSuccess -> "PASS"
    ExitFailure number -> "FAIL (exit code " <> show number <> ")"

-- | The count of the test suites that passed.
renderTestSummary :: [TestSuiteResult] -> String
renderTestSummary results =
  show passed <> " of " <> show (length results) <> " test suite" <> (if length results == 1 then "" else "s") <> " passed."
  where
    passed = length [() | TestSuiteResult _ ExitSuccess <- results]

-- | The build of the test suites that the options describe. The selected
-- test suites take the place of the executables.
testBuildOptions :: TestOptions -> BuildOptions
testBuildOptions options =
  BuildOptions
    { buildInput = testInput options,
      buildSourceDirectories = [],
      buildPackageConstraints = [],
      buildTarget = testTarget options,
      buildStoreRoot = testStoreRoot options,
      buildBuildRoot = testBuildRoot options,
      buildWorkspace = testWorkspace options,
      buildKeepCore = testKeepCore options,
      buildKeepGrin = testKeepGrin options,
      buildKeepLir = testKeepLir options,
      buildKeepNative = testKeepNative options,
      buildLint = testLint options,
      buildCheckPrimBounds = testCheckPrimBounds options,
      buildProfileAllocations = False,
      buildProfileTime = False,
      buildLto = testLto options,
      buildOptimization = testOptimization options,
      buildNoLink = False,
      buildOnlyDependencies = False,
      buildNoPie = False,
      buildVerbose = testVerbose options,
      buildPrintTimings = False,
      buildOutput = Nothing,
      buildExecutables = testSuites options,
      buildPlanOptions = testPlanOptions options
    }
