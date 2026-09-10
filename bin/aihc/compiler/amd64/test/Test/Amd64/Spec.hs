{-# LANGUAGE OverloadedStrings #-}

module Test.Amd64.Spec (tests) where

import Aihc.Amd64.Lir (compileLirObjectWith, compileLirStatementsWith)
import Aihc.Amd64.Text (renderAmd64Statements)
import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Lir.Optimization (OptimizationLevel (..), renderOptimizationLevel)
import Aihc.Native (NativeTarget (LinuxAmd64))
import System.Environment (lookupEnv)
import System.Info (arch, os)
import Test.Lir.AsmSuite (AsmBackend (..))
import Test.Lir.AsmSuite qualified as AsmSuite
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree, testGroup)

-- | The suites run at each optimization level. Level 0 skips the optional
-- passes, so its programs must behave like the level 2 programs and its
-- assembly has a golden of its own.
tests :: IO TestTree
tests = do
  suites <- mapM levelTests [minBound .. maxBound]
  pure (testGroup "amd64" suites)

levelTests :: OptimizationLevel -> IO TestTree
levelTests level = do
  native <- nativeTests level
  assembly <- assemblyTests level
  pure (testGroup ("-O" <> renderOptimizationLevel level) [native, assembly])

assemblyTests :: OptimizationLevel -> IO TestTree
assemblyTests level =
  AsmSuite.tests
    AsmBackend
      { asmBackendName = "aihc-amd64",
        asmBackendExtension = levelExtension level <> ".amd64.s",
        asmBackendRender = either (Left . show) (Right . renderAmd64Statements) . compileLirStatementsWith level
      }

-- | The golden of the default level keeps its name.
levelExtension :: OptimizationLevel -> String
levelExtension level =
  case level of
    O2 -> ""
    _ -> ".O" <> renderOptimizationLevel level

-- | The linked programs run on a Linux AMD64 host. Set @AIHC_RUN_AMD64=1@
-- to run them elsewhere, with @clang@ and @ar@ on the path that compile and
-- link inside a Linux AMD64 container and a linked program that runs there.
nativeTests :: OptimizationLevel -> IO TestTree
nativeTests level = do
  forced <- (== Just "1") <$> lookupEnv "AIHC_RUN_AMD64"
  NativeSuite.tests
    NativeBackend
      { backendName = "aihc-amd64",
        backendTarget = LinuxAmd64,
        backendLowerTarget = posixTarget64,
        backendClangArguments = ["--target=x86_64-unknown-linux-gnu"],
        backendRuns = (arch == "x86_64" && os == "linux") || forced,
        backendOptimized = level == O2,
        backendAllocationKey = "linux-amd64",
        backendSourceExtension = ".o",
        backendCompile = either (Left . show) (Right . BackendObject) . compileLirObjectWith level
      }
