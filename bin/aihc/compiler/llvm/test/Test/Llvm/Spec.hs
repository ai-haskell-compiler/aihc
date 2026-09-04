{-# LANGUAGE OverloadedStrings #-}

module Test.Llvm.Spec (tests) where

import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Llvm.Lir (compileLirModule)
import Aihc.Native (NativeTarget (Llvm))
import System.Info (arch, os)
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree)

-- | The LLVM backend compiles for the host, so the programs run wherever
-- Clang runs. The snapshot fixtures carry one allocation count per host.
tests :: IO TestTree
tests =
  NativeSuite.tests
    NativeBackend
      { backendName = "aihc-llvm",
        backendTarget = Llvm,
        backendLowerTarget = posixTarget64,
        backendClangArguments = ["-Wno-override-module"],
        backendRuns = (arch == "aarch64" && os == "darwin") || (arch == "x86_64" && os == "linux"),
        backendAllocationKey = if os == "darwin" then "macos-arm64" else "linux-amd64",
        backendSourceExtension = ".ll",
        backendCompile = either (Left . show) (Right . BackendSource) . compileLirModule
      }
