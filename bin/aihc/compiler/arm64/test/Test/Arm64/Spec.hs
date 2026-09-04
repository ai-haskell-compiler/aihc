{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Spec (tests) where

import Aihc.Arm64.Lir (compileLirObject)
import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Native (NativeTarget (AppleArm64))
import System.Info (arch, os)
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree)

tests :: IO TestTree
tests =
  NativeSuite.tests
    NativeBackend
      { backendName = "aihc-arm64",
        backendTarget = AppleArm64,
        backendLowerTarget = posixTarget64,
        backendClangArguments = ["--target=arm64-apple-darwin"],
        backendRuns = arch == "aarch64" && os == "darwin",
        backendAllocationKey = "macos-arm64",
        backendSourceExtension = ".o",
        backendCompile = either (Left . show) (Right . BackendObject) . compileLirObject
      }
