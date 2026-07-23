{-# LANGUAGE OverloadedStrings #-}

module Test.Llvm.Suite (tests) where

import Aihc.Grin (GcGrinProgram, lintProgram, lowerGc, toCpsGrin)
import Aihc.Grin.Gc (gcGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Llvm (compileProgram, validatePrimitiveNames)
import Aihc.Native (runtimeSourcePath, supportedNativePrimitiveNames)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Aihc.Testing.SchedulerProgram (schedulerProgram, stdioSchedulerProgram)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "LLVM backend"
    [ testCase "emits verified guaranteed tail calls" testGuaranteedTailCalls,
      testCase "accepts every shared native primitive" $
        assertEqual "LLVM primitive coverage" (Right ()) (validatePrimitiveNames supportedNativePrimitiveNames),
      testCase "lowers byte-array primitives" testByteArrayPrimitives,
      testCase "executes Int# addition" (testProgram "*" intAddProgram),
      testCase "executes cooperative scheduling" (testProgram "PCAB" schedulerProgram)
    ]

testGuaranteedTailCalls :: IO ()
testGuaranteedTailCalls = do
  source <- compile schedulerProgram
  forM_
    [ "define internal tailcc void",
      "musttail call tailcc void",
      "call { ptr, ptr } @aihc_portable_yield_cps"
    ]
    (\needle -> assertBool ("missing generated LLVM fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  assertBool "LLVM backend does not emit the portable-C trampoline" (not ("while (aihc_next_transfer.entry" `T.isInfixOf` source))
  verifyModule source

testByteArrayPrimitives :: IO ()
testByteArrayPrimitives = do
  source <- compile stdioSchedulerProgram
  forM_
    [ "declare ptr @aihc_byte_array_new_pinned(i64)",
      "declare ptr @aihc_byte_array_contents(ptr)",
      "call ptr @aihc_byte_array_new_pinned",
      "call ptr @aihc_byte_array_contents"
    ]
    (\needle -> assertBool ("missing generated LLVM fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  verifyModule source

verifyModule :: T.Text -> IO ()
verifyModule source =
  withTempDirectory "aihc-llvm-verify" $ \directory -> do
    let sourcePath = directory </> "program.ll"
        objectPath = directory </> "program.o"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ["-Wno-override-module", "-c", sourcePath, "-o", objectPath]
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit

intAddProgram :: GrinProgram
intAddProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "+#" 30 IntRep, 2)],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [sumValue] (GrinPrimitiveCall IntRep "+#" [intLiteral 40, intLiteral 2]) $
                  GrinCase
                    (GrinVarValue sumValue)
                    caseBinder
                    [ outputAlternative (GrinLitAlt (GrinLitInt IntRep 42)) '*' successOutput,
                      outputAlternative GrinDefaultAlt '?' failureOutput
                    ]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$int_add_main"
    mainClosure = GrinVar "main" 31 lifted
    sumValue = GrinVar "sum" 32 IntRep
    caseBinder = GrinVar "case_binder" 33 IntRep
    successOutput = GrinVar "success_output" 34 Int32Rep
    failureOutput = GrinVar "failure_output" 35 Int32Rep
    unitValue = GrinVar "()" 36 lifted
    intLiteral = GrinLitValue . GrinLitInt IntRep
    outputAlternative constructor character output =
      GrinAlt
        { grinAltCon = constructor,
          grinAltBinders = [],
          grinAltRhs =
            GrinBind [output] (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum character)))]) $
              GrinConstant [GrinVarValue unitValue]
        }

putcharCall :: GrinForeignCall
putcharCall =
  GrinForeignCall
    { grinForeignCallName = "$ffi$putchar",
      grinForeignCallSymbol = "putchar",
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = [GrinForeignInt32],
            grinForeignResultType = GrinForeignInt32,
            grinForeignEffect = GrinForeignPure
          }
    }

testProgram :: String -> GrinProgram -> IO ()
testProgram expected program = do
  source <- compile program
  withTempDirectory "aihc-llvm" $ \directory -> do
    runtime <- runtimeSourcePath
    let sourcePath = directory </> "program.ll"
        executablePath = directory </> "program"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ["-std=c11", "-Wall", "-Wextra", "-Werror", "-Wno-override-module", runtime, sourcePath, "-o", executablePath]
        ""
    assertEqual ("clang rejected generated LLVM IR:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("generated program stderr: " <> programErr) ExitSuccess programExit
    assertEqual "generated program stdout" expected programOut

compile :: GrinProgram -> IO T.Text
compile program = do
  assertEqual "direct GRIN lint" [] (lintProgram program)
  let gcProgram = expectGcGrin program
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gcProgram))
  case compileProgram "main" gcProgram of
    Right source -> pure source
    Left err -> assertFailure ("LLVM lowering failed: " <> show err)

expectGcGrin :: GrinProgram -> GcGrinProgram
expectGcGrin program =
  case toCpsGrin program of
    Right cpsProgram -> lowerGc cpsProgram
    Left err -> error ("test GRIN failed CPS conversion: " <> show err)

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
