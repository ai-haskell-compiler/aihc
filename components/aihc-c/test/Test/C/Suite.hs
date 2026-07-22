{-# LANGUAGE OverloadedStrings #-}

module Test.C.Suite (tests) where

import Aihc.C (CError (..), compileModule, compileProgram, compileProgramWithDependencies, validateProgramPrimitives)
import Aihc.Grin (GcGrinProgram, lintProgram, lowerGc, toCpsGrin)
import Aihc.Grin.Gc (gcGrinProgram)
import Aihc.Grin.Syntax
import Aihc.Native (buildLinkLayout, runtimeSourcePath)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Portable C backend"
    [ testCase "emits a portable trampoline" testTrampoline,
      testCase "links separately compiled C units" testIncrementalProgram,
      testCase "traps dormant unsupported primitives in compiled modules" testDormantPrimitive,
      testCase "executes Int# addition" (testProgram "*" (intAddProgram 40 2 42)),
      testCase "wraps overflowing Int# addition" (testProgram "*" (intAddProgram 9223372036854775807 1 (-9223372036854775808))),
      testCase "executes cooperative scheduling" (testProgram "PCAB" schedulerProgram),
      testCase "executes blackhole wakeups" (testProgram "TA" blackholeSchedulerProgram)
    ]

intAddProgram :: Integer -> Integer -> Integer -> GrinProgram
intAddProgram left right expected =
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
                GrinBind [sumValue] (GrinPrimitiveCall IntRep "+#" [intLiteral left, intLiteral right]) $
                  GrinCase
                    (GrinVarValue sumValue)
                    caseBinder
                    [ outputAlternative (GrinLitAlt (GrinLitInt IntRep expected)) '*' successOutput,
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

testTrampoline :: IO ()
testTrampoline = do
  source <- compile schedulerProgram
  forM_
    [ "while (aihc_next_transfer.entry != NULL)",
      "aihc_next_transfer = aihc_portable_fork_cps",
      "aihc_next_transfer = aihc_portable_yield_cps",
      "extern int32_t putchar(int32_t)"
    ]
    (\needle -> assertBool ("missing generated C fragment: " <> T.unpack needle) (needle `T.isInfixOf` source))
  assertBool "generated C does not use a machine argument field" (not ("aihc_machine->args" `T.isInfixOf` source))
  assertBool "portable C owns a static reusable argument buffer" ("static AihcSlot aihc_arguments[" `T.isInfixOf` source)
  let wideGcProgram = expectGcGrin wideArgumentProgram
      wideLayout = buildLinkLayout [wideArgumentProgram]
  wideSource <-
    case compileProgramWithDependencies wideLayout [] "main" wideGcProgram of
      Right generated -> pure generated
      Left err -> assertFailure ("wide-argument C lowering failed: " <> show err)
  assertBool "argument buffer includes the CPS continuation" ("static AihcSlot aihc_arguments[5];" `T.isInfixOf` wideSource)
  runtime <- readFile =<< runtimeSourcePath
  assertBool "runtime does not contain a machine argument field" (not ("machine->args" `isInfixOf` runtime))

wideArgumentProgram :: GrinProgram
wideArgumentProgram =
  schedulerProgram
    { grinFunctions =
        grinFunctions schedulerProgram
          <> [ GrinFunction
                 { grinFunctionName = FunctionName "$wide_arguments",
                   grinFunctionLinkName = Nothing,
                   grinFunctionParameters = parameters,
                   grinFunctionResultRep = Int32Rep,
                   grinFunctionBody = GrinConstant [GrinVarValue firstParameter]
                 }
             ]
    }
  where
    firstParameter = GrinVar "argument_0" 50 Int32Rep
    parameters = firstParameter : [GrinVar ("argument_" <> T.pack (show index)) (50 + index) Int32Rep | index <- [1 .. 3]]

testIncrementalProgram :: IO ()
testIncrementalProgram = do
  let initializer = "aihc_init_dependency"
      layout = buildLinkLayout [incrementalDependencyProgram, incrementalMainProgram]
      dependencyGc = expectGcGrin incrementalDependencyProgram
      mainGc = expectGcGrin incrementalMainProgram
  dependencySource <-
    case compileModule layout initializer dependencyGc of
      Right source -> pure source
      Left err -> assertFailure ("dependency C lowering failed: " <> show err)
  mainSource <-
    case compileProgramWithDependencies layout [initializer] "main" mainGc of
      Right source -> pure source
      Left err -> assertFailure ("main C lowering failed: " <> show err)
  assertBool "exports the dependency initializer" (("void " <> initializer <> "(void)") `T.isInfixOf` dependencySource)
  assertBool "calls the dependency initializer" ((initializer <> "();") `T.isInfixOf` mainSource)
  withTempDirectory "aihc-c-incremental" $ \directory -> do
    runtime <- runtimeSourcePath
    let dependencyPath = directory </> "dependency.c"
        mainPath = directory </> "main.c"
        executablePath = directory </> "program"
    TIO.writeFile dependencyPath dependencySource
    TIO.writeFile mainPath mainSource
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        [ "-std=c11",
          "-Wall",
          "-Wextra",
          "-Werror",
          "-I" <> takeDirectory runtime,
          runtime,
          dependencyPath,
          mainPath,
          "-o",
          executablePath
        ]
        ""
    assertEqual ("clang rejected incremental generated C:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("incremental program stderr: " <> programErr) ExitSuccess programExit
    assertEqual "incremental program stdout" "D" programOut

testDormantPrimitive :: IO ()
testDormantPrimitive = do
  let result = GrinVar "result" 51 IntRep
      program =
        GrinProgram
          { grinConstructors = [],
            grinPrimitives = [(GrinVar "unsupported#" 50 IntRep, 1)],
            grinForeignCalls = [],
            grinExternalGlobals = [],
            grinExternalFunctions = [],
            grinWhnfGlobals = [],
            grinCafs = [],
            grinFunctions =
              [ GrinFunction
                  { grinFunctionName = FunctionName "dormant_unsupported",
                    grinFunctionLinkName = Nothing,
                    grinFunctionParameters = [],
                    grinFunctionResultRep = IntRep,
                    grinFunctionBody =
                      GrinBind
                        [result]
                        (GrinPrimitiveCall IntRep "unsupported#" [GrinLitValue (GrinLitInt IntRep 1)])
                        (GrinConstant [GrinVarValue result])
                  }
              ]
          }
      layout = buildLinkLayout [program]
  assertEqual "linked primitive validation" (Left (CUnsupportedPrimitive "unsupported#")) (validateProgramPrimitives program)
  case compileModule layout "aihc_init_dormant" (expectGcGrin program) of
    Left err -> assertFailure ("dormant primitive module lowering failed: " <> show err)
    Right source -> assertBool "emits the runtime trap" ("aihc_unsupported_primitive();" `T.isInfixOf` source)

incrementalDependencyProgram :: GrinProgram
incrementalDependencyProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = incrementalDependencyFunction,
              grinFunctionLinkName = Just "dependency",
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [GrinVar "output" 41 Int32Rep]
                  (GrinForeignCallExpr putcharCall [GrinLitValue (GrinLitInt Int32Rep 68)])
                  (GrinConstant [GrinVarValue (GrinVar "()" 42 (BoxedRep Lifted))])
            }
        ]
    }

incrementalMainProgram :: GrinProgram
incrementalMainProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions =
        [ GrinCodeInfo
            { grinCodeSourceName = "dependency",
              grinCodeFunctionName = incrementalDependencyFunction,
              grinCodeParameterLayouts = [],
              grinCodeResultRep = BoxedRep Lifted
            }
        ],
      grinWhnfGlobals = [(GrinVar "main" 43 (BoxedRep Lifted), GrinNode (GrinClosure incrementalMainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = incrementalMainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinCall (BoxedRep Lifted) incrementalDependencyFunction []
            }
        ]
    }

incrementalDependencyFunction, incrementalMainFunction :: FunctionName
incrementalDependencyFunction = FunctionName "$entry$dependency"
incrementalMainFunction = FunctionName "$incremental_main"

testProgram :: String -> GrinProgram -> IO ()
testProgram expected program = do
  source <- compile program
  withTempDirectory "aihc-c" $ \directory -> do
    runtime <- runtimeSourcePath
    let sourcePath = directory </> "program.c"
        executablePath = directory </> "program"
    TIO.writeFile sourcePath source
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        [ "-std=c11",
          "-Wall",
          "-Wextra",
          "-Werror",
          "-I" <> takeDirectory runtime,
          runtime,
          sourcePath,
          "-o",
          executablePath
        ]
        ""
    assertEqual ("clang rejected generated C:\n" <> clangErr) ExitSuccess clangExit
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
    Left err -> assertFailure ("C lowering failed: " <> show err)

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
