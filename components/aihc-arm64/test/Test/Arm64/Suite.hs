{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Suite
  ( tests,
  )
where

import Aihc.Arm64 (Arm64Error (..), buildLinkLayout, compileModule, compileProgram, runtimeSourcePath, validateProgramPrimitives)
import Aihc.Arm64.Emit (renderAllocatedBlock)
import Aihc.Arm64.Lir
import Aihc.Arm64.RegisterAllocate
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), Unique (..))
import Aihc.Testing.EvalFixture (EvalCase (..), compileEvalCase, evalBindingName, loadEvalCases)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (find, isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-arm64"
    [ testCase "linear scan reuses an expired register" $ do
        let first = VirtualReg 0
            second = VirtualReg 1
            allocation =
              allocateBlock
                [ MoveImmediate (Virtual first) 1,
                  Move (Physical X0) (Virtual first),
                  MoveImmediate (Virtual second) 2,
                  Move (Physical X1) (Virtual second)
                ]
        assertEqual
          "locations"
          (Map.fromList [(first, InRegister X9), (second, InRegister X9)])
          (allocationLocations allocation),
      testCase "linear scan spills into a heap-frame slot" $ do
        let registers = map VirtualReg [0 .. 7]
            definitions = [MoveImmediate (Virtual register) (fromIntegral index) | (index, register) <- zip [0 :: Int ..] registers]
            uses = [Move (Physical X0) (Virtual register) | register <- registers]
            instructions = definitions <> uses
            allocation = allocateBlock instructions
        assertEqual "one spill" 1 (allocationSpillCount allocation)
        assertBool "heap spill assigned" (InHeapSpill 0 `elem` Map.elems (allocationLocations allocation))
        case renderAllocatedBlock 10 instructions of
          Left err -> assertFailure ("failed to emit allocated block: " <> show err)
          Right (assembly, spillCount) -> do
            assertEqual "emitted spill count" 1 spillCount
            assertBool "spill stored in heap frame" ("  str x8, [x19, #80]" `elem` assembly)
            assertBool "spill loaded from heap frame" ("  ldr x8, [x19, #80]" `elem` assembly),
      testCase "linear scan spills values live across C calls" $ do
        let register = VirtualReg 0
            allocation =
              allocateBlock
                [ MoveImmediate (Virtual register) 1,
                  Call "_external",
                  Move (Physical X0) (Virtual register)
                ]
        assertEqual "call-spanning location" (Just (InHeapSpill 0)) (Map.lookup register (allocationLocations allocation)),
      testCase "rejects unresolved representation-polymorphic native layouts" $ do
        let runtimeRep = RuntimeRepVar (Unique 1)
            program =
              GrinProgram
                { grinConstructors = [("Box", [runtimeRep])],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinIoBindings = mempty,
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions = []
                }
        assertEqual
          "native representation diagnostic"
          (Left (Arm64UnsupportedRuntimeRep runtimeRep))
          (compileProgram "missing" program),
      testCase "keeps unsupported dormant primitives out of linked programs" $ do
        let primitive = GrinVar "+#" 1 (BoxedRep Lifted)
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [(primitive, 2)],
                  grinForeignCalls = [],
                  grinIoBindings = mempty,
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions = []
                }
        assertEqual "linked primitive validation" (Left (Arm64UnsupportedPrimitive "+#")) (validateProgramPrimitives program)
        case compileModule (buildLinkLayout [program]) "_aihc_init_test" program of
          Left err -> assertFailure ("relocatable module rejected a dormant primitive: " <> show err)
          Right assembly -> assertBool "module initializer" (".globl _aihc_init_test" `T.isInfixOf` assembly),
      testCase "canonicalizes narrow signed literals in machine-word slots" $ do
        let functionName = FunctionName "narrow_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinIoBindings = mempty,
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = Int8Rep,
                          grinFunctionBody = GrinReturn [GrinLitValue (GrinLitInt Int8Rep 255)]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_narrow" program of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> assertBool "255 :: Int8# is stored as -1" ("ldr x0, =-1" `T.isInfixOf` assembly),
      testCase "returns unboxed tuples as direct machine values" $ do
        let functionName = FunctionName "pair_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinIoBindings = mempty,
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = TupleRep [TupleRep [], IntRep, WordRep],
                          grinFunctionBody =
                            GrinReturn
                              [ GrinLitValue (GrinLitInt IntRep 1),
                                GrinLitValue (GrinLitInt WordRep 2)
                              ]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_pair" program of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> do
            assertBool "returns two values" ("ldr x1, =2" `T.isInfixOf` assembly)
            assertBool "uses the multi-value return ABI" ("bl _aihc_return_values" `T.isInfixOf` assembly)
            assertBool "does not allocate an aggregate node" (not ("bl _aihc_make_node" `T.isInfixOf` assembly)),
      testCase "compiles standalone HelloWorld GRIN to native ARM64" testNativeHelloWorld
    ]

testNativeHelloWorld :: IO ()
testNativeHelloWorld = do
  cases <- loadEvalCases
  evalCase <-
    case find ((== "native-hello-world.yaml") . evalCaseId) cases of
      Just value -> pure value
      Nothing -> assertFailure "native HelloWorld fixture is missing"
  compileResult <- compileEvalCase evalCase
  fcProgram <-
    case compileResult of
      Right value -> pure value
      Left err -> assertFailure ("HelloWorld failed before GRIN lowering: " <> err)
  let grinProgram = lowerProgram fcProgram
  assertBool "GRIN has no unboxed-tuple nodes" (not ("(#,#)" `isInfixOf` renderProgram grinProgram))
  assembly <-
    case compileProgram evalBindingName grinProgram of
      Right value -> pure value
      Left err -> assertFailure ("ARM64 lowering failed: " <> show err)
  let rendered = T.unpack assembly
  assertBool "emits indirect Haskell tail transfers" ("br x9" `isInfixOf` rendered)
  assertBool "never calls a generated Haskell entry" (not ("bl .Laihc_function_" `isInfixOf` rendered))
  assertBool "emits unboxed literals as raw words" (not ("_aihc_make_literal" `isInfixOf` rendered))
  when (arch == "aarch64" && os == "darwin") $
    runHelloWorldAssembly assembly

runHelloWorldAssembly :: T.Text -> IO ()
runHelloWorldAssembly assembly =
  withTempDirectory "aihc-arm64-hello" $ \directory -> do
    runtime <- runtimeSourcePath
    let assemblyPath = directory </> "hello.s"
        executablePath = directory </> "hello"
    writeFile assemblyPath (T.unpack assembly)
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode "clang" ["-std=c11", "-Wall", "-Wextra", "-Werror", runtime, assemblyPath, "-o", executablePath] ""
    case clangExit of
      ExitSuccess -> pure ()
      ExitFailure _ -> assertFailure ("clang failed to assemble HelloWorld:\n" <> clangErr)
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("native stderr: " <> programErr) ExitSuccess programExit
    assertEqual "native stdout" "Hello, world!\n" programOut

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
