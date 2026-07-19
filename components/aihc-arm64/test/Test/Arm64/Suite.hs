{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Suite
  ( tests,
  )
where

import Aihc.Arm64
  ( Arm64Error (..),
    ObservedProgram (..),
    buildLinkLayout,
    compileModule,
    compileObservedFunction,
    compileProgram,
    runtimeSourcePath,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
  )
import Aihc.Arm64.Emit (renderAllocatedBlock)
import Aihc.Arm64.Lir
import Aihc.Arm64.RegisterAllocate
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), Unique (..))
import Aihc.Testing.EvalFixture (EvalCase (..), compileEvalCase, evalBindingName, loadEvalCases)
import Aihc.Testing.GrinProgram (parseProgram)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.List (find, isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Info (arch, os)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-arm64"
    [ testCase "linear scan spills into a heap-frame slot" $ do
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
      testCase "rejects unresolved representation-polymorphic native layouts" $ do
        let runtimeRep = RuntimeRepVar (Unique 1)
            program =
              GrinProgram
                { grinConstructors = [("Box", [[runtimeRep]])],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions = []
                }
        assertEqual
          "native representation diagnostic"
          (Left (Arm64UnsupportedRuntimeRep runtimeRep))
          (compileProgram "missing" (expectCpsGrin program)),
      testCase "keeps unsupported dormant primitives out of linked programs" $ do
        let primitive = GrinVar "+#" 1 (BoxedRep Lifted)
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [(primitive, 2)],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions = []
                }
        assertEqual "linked primitive validation" (Left (Arm64UnsupportedPrimitive "+#")) (validateProgramPrimitives program)
        case compileModule (buildLinkLayout [program]) "_aihc_init_test" (expectCpsGrin program) of
          Left err -> assertFailure ("relocatable module rejected a dormant primitive: " <> show err)
          Right assembly -> assertBool "module initializer" (".globl _aihc_init_test" `T.isInfixOf` assembly),
      testCase "canonicalizes narrow signed literals in machine-word slots" $ do
        let functionName = FunctionName "narrow_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionLinkName = Nothing,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = Int8Rep,
                          grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt Int8Rep 255)]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_narrow" (expectCpsGrin program) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> assertBool "255 :: Int8# is stored as -1" ("ldr x0, =-1" `T.isInfixOf` assembly),
      testCase "passes static Addr# literals to native foreign calls" $ do
        let functionName = FunctionName "puts_addr"
            foreignCall =
              GrinForeignCall
                { grinForeignCallName = "$ffi$puts",
                  grinForeignCallSymbol = "puts",
                  grinForeignCallSignature =
                    GrinForeignSignature
                      { grinForeignArgumentTypes = [GrinForeignAddr],
                        grinForeignResultType = GrinForeignInt32,
                        grinForeignEffect = GrinForeignPure
                      }
                }
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [foreignCall],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionLinkName = Nothing,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = Int32Rep,
                          grinFunctionBody = GrinForeignCallExpr foreignCall [GrinLitValue (GrinLitAddr "hello\n")]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_addr" (expectCpsGrin program) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> do
            assertBool "loads the static string address" ("adrp x0, .Laihc_addr_0@PAGE" `T.isInfixOf` assembly)
            assertBool "emits NUL-terminated UTF-8" (".byte 104, 101, 108, 108, 111, 10, 0" `T.isInfixOf` assembly)
            assertBool "calls puts" ("bl _puts" `T.isInfixOf` assembly),
      testCase "returns unboxed tuples as direct machine values" $ do
        let functionName = FunctionName "pair_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionLinkName = Nothing,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = TupleRep [TupleRep [], IntRep, WordRep],
                          grinFunctionBody =
                            GrinConstant
                              [ GrinLitValue (GrinLitInt IntRep 1),
                                GrinLitValue (GrinLitInt WordRep 2)
                              ]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_pair" (expectCpsGrin program) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> do
            assertBool "passes two values" ("ldr x2, =2" `T.isInfixOf` assembly)
            assertBool "uses the explicit continuation ABI" ("bl _aihc_continue_values" `T.isInfixOf` assembly),
      testCase "exports stable entries and branches directly to dependency code" $ do
        let identityName = FunctionName "$entry$identity"
            callerName = FunctionName "$entry$caller"
            argument = GrinVar "argument" 1 (BoxedRep Lifted)
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions =
                    [ GrinCodeInfo
                        { grinCodeSourceName = "identity",
                          grinCodeFunctionName = identityName,
                          grinCodeParameterLayouts = [[BoxedRep Lifted]],
                          grinCodeResultRep = BoxedRep Lifted
                        }
                    ],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = callerName,
                          grinFunctionLinkName = Just "caller",
                          grinFunctionParameters = [argument],
                          grinFunctionResultRep = BoxedRep Lifted,
                          grinFunctionBody = GrinCall (BoxedRep Lifted) identityName [GrinVarValue argument]
                        }
                    ]
                }
        case compileModule (buildLinkLayout [program]) "_aihc_init_direct_call" (expectCpsGrin program) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> do
            assertBool "exports caller entry" (".globl _aihc_entry_63_61_6c_6c_65_72_" `T.isInfixOf` assembly)
            assertBool "branches to dependency entry" ("b _aihc_entry_69_64_65_6e_74_69_74_79_" `T.isInfixOf` assembly),
      testGroup "raw GRIN heap snapshots" (map snapshotTest snapshotCases),
      testCase "case and apply never evaluate operands implicitly" $ do
        case compileModule (buildLinkLayout [explicitEvaluationProgram]) "_aihc_init_explicit_eval" (expectCpsGrin explicitEvaluationProgram) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly ->
            assertBool "generated case and apply contain no direct-style eval call" (not ("bl _aihc_eval\n" `T.isInfixOf` assembly))
        runtime <- readFile =<< runtimeSourcePath
        assertBool
          "runtime apply does not enter its function"
          (not ("aihc_eval_cps(machine, function" `isInfixOf` runtime)),
      testCase "dynamic CPS transfers branch to runtime-selected entries" $ do
        case compileModule (buildLinkLayout [explicitEvaluationProgram]) "_aihc_init_tail_dispatch" (expectCpsGrin explicitEvaluationProgram) of
          Left err -> assertFailure ("native compilation failed: " <> show err)
          Right assembly -> do
            assertBool
              "apply tail-branches to the returned entry"
              ("bl _aihc_apply_cps\n  br x0" `T.isInfixOf` assembly)
            assertBool
              "generated code does not reload a scheduled entry"
              (not ("ldr x9, [x22, #0]\n  br x9" `T.isInfixOf` assembly))
        runtime <- readFile =<< runtimeSourcePath
        assertBool "apply returns its selected entry" ("void *aihc_apply_cps" `isInfixOf` runtime)
        forM_ ["void *next;", "machine->next", "machine->locals", "aihc_schedule"] $ \forbidden ->
          assertBool ("runtime still contains " <> forbidden) (not (forbidden `isInfixOf` runtime)),
      testCase "runtime has no built-in continuation stack" $ do
        runtime <- readFile =<< runtimeSourcePath
        forM_
          [ "AihcContinuation",
            "AIHC_CONT_",
            "aihc_push_normal",
            "aihc_return_values",
            "aihc_return("
          ]
          $ \forbidden ->
            assertBool ("runtime still contains " <> forbidden) (not (forbidden `isInfixOf` runtime)),
      testCase "runtime object ABI compiles cleanly on the host C compiler" $
        withTempDirectory "aihc-arm64-runtime" $ \directory -> do
          runtime <- runtimeSourcePath
          snapshotRuntime <- snapshotSourcePath
          let executable = directory </> "runtime-check"
          (compilerExit, _compilerOut, compilerErr) <-
            readProcessWithExitCode
              "cc"
              [ "-std=c11",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-I",
                takeDirectory runtime,
                runtime,
                snapshotRuntime,
                "-x",
                "c",
                "-",
                "-o",
                executable
              ]
              "int main(void) { return 0; }\n"
          assertEqual ("C compiler runtime diagnostics:\n" <> compilerErr) ExitSuccess compilerExit,
      testCase "compiles standalone HelloWorld GRIN to native ARM64" testNativeHelloWorld
    ]

data SnapshotCase = SnapshotCase
  { snapshotCaseName :: !String,
    snapshotCaseProgram :: !GrinProgram,
    snapshotCaseEntry :: !FunctionName,
    snapshotCaseExpectation :: !SnapshotExpectation
  }

data SnapshotExpectation
  = SnapshotSuccess !T.Text
  | SnapshotFailure !T.Text

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !T.Text,
    snapshotFixtureProgram :: !T.Text,
    snapshotFixtureReturn :: !(Maybe T.Text),
    snapshotFixtureHeap :: !(Maybe T.Text),
    snapshotFixtureError :: !(Maybe T.Text),
    snapshotFixtureStatus :: !T.Text,
    snapshotFixtureReason :: !T.Text
  }

instance FromJSON SnapshotFixture where
  parseJSON =
    withObject "GRIN snapshot fixture" $ \object ->
      SnapshotFixture
        <$> object .: "entry"
        <*> object .: "program"
        <*> object .:? "return"
        <*> object .:? "heap"
        <*> object .:? "error"
        <*> object .: "status"
        <*> object .: "reason"

snapshotCases :: [(String, FilePath)]
snapshotCases =
  [ ("stores one value", "store-one.yaml"),
    ("preserves a suspended thunk", "store-suspended.yaml"),
    ("stores linked values", "store-linked.yaml"),
    ("stores a self-referential value", "store-self-referential.yaml"),
    ("returns an unboxed value", "return-unboxed.yaml"),
    ("evaluates only through GrinEval", "eval.yaml"),
    ("preserves WHNF pointers through GrinEval", "eval-whnf.yaml"),
    ("rejects blackholed thunk re-entry", "eval-blackhole.yaml"),
    ("applies a stored closure", "apply.yaml")
  ]

snapshotTest :: (String, FilePath) -> TestTree
snapshotTest (name, fixtureName) =
  testCase name $ do
    snapshotCase <- loadSnapshotCase name fixtureName
    let program = snapshotCaseProgram snapshotCase
        entry = snapshotCaseEntry snapshotCase
        expectation = snapshotCaseExpectation snapshotCase
    assertEqual "direct GRIN lint" [] (lintProgram program)
    interpreted <- interpretProgramFunctionSnapshot entry program
    assertInterpretedExpectation expectation interpreted
    let cps = expectCpsGrin program
    assertEqual "CPS GRIN lint" [] (lintProgram (cpsGrinProgram cps))
    observed <-
      case compileObservedFunction entry cps of
        Left err -> assertFailure ("native snapshot compilation failed: " <> show err)
        Right value -> pure value
    when (arch == "aarch64" && os == "darwin") $ do
      native <- runObservedProgram observed
      assertNativeExpectation expectation native

assertInterpretedExpectation :: SnapshotExpectation -> Either InterpretError HeapSnapshot -> IO ()
assertInterpretedExpectation expectation interpreted =
  case (expectation, interpreted) of
    (SnapshotSuccess expected, Right snapshot) ->
      assertEqual "interpreter snapshot" (T.stripEnd expected) (T.stripEnd (renderHeapSnapshot snapshot))
    (SnapshotFailure expected, Left err) ->
      assertEqual "interpreter error" expected (renderInterpretFailure err)
    (SnapshotSuccess _, Left err) ->
      assertFailure ("GRIN interpreter failed: " <> show err)
    (SnapshotFailure _, Right snapshot) ->
      assertFailure ("GRIN interpreter unexpectedly succeeded:\n" <> T.unpack (renderHeapSnapshot snapshot))

assertNativeExpectation :: SnapshotExpectation -> Either T.Text T.Text -> IO ()
assertNativeExpectation expectation native =
  case (expectation, native) of
    (SnapshotSuccess expected, Right snapshot) ->
      assertEqual "native snapshot" (T.stripEnd expected) (T.stripEnd snapshot)
    (SnapshotFailure expected, Left err) ->
      assertEqual "native error" expected err
    (SnapshotSuccess _, Left err) ->
      assertFailure ("native snapshot failed: " <> T.unpack err)
    (SnapshotFailure _, Right snapshot) ->
      assertFailure ("native snapshot unexpectedly succeeded:\n" <> T.unpack snapshot)

renderInterpretFailure :: InterpretError -> T.Text
renderInterpretFailure err =
  case err of
    InterpretBlackhole _ -> "blackholed thunk re-entered"
    _ -> T.pack (show err)

loadSnapshotCase :: String -> FilePath -> IO SnapshotCase
loadSnapshotCase name fixtureName = do
  root <- snapshotFixtureRoot
  result <- Y.decodeFileEither (root </> fixtureName)
  fixture <-
    case result of
      Left err -> assertFailure ("invalid GRIN snapshot fixture: " <> Y.prettyPrintParseException err)
      Right value -> pure value
  assertEqual "fixture status" "pass" (snapshotFixtureStatus fixture)
  assertBool "fixture reason is present" (not (T.null (T.strip (snapshotFixtureReason fixture))))
  program <-
    case parseProgram (snapshotFixtureProgram fixture) of
      Left err -> assertFailure ("invalid GRIN program: " <> err)
      Right value -> pure value
  expectation <-
    case (snapshotFixtureReturn fixture, snapshotFixtureHeap fixture, snapshotFixtureError fixture) of
      (Just returnValue, Just heapValue, Nothing) -> do
        let heap = T.stripEnd heapValue
            expected
              | heap == "[]" = "return: " <> returnValue <> "\nheap: []"
              | otherwise =
                  "return: "
                    <> returnValue
                    <> "\nheap:\n"
                    <> T.unlines (map ("  " <>) (T.lines heap))
        pure (SnapshotSuccess expected)
      (Nothing, Nothing, Just err)
        | not (T.null (T.strip err)) -> pure (SnapshotFailure (T.strip err))
      _ -> assertFailure "snapshot fixture must define either return and heap, or a non-empty error"
  pure
    SnapshotCase
      { snapshotCaseName = name,
        snapshotCaseProgram = program,
        snapshotCaseEntry = FunctionName (snapshotFixtureEntry fixture),
        snapshotCaseExpectation = expectation
      }

snapshotFixtureRoot :: IO FilePath
snapshotFixtureRoot = getCurrentDirectory >>= findRoot
  where
    findRoot directory = do
      let candidate = directory </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else do
          let parent = takeDirectory directory
          if parent == directory
            then assertFailure "GRIN snapshot fixture root is missing"
            else findRoot parent

runObservedProgram :: ObservedProgram -> IO (Either T.Text T.Text)
runObservedProgram observed =
  withTempDirectory "aihc-arm64-snapshot" $ \directory -> do
    runtime <- runtimeSourcePath
    snapshotRuntime <- snapshotSourcePath
    let assemblyPath = directory </> "snapshot.s"
        metadataPath = directory </> "snapshot_metadata.c"
        executablePath = directory </> "snapshot"
    TIO.writeFile assemblyPath (observedAssembly observed)
    TIO.writeFile metadataPath (observedMetadataSource observed)
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        [ "--target=" <> targetTriple,
          "-std=c11",
          "-Wall",
          "-Wextra",
          "-Werror",
          "-I",
          takeDirectory runtime,
          runtime,
          snapshotRuntime,
          metadataPath,
          assemblyPath,
          "-o",
          executablePath
        ]
        ""
    case clangExit of
      ExitSuccess -> pure ()
      ExitFailure _ -> assertFailure ("clang failed to assemble observed GRIN:\n" <> clangErr)
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    case programExit of
      ExitSuccess -> do
        assertEqual "native stderr" "" programErr
        pure (Right (T.pack programOut))
      ExitFailure _ -> do
        assertEqual "native stdout" "" programOut
        pure (Left (renderNativeFailure (T.pack programErr)))

renderNativeFailure :: T.Text -> T.Text
renderNativeFailure stderr =
  let message = T.strip stderr
   in fromMaybe message (T.stripPrefix "aihc runtime: " message)

explicitEvaluationProgram :: GrinProgram
explicitEvaluationProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "case_operand",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [caseOperand],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinCase
                  (GrinVarValue caseOperand)
                  caseBinder
                  [GrinAlt GrinDefaultAlt [] (GrinConstant [GrinVarValue caseBinder])]
            },
          GrinFunction
            { grinFunctionName = FunctionName "apply_operand",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [applyOperand],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinApply (BoxedRep Lifted) (GrinVarValue applyOperand) []
            }
        ]
    }
  where
    caseOperand = GrinVar "case_operand" 201 (BoxedRep Lifted)
    caseBinder = GrinVar "case_binder" 202 (BoxedRep Lifted)
    applyOperand = GrinVar "apply_operand" 203 (BoxedRep Lifted)

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
    case compileProgram evalBindingName (expectCpsGrin grinProgram) of
      Right value -> pure value
      Left err -> assertFailure ("ARM64 lowering failed: " <> show err)
  let rendered = T.unpack assembly
  assertBool "emits indirect Haskell tail transfers" ("br x0" `isInfixOf` rendered)
  assertBool "never calls a generated Haskell entry" (not ("bl .Laihc_function_" `isInfixOf` rendered))
  assertBool "emits unboxed literals as raw words" (not ("_aihc_make_literal" `isInfixOf` rendered))
  when (arch == "aarch64" && os == "darwin") $
    runHelloWorldAssembly assembly

expectCpsGrin :: GrinProgram -> CpsGrinProgram
expectCpsGrin program =
  case toCpsGrin program of
    Right cpsProgram -> cpsProgram
    Left err -> error ("test GRIN failed CPS conversion: " <> show err)

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
