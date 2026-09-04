{-# LANGUAGE OverloadedStrings #-}

module Test.Amd64.Suite
  ( tests,
  )
where

import Aihc.Amd64
  ( Amd64Error (..),
    compileEntryObject,
    compileModuleObject,
    targetTriple,
    validateProgramPrimitives,
  )
import Aihc.Grin
import Aihc.Native
  ( NativeTarget (LinuxAmd64),
    RuntimeGarbageCollector (..),
    RuntimePlan (..),
    executableEntryName,
    runtimePlan,
  )
import Aihc.Testing.ExceptionProgram (synchronousExceptionProgram)
import Aihc.Testing.SchedulerProgram (blackholeSchedulerProgram, schedulerProgram, stdioSchedulerProgram)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word64)
import Data.Yaml qualified as Y
import System.Directory (createDirectory, doesDirectoryExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Info (arch, os)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Test.Amd64.Observed (compileObservedFunction)
import Test.Native.Observed (ObservedProgram (..), snapshotSourcePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-amd64"
    [ testCase "keeps unsupported dormant primitives out of linked programs" $ do
        let primitive = GrinVar "unsupported#" 1 (BoxedRep Lifted)
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [(primitive, 2)],
                  grinForeignCalls = [],
                  grinGlobals = [],
                  grinFunctions = []
                }
        assertEqual "linked primitive validation" (Left (Amd64UnsupportedPrimitive "unsupported#")) (validateProgramPrimitives program)
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "linked locals section" ("aihc_locals" `T.isInfixOf` listing),
      testCase "adds Int# values with wrapping machine arithmetic" $ do
        let entryName = FunctionName "int_add"
            result = GrinVar "result" 2 IntRep
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [(GrinVar "+#" 1 IntRep, 2)],
                  grinForeignCalls = [],
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = entryName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = IntRep,
                          grinFunctionBody =
                            GrinBind
                              [result]
                              ( GrinPrimitiveCall
                                  IntRep
                                  "+#"
                                  [ GrinLitValue (GrinLitInt IntRep 9223372036854775807),
                                    GrinLitValue (GrinLitInt IntRep 1)
                                  ]
                              )
                              (GrinConstant [GrinVarValue result])
                        }
                    ]
                }
        assertEqual "direct GRIN lint" [] (lintProgram program)
        assertEqual "linked primitive validation" (Right ()) (validateProgramPrimitives program)
        let gc = expectGcGrin program
        assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
        observed <-
          case compileObservedFunction entryName gc of
            Left err -> assertFailure ("native Int# addition compilation failed: " <> show err)
            Right value -> pure value
        listing <- disassembleObjectBytes (observedObject observed)
        assertBool "emits a wrapping 64-bit add" ("add r10, rax" `T.isInfixOf` listing)
        when (arch == "x86_64" && os == "linux") $ do
          native <- runObservedProgram observed
          assertEqual "native result" (Right "return: -9223372036854775808\nheap: []\nallocations: 0\n") native,
      testCase "multiplies Word# values to a high/low pair" $ do
        let entryName = FunctionName "word_mul_wide"
            high = GrinVar "high" 2 WordRep
            low = GrinVar "low" 3 WordRep
            wordLiteral = GrinLitValue . GrinLitInt WordRep
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [(GrinVar "timesWord2#" 1 (TupleRep [WordRep, WordRep]), 2)],
                  grinForeignCalls = [],
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = entryName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = TupleRep [WordRep, WordRep],
                          grinFunctionBody =
                            GrinBind
                              [high, low]
                              (GrinPrimitiveCall (TupleRep [WordRep, WordRep]) "timesWord2#" [wordLiteral 0xffffffffffffffff, wordLiteral 2])
                              (GrinConstant [GrinVarValue high, GrinVarValue low])
                        }
                    ]
                }
        observed <-
          case compileObservedFunction entryName (expectGcGrin program) of
            Left err -> assertFailure ("native timesWord2# compilation failed: " <> show err)
            Right value -> pure value
        listing <- disassembleObjectBytes (observedObject observed)
        assertBool "emits unsigned wide multiplication" ("mul r10" `T.isInfixOf` listing),
      testCase "emits boundary integer literals in machine-word slots" $ do
        let functionName = FunctionName "narrow_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = TupleRep [Int8Rep, Word64Rep],
                          grinFunctionBody =
                            GrinConstant
                              [ GrinLitValue (GrinLitInt Int8Rep 255),
                                GrinLitValue (GrinLitInt Word64Rep 18446744073709551615)
                              ]
                        }
                    ]
                }
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "255 :: Int8# is stored as -1" ("movabs rax, -0x1" `T.isInfixOf` listing)
        assertBool "maxBound :: Word64# remains unsigned" ("movabs rdi, -0x1" `T.isInfixOf` listing),
      testCase "passes static Addr# literals to native foreign calls" $ do
        let functionName = FunctionName "puts_addr"
            foreignCall =
              GrinForeignCall
                { grinForeignCallName = "$ffi$puts",
                  grinForeignCallSymbol = "puts",
                  grinForeignCallTarget = GrinForeignFunction,
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
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = Int32Rep,
                          grinFunctionBody = GrinForeignCallExpr foreignCall [GrinLitValue (GrinLitAddr "\xFF\0bar")]
                        }
                    ]
                }
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "loads the static string address" (".Laihc_addr_0" `T.isInfixOf` listing)
        assertBool "emits NUL-terminated Latin-1" ("ff0062617200" `T.isInfixOf` T.filter (/= ' ') listing)
        assertBool "calls puts" ("puts" `T.isInfixOf` listing),
      testCase "materializes the address of a static foreign symbol" $ do
        let functionName = FunctionName "table_addr"
            foreignCall =
              GrinForeignCall
                { grinForeignCallName = "$ffi$hs_table",
                  grinForeignCallSymbol = "hs_table",
                  grinForeignCallTarget = GrinForeignAddress,
                  grinForeignCallSignature =
                    GrinForeignSignature
                      { grinForeignArgumentTypes = [],
                        grinForeignResultType = GrinForeignAddr,
                        grinForeignEffect = GrinForeignPure
                      }
                }
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [foreignCall],
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = AddrRep,
                          grinFunctionBody = GrinForeignCallExpr foreignCall []
                        }
                    ]
                }
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "relocates the symbol address into the instruction stream" (any (\line -> "R_X86_64_PC32" `T.isInfixOf` line && "hs_table" `T.isInfixOf` line) (T.lines listing))
        assertBool "does not call the symbol" (not (any (\line -> "call" `T.isInfixOf` line && "hs_table" `T.isInfixOf` line) (T.lines listing))),
      testCase "returns unboxed tuples as direct machine values" $ do
        let functionName = FunctionName "pair_code"
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinGlobals = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = functionName,
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
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "passes two values" ("movabs rdi, 0x2" `T.isInfixOf` listing)
        assertBool "enters the continuation through registers" (".Laihc_enter" `T.isInfixOf` listing)
        assertBool "does not call a C continuation adapter" (not ("aihc_continue_values" `T.isInfixOf` listing)),
      testGroup "raw GRIN heap snapshots" (map snapshotTest snapshotCases),
      testCase "case and apply never evaluate operands implicitly" $ do
        listing <- compileModuleListing (expectGcGrin explicitEvaluationProgram)
        assertEqual "generated case and apply contain no direct-style eval call" 0 (relocationCount "aihc_eval" listing)
        pure (),
      testCase "case dispatch preserves allocatable registers" $
        do
          listing <- compileModuleListing (expectGcGrin caseDispatchProgram)
          assertBool "uses the reserved scratch register" ("cmp r10, r11" `T.isInfixOf` listing)
          assertBool "does not clobber allocatable r9" (not ("cmp r10, r9" `T.isInfixOf` listing)),
      testCase "dynamic CPS transfers branch to runtime-selected entries" $ do
        listing <- compileModuleListing (expectGcGrin explicitEvaluationProgram)
        assertBool
          "slow apply returns a value that is passed to the continuation in registers"
          ("aihc_apply_slow" `T.isInfixOf` listing && ".Laihc_enter" `T.isInfixOf` listing)
        assertBool
          "generated code does not reload a scheduled entry"
          (not ("mov r11, qword ptr [r15]" `T.isInfixOf` T.toLower listing && "jmp r11" `T.isInfixOf` listing))
        pure (),
      testCase "runtime object ABI compiles cleanly on the host C compiler" $
        withTempDirectory "aihc-amd64-runtime" $ \directory -> do
          runtimeArguments <- nativeRuntimeArguments RuntimeGcCalloc
          snapshotRuntime <- snapshotSourcePath
          let executable = directory </> "runtime-check"
          (compilerExit, _compilerOut, compilerErr) <-
            readProcessWithExitCode
              "cc"
              ( ["-std=c11", "-Wall", "-Wextra", "-Werror"]
                  <> runtimeArguments
                  <> [snapshotRuntime, "-x", "c", "-", "-o", executable]
              )
              "int main(void) { return 0; }\n"
          assertEqual ("C compiler runtime diagnostics:\n" <> compilerErr) ExitSuccess compilerExit,
      testCase "runs fork# and yield# with FIFO scheduling" testNativeScheduler,
      testCase "catches a synchronous exception" testNativeSynchronousException,
      testCase "blocks and wakes threads that enter a shared blackhole" testNativeBlackholeScheduler,
      testCase "waits for stdin and resumes an async stdio continuation" testNativeStdioScheduler
    ]

data SnapshotCase = SnapshotCase
  { snapshotCaseName :: !String,
    snapshotCaseProgram :: !GrinProgram,
    snapshotCaseEntry :: !FunctionName,
    snapshotCaseExpectation :: !SnapshotExpectation
  }

data SnapshotExpectation
  = SnapshotSuccess !T.Text !Word64
  | SnapshotFailure !T.Text

data SnapshotFixture = SnapshotFixture
  { snapshotFixtureEntry :: !T.Text,
    snapshotFixtureProgram :: !T.Text,
    snapshotFixtureReturn :: !(Maybe T.Text),
    snapshotFixtureHeap :: !(Maybe T.Text),
    snapshotFixtureError :: !(Maybe T.Text),
    snapshotFixtureAllocations :: !(Maybe (Map.Map T.Text Word64)),
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
        <*> object .:? "allocations"
        <*> object .: "status"
        <*> object .: "reason"

snapshotCases :: [(String, FilePath)]
snapshotCases =
  [ ("stores one value", "store-one.yaml"),
    ("preserves a suspended thunk", "store-suspended.yaml"),
    ("stores linked values", "store-linked.yaml"),
    ("merges case branch reservations", "case-reservation.yaml"),
    ("stores a self-referential value", "store-self-referential.yaml"),
    ("returns an unboxed value", "return-unboxed.yaml"),
    ("loops ten million times", "loop-add.yaml"),
    ("evaluates only through GrinEval", "eval.yaml"),
    ("preserves WHNF pointers through GrinEval", "eval-whnf.yaml"),
    ("rejects blackholed thunk re-entry", "eval-blackhole.yaml"),
    ("saturates a partial constructor through C", "apply-constructor.yaml"),
    ("allocates a multi-stage partial closure", "apply-multistage.yaml"),
    ("saturates a partial closure through registers", "apply-partial.yaml"),
    ("passes excess saturated arguments through the native stack", "apply-register-overflow.yaml"),
    ("passes direct-call arguments through registers and the native stack", "call-register-overflow.yaml"),
    ("applies a stored closure", "apply.yaml"),
    ("snapshots the ThreadId# returned by fork#", "fork.yaml"),
    ("snapshots child evaluation after yield#", "yield.yaml")
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
    let gc = expectGcGrin program
    assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
    observed <-
      case compileObservedFunction entry gc of
        Left err -> assertFailure ("native snapshot compilation failed: " <> show err)
        Right value -> pure value
    listing <- disassembleObjectBytes (observedObject observed)
    when (fixtureName == "store-one.yaml") $ do
      assertBool "stores a new node field directly" ("mov qword ptr [r13 + 0x8], rax" `T.isInfixOf` T.toLower listing)
      assertBool "does not call the field store function" (not ("aihc_set_field" `T.isInfixOf` listing))
    when (fixtureName == "apply.yaml") $ do
      assertBool "enters a closure without a transfer stub" ("aihc_exposed_function_0" `T.isInfixOf` listing)
      assertBool "does not move an argument to the same register" (not ("mov rax, rax" `T.isInfixOf` listing))
    when (fixtureName == "store-linked.yaml") $
      assertEqual "one reservation for adjacent source stores" 2 (relocationCount "aihc_ensure_heap" listing)
    when (fixtureName == "case-reservation.yaml") $
      assertEqual "one reservation for all case branches" 2 (relocationCount "aihc_ensure_heap" listing)
    when (fixtureName == "store-suspended.yaml") $
      assertBool
        "does not save a node without fields"
        (not ("mov r13, rax\nmov rax, r13" `T.isInfixOf` listing))
    when (fixtureName == "apply-partial.yaml") $ do
      assertBool "dispatches through the info-table apply entry" ("mov r11, qword ptr [r11 + 0x30]" `T.isInfixOf` T.toLower listing)
      assertBool
        "loads captured and supplied arguments into registers"
        ("mov rdi, rax\nmov rax, qword ptr [r12 + 0x8]\nmov r11, qword ptr [r12]\nmov r11, qword ptr [r11 + 0x8]\njmp r11" `T.isInfixOf` T.toLower listing)
    when (fixtureName == "apply-register-overflow.yaml") $ do
      assertBool "spills supplied register overflow" ("sub rsp, 0x10" `T.isInfixOf` listing)
      assertBool "reloads supplied register overflow" ("mov r11, qword ptr [rsp]" `T.isInfixOf` T.toLower listing)
    when (fixtureName == "call-register-overflow.yaml") $ do
      assertBool "spills direct-call register overflow" ("sub rsp, 0x20" `T.isInfixOf` listing)
      assertBool "uses canonical direct-call entries" (not ("_register" `T.isInfixOf` listing))
    when (fixtureName == "loop-add.yaml") $ do
      let loopAndRest = snd (T.breakOn "<aihc_exposed_function_0>:" listing)
          loopAssembly = fst (T.breakOn "<aihc_exposed_function_1>:" loopAndRest)
      assertBool "keeps the loop's GRIN variables out of local spill storage" (not ("[r14" `T.isInfixOf` loopAssembly))
      assertBool "self-tail-call jumps to the allocated body" ("aihc_exposed_function_0_body" `T.isInfixOf` loopAssembly)
      assertBool "merges case dispatch into the loop body" (not ("case_dispatch" `T.isInfixOf` loopAssembly))
      assertBool "uses the canonical label as the register entry" (not ("_register" `T.isInfixOf` loopAssembly))
    when (arch == "x86_64" && os == "linux") $ do
      native <- runObservedProgram observed
      assertNativeExpectation expectation native

assertInterpretedExpectation :: SnapshotExpectation -> Either InterpretError HeapSnapshot -> IO ()
assertInterpretedExpectation expectation interpreted =
  case (expectation, interpreted) of
    (SnapshotSuccess expected _, Right snapshot) ->
      assertEqual "interpreter snapshot" (T.stripEnd expected) (T.stripEnd (renderHeapSnapshot snapshot))
    (SnapshotFailure expected, Left err) ->
      assertEqual "interpreter error" expected (renderInterpretFailure err)
    (SnapshotSuccess _ _, Left err) ->
      assertFailure ("GRIN interpreter failed: " <> show err)
    (SnapshotFailure _, Right snapshot) ->
      assertFailure ("GRIN interpreter unexpectedly succeeded:\n" <> T.unpack (renderHeapSnapshot snapshot))

assertNativeExpectation :: SnapshotExpectation -> Either T.Text T.Text -> IO ()
assertNativeExpectation expectation native =
  case (expectation, native) of
    (SnapshotSuccess expected expectedAllocations, Right snapshot) ->
      assertEqual
        "native snapshot"
        (T.stripEnd expected <> "\nallocations: " <> T.pack (show expectedAllocations))
        (T.stripEnd snapshot)
    (SnapshotFailure expected, Left err) ->
      assertEqual "native error" expected err
    (SnapshotSuccess _ _, Left err) ->
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
      Left err -> assertFailure ("invalid GRIN program: " <> renderParseError err)
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
        allocations <-
          case snapshotFixtureAllocations fixture >>= Map.lookup "linux-amd64" of
            Just count -> pure count
            Nothing -> assertFailure "successful snapshot fixture must define allocations.linux-amd64"
        pure (SnapshotSuccess expected allocations)
      (Nothing, Nothing, Just err)
        | not (T.null (T.strip err)) -> do
            assertEqual "failing snapshot allocations" Nothing (snapshotFixtureAllocations fixture)
            pure (SnapshotFailure (T.strip err))
      _ -> assertFailure "snapshot fixture must define either return and heap, or a non-empty error"
  pure
    SnapshotCase
      { snapshotCaseName = name,
        snapshotCaseProgram = program,
        snapshotCaseEntry = FunctionName (snapshotFixtureEntry fixture),
        snapshotCaseExpectation = expectation
      }

snapshotFixtureRoot :: IO FilePath
snapshotFixtureRoot = do
  configured <- lookupEnv "AIHC_TEST_ROOT"
  case configured of
    Just root -> validate (root </> "bin" </> "aihc" </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot")
    Nothing -> getCurrentDirectory >>= findRoot
  where
    validate candidate = do
      exists <- doesDirectoryExist candidate
      if exists
        then pure candidate
        else assertFailure "GRIN snapshot fixture root is missing"
    findRoot directory = do
      let candidate = directory </> "compiler" </> "grin" </> "test" </> "Test" </> "Fixtures" </> "grin-snapshot"
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
  withTempDirectory "aihc-amd64-snapshot" $ \directory -> do
    runtimeArguments <- nativeRuntimeArguments RuntimeGcCalloc
    snapshotRuntime <- snapshotSourcePath
    let objectPath = directory </> "snapshot.o"
        metadataPath = directory </> "snapshot_metadata.c"
        executablePath = directory </> "snapshot"
    BL.writeFile objectPath (observedObject observed)
    TIO.writeFile metadataPath (observedMetadataSource observed)
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        ( ["--target=" <> targetTriple, "-std=c11", "-Wall", "-Wextra", "-Werror", "-I", takeDirectory snapshotRuntime]
            <> runtimeArguments
            <> [snapshotRuntime, metadataPath, objectPath, "-o", executablePath]
        )
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
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "case_operand",
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

caseDispatchProgram :: GrinProgram
caseDispatchProgram =
  GrinProgram
    { grinConstructors = [("CaseA", [[]]), ("CaseB", [[]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "data_case_dispatch",
              grinFunctionParameters = [dataOperand],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinCase
                  (GrinVarValue dataOperand)
                  dataBinder
                  [ GrinAlt (GrinDataAlt "CaseA") [] (GrinConstant [GrinVarValue dataBinder]),
                    GrinAlt GrinDefaultAlt [] (GrinConstant [GrinVarValue dataBinder])
                  ]
            },
          GrinFunction
            { grinFunctionName = FunctionName "literal_case_dispatch",
              grinFunctionParameters = [literalOperand],
              grinFunctionResultRep = IntRep,
              grinFunctionBody =
                GrinCase
                  (GrinVarValue literalOperand)
                  literalBinder
                  [ GrinAlt (GrinLitAlt (GrinLitInt IntRep 7)) [] (GrinConstant [GrinVarValue literalBinder]),
                    GrinAlt GrinDefaultAlt [] (GrinConstant [GrinVarValue literalBinder])
                  ]
            }
        ]
    }
  where
    dataOperand = GrinVar "data_operand" 204 (BoxedRep Lifted)
    dataBinder = GrinVar "data_binder" 205 (BoxedRep Lifted)
    literalOperand = GrinVar "literal_operand" 206 IntRep
    literalBinder = GrinVar "literal_binder" 207 IntRep

testNativeScheduler :: IO ()
testNativeScheduler = do
  assertEqual "direct GRIN lint" [] (lintProgram schedulerProgram)
  let gc = expectGcGrin schedulerProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
  (moduleObject, entryObject) <- compileEntryTestUnits schedulerProgram
  moduleListing <- disassembleObjectBytes moduleObject
  entryListing <- disassembleObjectBytes entryObject
  assertBool "captures argc and argv before machine startup" ("aihc_program_arguments_initialize" `T.isInfixOf` entryListing)
  assertBool "emits fork state operation" ("aihc_fork" `T.isInfixOf` moduleListing)
  assertBool "emits yield state operation" ("aihc_yield" `T.isInfixOf` moduleListing)
  assertBool "emits child completion transfer" ("aihc_thread_done" `T.isInfixOf` entryListing)
  assertBool "emits update continuation frame metadata" (".Laihc_update_info" `T.isInfixOf` entryListing)
  assertBool "emits stop continuation frame metadata" (".Laihc_final_info" `T.isInfixOf` entryListing)
  when (arch == "x86_64" && os == "linux") $
    runSchedulerObjects "PCAB" [moduleObject, entryObject]

testNativeSynchronousException :: IO ()
testNativeSynchronousException = do
  (moduleObject, entryObject) <- compileEntryTestUnits synchronousExceptionProgram
  moduleListing <- disassembleObjectBytes moduleObject
  assertBool "emits the shared raise transfer" ("aihc_raise" `T.isInfixOf` moduleListing)
  when (arch == "x86_64" && os == "linux") $
    runSchedulerObjects "E" [moduleObject, entryObject]

testNativeBlackholeScheduler :: IO ()
testNativeBlackholeScheduler = do
  assertEqual "direct GRIN lint" [] (lintProgram blackholeSchedulerProgram)
  let gc = expectGcGrin blackholeSchedulerProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
  (moduleObject, entryObject) <- compileEntryTestUnits blackholeSchedulerProgram
  when (arch == "x86_64" && os == "linux") $
    runSchedulerObjects "TA" [moduleObject, entryObject]

testNativeStdioScheduler :: IO ()
testNativeStdioScheduler = do
  assertEqual "direct GRIN lint" [] (lintProgram stdioSchedulerProgram)
  let gc = expectGcGrin stdioSchedulerProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
  (moduleObject, entryObject) <- compileEntryTestUnits stdioSchedulerProgram
  moduleListing <- disassembleObjectBytes moduleObject
  entryListing <- disassembleObjectBytes entryObject
  let listing = moduleListing <> entryListing
  assertBool "emits generic IO suspension transfer" ("aihc_await_io" `T.isInfixOf` listing)
  assertBool "allocates a pinned byte array" ("aihc_byte_array_new_pinned" `T.isInfixOf` listing)
  assertBool "obtains the byte-array payload" ("aihc_byte_array_contents" `T.isInfixOf` listing)
  assertBool "obtains stdin through the runtime ABI" ("aihc_io_stdin" `T.isInfixOf` listing)
  assertBool "obtains stdout through the runtime ABI" ("aihc_io_stdout" `T.isInfixOf` listing)
  assertBool "submits a generic read through the runtime ABI" ("aihc_io_submit_read" `T.isInfixOf` listing)
  assertBool "submits a generic write through the runtime ABI" ("aihc_io_submit_write" `T.isInfixOf` listing)
  assertBool "consumes results through the runtime ABI" ("aihc_io_take_result" `T.isInfixOf` listing)
  assertBool "compiler has no operation-specific CPS transfer" (not ("aihc_read_stdin_cps" `T.isInfixOf` listing || "aihc_write_stdout_cps" `T.isInfixOf` listing))
  when (arch == "x86_64" && os == "linux") $
    runStdioObjects [moduleObject, entryObject]

expectGcGrin :: GrinProgram -> GcGrinProgram
expectGcGrin program =
  case toCpsGrin program of
    Right cpsProgram -> lowerGc cpsProgram
    Left err -> error ("test GRIN failed CPS conversion: " <> show err)

compileEntryTestUnits :: GrinProgram -> IO (BL.ByteString, BL.ByteString)
compileEntryTestUnits program = do
  let linkedProgram =
        program
          { grinGlobals =
              [ (if name == "main" then executableEntryName else name, node)
              | (name, node) <- grinGlobals program
              ]
          }
  moduleObject <- either (assertFailure . show) pure (compileModuleObject (expectGcGrin linkedProgram))
  entryObject <- either (assertFailure . show) pure compileEntryObject
  pure (moduleObject, entryObject)

runSchedulerObjects :: String -> [BL.ByteString] -> IO ()
runSchedulerObjects expected objects =
  withTempDirectory "aihc-amd64-scheduler" $ \directory -> do
    runtimeArguments <- nativeRuntimeArguments RuntimeGcCalloc
    objectPaths <- forM (zip [0 :: Int ..] objects) $ \(index, object) -> do
      let objectPath = directory </> "scheduler-" <> show index <> ".o"
      BL.writeFile objectPath object
      pure objectPath
    let executablePath = directory </> "scheduler"
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        (["-std=c11", "-Wall", "-Wextra", "-Werror"] <> runtimeArguments <> objectPaths <> ["-o", executablePath])
        ""
    assertEqual ("clang failed to assemble scheduler program:\n" <> clangErr) ExitSuccess clangExit
    (programExit, programOut, programErr) <- readProcessWithExitCode executablePath [] ""
    assertEqual ("native stderr: " <> programErr) ExitSuccess programExit
    assertEqual "scheduler stdout" expected programOut

runStdioObjects :: [BL.ByteString] -> IO ()
runStdioObjects objects =
  withTempDirectory "aihc-amd64-stdio" $ \directory -> do
    runtimeArguments <- nativeRuntimeArguments RuntimeGcCalloc
    objectPaths <- forM (zip [0 :: Int ..] objects) $ \(index, object) -> do
      let objectPath = directory </> "stdio-" <> show index <> ".o"
      BL.writeFile objectPath object
      pure objectPath
    let executablePath = directory </> "stdio"
    (clangExit, _clangOut, clangErr) <-
      readProcessWithExitCode
        "clang"
        (["-std=c11", "-Wall", "-Wextra", "-Werror"] <> runtimeArguments <> objectPaths <> ["-o", executablePath])
        ""
    assertEqual ("clang failed to assemble async stdio program:\n" <> clangErr) ExitSuccess clangExit
    (Just childInput, Just childOutput, Just childError, processHandle) <-
      createProcess
        (proc executablePath [])
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }
    threadDelay 50000
    hPutStr childInput "Buffered async IO\n"
    hFlush childInput
    hClose childInput
    programOut <- TIO.hGetContents childOutput
    programErr <- TIO.hGetContents childError
    programExit <- waitForProcess processHandle
    assertEqual ("native stderr: " <> T.unpack programErr) ExitSuccess programExit
    assertEqual "async stdout" "Buffered async IO\n" programOut

compileModuleListing :: GcGrinProgram -> IO T.Text
compileModuleListing program =
  either (assertFailure . show) disassembleObjectBytes (compileModuleObject program)

disassembleObjectBytes :: BL.ByteString -> IO T.Text
disassembleObjectBytes object =
  withTempDirectory "aihc-amd64-objdump" $ \directory -> do
    let objectPath = directory </> "program.o"
    BL.writeFile objectPath object
    executable <- findObjdump
    (objdumpExit, objdumpOut, objdumpErr) <-
      readProcessWithExitCode executable ["-d", "-r", "-t", "-s", "-M", "intel", objectPath] ""
    assertEqual ("object disassembler diagnostics:\n" <> objdumpErr) ExitSuccess objdumpExit
    pure (normalizeObjdump (T.pack objdumpOut))

findObjdump :: IO FilePath
findObjdump = do
  llvmObjdump <- findExecutable "llvm-objdump"
  objdump <- findExecutable "objdump"
  case llvmObjdump <|> objdump of
    Just executable -> pure executable
    Nothing -> assertFailure "llvm-objdump or objdump is required"

normalizeObjdump :: T.Text -> T.Text
normalizeObjdump = T.unlines . map normalizeLine . T.lines
  where
    normalizeLine line =
      case T.splitOn "\t" line of
        prefix : fields
          | ":" `T.isInfixOf` prefix,
            not (null fields) ->
              T.unwords (filter (not . T.null) (map T.strip fields))
        _ -> T.strip line

relocationCount :: T.Text -> T.Text -> Int
relocationCount symbol =
  length
    . filter
      (\line -> "R_X86_64" `T.isInfixOf` line && symbol `T.isInfixOf` line)
    . T.lines

nativeRuntimeArguments :: RuntimeGarbageCollector -> IO [String]
nativeRuntimeArguments garbageCollector = do
  plan <- runtimePlan LinuxAmd64 garbageCollector
  pure
    ( ["-I" <> directory | directory <- runtimeIncludeDirectories plan]
        <> runtimeSources plan
    )

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
