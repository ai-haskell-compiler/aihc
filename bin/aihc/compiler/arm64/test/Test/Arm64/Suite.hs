{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Suite
  ( tests,
  )
where

import Aihc.Arm64
  ( Arm64Error (..),
    compileEntryObject,
    compileModuleObject,
    targetTriple,
    validateProgramPrimitives,
  )
import Aihc.Grin
import Aihc.Native
  ( NativeTarget (AppleArm64),
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
import Test.Arm64.Observed (compileObservedFunction)
import Test.Native.Observed (ObservedProgram (..), snapshotSourcePath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "aihc-arm64"
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
        assertEqual "linked primitive validation" (Left (Arm64UnsupportedPrimitive "unsupported#")) (validateProgramPrimitives program)
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "no linked locals section" (not ("aihc_locals" `T.isInfixOf` listing)),
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
        assertBool "emits a wrapping 64-bit add" ("add x0, x9, x0" `T.isInfixOf` listing)
        when (arch == "aarch64" && os == "darwin") $ do
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
        assertBool "emits high-half multiplication" ("umulh x10, x9, x0" `T.isInfixOf` listing)
        assertBool "emits low-half multiplication" ("mul x11, x9, x0" `T.isInfixOf` listing),
      testCase "canonicalizes narrow signed literals in machine-word slots" $ do
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
                          grinFunctionResultRep = Int8Rep,
                          grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt Int8Rep 255)]
                        }
                    ]
                }
        listing <- compileModuleListing (expectGcGrin program)
        assertBool "255 :: Int8# is stored as -1" ("mov x0, #-0x1" `T.isInfixOf` listing || "mov x0, #-1" `T.isInfixOf` listing),
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
        assertBool "calls puts" ("_puts" `T.isInfixOf` listing),
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
        assertBool "takes the symbol address" (any (\line -> "ARM64_RELOC_PAGE21" `T.isInfixOf` line && "_hs_table" `T.isInfixOf` line) (T.lines listing))
        assertBool "does not call the symbol" (not (any (\line -> "bl" `T.isPrefixOf` T.stripStart line && "_hs_table" `T.isInfixOf` line) (T.lines listing))),
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
        assertBool "passes two values" ("mov x1, #0x2" `T.isInfixOf` listing || "mov x1, #2" `T.isInfixOf` listing)
        assertBool "enters the continuation through registers" (".Laihc_enter" `T.isInfixOf` listing)
        assertBool "does not call a C continuation adapter" (not ("_aihc_continue_values" `T.isInfixOf` listing)),
      testGroup "raw GRIN heap snapshots" (map snapshotTest snapshotCases),
      testCase "case and apply never evaluate operands implicitly" $ do
        listing <- compileModuleListing (expectGcGrin explicitEvaluationProgram)
        assertBool "generated case and apply contain no direct-style eval call" (not ("_aihc_eval" `T.isInfixOf` listing))
        pure (),
      testCase "dynamic CPS transfers branch to runtime-selected entries" $ do
        listing <- compileModuleListing (expectGcGrin explicitEvaluationProgram)
        assertBool
          "slow apply returns a value that is passed to the continuation in registers"
          ("_aihc_apply_slow" `T.isInfixOf` listing && ".Laihc_enter" `T.isInfixOf` listing)
        assertBool
          "generated code does not reload a scheduled entry"
          (not ("ldr x9, [x22]" `T.isInfixOf` listing && "br x9" `T.isInfixOf` listing))
        pure (),
      testCase "runtime object ABI compiles cleanly on the host C compiler" $
        withTempDirectory "aihc-arm64-runtime" $ \directory -> do
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
      testCase "semispace collector relocates a transitive global root" $
        withTempDirectory "aihc-arm64-semispace" $ \directory -> do
          runtimeArguments <- nativeRuntimeArguments RuntimeGcSemispace
          let executable = directory </> "semispace-check"
              source =
                unlines
                  [ "#include \"aihc_runtime.h\"",
                    "_Static_assert(sizeof(AihcValue) == sizeof(AihcSlot), \"one-word object header\");",
                    "static void entry_8(AihcSlot *arguments) { (void)arguments; }",
                    "static void entry_9(AihcSlot *arguments) { (void)arguments; }",
                    "static void entry_10(AihcSlot *arguments) { (void)arguments; }",
                    "static void entry_12(AihcSlot *arguments) { (void)arguments; }",
                    "static const uint8_t pointer_field[] = {1};",
                    "static const uint8_t pointer_then_scalar[] = {1, 0};",
                    "static const AihcInfo leaf_info = {1, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
                    "static const AihcInfo box_info = {2, 0, 1, 0, pointer_field, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
                    "static const AihcInfo thunk_info = {4, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_THUNK, 0};",
                    "static const AihcInfo partial_final_info = {3, 0, 2, 0, pointer_then_scalar, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
                    "static const AihcInfo partial_one_info = {3, 0, 1, 1, pointer_then_scalar, &partial_final_info, 0, AIHC_FRAME_NONE, AIHC_OBJECT_PARTIAL_CONSTRUCTOR, 0};",
                    "static const AihcInfo partial_info = {3, 0, 0, 2, 0, &partial_one_info, 0, AIHC_FRAME_NONE, AIHC_OBJECT_PARTIAL_CONSTRUCTOR, 0};",
                    "static const AihcInfo continuation_final_info = {8, entry_8, 1, 0, pointer_field, 0, 0, AIHC_FRAME_NORMAL, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo continuation_info = {8, entry_8, 0, 1, 0, &continuation_final_info, 0, AIHC_FRAME_NORMAL, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo action_final_info = {9, entry_9, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo action_info = {9, entry_9, 0, 1, 0, &action_final_info, 0, AIHC_FRAME_NONE, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo thread_done_final_info = {10, entry_10, 1, 0, pointer_field, 0, 0, AIHC_FRAME_STOP, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo thread_done_info = {10, entry_10, 0, 1, 0, &thread_done_final_info, 0, AIHC_FRAME_STOP, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo yield_final_info = {12, entry_12, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_CLOSURE, 0};",
                    "static const AihcInfo yield_info = {12, entry_12, 0, 1, 0, &yield_final_info, 0, AIHC_FRAME_NONE, AIHC_OBJECT_CLOSURE, 0};",
                    "static int test_apply_roots(void) {",
                    "  AihcMachine *machine = aihc_machine_new(0);",
                    "  AihcValue *leaf = aihc_make_node(machine, &leaf_info);",
                    "  AihcValue *partial = aihc_make_node(machine, &partial_info);",
                    "  AihcValue *continuation = aihc_make_node(machine, &continuation_info);",
                    "  (void)aihc_make_node(machine, &leaf_info);",
                    "  (void)aihc_make_node(machine, &leaf_info);",
                    "  (void)aihc_make_node(machine, &leaf_info);",
                    "  (void)aihc_make_node(machine, &leaf_info);",
                    "  AihcSlot argument = (AihcSlot)leaf;",
                    "  AihcValue *result = aihc_apply_slow(machine, partial, 1, &argument, &continuation);",
                    "  return aihc_value_kind(result) == AIHC_OBJECT_PARTIAL_CONSTRUCTOR &&",
                    "         aihc_value_arity(result) == 1 && aihc_value_count(result) == 1 &&",
                    "         aihc_value_info_table(result) == &partial_one_info &&",
                    "         aihc_value_info((AihcValue *)result->fields[0]) == 1 &&",
                    "         aihc_value_info(continuation) == 8;",
                    "}",
                    "static int test_scheduler_roots(void) {",
                    "  AihcMachine *machine = aihc_machine_new(1);",
                    "  AihcValue *thread_done = aihc_make_node(machine, &thread_done_info);",
                    "  AihcValue *action = aihc_make_node(machine, &action_info);",
                    "  AihcValue *yield = aihc_make_node(machine, &yield_info);",
                    "  aihc_set_thread_done_continuation(machine, thread_done);",
                    "  machine->globals[0] = (AihcSlot)yield;",
                    "  if (aihc_fork(machine, action) == 0) return 0;",
                    "  for (int index = 0; index < 5; ++index) {",
                    "    (void)aihc_make_node(machine, &leaf_info);",
                    "  }",
                    "  yield = (AihcValue *)machine->globals[0];",
                    "  const AihcResume *resume = aihc_yield(machine, yield);",
                    "  if (resume->kind != AIHC_RESUME_APPLY) return 0;",
                    "  thread_done = machine->thread_done_continuation;",
                    "  return aihc_value_info(resume->function) == 9 &&",
                    "         resume->continuation == thread_done &&",
                    "         aihc_value_info(thread_done) == 10;",
                    "}",
                    "static int test_blackhole_roots(void) {",
                    "  AihcMachine *machine = aihc_machine_new(2);",
                    "  AihcValue *thunk = aihc_make_node(machine, &thunk_info);",
                    "  AihcValue *leaf = aihc_make_node(machine, &leaf_info);",
                    "  machine->globals[0] = (AihcSlot)thunk;",
                    "  machine->globals[1] = (AihcSlot)leaf;",
                    "  aihc_begin_blackhole(machine, thunk);",
                    "  for (int index = 0; index < 8; ++index) {",
                    "    (void)aihc_make_node(machine, &leaf_info);",
                    "  }",
                    "  thunk = (AihcValue *)machine->globals[0];",
                    "  leaf = (AihcValue *)machine->globals[1];",
                    "  if (aihc_value_kind(thunk) != AIHC_OBJECT_BLACKHOLE) return 0;",
                    "  aihc_update_blackhole(machine, thunk, leaf);",
                    "  return aihc_value_kind(thunk) == AIHC_OBJECT_INDIRECTION &&",
                    "         thunk->fields[0] == (AihcSlot)leaf;",
                    "}",
                    "static int test_array_roots(void) {",
                    "  AihcMachine *machine = aihc_machine_new(1);",
                    "  AihcValue *leaf = aihc_make_node(machine, &leaf_info);",
                    "  AihcSlot root = (AihcSlot)leaf;",
                    "  aihc_ensure_heap(machine, 4, 1, &root);",
                    "  leaf = (AihcValue *)root;",
                    "  AihcValue *array = aihc_array_new(machine, 2, (AihcSlot)leaf);",
                    "  if (aihc_array_write(array, 1, (AihcSlot)leaf) != 0) return 0;",
                    "  machine->globals[0] = (AihcSlot)array;",
                    "  for (int index = 0; index < 100; ++index) {",
                    "    (void)aihc_make_node(machine, &leaf_info);",
                    "  }",
                    "  array = (AihcValue *)machine->globals[0];",
                    "  AihcValue *first = (AihcValue *)aihc_array_index(array, 0);",
                    "  AihcValue *second = (AihcValue *)aihc_array_index(array, 1);",
                    "  return aihc_value_kind(array) == AIHC_OBJECT_ARRAY &&",
                    "         aihc_array_same(array, array) == 1 &&",
                    "         first == second && aihc_value_info(first) == 1;",
                    "}",
                    "static int test_mutvar_roots(void) {",
                    "  AihcMachine *machine = aihc_machine_new(2);",
                    "  AihcValue *leaf = aihc_make_node(machine, &leaf_info);",
                    "  AihcSlot root = (AihcSlot)leaf;",
                    "  aihc_ensure_heap(machine, 3, 1, &root);",
                    "  leaf = (AihcValue *)root;",
                    "  AihcValue *first = aihc_mutvar_new(machine, (AihcSlot)leaf);",
                    "  machine->globals[0] = (AihcSlot)first;",
                    "  root = (AihcSlot)leaf;",
                    "  aihc_ensure_heap(machine, 3, 1, &root);",
                    "  leaf = (AihcValue *)root;",
                    "  AihcValue *second = aihc_mutvar_new(machine, (AihcSlot)leaf);",
                    "  machine->globals[1] = (AihcSlot)second;",
                    "  for (int index = 0; index < 100; ++index) {",
                    "    (void)aihc_make_node(machine, &leaf_info);",
                    "  }",
                    "  first = (AihcValue *)machine->globals[0];",
                    "  second = (AihcValue *)machine->globals[1];",
                    "  if (aihc_mutvar_write(first, aihc_mutvar_read(second)) != 0) return 0;",
                    "  leaf = (AihcValue *)aihc_mutvar_read(first);",
                    "  if (aihc_mutvar_compare_and_swap(first, (AihcSlot)leaf, (AihcSlot)second) != 0) return 0;",
                    "  if (aihc_mutvar_compare_and_swap(first, (AihcSlot)leaf, (AihcSlot)first) != 1) return 0;",
                    "  return aihc_mutvar_same(first, first) == 1 &&",
                    "         aihc_mutvar_same(first, second) == 0 &&",
                    "         aihc_mutvar_read(first) == (AihcSlot)second &&",
                    "         aihc_value_info(leaf) == 1;",
                    "}",
                    "int main(void) {",
                    "  if (!test_apply_roots()) return 1;",
                    "  if (!test_scheduler_roots()) return 1;",
                    "  if (!test_blackhole_roots()) return 1;",
                    "  if (!test_array_roots()) return 1;",
                    "  if (!test_mutvar_roots()) return 1;",
                    "  AihcMachine *machine = aihc_machine_new(1);",
                    "  AihcValue *leaf = aihc_make_node(machine, &leaf_info);",
                    "  if (leaf->header != (AihcSlot)(uintptr_t)&leaf_info) return 1;",
                    "  AihcValue *box = aihc_make_node(machine, &box_info);",
                    "  aihc_set_field(box, 0, (AihcSlot)leaf);",
                    "  machine->globals[0] = (AihcSlot)box;",
                    "  for (int index = 0; index < 100; ++index) {",
                    "    (void)aihc_make_node(machine, &leaf_info);",
                    "  }",
                    "  box = (AihcValue *)machine->globals[0];",
                    "  leaf = (AihcValue *)box->fields[0];",
                    "  return aihc_value_info(box) == 2 && aihc_value_info(leaf) == 1 ? 0 : 1;",
                    "}"
                  ]
          (compilerExit, _compilerOut, compilerErr) <-
            readProcessWithExitCode
              "cc"
              ( [ "-std=c11",
                  "-Wall",
                  "-Wextra",
                  "-Werror",
                  "-DAIHC_SEMISPACE_BYTES=64"
                ]
                  <> runtimeArguments
                  <> ["-x", "c", "-", "-o", executable]
              )
              source
          assertEqual ("C compiler semispace diagnostics:\n" <> compilerErr) ExitSuccess compilerExit
          (programExit, _programOut, programErr) <- readProcessWithExitCode executable [] ""
          assertEqual ("semispace runtime diagnostics:\n" <> programErr) ExitSuccess programExit,
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
      let functionListing = objectSymbolBody "_aihc_exposed_function_0" "_aihc_exposed_function_1" listing
      assertBool "uses one instruction for a small literal" ("mov x0, #0x1" `T.isInfixOf` functionListing)
      assertBool "does not use a literal pool for a small literal" (not ("ldr x0" `T.isInfixOf` functionListing))
      assertBool "stores a new node field directly" ("str x0, [x20, #0x8]" `T.isInfixOf` functionListing)
      assertBool "does not call the field store function" (not ("_aihc_set_field" `T.isInfixOf` functionListing))
    when (fixtureName == "apply.yaml") $ do
      assertBool "enters a closure without a transfer stub" ("_aihc_exposed_function_0" `T.isInfixOf` listing)
      assertBool "does not move an argument to the same register" (not ("mov x0, x0" `T.isInfixOf` listing))
    when (fixtureName == "store-linked.yaml") $
      assertEqual "one reservation for adjacent source stores" 2 (relocationCount "_aihc_ensure_heap" listing)
    when (fixtureName == "case-reservation.yaml") $
      assertEqual "one reservation for all case branches" 2 (relocationCount "_aihc_ensure_heap" listing)
    when (fixtureName == "store-suspended.yaml") $
      assertBool
        "does not save a node without fields"
        (not ("mov x20, x0\nmov x0, x20" `T.isInfixOf` listing))
    when (fixtureName == "apply-partial.yaml") $ do
      assertBool "dispatches through the info-table apply entry" ("ldr x8, [x8, #0x30]" `T.isInfixOf` listing)
      assertBool
        "loads captured and supplied arguments into registers"
        ("mov x1, x0\nldr x0, [x20, #0x8]\nldr x8, [x20]\nldr x8, [x8, #0x8]\nbr x8" `T.isInfixOf` listing)
    when (fixtureName == "apply-register-overflow.yaml") $ do
      assertBool "spills supplied register overflow" ("sub sp, sp, x8\nmov x9, sp" `T.isInfixOf` listing)
      assertBool "reloads supplied register overflow" ("mov x9, sp\nldr x8, [x9], #0x8" `T.isInfixOf` listing)
    when (fixtureName == "call-register-overflow.yaml") $ do
      assertBool "spills direct-call register overflow" ("sub sp, sp, x8\nmov x10, sp" `T.isInfixOf` listing)
      assertBool "uses canonical direct-call entries" (not ("_register" `T.isInfixOf` listing))
    when (fixtureName == "loop-add.yaml") $ do
      let loopAssembly = objectSymbolBody "_aihc_exposed_function_0" "_aihc_exposed_function_1" listing
      assertBool "keeps the loop's GRIN variables out of local spill storage" (not ("[x19" `T.isInfixOf` loopAssembly))
      assertBool "self-tail-call branches to the allocated body" ("_aihc_exposed_function_0_body" `T.isInfixOf` loopAssembly)
      assertBool "merges case dispatch into the loop body" (not ("case_dispatch" `T.isInfixOf` loopAssembly))
      assertBool "uses the canonical label as the register entry" (not ("_register" `T.isInfixOf` loopAssembly))
    when (arch == "aarch64" && os == "darwin") $ do
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
          case snapshotFixtureAllocations fixture >>= Map.lookup "macos-arm64" of
            Just count -> pure count
            Nothing -> assertFailure "successful snapshot fixture must define allocations.macos-arm64"
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
  withTempDirectory "aihc-arm64-snapshot" $ \directory -> do
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

testNativeScheduler :: IO ()
testNativeScheduler = do
  assertEqual "direct GRIN lint" [] (lintProgram schedulerProgram)
  let gc = expectGcGrin schedulerProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
  (moduleObject, entryObject) <- compileEntryTestUnits schedulerProgram
  moduleListing <- disassembleObjectBytes moduleObject
  entryListing <- disassembleObjectBytes entryObject
  assertBool "captures argc and argv before machine startup" ("_aihc_program_arguments_initialize" `T.isInfixOf` entryListing)
  assertBool "emits fork state operation" ("_aihc_fork" `T.isInfixOf` moduleListing)
  assertBool "emits yield state operation" ("_aihc_yield" `T.isInfixOf` moduleListing)
  assertBool "emits child completion transfer" ("_aihc_thread_done" `T.isInfixOf` entryListing)
  assertBool "emits update continuation frame metadata" (".Laihc_update_info" `T.isInfixOf` entryListing)
  assertBool "emits stop continuation frame metadata" (".Laihc_final_info" `T.isInfixOf` entryListing)
  when (arch == "aarch64" && os == "darwin") $
    runSchedulerObjects "PCAB" [moduleObject, entryObject]

testNativeSynchronousException :: IO ()
testNativeSynchronousException = do
  (moduleObject, entryObject) <- compileEntryTestUnits synchronousExceptionProgram
  moduleListing <- disassembleObjectBytes moduleObject
  assertBool "emits the shared raise transfer" ("_aihc_raise" `T.isInfixOf` moduleListing)
  when (arch == "aarch64" && os == "darwin") $
    runSchedulerObjects "E" [moduleObject, entryObject]

testNativeBlackholeScheduler :: IO ()
testNativeBlackholeScheduler = do
  assertEqual "direct GRIN lint" [] (lintProgram blackholeSchedulerProgram)
  let gc = expectGcGrin blackholeSchedulerProgram
  assertEqual "GC-GRIN lint" [] (lintProgram (gcGrinProgram gc))
  (moduleObject, entryObject) <- compileEntryTestUnits blackholeSchedulerProgram
  when (arch == "aarch64" && os == "darwin") $
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
  assertBool "emits generic IO suspension transfer" ("_aihc_await_io" `T.isInfixOf` listing)
  assertBool "allocates a pinned byte array" ("_aihc_byte_array_new_pinned" `T.isInfixOf` listing)
  assertBool "obtains the byte-array payload" ("_aihc_byte_array_contents" `T.isInfixOf` listing)
  assertBool "obtains stdin through the runtime ABI" ("_aihc_io_stdin" `T.isInfixOf` listing)
  assertBool "obtains stdout through the runtime ABI" ("_aihc_io_stdout" `T.isInfixOf` listing)
  assertBool "submits a generic read through the runtime ABI" ("_aihc_io_submit_read" `T.isInfixOf` listing)
  assertBool "submits a generic write through the runtime ABI" ("_aihc_io_submit_write" `T.isInfixOf` listing)
  assertBool "consumes results through the runtime ABI" ("_aihc_io_take_result" `T.isInfixOf` listing)
  assertBool "compiler has no operation-specific CPS transfer" (not ("_aihc_read_stdin_cps" `T.isInfixOf` listing || "_aihc_write_stdout_cps" `T.isInfixOf` listing))
  when (arch == "aarch64" && os == "darwin") $
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
  withTempDirectory "aihc-arm64-scheduler" $ \directory -> do
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
  withTempDirectory "aihc-arm64-stdio" $ \directory -> do
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
  withTempDirectory "aihc-arm64-objdump" $ \directory -> do
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

objectSymbolBody :: T.Text -> T.Text -> T.Text -> T.Text
objectSymbolBody start end listing =
  fst (T.breakOn ("<" <> end <> ">:") afterStart)
  where
    afterStart = snd (T.breakOn ("<" <> start <> ">:") listing)

relocationCount :: T.Text -> T.Text -> Int
relocationCount symbol =
  length
    . filter
      (\line -> "ARM64_RELOC" `T.isInfixOf` line && symbol `T.isInfixOf` line)
    . T.lines

nativeRuntimeArguments :: RuntimeGarbageCollector -> IO [String]
nativeRuntimeArguments garbageCollector = do
  plan <- runtimePlan AppleArm64 garbageCollector
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
