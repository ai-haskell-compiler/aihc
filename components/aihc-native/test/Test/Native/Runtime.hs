module Test.Native.Runtime
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), RuntimeGarbageCollector (..), RuntimePlan (..), runtimePlan)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "native runtime"
    [ stableNameTest RuntimeGcCalloc,
      stableNameTest RuntimeGcSemispace
    ]

stableNameTest :: RuntimeGarbageCollector -> TestTree
stableNameTest collector =
  testCase ("stable names survive " <> show collector) $
    withSystemTempDirectory "aihc-stable-name" $ \directory -> do
      plan <- runtimePlan Llvm collector
      let executable = directory </> "stable-name"
          arguments =
            ["-std=c11", "-Wall", "-Wextra", "-Werror", "-DAIHC_SEMISPACE_BYTES=64"]
              <> concatMap (\include -> ["-I", include]) (runtimeIncludeDirectories plan)
              <> runtimeSources plan
              <> ["-x", "c", "-", "-o", executable]
      (compilerExit, _compilerOut, compilerErr) <- readProcessWithExitCode "cc" arguments stableNameSource
      assertEqual ("C compiler diagnostics:\n" <> compilerErr) ExitSuccess compilerExit
      (programExit, _programOut, programErr) <- readProcessWithExitCode executable [] ""
      assertEqual ("runtime diagnostics:\n" <> programErr) ExitSuccess programExit

stableNameSource :: String
stableNameSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "static const AihcInfo leaf_info = {1, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE};",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(2);",
      "  AihcValue *first = aihc_make_node(machine, &leaf_info);",
      "  machine->globals[0] = (AihcSlot)first;",
      "  void *first_name = aihc_stable_name_make(machine, first);",
      "  void *first_again = aihc_stable_name_make(machine, first);",
      "  AihcValue *second = aihc_make_node(machine, &leaf_info);",
      "  machine->globals[1] = (AihcSlot)second;",
      "  void *second_name = aihc_stable_name_make(machine, second);",
      "  if (!aihc_stable_name_equal(first_name, first_again)) return 1;",
      "  if (aihc_stable_name_hash(first_name) != aihc_stable_name_hash(first_again)) return 2;",
      "  if (aihc_stable_name_equal(first_name, second_name)) return 3;",
      "  for (int index = 0; index < 100; ++index) (void)aihc_make_node(machine, &leaf_info);",
      "  first = (AihcValue *)machine->globals[0];",
      "  second = (AihcValue *)machine->globals[1];",
      "  if (aihc_stable_name_make(machine, first) != first_name) return 4;",
      "  if (aihc_stable_name_make(machine, second) != second_name) return 5;",
      "  return 0;",
      "}"
    ]
