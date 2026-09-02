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
      stableNameTest RuntimeGcSemispace,
      runtimeProgramTest "semispace grows when live data exceeds the initial space" RuntimeGcSemispace [] growthSource,
      runtimeProgramTest "semispace stops at the heap limit" RuntimeGcSemispace ["+RTS", "-M256", "-RTS"] heapLimitSource,
      runtimeProgramTest
        "static reference roots collect a static object no table names"
        RuntimeGcSemispace
        ["+RTS", "-Zs", "-RTS"]
        (staticReferenceSource CollectsUnreachableCaf),
      runtimeProgramTest
        "every static object stays alive by default"
        RuntimeGcSemispace
        []
        (staticReferenceSource KeepsEveryCaf),
      runtimeProgramTest
        "poisoning leaves the named static object alone"
        RuntimeGcSemispace
        ["+RTS", "-Zs", "-Zp", "-RTS"]
        (staticReferenceSource CollectsUnreachableCaf)
    ]

stableNameTest :: RuntimeGarbageCollector -> TestTree
stableNameTest collector =
  runtimeProgramTest ("stable names survive " <> show collector) collector [] stableNameSource

-- | Compile one C program against the selected runtime with a 64-byte initial
-- semispace. Then, run it with the given arguments and expect exit status 0.
runtimeProgramTest :: String -> RuntimeGarbageCollector -> [String] -> String -> TestTree
runtimeProgramTest name collector programArguments source =
  testCase name $
    withSystemTempDirectory "aihc-runtime" $ \directory -> do
      plan <- runtimePlan Llvm collector
      let executable = directory </> "program"
          arguments =
            ["-std=c11", "-Wall", "-Wextra", "-Werror", "-DAIHC_SEMISPACE_BYTES=64"]
              <> concatMap (\include -> ["-I", include]) (runtimeIncludeDirectories plan)
              <> runtimeSources plan
              <> ["-x", "c", "-", "-o", executable]
      (compilerExit, _compilerOut, compilerErr) <- readProcessWithExitCode "cc" arguments source
      assertEqual ("C compiler diagnostics:\n" <> compilerErr) ExitSuccess compilerExit
      (programExit, _programOut, programErr) <- readProcessWithExitCode executable programArguments ""
      assertEqual ("runtime diagnostics:\n" <> programErr) ExitSuccess programExit

stableNameSource :: String
stableNameSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "static const AihcInfo leaf_info = {1, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
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

-- | Build a list of 1000 cells while every cell stays live. The list needs
-- 16000 bytes, so the 64-byte initial space must grow several times.
growthSource :: String
growthSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "static const uint8_t cell_is_pointer[] = {1};",
      "static const AihcInfo cell_info = {1, 0, 1, 0, cell_is_pointer, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
      "static const AihcInfo leaf_info = {2, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(1);",
      "  machine->globals[0] = (AihcSlot)aihc_make_node(machine, &leaf_info);",
      "  for (int index = 0; index < 1000; ++index) {",
      "    AihcValue *cell = aihc_make_node(machine, &cell_info);",
      "    aihc_set_field(cell, 0, machine->globals[0]);",
      "    machine->globals[0] = (AihcSlot)cell;",
      "  }",
      "  int length = 0;",
      "  AihcValue *cursor = (AihcValue *)machine->globals[0];",
      "  while (aihc_value_info(cursor) == 1) {",
      "    cursor = (AihcValue *)aihc_value_fields(cursor)[0];",
      "    ++length;",
      "  }",
      "  if (aihc_value_info(cursor) != 2) return 1;",
      "  if (length != 1000) return 2;",
      "  if (machine->semispace_bytes < 16000) return 3;",
      "  return 0;",
      "}"
    ]

-- | Which behaviour one run of 'staticReferenceSource' expects.
data StaticReferenceExpectation
  = CollectsUnreachableCaf
  | KeepsEveryCaf

-- | Evaluate two static thunks, then collect with a table that names only one
-- of them. Under @-Zs@ the named thunk must still reach its list and the list
-- behind the unnamed thunk must be gone. By default the collector ignores
-- tables and both lists survive.
staticReferenceSource :: StaticReferenceExpectation -> String
staticReferenceSource expectation =
  unlines
    ( [ "#include \"aihc_runtime.h\"",
        "#if defined(__APPLE__)",
        "#define AIHC_ROOTS __attribute__((used, section(\"__DATA,__aihc_roots\")))",
        "#else",
        "#define AIHC_ROOTS __attribute__((used, section(\"aihc_roots\")))",
        "#endif",
        "static const uint8_t cell_is_pointer[] = {1};",
        "static const AihcInfo cell_info = {1, 0, 1, 0, cell_is_pointer, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
        "static const AihcInfo leaf_info = {2, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
        "static const AihcInfo thunk_info = {3, 0, 0, 0, 0, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_THUNK, 0};",
        "typedef struct { AihcSlot header; AihcSlot target; } StaticThunk;",
        "static StaticThunk named_caf = {(AihcSlot)(uintptr_t)&thunk_info, 0};",
        "static StaticThunk unnamed_caf = {(AihcSlot)(uintptr_t)&thunk_info, 0};",
        "AIHC_ROOTS static AihcValue *named_root = (AihcValue *)&named_caf;",
        "AIHC_ROOTS static AihcValue *unnamed_root = (AihcValue *)&unnamed_caf;",
        "/* The emitted table layout: walk link, the two counts, then the",
        "   static objects followed by the tables of called functions. */",
        "typedef struct {",
        "  AihcSrt *walked;",
        "  uintptr_t object_count;",
        "  uintptr_t child_count;",
        "  uintptr_t objects[1];",
        "} NamedSrt;",
        "static NamedSrt named_srt = {0, 1, 0, {(uintptr_t)&named_caf}};",
        "static AihcValue *build_list(AihcMachine *machine, int length) {",
        "  AihcSlot head = (AihcSlot)aihc_make_node(machine, &leaf_info);",
        "  AihcMachine *held = machine;",
        "  for (int index = 0; index < length; ++index) {",
        "    aihc_ensure_heap(held, 2, 1, &head);",
        "    AihcValue *cell = aihc_make_node_unchecked(held, &cell_info);",
        "    aihc_set_field(cell, 0, head);",
        "    head = (AihcSlot)cell;",
        "  }",
        "  return (AihcValue *)head;",
        "}",
        "static int list_length(AihcValue *cursor) {",
        "  int length = 0;",
        "  while (aihc_value_info(cursor) == 1) {",
        "    cursor = (AihcValue *)aihc_value_fields(cursor)[0];",
        "    ++length;",
        "  }",
        "  return aihc_value_info(cursor) == 2 ? length : -1;",
        "}",
        "int main(int argc, char *const argv[]) {",
        "  aihc_program_arguments_initialize(argc, argv);",
        "  AihcMachine *machine = aihc_machine_new(1);",
        "  machine->globals[0] = 0;",
        "  aihc_current_srt = (const AihcSrt *)&named_srt;",
        "  aihc_update((AihcValue *)&named_caf, build_list(machine, 200));",
        "  aihc_update((AihcValue *)&unnamed_caf, build_list(machine, 200));",
        "  aihc_ensure_heap(machine, 4096, 0, 0);",
        "  uint64_t live = (uint64_t)(machine->heap_next - machine->heap_start);",
        "  if (list_length((AihcValue *)aihc_value_fields((AihcValue *)&named_caf)[0]) != 200) return 1;"
      ]
        <> expectationLines
        <> [ "  return 0;",
             "}"
           ]
    )
  where
    -- One 200-cell list plus its terminator occupies 200 * 16 + 8 bytes. The
    -- bound sits between one and two of them, so it distinguishes the two
    -- behaviours without depending on the exact object layout.
    expectationLines =
      case expectation of
        CollectsUnreachableCaf -> ["  if (live > 4800) return 2;"]
        KeepsEveryCaf ->
          [ "  if (live < 6400) return 2;",
            "  if (list_length((AihcValue *)aihc_value_fields((AihcValue *)&unnamed_caf)[0]) != 200) return 3;"
          ]

-- | Keep more live data than the 256-byte heap limit allows. The runtime must
-- stop with the heap limit diagnostic, which the program reports as success.
heapLimitSource :: String
heapLimitSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include <stdio.h>",
      "#include <stdlib.h>",
      "#include <string.h>",
      "#include <unistd.h>",
      "#include <sys/wait.h>",
      "static const uint8_t cell_is_pointer[] = {1};",
      "static const AihcInfo cell_info = {1, 0, 1, 0, cell_is_pointer, 0, 0, AIHC_FRAME_NONE, AIHC_OBJECT_NODE, 0};",
      "int main(int argc, char *const argv[]) {",
      "  int pipe_ends[2];",
      "  if (pipe(pipe_ends) != 0) return 1;",
      "  pid_t child = fork();",
      "  if (child < 0) return 2;",
      "  if (child == 0) {",
      "    dup2(pipe_ends[1], 2);",
      "    aihc_program_arguments_initialize(argc, argv);",
      "    AihcMachine *machine = aihc_machine_new(1);",
      "    for (int index = 0; index < 100; ++index) {",
      "      AihcValue *cell = aihc_make_node(machine, &cell_info);",
      "      aihc_set_field(cell, 0, machine->globals[0]);",
      "      machine->globals[0] = (AihcSlot)cell;",
      "    }",
      "    _exit(0);",
      "  }",
      "  close(pipe_ends[1]);",
      "  char diagnostic[256] = {0};",
      "  ssize_t count = read(pipe_ends[0], diagnostic, sizeof(diagnostic) - 1);",
      "  int status = 0;",
      "  waitpid(child, &status, 0);",
      "  if (count < 0) return 3;",
      "  if (WIFEXITED(status) && WEXITSTATUS(status) == 0) return 4;",
      "  if (strcmp(diagnostic, \"aihc runtime: heap limit exceeded\\n\") != 0) {",
      "    fputs(diagnostic, stderr);",
      "    return 5;",
      "  }",
      "  return 0;",
      "}"
    ]
