module Test.Native.Runtime
  ( tests,
  )
where

import Aihc.Cli.Runtime (RuntimeBuild (..))
import Aihc.Native (NativeTarget (Llvm), RuntimeGarbageCollector (..), backendCompiler)
import Aihc.Testing.RuntimeArchive (cachedRuntimeArchive)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "native runtime"
    [ runtimeProgramTest "stable names survive collections" RuntimeGcSemispace [] stableNameSource,
      runtimeProgramTest "Lir runtime units implement mutable references" RuntimeGcSemispace [] mutVarSource,
      runtimeProgramTest "Lir runtime units implement byte arrays" RuntimeGcSemispace [] byteArraySource,
      runtimeProgramTest
        "Lir runtime units parse the RTS options"
        RuntimeGcSemispace
        ["+RTS", "-M2k", "-Zs", "-RTS", "kept", "--RTS", "+RTS", "-M1X"]
        runtimeOptionsSource,
      runtimeProgramTest "semispace grows when live data exceeds the initial space" RuntimeGcSemispace [] growthSource,
      runtimeProgramTest "semispace stops at the heap limit" RuntimeGcSemispace ["+RTS", "-M256", "-RTS"] heapLimitSource,
      runtimeProgramTest
        "static reference roots collect a static object no table names"
        RuntimeGcSemispace
        ["+RTS", "-Zs", "-RTS"]
        (staticReferenceSource CollectsUnreachableCaf),
      runtimeProgramTest
        "every evaluated static object stays alive by default"
        RuntimeGcSemispace
        []
        (staticReferenceSource KeepsEveryCaf),
      runtimeStatisticsTest "AIHC_RTS_STATS receives the statistics when the process exits" True EndsWithProcessExit,
      runtimeStatisticsTest "AIHC_RTS_STATS receives the statistics when the machine halts" True EndsWithReturn,
      runtimeStatisticsTest "no statistics file is written without AIHC_RTS_STATS" False EndsWithProcessExit
    ]

-- | Compile one C program against the selected runtime with a 64-byte initial
-- semispace. Then, run it with the given arguments and expect exit status 0.
runtimeProgramTest :: String -> RuntimeGarbageCollector -> [String] -> String -> TestTree
runtimeProgramTest name collector programArguments source =
  runtimeProgramTestWith name collector programArguments (const []) source (const (pure ()))

-- | Like 'runtimeProgramTest', with environment variables for the program
-- and a check that runs in the temporary directory after the program exits.
-- Both receive the temporary directory, so a variable can name a file there.
runtimeProgramTestWith :: String -> RuntimeGarbageCollector -> [String] -> (FilePath -> [(String, String)]) -> String -> (FilePath -> IO ()) -> TestTree
runtimeProgramTestWith name collector programArguments extraEnvironment source check =
  testCase name $
    withSystemTempDirectory "aihc-runtime" $ \directory -> do
      -- The tiny semispace forces a collection in every one of these
      -- programs, so the runtime archive is built for this test rather than
      -- taken from the store. Every test here shares that one archive.
      build <- cachedRuntimeArchive Llvm collector ["-std=c11", "-Wall", "-Wextra", "-Werror", "-DAIHC_SEMISPACE_BYTES=64"]
      let executable = directory </> "program"
          arguments =
            ["-std=c11", "-Wall", "-Wextra", "-Werror"]
              <> concatMap (\include -> ["-I", include]) (runtimeBuildIncludeDirectories build)
              -- "-x c" reads the program from stdin; "-x none" ends it so the
              -- archive is a linker input rather than another C source.
              <> ["-x", "c", "-", "-x", "none", runtimeBuildArchive build, "-lm", "-o", executable]
      -- Link with the driver that built the archive, so a host whose "cc" is
      -- not Clang cannot mix two toolchains in one program.
      (compiler, _targetArguments) <- backendCompiler Llvm
      (compilerExit, _compilerOut, compilerErr) <- readProcessWithExitCode compiler arguments source
      assertEqual ("C compiler diagnostics:\n" <> compilerErr) ExitSuccess compilerExit
      inherited <- getEnvironment
      let extra = extraEnvironment directory
          environment = extra <> [entry | entry@(variable, _) <- inherited, variable `notElem` map fst extra]
          process = (proc executable programArguments) {env = Just environment}
      (programExit, _programOut, programErr) <- readCreateProcessWithExitCode process ""
      assertEqual ("runtime diagnostics:\n" <> programErr) ExitSuccess programExit
      check directory

stableNameSource :: String
stableNameSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "static const AihcInfo leaf_info = {.identity = 1, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(2);",
      "  AihcValue *first = aihc_make_node(machine, &leaf_info);",
      "  machine->globals[0] = (AihcSlot)first;",
      "  void *first_name = aihc_stable_name_make(machine, first);",
      "  void *first_again = aihc_stable_name_make(machine, first);",
      "  AihcValue *second = aihc_make_node(machine, &leaf_info);",
      "  machine->globals[1] = (AihcSlot)second;",
      "  void *second_name = aihc_stable_name_make(machine, second);",
      "  if (first_name != first_again) return 1;",
      "  if (first_name == second_name) return 3;",
      "  for (int index = 0; index < 100; ++index) (void)aihc_make_node(machine, &leaf_info);",
      "  first = (AihcValue *)machine->globals[0];",
      "  second = (AihcValue *)machine->globals[1];",
      "  if (aihc_stable_name_make(machine, first) != first_name) return 4;",
      "  if (aihc_stable_name_make(machine, second) != second_name) return 5;",
      "  return 0;",
      "}"
    ]

-- | Mutable references are boxed arrays of one element, and both live in
-- aihc_mutvar.lir and aihc_array.lir. Compiled code reads and writes the
-- element itself, so this checks the allocation through the array accessors
-- of the C runtime. See the "Runtime units" section of docs/lir.md.
mutVarSource :: String
mutVarSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include \"aihc_runtime_internal.h\"",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(0);",
      "  AihcValue *mutvar = aihc_mutvar_new(machine, 7);",
      "  if (aihc_array_length(mutvar) != 1) return 1;",
      "  if (aihc_array_elements(mutvar)[0] != 7) return 2;",
      "  AihcValue *array = aihc_array_new(machine, 3, 9);",
      "  if (aihc_array_length(array) != 3) return 3;",
      "  if (aihc_array_elements(array)[2] != 9) return 4;",
      "  aihc_array_copy(mutvar, 0, array, 1, 1);",
      "  if (aihc_array_elements(array)[1] != 7) return 5;",
      "  if (aihc_array_elements(array)[0] != 9) return 6;",
      "  return 0;",
      "}"
    ]

-- | The RTS option parser and the argument store live in
-- aihc_runtime_options.lir. The arguments of this program hold every marker:
-- the options between +RTS and -RTS are parsed and dropped, and after --RTS
-- an option is a plain argument that the parser never sees.
runtimeOptionsSource :: String
runtimeOptionsSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include \"aihc_runtime_internal.h\"",
      "#include <string.h>",
      "/* The implicit terminator of each literal ends its last argument. */",
      "static const char kept[] = \"kept\\0+RTS\\0-M1X\";",
      "static const char replaced[] = \"other\";",
      "int main(int argc, char *const argv[]) {",
      "  aihc_program_arguments_initialize(argc, argv);",
      "  AihcMachine *machine = aihc_machine_new(0);",
      "  if (!machine->heap_limit_enabled || machine->heap_max_bytes != 2048) return 1;",
      "  if (!aihc_rts_static_reference_roots()) return 19;",
      "  size_t name_length = strlen(argv[0]) + 1;",
      "  int64_t size = aihc_program_arguments_size();",
      "  if (size != (int64_t)(name_length + sizeof(kept))) return 2;",
      "  char buffer[512];",
      "  if (aihc_program_arguments_copy(buffer, 1) != size) return 3;",
      "  if (aihc_program_arguments_copy(NULL, 1) != -1) return 4;",
      "  if (aihc_program_arguments_copy(buffer, sizeof(buffer)) != size) return 5;",
      "  if (memcmp(buffer, argv[0], name_length) != 0) return 6;",
      "  if (memcmp(buffer + name_length, kept, sizeof(kept)) != 0) return 7;",
      "  if (aihc_program_arguments_replace(replaced, sizeof(replaced) - 1) != -1) return 8;",
      "  if (aihc_program_arguments_size() != size) return 9;",
      "  if (aihc_program_arguments_replace(replaced, sizeof(replaced)) != 0) return 10;",
      "  if (aihc_program_arguments_size() != (int64_t)sizeof(replaced)) return 11;",
      "  if (aihc_program_arguments_copy(buffer, sizeof(buffer)) != (int64_t)sizeof(replaced)) return 12;",
      "  if (memcmp(buffer, replaced, sizeof(replaced)) != 0) return 13;",
      "  if (aihc_program_arguments_replace(NULL, 0) != 0) return 14;",
      "  if (aihc_program_arguments_size() != 0) return 15;",
      "  if (aihc_runtime_arguments_initialize(replaced, sizeof(replaced) - 1) != -1) return 16;",
      "  if (aihc_runtime_arguments_initialize(replaced, sizeof(replaced)) != 0) return 17;",
      "  if (aihc_program_arguments_size() != (int64_t)sizeof(replaced)) return 18;",
      "  return 0;",
      "}"
    ]

-- | Byte arrays live entirely in aihc_byte_array.lir: the C runtime keeps no
-- description of their layout, so this exercises the whole unit through the
-- header it exports. Compiled code reads and writes the elements itself, so
-- the program reaches them through the contents address.
byteArraySource :: String
byteArraySource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include <string.h>",
      "static uint64_t word_at(void *array, int64_t offset) {",
      "  uint64_t value;",
      "  memcpy(&value, (char *)aihc_byte_array_contents(array) + offset, 8);",
      "  return value;",
      "}",
      "static void put_word(void *array, int64_t offset, uint64_t value) {",
      "  memcpy((char *)aihc_byte_array_contents(array) + offset, &value, 8);",
      "}",
      "int main(void) {",
      "  void *bytes = aihc_byte_array_new(16);",
      "  if (aihc_byte_array_get_size(bytes) != 16) return 1;",
      "  if (aihc_byte_array_is_pinned(bytes)) return 2;",
      "  if (!aihc_byte_array_is_pinned(aihc_byte_array_new_pinned(8))) return 3;",
      "  void *aligned = aihc_byte_array_new_aligned_pinned(8, 64);",
      "  if ((uintptr_t)aihc_byte_array_contents(aligned) % 64 != 0) return 4;",
      "  put_word(bytes, 0, 0x0102030405060708ULL);",
      "  put_word(bytes, 8, UINT64_MAX);",
      "  if (word_at(bytes, 0) != 0x0102030405060708ULL) return 5;",
      "  if (word_at(bytes, 8) != UINT64_MAX) return 6;",
      "  const char *source = \"hello world!!!!!\";",
      "  char out[17] = {0};",
      "  aihc_byte_array_copy_from_addr((void *)source, bytes, 0, 16);",
      "  aihc_byte_array_copy_to_addr(bytes, 0, out, 16);",
      "  if (memcmp(out, source, 16) != 0) return 11;",
      "  void *other = aihc_byte_array_new(16);",
      "  aihc_byte_array_copy(bytes, 0, other, 0, 16);",
      "  if (aihc_byte_array_compare(bytes, 0, other, 0, 16) != 0) return 12;",
      "  put_word(other, 0, 0);",
      "  if ((int64_t)aihc_byte_array_compare(bytes, 0, other, 0, 16) != 1) return 13;",
      "  if ((int64_t)aihc_byte_array_compare(other, 0, bytes, 0, 16) != -1) return 14;",
      "  /* Ranges of one array may overlap, so a copy has to move. */",
      "  aihc_byte_array_copy(bytes, 0, bytes, 4, 12);",
      "  aihc_byte_array_copy_to_addr(bytes, 0, out, 16);",
      "  if (memcmp(out, \"hellhello world!\", 16) != 0) return 15;",
      "  void *grown = aihc_byte_array_resize(bytes, 32);",
      "  if (aihc_byte_array_get_size(grown) != 32) return 16;",
      "  if (aihc_byte_array_compare(grown, 0, bytes, 0, 16) != 0) return 17;",
      "  aihc_byte_array_shrink(grown, 4);",
      "  if (aihc_byte_array_get_size(grown) != 4) return 18;",
      "  void *empty = aihc_byte_array_new(0);",
      "  if (aihc_byte_array_get_size(empty) != 0) return 19;",
      "  aihc_byte_array_copy_from_addr(NULL, empty, 0, 0);",
      "  /* The atomic operations index by word and give the old value. */",
      "  void *sized = aihc_byte_array_new(32);",
      "  aihc_byte_array_set(sized, 0, 32, 0);",
      "  aihc_byte_array_set(sized, 24, 8, 0xff);",
      "  if (aihc_byte_array_fetch_add_word(sized, 1, 5) != 0) return 20;",
      "  if (aihc_byte_array_fetch_add_word(sized, 1, 5) != 5) return 21;",
      "  if (word_at(sized, 8) != 10) return 22;",
      "  if (aihc_byte_array_fetch_or_word(sized, 1, 1) != 10) return 23;",
      "  if (aihc_byte_array_compare_and_swap_word(sized, 1, 11, 12) != 11) return 24;",
      "  if (aihc_byte_array_compare_and_swap_word(sized, 1, 11, 13) != 12) return 25;",
      "  if (word_at(sized, 8) != 12) return 26;",
      "  if (word_at(sized, 0) != 0) return 27;",
      "  if (word_at(sized, 24) != UINT64_MAX) return 28;",
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
      "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
      "static const AihcInfo leaf_info = {.identity = 2, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
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
-- tables and both lists survive. No code and no section lists the two
-- thunks: the runtime records them when they become indirections.
staticReferenceSource :: StaticReferenceExpectation -> String
staticReferenceSource expectation =
  unlines
    ( [ "#include \"aihc_runtime.h\"",
        "static const uint8_t cell_is_pointer[] = {1};",
        "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static const AihcInfo leaf_info = {.identity = 2, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static const AihcInfo thunk_info = {.identity = 3, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_THUNK};",
        "typedef struct { AihcSlot header; AihcSlot target; } StaticThunk;",
        "static StaticThunk named_caf = {(AihcSlot)(uintptr_t)&thunk_info, 0};",
        "static StaticThunk unnamed_caf = {(AihcSlot)(uintptr_t)&thunk_info, 0};",
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
      "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
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

-- | How one run of 'statisticsSource' ends: through 'aihc_exit_process',
-- as @exitWith@ does, or by a return from @main@ after the machine halts.
data StatisticsEnding
  = EndsWithProcessExit
  | EndsWithReturn

-- | Run 'statisticsSource' with or without @AIHC_RTS_STATS@ in its
-- environment. With the variable, the program must leave one JSON object in
-- the named file. Without it, no file appears.
runtimeStatisticsTest :: String -> Bool -> StatisticsEnding -> TestTree
runtimeStatisticsTest name requested ending =
  runtimeProgramTestWith name RuntimeGcSemispace [] environment (statisticsSource ending) check
  where
    statisticsFile directory = directory </> "stats.json"
    environment directory = [("AIHC_RTS_STATS", statisticsFile directory) | requested]
    check directory = do
      present <- doesFileExist (statisticsFile directory)
      if requested
        then do
          assertBool "the statistics file exists" present
          decoded <- eitherDecodeFileStrict (statisticsFile directory)
          statistics <- either (assertFailure . ("statistics JSON: " <>)) pure decoded :: IO (Map String Integer)
          assertEqual "field names" ["allocated_bytes", "gc_count", "gc_time_ns", "peak_heap_bytes", "schema"] (Map.keys statistics)
          assertEqual "schema" (Just 1) (Map.lookup "schema" statistics)
          -- One leaf and 1000 cells: 8 + 1000 * 16 bytes.
          assertEqual "allocated_bytes" (Just 16008) (Map.lookup "allocated_bytes" statistics)
          assertBool "peak_heap_bytes holds the live list" (Map.lookup "peak_heap_bytes" statistics >= Just 16008)
          assertBool "gc_count counts the collections" (Map.lookup "gc_count" statistics >= Just 1)
        else assertBool "no statistics file exists" (not present)

-- | Check the environment parser of aihc_runtime_options.lir on crafted
-- environments, then take the real one. Build a live list of 1000 cells so
-- the 64-byte initial space collects many times, and end the program the
-- given way.
statisticsSource :: StatisticsEnding -> String
statisticsSource ending =
  unlines
    ( [ "#include \"aihc_runtime.h\"",
        "#include \"aihc_runtime_internal.h\"",
        "#include <stdlib.h>",
        "#include <string.h>",
        "static const uint8_t cell_is_pointer[] = {1};",
        "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static const AihcInfo leaf_info = {.identity = 2, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static char *const crafted[] = {\"OTHER=1\", \"AIHC_RTS_STATS=crafted\", \"AIHC_RTS_STATSX=no\", NULL};",
        "static char *const empty_value[] = {\"AIHC_RTS_STATS=\", NULL};",
        "/* The missing terminator makes this buffer malformed. */",
        "static const char malformed[] = \"AIHC_RTS_STATS=x\";",
        "static int path_is(const char *expected) {",
        "  const char *path = aihc_rts_stats_path();",
        "  if (expected == NULL || path == NULL) return expected == path;",
        "  return strcmp(path, expected) == 0;",
        "}",
        "int main(int argc, char *const argv[]) {",
        "  aihc_program_arguments_initialize(argc, argv);",
        "  if (!path_is(NULL)) return 1;",
        "  aihc_environment_initialize(crafted);",
        "  if (!path_is(\"crafted\")) return 2;",
        "  if (aihc_runtime_environment_initialize(malformed, sizeof(malformed) - 1) != -1) return 3;",
        "  if (!path_is(\"crafted\")) return 4;",
        "  aihc_environment_initialize(empty_value);",
        "  if (!path_is(NULL)) return 5;",
        "  aihc_program_environment_initialize();",
        "  if (!path_is(getenv(\"AIHC_RTS_STATS\"))) return 6;",
        "  AihcMachine *machine = aihc_machine_new(1);",
        "  machine->globals[0] = (AihcSlot)aihc_make_node(machine, &leaf_info);",
        "  for (int index = 0; index < 1000; ++index) {",
        "    AihcValue *cell = aihc_make_node(machine, &cell_info);",
        "    aihc_set_field(cell, 0, machine->globals[0]);",
        "    machine->globals[0] = (AihcSlot)cell;",
        "  }",
        "  if (machine->heap_allocated_bytes != 16008) return 7;",
        "  if (machine->gc_count == 0) return 8;",
        "  if (machine->heap_peak_bytes == 0) return 9;"
      ]
        <> endingLines
        <> ["}"]
    )
  where
    endingLines =
      case ending of
        EndsWithProcessExit -> ["  aihc_exit_process(0);"]
        EndsWithReturn ->
          [ "  aihc_runtime_statistics_report();",
            "  return 0;"
          ]
