module Test.Native.Runtime
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), backendCompiler)
import Aihc.Testing.RuntimeArchive (RuntimeBuild (..), cachedRuntimeArchive)
import Data.Aeson (Value, eitherDecodeFileStrict, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
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
    [ runtimeProgramTest "Lir runtime units implement mutable references" [] mutVarSource,
      runtimeProgramTest "Lir runtime units implement byte arrays" [] byteArraySource,
      runtimeProgramTest
        "Lir runtime units parse the RTS options"
        ["+RTS", "-M2k", "-RTS", "kept", "--RTS", "+RTS", "-M1X"]
        runtimeOptionsSource,
      runtimeProgramTestWith
        "Lir runtime units keep the process environment"
        []
        (const [("AIHC_RUNTIME_TEST", "one")])
        runtimeEnvironmentSource
        (const (pure ())),
      runtimeProgramTest "semispace grows when live data exceeds the initial space" [] growthSource,
      runtimeProgramTest "semispace stops at the heap limit" ["+RTS", "-M256", "-RTS"] heapLimitSource,
      runtimeProgramTest
        "static reference roots collect a static object no table names"
        []
        staticReferenceSource,
      runtimeProgramTest "stack growth brings the next collection closer" [] stackChargeSource,
      runtimeStatisticsTest "AIHC_RTS_STATS receives the statistics when the process exits" True EndsWithProcessExit,
      runtimeStatisticsTest "AIHC_RTS_STATS receives the statistics when the machine halts" True EndsWithReturn,
      runtimeStatisticsTest "no statistics file is written without AIHC_RTS_STATS" False EndsWithProcessExit,
      allocationProfileTest
    ]

-- | Compile one C program against the selected runtime with a 64-byte initial
-- semispace. Then, run it with the given arguments and expect exit status 0.
runtimeProgramTest :: String -> [String] -> String -> TestTree
runtimeProgramTest name programArguments source =
  runtimeProgramTestWith name programArguments (const []) source (const (pure ()))

-- | Like 'runtimeProgramTest', with environment variables for the program
-- and a check that runs in the temporary directory after the program exits.
-- Both receive the temporary directory, so a variable can name a file there.
runtimeProgramTestWith :: String -> [String] -> (FilePath -> [(String, String)]) -> String -> (FilePath -> IO ()) -> TestTree
runtimeProgramTestWith name programArguments extraEnvironment source check =
  testCase name $
    withSystemTempDirectory "aihc-runtime" $ \directory -> do
      -- The tiny semispace forces a collection in every one of these
      -- programs, so the runtime archive is built for this test rather than
      -- taken from the store. Every test here shares that one archive.
      build <- cachedRuntimeArchive Llvm ["-std=c11", "-Wall", "-Wextra", "-Werror", "-DAIHC_NURSERY_BYTES=64", "-DAIHC_GEN2_MINIMUM_BYTES=4096", "-DAIHC_MARK_SLICE_FLOOR=256", "-DAIHC_MARK_SLICE_CAP=512"]
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
      "  aihc_ensure_heap(machine, 8, 0, NULL, NULL);",
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
      "  size_t name_length = strlen(argv[0]) + 1;",
      "  int64_t size = aihc_program_arguments_size();",
      "  if (size != (int64_t)(name_length + sizeof(kept))) return 2;",
      "  char buffer[512];",
      "  if (aihc_program_arguments_copy(buffer, 1) != size) return 3;",
      "  if (aihc_program_arguments_copy(NULL, 1) != -1) return 4;",
      "  if (aihc_program_arguments_copy(buffer, sizeof(buffer)) != size) return 5;",
      "  if (memcmp(buffer, argv[0], name_length) != 0) return 6;",
      "  if (memcmp(buffer + name_length, kept, sizeof(kept)) != 0) return 7;",
      "  aihc_ensure_heap(machine, aihc_byte_array_words(sizeof(replaced), 0, 1), 0, NULL, NULL);",
      "  void *replacement = aihc_byte_array_new(machine, sizeof(replaced));",
      "  memcpy(aihc_byte_array_contents(replacement), replaced, sizeof(replaced));",
      "  if (aihc_program_arguments_replace(replacement, sizeof(replaced) - 1) != -1) return 8;",
      "  if (aihc_program_arguments_size() != size) return 9;",
      "  if (aihc_program_arguments_replace(replacement, sizeof(replaced)) != 0) return 10;",
      "  if (aihc_program_arguments_size() != (int64_t)sizeof(replaced)) return 11;",
      "  if (aihc_program_arguments_copy(buffer, sizeof(buffer)) != (int64_t)sizeof(replaced)) return 12;",
      "  if (memcmp(buffer, replaced, sizeof(replaced)) != 0) return 13;",
      "  if (aihc_program_arguments_replace(NULL, 0) != 0) return 14;",
      "  if (aihc_program_arguments_size() != 0) return 15;",
      "  if (aihc_runtime_arguments_initialize(replaced, sizeof(replaced) - 1) != -1) return 16;",
      "  if (aihc_runtime_arguments_initialize(replaced, sizeof(replaced)) != -1) return 17;",
      "  if (aihc_program_arguments_size() != 0) return 18;",
      "  return 0;",
      "}"
    ]

-- | The environment store lives beside the RTS option parser in
-- aihc_runtime_options.lir. The host hands over the flattened process
-- environment, which lookupEnv and getEnvironment read back through these
-- two accessors.
runtimeEnvironmentSource :: String
runtimeEnvironmentSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include \"aihc_runtime_internal.h\"",
      "#include <string.h>",
      "/* The implicit terminator of the literal ends its last entry. */",
      "static const char entries[] = \"AIHC_RUNTIME_TEST=two\\0AIHC_RTS_STATS=\";",
      "static const char unterminated[] = \"BROKEN=1\";",
      "/* One entry of the buffer at a time, as the Haskell side reads it. */",
      "static int names(const char *buffer, int64_t length, const char *entry) {",
      "  for (int64_t offset = 0; offset < length;) {",
      "    if (strcmp(buffer + offset, entry) == 0) return 1;",
      "    offset += (int64_t)strlen(buffer + offset) + 1;",
      "  }",
      "  return 0;",
      "}",
      "int main(void) {",
      "  aihc_program_environment_initialize();",
      "  int64_t inherited = aihc_program_environment_size();",
      "  char buffer[65536];",
      "  if (inherited <= 0 || (size_t)inherited > sizeof(buffer)) return 1;",
      "  if (aihc_program_environment_copy(NULL, 1) != -1) return 2;",
      "  if (aihc_program_environment_copy(buffer, -1) != -1) return 3;",
      "  if (aihc_program_environment_copy(buffer, 1) != inherited) return 4;",
      "  if (aihc_program_environment_copy(buffer, sizeof(buffer)) != inherited) return 5;",
      "  if (!names(buffer, inherited, \"AIHC_RUNTIME_TEST=one\")) return 6;",
      "  if (aihc_runtime_environment_initialize(unterminated, sizeof(unterminated) - 1) != -1) return 7;",
      "  if (aihc_program_environment_size() != inherited) return 8;",
      "  if (aihc_runtime_environment_initialize(entries, sizeof(entries)) != 0) return 9;",
      "  if (aihc_program_environment_size() != (int64_t)sizeof(entries)) return 10;",
      "  if (aihc_program_environment_copy(buffer, sizeof(buffer)) != (int64_t)sizeof(entries)) return 11;",
      "  if (memcmp(buffer, entries, sizeof(entries)) != 0) return 12;",
      "  if (!names(buffer, (int64_t)sizeof(entries), \"AIHC_RUNTIME_TEST=two\")) return 13;",
      "  if (aihc_runtime_environment_initialize(NULL, 0) != 0) return 14;",
      "  if (aihc_program_environment_size() != 0) return 15;",
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
      "  AihcMachine *machine = aihc_machine_new(0);",
      "  aihc_ensure_heap(machine, 1024, 0, NULL, NULL);",
      "  void *bytes = aihc_byte_array_new(machine, 16);",
      "  if (aihc_byte_array_get_size(bytes) != 16) return 1;",
      "  if (aihc_byte_array_is_pinned(bytes)) return 2;",
      "  if (!aihc_byte_array_is_pinned(aihc_byte_array_new_pinned(machine, 8))) return 3;",
      "  void *aligned = aihc_byte_array_new_aligned_pinned(machine, 8, 64);",
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
      "  void *other = aihc_byte_array_new(machine, 16);",
      "  aihc_byte_array_copy(bytes, 0, other, 0, 16);",
      "  if (aihc_byte_array_compare(bytes, 0, other, 0, 16) != 0) return 12;",
      "  put_word(other, 0, 0);",
      "  if ((int64_t)aihc_byte_array_compare(bytes, 0, other, 0, 16) != 1) return 13;",
      "  if ((int64_t)aihc_byte_array_compare(other, 0, bytes, 0, 16) != -1) return 14;",
      "  /* Ranges of one array may overlap, so a copy has to move. */",
      "  aihc_byte_array_copy(bytes, 0, bytes, 4, 12);",
      "  aihc_byte_array_copy_to_addr(bytes, 0, out, 16);",
      "  if (memcmp(out, \"hellhello world!\", 16) != 0) return 15;",
      "  void *grown = aihc_byte_array_resize(machine, bytes, 32);",
      "  if (aihc_byte_array_get_size(grown) != 32) return 16;",
      "  if (aihc_byte_array_compare(grown, 0, bytes, 0, 16) != 0) return 17;",
      "  aihc_byte_array_shrink(grown, 4);",
      "  if (aihc_byte_array_get_size(grown) != 4) return 18;",
      "  void *empty = aihc_byte_array_new(machine, 0);",
      "  if (aihc_byte_array_get_size(empty) != 0) return 19;",
      "  aihc_byte_array_copy_from_addr(NULL, empty, 0, 0);",
      "  /* The atomic operations index by word and give the old value. */",
      "  void *sized = aihc_byte_array_new(machine, 32);",
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
-- 16000 bytes, so the 64-byte nursery collects several times.
growthSource :: String
growthSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "/* The allocation protocol of compiled code: reserve the words of the",
      "   object, then bump the heap pointer and write the header. The runtime has",
      "   no allocator to call, so a program here does what a backend does. */",
      "static inline AihcValue *place_node(AihcMachine *machine,",
      "                                    const AihcInfo *info, uint64_t words) {",
      "  AihcValue *value = (AihcValue *)machine->heap_next;",
      "  machine->heap_next += 8 * words;",
      "  value->header = (AihcSlot)(uintptr_t)info;",
      "  return value;",
      "}",
      "static inline AihcValue *make_node(AihcMachine *machine,",
      "                                   const AihcInfo *info, uint64_t words) {",
      "  aihc_ensure_heap(machine, words, 0, 0, 0);",
      "  return place_node(machine, info, words);",
      "}",
      "static const uint8_t cell_is_pointer[] = {1};",
      "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
      "static const AihcInfo leaf_info = {.identity = 2, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(1);",
      "  machine->globals[0] = (AihcSlot)make_node(machine, &leaf_info, 1);",
      "  for (int index = 0; index < 1000; ++index) {",
      "    AihcValue *cell = make_node(machine, &cell_info, 2);",
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
      "  if (machine->gc_count == 0) return 3;",
      "  return 0;",
      "}"
    ]

-- | A minor collection scans each young stack chunk, so a stack growth
-- charges its chunk to the nursery. The program grows the stack by whole
-- chunks without an allocation, and the heap limit goes down by one chunk
-- each time. A push and a pop across one boundary charge the chunk once.
-- When the charges fill the nursery, the next reservation collects.
stackChargeSource :: String
stackChargeSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include \"aihc_runtime_internal.h\"",
      "/* Two frames of this size fill the frame space of a chunk. */",
      "#define FRAME_WORDS ((AIHC_STACK_CHUNK_BYTES - AIHC_STACK_CHUNK_HEADER_BYTES) / sizeof(AihcSlot) / 2)",
      "#define CHUNK AIHC_STACK_CHUNK_BYTES",
      "#define CHUNKS 16",
      "static const uint8_t frame_is_pointer[FRAME_WORDS - 1] = {0};",
      "static const AihcInfo frame_info = {.identity = 1, .field_is_pointer = frame_is_pointer, .field_count = FRAME_WORDS - 1, .frame_kind = AIHC_FRAME_NORMAL, .object_kind = AIHC_OBJECT_CLOSURE, .remaining_arity = 1};",
      "/* No root names these frames, so a collection does not scan them. */",
      "static AihcValue *push(AihcMachine *machine) {",
      "  AihcValue *frame = aihc_stack_push(machine, FRAME_WORDS);",
      "  frame->header = (AihcSlot)(uintptr_t)&frame_info;",
      "  return frame;",
      "}",
      "/* Grow a full stack by one chunk and fill that chunk. Give the top frame. */",
      "static AihcValue *push_chunk(AihcMachine *machine) {",
      "  push(machine);",
      "  return push(machine);",
      "}",
      "int main(void) {",
      "  AihcMachine *machine = aihc_machine_new(0);",
      "  /* Move the start-up objects out of the nursery. Then give the machine",
      "     a nursery of CHUNKS chunks. */",
      "  aihc_gc_collect_generation(machine, 0, 0, NULL, NULL);",
      "  aihc_nursery_replace(machine, CHUNKS * CHUNK);",
      "  uint8_t *start = machine->heap_start;",
      "  uint64_t collections = machine->gc_count;",
      "  /* The stack can hold frames of the start-up already. Push until a",
      "     frame starts a chunk, and fill that chunk. The checks start there. */",
      "  AihcValue *base;",
      "  do {",
      "    base = push(machine);",
      "  } while (((uintptr_t)base & (CHUNK - 1)) != AIHC_STACK_CHUNK_HEADER_BYTES);",
      "  base = push(machine);",
      "  uint8_t *limit = machine->heap_limit;",
      "  uint64_t charged = machine->fixed_since_gc;",
      "  if (limit < start + (CHUNKS - 1) * CHUNK) return 1;",
      "  AihcValue *frames[CHUNKS] = {0};",
      "  for (int index = 0; index < CHUNKS / 2; ++index) {",
      "    frames[index] = push_chunk(machine);",
      "  }",
      "  if (machine->heap_limit != limit - CHUNKS / 2 * CHUNK) return 2;",
      "  if (machine->fixed_since_gc != charged + CHUNKS / 2 * CHUNK) return 3;",
      "  /* Pop the top chunk and push it again many times. The chunk has its",
      "     charge already. */",
      "  for (int index = 0; index < 1000; ++index) {",
      "    aihc_stack_resume_after(machine, frames[CHUNKS / 2 - 2]);",
      "    frames[CHUNKS / 2 - 1] = push_chunk(machine);",
      "  }",
      "  if (machine->heap_limit != limit - CHUNKS / 2 * CHUNK) return 4;",
      "  for (int index = CHUNKS / 2; index < CHUNKS; ++index) {",
      "    frames[index] = push_chunk(machine);",
      "  }",
      "  /* The charges used the nursery, so the limit is below the nursery. */",
      "  if (machine->heap_limit >= start) return 5;",
      "  if (machine->gc_count != collections) return 6;",
      "  /* A reservation of zero words collects. */",
      "  aihc_ensure_heap(machine, 0, 0, NULL, NULL);",
      "  if (machine->gc_count != collections + 1) return 7;",
      "  if (machine->heap_limit != start + CHUNKS * CHUNK) return 8;",
      "  /* The collection starts a new charge for each chunk, so a pop and a",
      "     push across the top boundary charge the chunk again. */",
      "  aihc_stack_resume_after(machine, frames[CHUNKS - 2]);",
      "  push_chunk(machine);",
      "  if (machine->heap_limit != start + (CHUNKS - 1) * CHUNK) return 9;",
      "  aihc_stack_resume_after(machine, base);",
      "  return 0;",
      "}"
    ]

-- | The table retains one CAF. The other CAF must become unreachable.
staticReferenceSource :: String
staticReferenceSource =
  unlines
    ( [ "#include \"aihc_runtime.h\"",
        "/* The allocation protocol of compiled code: reserve the words of the",
        "   object, then bump the heap pointer and write the header. The runtime has",
        "   no allocator to call, so a program here does what a backend does. */",
        "static inline AihcValue *place_node(AihcMachine *machine,",
        "                                    const AihcInfo *info, uint64_t words) {",
        "  AihcValue *value = (AihcValue *)machine->heap_next;",
        "  machine->heap_next += 8 * words;",
        "  value->header = (AihcSlot)(uintptr_t)info;",
        "  return value;",
        "}",
        "static const uint8_t cell_is_pointer[] = {1};",
        "static const AihcInfo cell_info = {.identity = 1, .field_is_pointer = cell_is_pointer, .field_count = 1, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static const AihcInfo leaf_info = {.identity = 2, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_NODE};",
        "static const AihcInfo thunk_info = {.identity = 3, .field_count = 0, .frame_kind = AIHC_FRAME_NONE, .object_kind = AIHC_OBJECT_THUNK, .needs_eval = AIHC_NEEDS_EVAL_ENTER};",
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
        "/* The code building a list is the running function, and it reaches the",
        "   named CAF, so every collection it requests passes the table. */",
        "static AihcValue *build_list(AihcMachine *machine, const AihcSrt *srt,",
        "                             int length) {",
        "  aihc_ensure_heap(machine, 1, 0, 0, srt);",
        "  AihcSlot head = (AihcSlot)place_node(machine, &leaf_info, 1);",
        "  AihcMachine *held = machine;",
        "  for (int index = 0; index < length; ++index) {",
        "    aihc_ensure_heap(held, 2, 1, &head, srt);",
        "    AihcValue *cell = place_node(held, &cell_info, 2);",
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
        "  const AihcSrt *srt = (const AihcSrt *)&named_srt;",
        "  aihc_update((AihcValue *)&named_caf, build_list(machine, srt, 200));",
        "  aihc_update((AihcValue *)&unnamed_caf, build_list(machine, srt, 200));",
        "  aihc_ensure_heap(machine, 4096, 0, 0, srt);",
        "  uint64_t live = (uint64_t)(machine->heap_next - machine->heap_start);",
        "  if (list_length((AihcValue *)aihc_value_fields((AihcValue *)&named_caf)[0]) != 200) return 1;"
      ]
        <> ["  if (live > 4800) return 2;"]
        <> [ "  return 0;",
             "}"
           ]
    )

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
      "/* The allocation protocol of compiled code: reserve the words of the",
      "   object, then bump the heap pointer and write the header. The runtime has",
      "   no allocator to call, so a program here does what a backend does. */",
      "static inline AihcValue *place_node(AihcMachine *machine,",
      "                                    const AihcInfo *info, uint64_t words) {",
      "  AihcValue *value = (AihcValue *)machine->heap_next;",
      "  machine->heap_next += 8 * words;",
      "  value->header = (AihcSlot)(uintptr_t)info;",
      "  return value;",
      "}",
      "static inline AihcValue *make_node(AihcMachine *machine,",
      "                                   const AihcInfo *info, uint64_t words) {",
      "  aihc_ensure_heap(machine, words, 0, 0, 0);",
      "  return place_node(machine, info, words);",
      "}",
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
      "      AihcValue *cell = make_node(machine, &cell_info, 2);",
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
  runtimeProgramTestWith name [] environment (statisticsSource ending) check
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
          assertEqual "field names" ["allocated_bytes", "gc_count", "gc_full_count", "gc_gen1_count", "gc_max_pause_ns", "gc_minor_count", "gc_time_ns", "live_bytes", "peak_heap_bytes", "schema"] (Map.keys statistics)
          assertEqual "schema" (Just 3) (Map.lookup "schema" statistics)
          -- After the counter reset, one leaf and 1000 cells use 8 + 1000 * 16 bytes.
          assertEqual "allocated_bytes" (Just 16008) (Map.lookup "allocated_bytes" statistics)
          assertBool "peak_heap_bytes holds the live list" (Map.lookup "peak_heap_bytes" statistics >= Just 16008)
          assertBool "gc_count counts the collections" (Map.lookup "gc_count" statistics >= Just 1)
          -- The last collection kept the leaf and the cells built so far.
          assertBool "live_bytes holds part of the list" (Map.lookup "live_bytes" statistics > Just 0)
          assertBool "live_bytes stays below the peak" (Map.lookup "live_bytes" statistics <= Map.lookup "peak_heap_bytes" statistics)
          assertBool "gc_max_pause_ns is one of the collections" (Map.lookup "gc_max_pause_ns" statistics <= Map.lookup "gc_time_ns" statistics)
        else assertBool "no statistics file exists" (not present)

-- | The allocation profile in the statistics file.
--
-- Exception to the fixture rule, approved by the user on Oct 3 2026. The
-- tested property is that the runtime writes the counters that the entry
-- registers: the entries with no objects are left out, the others come
-- with the most bytes first, and a name is a valid JSON string. No fixture
-- can test this today: the fixture harnesses that run a program link it
-- with their own main, not with the generated entry that registers the
-- counters, and the Lir fixture @profile-allocations.yaml@ only shows the
-- lowering. This program registers a table by hand, as the entry does.
allocationProfileTest :: TestTree
allocationProfileTest =
  runtimeProgramTestWith "AIHC_RTS_STATS lists the registered allocation counters" [] environment allocationProfileSource check
  where
    statisticsFile directory = directory </> "stats.json"
    environment directory = [("AIHC_RTS_STATS", statisticsFile directory)]
    check directory = do
      decoded <- eitherDecodeFileStrict (statisticsFile directory)
      statistics <- either (assertFailure . ("statistics JSON: " <>)) pure decoded :: IO (Map String Value)
      assertEqual
        "allocations"
        ( Just
            ( toJSON
                [ object [Key.fromString "name" .= ("C big" :: String), Key.fromString "objects" .= (5 :: Int), Key.fromString "bytes" .= (80 :: Int)],
                  object [Key.fromString "name" .= ("F \"quoted\\name\"" :: String), Key.fromString "objects" .= (1 :: Int), Key.fromString "bytes" .= (32 :: Int)]
                ]
            )
        )
        (Map.lookup "allocations" statistics)

-- | Register three counters, one of them without objects, and write the
-- statistics.
allocationProfileSource :: String
allocationProfileSource =
  unlines
    [ "#include \"aihc_runtime.h\"",
      "#include \"aihc_runtime_internal.h\"",
      "static const char *const names[] = {\"F \\\"quoted\\\\name\\\"\", \"P empty/1\", \"C big\"};",
      "static uint64_t counts[] = {1, 4, 0, 0, 5, 10};",
      "static const uint64_t size = 3;",
      "int main(int argc, char *const argv[]) {",
      "  aihc_program_arguments_initialize(argc, argv);",
      "  aihc_program_environment_initialize();",
      "  aihc_allocation_profile_register(names, counts, &size);",
      "  (void)aihc_machine_new(1);",
      "  aihc_runtime_statistics_report();",
      "  return 0;",
      "}"
    ]

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
        "/* The allocation protocol of compiled code: reserve the words of the",
        "   object, then bump the heap pointer and write the header. The runtime has",
        "   no allocator to call, so a program here does what a backend does. */",
        "static inline AihcValue *place_node(AihcMachine *machine,",
        "                                    const AihcInfo *info, uint64_t words) {",
        "  AihcValue *value = (AihcValue *)machine->heap_next;",
        "  machine->heap_next += 8 * words;",
        "  value->header = (AihcSlot)(uintptr_t)info;",
        "  return value;",
        "}",
        "static inline AihcValue *make_node(AihcMachine *machine,",
        "                                   const AihcInfo *info, uint64_t words) {",
        "  aihc_ensure_heap(machine, words, 0, 0, 0);",
        "  return place_node(machine, info, words);",
        "}",
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
        "  aihc_reset_heap_allocated_bytes(machine);",
        "  machine->globals[0] = (AihcSlot)make_node(machine, &leaf_info, 1);",
        "  for (int index = 0; index < 1000; ++index) {",
        "    AihcValue *cell = make_node(machine, &cell_info, 2);",
        "    aihc_set_field(cell, 0, machine->globals[0]);",
        "    machine->globals[0] = (AihcSlot)cell;",
        "  }",
        "  /* Compiled code allocates without telling the runtime, so the total",
        "     is only exact once the bump pointer has been accounted for. */",
        "  aihc_heap_account(machine);",
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
