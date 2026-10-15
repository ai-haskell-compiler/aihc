#include "aihc_runtime.h"
#include "aihc_runtime_internal.h"

#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* Match the offsets in aihc_constants.lir on both pointer widths. */
_Static_assert(offsetof(AihcCapability, machine) == 0 * sizeof(void *),
               "capability machine ABI");
_Static_assert(offsetof(AihcCapability, exit_code) == 1 * sizeof(void *),
               "capability exit_code ABI");
_Static_assert(offsetof(AihcCapability, heap_next) == 2 * sizeof(void *),
               "capability heap_next ABI");
_Static_assert(offsetof(AihcCapability, heap_limit) == 3 * sizeof(void *),
               "capability heap_limit ABI");
_Static_assert(offsetof(AihcCapability, stack_next) == 4 * sizeof(void *),
               "capability stack_next ABI");
_Static_assert(offsetof(AihcCapability, heap_start) == 5 * sizeof(void *),
               "capability heap_start ABI");
_Static_assert(offsetof(AihcCapability, current_thread) == 6 * sizeof(void *),
               "capability current_thread ABI");
_Static_assert(offsetof(AihcCapability, nursery_bytes) ==
                   6 * sizeof(void *) + 8,
               "capability nursery_bytes ABI");
_Static_assert(offsetof(AihcCapability, pause_top) == 6 * sizeof(void *) + 16,
               "capability pause_top ABI");

/* Five words and five bytes, rounded up to a word. This matches
   @AIHC_INFO_BYTES in aihc_constants.lir. */
_Static_assert(sizeof(AihcInfo) == 5 * sizeof(void *) + 8,
               "info-table size ABI");
_Static_assert(_Alignof(AihcInfo) >= 4, "info tables need two header tag bits");

#if UINTPTR_MAX == UINT64_MAX
_Static_assert(offsetof(AihcInfo, field_is_pointer) == 8,
               "info-table bitmap ABI");
_Static_assert(offsetof(AihcInfo, next) == 16, "info-table next ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 24,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, srt) == 32, "info-table SRT ABI");
_Static_assert(offsetof(AihcInfo, field_count) == 40,
               "info-table field-count ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 41,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 42,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 43,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, needs_eval) == 44,
               "info-table needs-eval ABI");
_Static_assert(sizeof(AihcInfo) == 48, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 8, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 16, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 24, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 16,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 24, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 32, "resume count ABI");
_Static_assert(offsetof(AihcThread, id) == 8, "thread identifier ABI");
_Static_assert(offsetof(AihcStableName, value) == 8, "stable-name value ABI");
_Static_assert(offsetof(AihcStableName, hash) == 16, "stable-name hash ABI");
_Static_assert(offsetof(AihcStableName, next) == 24, "stable-name next ABI");
_Static_assert(sizeof(AihcStableName) == 32, "stable-name size ABI");
_Static_assert(offsetof(AihcMVar, full) == 8, "MVar full-flag ABI");
_Static_assert(offsetof(AihcMVar, value) == 16, "MVar value ABI");
#elif UINTPTR_MAX == UINT32_MAX
_Static_assert(offsetof(AihcInfo, field_is_pointer) == 4,
               "info-table bitmap ABI");
_Static_assert(offsetof(AihcInfo, next) == 8, "info-table next ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 12,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, srt) == 16, "info-table SRT ABI");
_Static_assert(offsetof(AihcInfo, field_count) == 20,
               "info-table field-count ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 21,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 22,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 23,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, needs_eval) == 24,
               "info-table needs-eval ABI");
_Static_assert(sizeof(AihcInfo) == 28, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 4, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 8, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 12, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 12,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 16, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 24, "resume count ABI");
_Static_assert(offsetof(AihcThread, id) == 8, "thread identifier ABI");
_Static_assert(offsetof(AihcStableName, value) == 8, "stable-name value ABI");
_Static_assert(offsetof(AihcStableName, hash) == 16, "stable-name hash ABI");
_Static_assert(offsetof(AihcMVar, full) == 8, "MVar full-flag ABI");
_Static_assert(offsetof(AihcMVar, value) == 16, "MVar value ABI");
_Static_assert(offsetof(AihcStableName, next) == 24, "stable-name next ABI");
_Static_assert(sizeof(AihcStableName) == 32, "stable-name size ABI");
#endif

/* Copy the record to fixed-width slots, then release its object references. */
void aihc_lir_take_resume(AihcCapability *capability, AihcResume *resume,
                          uint64_t *slots) {
  /* An application returns to its continuation, so that frame is the top of
     the stack. A continuation that the resumption enters sets the stack
     pointer itself when the continue helper enters it. */
  if (resume->kind == AIHC_RESUME_APPLY) {
    aihc_stack_resume_after(capability, resume->continuation);
  } else if (resume->kind == AIHC_RESUME_CONTINUE) {
    aihc_stack_resume_after(capability, resume->function);
  } else if (resume->continuation != NULL) {
    /* The continue helper enters the frame without a chunk change when the
       same thread runs again, so the chunk is made young here. */
    aihc_stack_enter_chunk(capability, resume->continuation);
  }
  slots[0] = resume->kind;
  slots[1] = (uintptr_t)resume->function;
  slots[2] = (uintptr_t)resume->continuation;
  slots[3] = resume->value;
  slots[4] = resume->count;
  memset(resume, 0, sizeof(*resume));
}

_Noreturn void aihc_fail(const char *message) { aihc_host_fail(message); }

static const AihcResume *aihc_schedule(AihcCapability *capability);
static void aihc_workers_start(AihcCapability *capability);

/* aihc_helpers.lir defines the indirection table, because the update of a
   thunk without waiters writes it there. */
extern const AihcInfo aihc_indirection_info;
static const AihcInfo aihc_io_request_info = {
    .object_kind = AIHC_OBJECT_IO_REQUEST,
};
static const AihcInfo aihc_thread_info = {
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_THREAD,
};
static const AihcInfo aihc_blackhole_waiter_info = {
    .object_kind = AIHC_OBJECT_BLACKHOLE_WAITER,
};
static const AihcInfo aihc_blackhole_queue_info = {
    .object_kind = AIHC_OBJECT_BLACKHOLE_QUEUE,
};
static const AihcInfo aihc_mvar_info = {
    .object_kind = AIHC_OBJECT_MVAR,
};
static const AihcInfo aihc_mvar_waiter_info = {
    .object_kind = AIHC_OBJECT_MVAR_WAITER,
};
static const AihcInfo aihc_transaction_info = {
    .object_kind = AIHC_OBJECT_TRANSACTION,
};
static const AihcInfo aihc_transaction_write_info = {
    .object_kind = AIHC_OBJECT_TRANSACTION_WRITE,
};
static const AihcInfo aihc_transaction_timer_info = {
    .object_kind = AIHC_OBJECT_TRANSACTION_TIMER,
};

/* Native records contain C pointers, whose width depends on the target. */
static uint64_t aihc_record_words(size_t bytes) {
  return (bytes + sizeof(AihcSlot) - 1) / sizeof(AihcSlot);
}

void aihc_record_allocation(AihcCapability *capability) {
  if (capability->allocation_count == UINT64_MAX) {
    aihc_fail("allocation counter overflow");
  }
  ++capability->allocation_count;
}

uint64_t aihc_array_words(int64_t count) {
  /* The bound matches the plausibility check of aihc_array_new. */
  if (count < 0 || (uint64_t)count > UINT64_C(2305843009213693949)) {
    aihc_fail("boxed-array size is invalid");
  }
  return (uint64_t)count + 2;
}

/* Compute the complete allocation charge before a collection can occur. */
uint64_t aihc_byte_array_words(int64_t size, uint64_t pinned,
                               int64_t alignment) {
  if (size < 0) {
    aihc_fail("invalid byte array size");
  }
  if (alignment <= 0 ||
      ((uint64_t)alignment & ((uint64_t)alignment - 1)) != 0) {
    aihc_fail("invalid byte array alignment");
  }
  uint64_t occupied = size == 0 ? 1 : (uint64_t)size;
  uint64_t slack = pinned && alignment > 8 ? (uint64_t)alignment - 1 : 0;
  uint64_t overhead =
      sizeof(AihcByteArray) + (pinned ? sizeof(AihcPinnedBlock) : 0);
  if (slack > SIZE_MAX - overhead - 7 ||
      occupied > SIZE_MAX - overhead - 7 - slack) {
    aihc_fail("byte array allocation is too large");
  }
  return (overhead + occupied + slack + 7) / sizeof(AihcSlot);
}

uint64_t aihc_byte_array_resize_words(void *opaque_array, int64_t size) {
  AihcByteArray *array = opaque_array;
  if (array == NULL) {
    aihc_fail("attempted to resize a null byte array");
  }
  return aihc_byte_array_words(size, array->pinned, (int64_t)array->alignment);
}

static AihcSlot aihc_make_header(const AihcInfo *info) {
  if (info == NULL) {
    aihc_fail("info table is null");
  }
  switch (info->object_kind) {
  case AIHC_OBJECT_CLOSURE:
  case AIHC_OBJECT_THUNK:
  case AIHC_OBJECT_NODE:
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR:
    return (AihcSlot)(uintptr_t)info;
  default:
    aihc_fail("attempted to allocate an invalid object kind");
  }
}

uint64_t aihc_object_words(const AihcInfo *info) {
  uint64_t field_words = info->field_count;
  if (field_words == 0 && (info->object_kind == AIHC_OBJECT_THUNK ||
                           info->object_kind == AIHC_OBJECT_BLACKHOLE ||
                           info->object_kind == AIHC_OBJECT_INDIRECTION)) {
    field_words = 1;
  }
  return 1 + field_words;
}

uint64_t aihc_value_words(const AihcValue *value) {
  switch (aihc_value_kind(value)) {
  case AIHC_OBJECT_IO_HANDLE:
    return aihc_record_words(sizeof(AihcIoHandle));
  case AIHC_OBJECT_IO_REQUEST:
    return aihc_record_words(sizeof(AihcIoRequest));
  case AIHC_OBJECT_BYTE_ARRAY:
    return ((const AihcByteArray *)value)->words;
  case AIHC_OBJECT_STABLE_NAME:
    return aihc_record_words(sizeof(AihcStableName));
  case AIHC_OBJECT_THREAD:
    return aihc_record_words(sizeof(AihcThread));
  case AIHC_OBJECT_BLACKHOLE_WAITER:
    return aihc_record_words(sizeof(AihcBlackholeWaiter));
  case AIHC_OBJECT_BLACKHOLE_QUEUE:
    return aihc_record_words(sizeof(AihcBlackholeQueue));
  case AIHC_OBJECT_MVAR:
    return aihc_record_words(sizeof(AihcMVar));
  case AIHC_OBJECT_MVAR_WAITER:
    return aihc_record_words(sizeof(AihcMVarWaiter));
  case AIHC_OBJECT_TRANSACTION:
    return aihc_record_words(sizeof(AihcTransaction));
  case AIHC_OBJECT_TRANSACTION_WRITE:
    return aihc_record_words(sizeof(AihcTransactionWrite));
  case AIHC_OBJECT_TRANSACTION_TIMER:
    return aihc_record_words(sizeof(AihcTransactionTimer));
  default:
    break;
  }
  if (aihc_value_kind(value) == AIHC_OBJECT_ARRAY) {
    return 2 + aihc_array_length(value);
  }
  /* A partial constructor spends field zero on its applied count, and its
     shared info table cannot say how wide this stage is. */
  if (aihc_value_kind(value) == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return 2 + aihc_partial_applied(value);
  }
  return aihc_object_words(aihc_value_info_table(value));
}

uint64_t aihc_array_length(const AihcValue *array) {
  if (array == NULL || aihc_value_kind(array) != AIHC_OBJECT_ARRAY) {
    aihc_fail("boxed-array primitive received a non-array");
  }
  return array->fields[0];
}

AihcSlot *aihc_array_elements(AihcValue *array) {
  (void)aihc_array_length(array);
  return array->fields + 1;
}

/* aihc_array_new, aihc_array_index, aihc_array_write, and the info table
   they share live in core-libs/aihc-rts/native/aihc_array.lir.
   aihc_array_length and aihc_array_elements stay here: the collector walks
   arrays through them, including the ones the GC fuzz harness builds with
   info tables of its own. */

/* aihc_mutvar_*, aihc_stable_name_*, the byte-array primitives, and the RTS
   option parser live in core-libs/aihc-rts/native/aihc_mutvar.lir,
   aihc_stable_name.lir, aihc_byte_array.lir, and aihc_runtime_options.lir.
   The two accessors below reach the stable-name fields of the shared machine.
 */

AihcStableName **aihc_stable_names(AihcCapability *capability) {
  return &capability->machine->stable_names;
}

uint64_t aihc_stable_name_take_hash(AihcCapability *capability) {
  if (capability->machine->next_stable_name > (uint64_t)INT64_MAX) {
    aihc_fail("stable-name counter overflow");
  }
  return capability->machine->next_stable_name++;
}

void aihc_memory_copy(void *destination, const void *source, uint64_t length) {
  memcpy(destination, source, (size_t)length);
}

void aihc_memory_move(void *destination, const void *source, uint64_t length) {
  memmove(destination, source, (size_t)length);
}

void aihc_memory_set(void *destination, uint64_t byte, uint64_t length) {
  memset(destination, (int)(byte & 0xff), (size_t)length);
}

/* The RTS option parser, the environment parser, and the argument store
   live in core-libs/aihc-rts/native/aihc_runtime_options.lir. This flattens a
   list of C strings for them: the width of a C pointer is the one thing a
   Lir unit does not know. */
extern int64_t aihc_parse_runtime_arguments(AihcCapability *, AihcRootFrame *,
                                            const void *, int64_t);
extern int64_t aihc_parse_runtime_environment(AihcCapability *, AihcRootFrame *,
                                              const void *, int64_t);

static int64_t aihc_runtime_import(const void *buffer, int64_t length,
                                   int64_t (*parse)(AihcCapability *,
                                                    AihcRootFrame *,
                                                    const void *, int64_t)) {
  AihcCapability *capability = aihc_machine_initialize();
  if (capability->machine->program_started) {
    return -1;
  }
  AihcRootFrame frame;
  aihc_roots_enter(capability, &frame, 0, NULL);
  int64_t result = parse(capability, &frame, buffer, length);
  aihc_roots_leave(capability, &frame);
  return result;
}

/* The first program argument that the host gave. The host can release its
   own copy of the arguments after the import, so the runtime keeps one. */
static char *aihc_initial_program_name;

const char *aihc_program_initial_name(void) {
  return aihc_initial_program_name;
}

/* Copy the first string of an argument buffer. A buffer without a
   terminated first string keeps no name: the import rejects it. */
static void aihc_keep_initial_program_name(const void *buffer, int64_t length) {
  free(aihc_initial_program_name);
  aihc_initial_program_name = NULL;
  if (buffer == NULL || length <= 0) {
    return;
  }
  const char *end = memchr(buffer, 0, (size_t)length);
  if (end == NULL) {
    return;
  }
  size_t size = (size_t)(end - (const char *)buffer) + 1;
  char *name = malloc(size);
  if (name == NULL) {
    aihc_fail("out of memory for the program name");
  }
  memcpy(name, buffer, size);
  aihc_initial_program_name = name;
}

int64_t aihc_runtime_arguments_initialize(const void *buffer, int64_t length) {
  /* The import can collect and move the buffer, so copy the name first. */
  aihc_keep_initial_program_name(buffer, length);
  return aihc_runtime_import(buffer, length, aihc_parse_runtime_arguments);
}

int64_t aihc_runtime_environment_initialize(const void *buffer,
                                            int64_t length) {
  return aihc_runtime_import(buffer, length, aihc_parse_runtime_environment);
}

static void aihc_strings_initialize(size_t count, char *const strings[],
                                    int64_t (*initialize)(const void *,
                                                          int64_t),
                                    const char *invalid_message) {
  if (count != 0 && strings == NULL) {
    aihc_fail(invalid_message);
  }
  size_t length = 0;
  for (size_t index = 0; index < count; ++index) {
    if (strings[index] == NULL) {
      aihc_fail(invalid_message);
    }
    size_t string_length = strlen(strings[index]);
    if ((uint64_t)string_length >= (uint64_t)INT64_MAX - (uint64_t)length) {
      aihc_fail(invalid_message);
    }
    length += string_length + 1;
  }
  AihcCapability *capability = aihc_machine_initialize();
  if (capability->machine->program_started) {
    aihc_fail("runtime imports require startup");
  }
  AihcRootFrame frame;
  aihc_roots_enter(capability, &frame, 0, NULL);
  uint8_t *buffer = aihc_byte_array_contents(
      aihc_host_byte_array(capability, &frame, length));
  size_t offset = 0;
  for (size_t index = 0; index < count; ++index) {
    size_t string_length = strlen(strings[index]);
    memcpy(buffer + offset, strings[index], string_length);
    offset += string_length + 1;
  }
  if (initialize(buffer, (int64_t)length) != 0) {
    aihc_roots_leave(capability, &frame);
    aihc_fail(invalid_message);
  }
  aihc_roots_leave(capability, &frame);
}

void aihc_program_arguments_initialize(int argc, char *const argv[]) {
  if (argc < 0) {
    aihc_fail("invalid initial program arguments");
  }
  aihc_strings_initialize((size_t)argc, argv, aihc_runtime_arguments_initialize,
                          "invalid initial program arguments");
}

void aihc_environment_initialize(char *const envp[]) {
  size_t count = 0;
  while (envp != NULL && envp[count] != NULL) {
    ++count;
  }
  aihc_strings_initialize(count, envp, aihc_runtime_environment_initialize,
                          "invalid program environment");
}

/* The capability of the process, for the statistics report: the exit path of
   the POSIX host has no capability at hand. */
static AihcCapability *aihc_process_capability;
static int aihc_statistics_reported;

/* The report is formatted by hand: snprintf would pull the stdio layer of
   libc into the WASI P3 runtime, and that layer imports WASI preview 1. */
static char *aihc_append_text(char *cursor, const char *text) {
  while (*text != 0) {
    *cursor++ = *text++;
  }
  return cursor;
}

/* Twenty digits hold every uint64_t. */
static char *aihc_append_decimal(char *cursor, uint64_t value) {
  char digits[20];
  size_t count = 0;
  do {
    digits[count++] = (char)('0' + value % 10);
    value /= 10;
  } while (value != 0);
  while (count != 0) {
    *cursor++ = digits[--count];
  }
  return cursor;
}

static const char *const *aihc_allocation_names;
static const uint64_t *aihc_allocation_counts;
static uint64_t aihc_allocation_size;

void aihc_allocation_profile_register(const char *const *names,
                                      const uint64_t *counts,
                                      const uint64_t *size) {
  aihc_allocation_names = names;
  aihc_allocation_counts = counts;
  aihc_allocation_size = *size;
}

/* The order of the allocation report: the most bytes first. */
static int aihc_allocation_order(const void *left, const void *right) {
  uint64_t left_words = aihc_allocation_counts[2 * *(const uint64_t *)left + 1];
  uint64_t right_words =
      aihc_allocation_counts[2 * *(const uint64_t *)right + 1];
  return left_words < right_words ? 1 : left_words > right_words ? -1 : 0;
}

/* A JSON string. A name holds no control character, so only a quote and a
   backslash need an escape. */
static char *aihc_append_json_string(char *cursor, const char *text) {
  *cursor++ = '"';
  for (; *text != 0; ++text) {
    if (*text == '"' || *text == '\\') {
      *cursor++ = '\\';
    }
    *cursor++ = *text;
  }
  *cursor++ = '"';
  return cursor;
}

/* The counters as a JSON array, one object for each info table that
   allocated, the most bytes first. */
static char *aihc_append_allocations(char *cursor, const uint64_t *order) {
  cursor = aihc_append_text(cursor, ", \"allocations\": [");
  int first = 1;
  for (uint64_t index = 0; index < aihc_allocation_size; ++index) {
    uint64_t site = order[index];
    uint64_t objects = aihc_allocation_counts[2 * site];
    if (objects == 0) {
      continue;
    }
    cursor = aihc_append_text(cursor,
                              first ? "\n  {\"name\": " : ",\n  {\"name\": ");
    first = 0;
    cursor = aihc_append_json_string(cursor, aihc_allocation_names[site]);
    cursor = aihc_append_text(cursor, ", \"objects\": ");
    cursor = aihc_append_decimal(cursor, objects);
    cursor = aihc_append_text(cursor, ", \"bytes\": ");
    cursor =
        aihc_append_decimal(cursor, 8 * aihc_allocation_counts[2 * site + 1]);
    *cursor++ = '}';
  }
  return aihc_append_text(cursor, "]");
}

void aihc_runtime_statistics_report(void) {
  const char *path = aihc_rts_stats_path();
  AihcCapability *capability = aihc_process_capability;
  if (path == NULL || capability == NULL || aihc_statistics_reported) {
    return;
  }
  aihc_statistics_reported = 1;
  aihc_gc_record_peak(capability);
  /* The counts of the capabilities add up. */
  uint64_t allocated = 0;
  uint64_t escapes = 0;
  uint64_t pushes = 0;
  for (uint64_t index = 0; index < capability->machine->capability_count;
       ++index) {
    AihcCapability *each = capability->machine->capabilities[index];
    aihc_heap_account(each);
    allocated += each->heap_allocated_bytes;
    escapes += each->escape_count;
    pushes += each->push_count;
  }
  /* The fixed text is 204 bytes and the ten numbers take at most 200. An
     allocation entry takes its name, which can double with escapes, and at
     most 90 more bytes. */
  size_t capacity = 512;
  uint64_t *order = NULL;
  if (aihc_allocation_size != 0) {
    order = malloc(aihc_allocation_size * sizeof(uint64_t));
    if (order == NULL) {
      aihc_fail("out of memory");
    }
    for (uint64_t index = 0; index < aihc_allocation_size; ++index) {
      order[index] = index;
      capacity += 2 * strlen(aihc_allocation_names[index]) + 90;
    }
    qsort(order, aihc_allocation_size, sizeof(uint64_t), aihc_allocation_order);
  }
  char *text = malloc(capacity);
  if (text == NULL) {
    aihc_fail("out of memory");
  }
  char *cursor = text;
  cursor = aihc_append_text(cursor, "{\"schema\": 3, \"peak_heap_bytes\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->heap_peak_bytes);
  cursor = aihc_append_text(cursor, ", \"allocated_bytes\": ");
  cursor = aihc_append_decimal(cursor, allocated);
  cursor = aihc_append_text(cursor, ", \"gc_count\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_count);
  cursor = aihc_append_text(cursor, ", \"gc_time_ns\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_time_ns);
  cursor = aihc_append_text(cursor, ", \"gc_max_pause_ns\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_max_pause_ns);
  cursor = aihc_append_text(cursor, ", \"live_bytes\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->heap_live_bytes);
  cursor = aihc_append_text(cursor, ", \"gc_minor_count\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_minor_count);
  cursor = aihc_append_text(cursor, ", \"gc_gen1_count\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_gen1_count);
  cursor = aihc_append_text(cursor, ", \"gc_full_count\": ");
  cursor = aihc_append_decimal(cursor, capability->machine->gc_full_count);
  cursor = aihc_append_text(cursor, ", \"escape_count\": ");
  cursor = aihc_append_decimal(cursor, escapes);
  cursor = aihc_append_text(cursor, ", \"push_count\": ");
  cursor = aihc_append_decimal(cursor, pushes);
  if (order != NULL) {
    cursor = aihc_append_allocations(cursor, order);
    free(order);
  }
  cursor = aihc_append_text(cursor, "}\n");
  if (aihc_host_write_file(path, text, (size_t)(cursor - text)) != 0) {
    aihc_fail("cannot write the runtime statistics file");
  }
  free(text);
}

static void aihc_visit_value(AihcValue **value, AihcRootVisitor visitor,
                             void *context) {
  *value =
      (AihcValue *)(uintptr_t)visitor((AihcSlot)(uintptr_t)*value, context);
}

static void *aihc_visit_pointer(void *pointer, AihcRootVisitor visitor,
                                void *context) {
  return (void *)(uintptr_t)visitor((AihcSlot)(uintptr_t)pointer, context);
}

/* Trace native records through their C layouts on both target word sizes. */
int aihc_visit_runtime_object(AihcValue *object, AihcRootVisitor visitor,
                              void *context) {
  switch (aihc_value_kind(object)) {
  case AIHC_OBJECT_IO_HANDLE:
    return 1;
  case AIHC_OBJECT_IO_REQUEST: {
    AihcIoRequest *request = (AihcIoRequest *)object;
    request->handle = aihc_visit_pointer(request->handle, visitor, context);
    request->next = aihc_visit_pointer(request->next, visitor, context);
    aihc_visit_value(&request->buffer_owner, visitor, context);
    aihc_visit_value(&request->continuation, visitor, context);
    request->thread = aihc_visit_pointer(request->thread, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_BYTE_ARRAY: {
    AihcByteArray *array = (AihcByteArray *)object;
    if (!array->pinned) {
      array->contents = (uint8_t *)(array + 1);
    }
    return 1;
  }
  case AIHC_OBJECT_STABLE_NAME:
    /* The collector processes the referent and lookup link as weak pointers. */
    return 1;
  case AIHC_OBJECT_THREAD: {
    /* The resumption and the transaction are roots of the capability, which
       aihc_visit_roots visits. The link of the run queue names another
       thread record, and every thread record is in gen2. */
    AihcThread *thread = (AihcThread *)object;
    thread->next = aihc_visit_pointer(thread->next, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_BLACKHOLE_WAITER: {
    AihcBlackholeWaiter *waiter = (AihcBlackholeWaiter *)object;
    waiter->thread = aihc_visit_pointer(waiter->thread, visitor, context);
    aihc_visit_value(&waiter->continuation, visitor, context);
    waiter->next = aihc_visit_pointer(waiter->next, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_BLACKHOLE_QUEUE: {
    AihcBlackholeQueue *queue = (AihcBlackholeQueue *)object;
    queue->owner = aihc_visit_pointer(queue->owner, visitor, context);
    aihc_visit_value(&queue->thunk, visitor, context);
    queue->head = aihc_visit_pointer(queue->head, visitor, context);
    queue->tail = aihc_visit_pointer(queue->tail, visitor, context);
    queue->next = aihc_visit_pointer(queue->next, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_MVAR: {
    AihcMVar *mvar = (AihcMVar *)object;
    if (mvar->full) {
      mvar->value = visitor(mvar->value, context);
    }
    mvar->readers_head =
        aihc_visit_pointer(mvar->readers_head, visitor, context);
    mvar->readers_tail =
        aihc_visit_pointer(mvar->readers_tail, visitor, context);
    mvar->takers_head = aihc_visit_pointer(mvar->takers_head, visitor, context);
    mvar->takers_tail = aihc_visit_pointer(mvar->takers_tail, visitor, context);
    mvar->putters_head =
        aihc_visit_pointer(mvar->putters_head, visitor, context);
    mvar->putters_tail =
        aihc_visit_pointer(mvar->putters_tail, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_MVAR_WAITER: {
    AihcMVarWaiter *waiter = (AihcMVarWaiter *)object;
    waiter->thread = aihc_visit_pointer(waiter->thread, visitor, context);
    aihc_visit_value(&waiter->continuation, visitor, context);
    waiter->value = visitor(waiter->value, context);
    waiter->next = aihc_visit_pointer(waiter->next, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_TRANSACTION: {
    AihcTransaction *transaction = (AihcTransaction *)object;
    transaction->writes =
        aihc_visit_pointer(transaction->writes, visitor, context);
    transaction->parent =
        aihc_visit_pointer(transaction->parent, visitor, context);
    transaction->reads =
        aihc_visit_pointer(transaction->reads, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_TRANSACTION_WRITE: {
    AihcTransactionWrite *write = (AihcTransactionWrite *)object;
    aihc_visit_value(&write->variable, visitor, context);
    write->value = visitor(write->value, context);
    write->next = aihc_visit_pointer(write->next, visitor, context);
    return 1;
  }
  case AIHC_OBJECT_TRANSACTION_TIMER: {
    AihcTransactionTimer *timer = (AihcTransactionTimer *)object;
    aihc_visit_value(&timer->variable, visitor, context);
    timer->final = visitor(timer->final, context);
    timer->next = aihc_visit_pointer(timer->next, visitor, context);
    return 1;
  }
  default:
    return 0;
  }
}

struct AihcCallbackSlot {
  AihcBackendEntry entry;
  AihcCapability *capability;
  AihcValue *closure;
  struct AihcCallbackSlot *next;
};

_Static_assert(sizeof(AihcCallbackSlot) == 4 * sizeof(void *),
               "callback slot must have four pointer fields");

struct AihcCallbackFrame {
  AihcCallbackFrame *previous;
  AihcCapability *capability;
  AihcValue *closure;
  AihcValue *continuation;
  uint64_t result;
  int complete;
};

_Static_assert(sizeof(AihcCallbackFrame) <= 48,
               "callback frame exceeds its LIR stack space");

static AihcCallbackSlot *aihc_callback_slots;

void aihc_foreign_enter(AihcCapability *capability, AihcForeignFrame *frame,
                        AihcSlot *roots, uint64_t count, const AihcSrt *srt,
                        uint64_t allow_callbacks) {
  *frame = (AihcForeignFrame){capability->foreign_frames, roots, count, srt,
                              allow_callbacks};
  capability->foreign_frames = frame;
}

void aihc_foreign_leave(AihcCapability *capability, AihcForeignFrame *frame) {
  if (capability->foreign_frames != frame) {
    aihc_fail("foreign frames are out of order");
  }
  capability->foreign_frames = frame->previous;
}

AihcBackendEntry aihc_callback_create(AihcCapability *capability,
                                      AihcValue *closure,
                                      AihcCallbackSlot *slots, uint64_t count) {
  for (uint64_t index = 0; index < count; ++index) {
    AihcCallbackSlot *slot = &slots[index];
    AihcCallbackSlot *registered = aihc_callback_slots;
    while (registered != NULL && registered != slot) {
      registered = registered->next;
    }
    if (registered == NULL) {
      slot->next = aihc_callback_slots;
      aihc_callback_slots = slot;
    }
    if (slot->closure == NULL) {
      slot->capability = capability;
      slot->closure = closure;
      return slot->entry;
    }
  }
  aihc_fail("foreign callback pool is exhausted");
}

void aihc_free_haskell_fun_ptr(AihcBackendEntry entry) {
  if (entry == NULL) {
    return;
  }
  for (AihcCallbackSlot *slot = aihc_callback_slots; slot != NULL;
       slot = slot->next) {
    if (slot->entry == entry && slot->closure != NULL) {
      slot->closure = NULL;
      slot->capability = NULL;
      return;
    }
  }
  aihc_fail("invalid foreign callback release");
}

void aihc_callback_enter(AihcCallbackFrame *frame, AihcBackendEntry entry,
                         const AihcInfo *stop_info) {
  AihcCallbackSlot *slot = aihc_callback_slots;
  while (slot != NULL && slot->entry != entry) {
    slot = slot->next;
  }
  if (slot == NULL || slot->closure == NULL) {
    aihc_fail("invalid foreign callback");
  }
  AihcCapability *capability = slot->capability;
  if (capability->foreign_frames != NULL &&
      !capability->foreign_frames->allow_callbacks) {
    aihc_fail("an unsafe foreign call cannot enter a Haskell callback");
  }
  *frame = (AihcCallbackFrame){0};
  frame->previous = capability->callback_frames;
  frame->capability = capability;
  frame->closure = slot->closure;
  capability->callback_frames = frame;
  /* The callback runs on the stack of the thread that made the foreign call.
     Its stop frame goes above the frames of that thread. When the callback
     enters the stop frame, the stack pointer is back where it was. */
  frame->continuation = aihc_stack_push(capability, 1);
  frame->continuation->header = (AihcSlot)(uintptr_t)stop_info;
  aihc_stack_enter_chunk(capability, frame->continuation);
}

AihcCapability *aihc_callback_machine(AihcCallbackFrame *frame) {
  return frame->capability;
}

AihcValue *aihc_callback_closure(AihcCallbackFrame *frame) {
  return frame->closure;
}

AihcValue *aihc_callback_continuation(AihcCallbackFrame *frame) {
  return frame->continuation;
}

void aihc_callback_return(AihcCapability *capability, uint64_t result) {
  if (capability->callback_frames == NULL) {
    aihc_fail("callback return has no callback frame");
  }
  capability->callback_frames->result = result;
  capability->callback_frames->complete = 1;
}

uint64_t aihc_callback_leave(AihcCallbackFrame *frame) {
  if (!frame->complete || frame->capability->callback_frames != frame) {
    aihc_fail("a foreign callback did not complete synchronously");
  }
  uint64_t result = frame->result;
  frame->capability->callback_frames = frame->previous;
  return result;
}

/* Static objects are not visited here. They never move, so a collector marks
   and scans the ones it finds reachable instead of treating all of them as
   roots. */
/* A thread record is in gen2 and never moves, but its resumption and its
   transaction name objects of the capability that runs the thread. These
   fields and the blackhole queues are roots of the capability, not fields
   of a global object. Thus
   a store into them needs no write barrier. Each thread with a stack is
   visited. A thread that nothing reaches keeps them until a gen2 cycle
   frees its record. A thread with a resumption is on the run queue. */
static void aihc_visit_thread_roots(AihcMachine *machine,
                                    AihcRootVisitor visitor, void *context) {
  for (AihcStack *stack = machine->stacks; stack != NULL; stack = stack->next) {
    AihcThread *thread = stack->thread;
    thread->transaction =
        aihc_visit_pointer(thread->transaction, visitor, context);
    thread->queues = aihc_visit_pointer(thread->queues, visitor, context);
    aihc_visit_value(&thread->resume_function, visitor, context);
    aihc_visit_value(&thread->resume_continuation, visitor, context);
    if ((thread->resume_kind == AIHC_RESUME_CONTINUE ||
         thread->resume_kind == AIHC_RESUME_APPLY) &&
        thread->resume_count == 1) {
      thread->resume_value = visitor(thread->resume_value, context);
    }
  }
}

void aihc_visit_capability_roots(AihcCapability *capability,
                                 AihcRootVisitor visitor, void *context) {
  for (AihcCallbackSlot *slot = aihc_callback_slots; slot != NULL;
       slot = slot->next) {
    if (slot->capability == capability) {
      aihc_visit_value(&slot->closure, visitor, context);
    }
  }
  for (AihcCallbackFrame *frame = capability->callback_frames; frame != NULL;
       frame = frame->previous) {
    aihc_visit_value(&frame->closure, visitor, context);
    aihc_visit_value(&frame->continuation, visitor, context);
  }
  for (AihcForeignFrame *frame = capability->foreign_frames; frame != NULL;
       frame = frame->previous) {
    for (uint64_t index = 0; index < frame->count; ++index) {
      frame->roots[index] = visitor(frame->roots[index], context);
    }
  }
  aihc_visit_value(&capability->selected_resume.function, visitor, context);
  aihc_visit_value(&capability->selected_resume.continuation, visitor, context);
  if ((capability->selected_resume.kind == AIHC_RESUME_CONTINUE ||
       capability->selected_resume.kind == AIHC_RESUME_APPLY) &&
      capability->selected_resume.count == 1) {
    capability->selected_resume.value =
        visitor(capability->selected_resume.value, context);
  }
  capability->current_thread =
      aihc_visit_pointer(capability->current_thread, visitor, context);
  capability->run_queue_head =
      aihc_visit_pointer(capability->run_queue_head, visitor, context);
  capability->run_queue_tail =
      aihc_visit_pointer(capability->run_queue_tail, visitor, context);
  capability->inbox_head =
      aihc_visit_pointer(capability->inbox_head, visitor, context);
  capability->inbox_tail =
      aihc_visit_pointer(capability->inbox_tail, visitor, context);
  capability->io_requests_head =
      aihc_visit_pointer(capability->io_requests_head, visitor, context);
  capability->io_requests_tail =
      aihc_visit_pointer(capability->io_requests_tail, visitor, context);
  aihc_visit_host_roots(capability, visitor, context);
}

void aihc_visit_machine_roots(AihcMachine *machine, uint64_t root_count,
                              AihcSlot *roots, AihcRootVisitor visitor,
                              void *context) {
  aihc_visit_thread_roots(machine, visitor, context);
  aihc_visit_value(&machine->global_array, visitor, context);
  for (uint64_t index = 0; index < machine->global_count; ++index) {
    machine->globals[index] = visitor(machine->globals[index], context);
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    roots[index] = visitor(roots[index], context);
  }
  machine->transaction_timers =
      aihc_visit_pointer(machine->transaction_timers, visitor, context);
  for (uint64_t index = 0; index < 3; ++index) {
    aihc_rts_set_root(
        index, aihc_visit_pointer(aihc_rts_root(index), visitor, context));
  }
}

void aihc_visit_roots(AihcCapability *capability, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context) {
  aihc_visit_capability_roots(capability, visitor, context);
  aihc_visit_machine_roots(capability->machine, root_count, roots, visitor,
                           context);
}

void aihc_ensure_heap(AihcCapability *capability, uint64_t words,
                      uint64_t root_count, AihcSlot *roots,
                      const AihcSrt *srt) {
  aihc_gc_ensure(capability, words, root_count, roots, srt);
}

void aihc_heap_collect(AihcCapability *capability, uint64_t words,
                       uint64_t root_count, AihcSlot *roots,
                       const AihcSrt *srt) {
  aihc_gc_refill(capability, words, root_count, roots, srt);
}

/* Place one object in heap the caller has already reserved. Compiled code
   inlines the same two steps - bump the heap pointer, write the header - and
   the slow apply path below is the only caller left in the runtime. */
static AihcValue *aihc_place_node(AihcCapability *capability,
                                  const AihcInfo *info, uint64_t words) {
  AihcValue *value = aihc_gc_allocate(capability, words);
  value->header = aihc_make_header(info);
  return value;
}

/* One stage of a constructor that is not saturated yet. Its width is not in
   the info table - every stage of the constructor shares one - so the caller
   names the slots and the object records them in field zero. */
static AihcValue *aihc_place_partial(AihcCapability *capability,
                                     const AihcInfo *info, uint64_t applied) {
  AihcValue *value = aihc_place_node(capability, info, 2 + applied);
  value->fields[0] = applied;
  return value;
}

uint64_t aihc_allocation_count(const AihcCapability *capability) {
  return capability->allocation_count;
}

void aihc_reset_allocation_count(AihcCapability *capability) {
  capability->allocation_count = 0;
}

uint64_t aihc_heap_allocated_bytes(AihcCapability *capability) {
  aihc_heap_account(capability);
  return capability->heap_allocated_bytes;
}

void aihc_reset_heap_allocated_bytes(AihcCapability *capability) {
  aihc_heap_account(capability);
  capability->heap_allocated_bytes = 0;
}

/* The stage of a closure after the given number of stages. Closures keep a
   chain of info tables: each stage names the slots it holds and points at
   the stage after it. */
const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                           uint64_t stages,
                                           uint64_t supplied_count) {
  const AihcInfo *next = info;
  for (uint64_t stage = 0; stage < stages; ++stage) {
    const AihcInfo *current = next;
    next = current->next;
    if (current->remaining_arity == 0 || next == NULL ||
        next->remaining_arity + 1 != current->remaining_arity ||
        next->field_count < current->field_count) {
      aihc_fail("application does not match static info-table transition");
    }
  }
  if (next->field_count - info->field_count != supplied_count) {
    aihc_fail("application does not match static info-table transition");
  }
  return next;
}

/* The info table a partial constructor takes on once it has the given slots.
   A stage that fills the last slot becomes the saturated constructor and
   drops the applied count; every earlier stage keeps the shared partial info
   table it already has. */
const AihcInfo *aihc_applied_constructor_info(const AihcInfo *info,
                                              uint64_t applied) {
  const AihcInfo *saturated = info->next;
  if (info->object_kind != AIHC_OBJECT_PARTIAL_CONSTRUCTOR ||
      saturated == NULL) {
    aihc_fail("applied a value that is not a partial constructor");
  }
  if (applied > saturated->field_count) {
    aihc_fail("constructor application overruns the constructor");
  }
  return applied == saturated->field_count ? saturated : info;
}

/* Extend one object by the given slots. A closure grows by the given number
   of stages of its own chain; a partial constructor keeps its single shared
   info table until the last slot arrives and it becomes the saturated
   constructor. */
static AihcValue *aihc_copy_with_fields(AihcCapability *capability,
                                        AihcValue **value_pointer,
                                        uint64_t stages, uint64_t count,
                                        const AihcSlot *fields,
                                        AihcValue **continuation_pointer) {
  AihcValue *value = *value_pointer;
  const AihcInfo *info = aihc_value_info_table(value);
  int partial = info->object_kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  uint64_t original_count =
      partial ? aihc_partial_applied(value) : info->field_count;
  const AihcInfo *next_info =
      partial ? aihc_applied_constructor_info(info, original_count + count)
              : aihc_next_application_info(info, stages, count);
  /* Both stages of a constructor index the saturated bitmap, so the pointer
     map of the arriving slots is the same array either way. */
  const uint8_t *field_is_pointer = next_info->field_is_pointer;
  int next_partial = next_info->object_kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  uint64_t words =
      next_partial ? 2 + original_count + count : aihc_object_words(next_info);

  uint64_t pointer_count = 0;
  for (uint64_t index = 0; index < count; ++index) {
    if (field_is_pointer[original_count + index]) {
      ++pointer_count;
    }
  }
  AihcSlot roots[2 + pointer_count];
  roots[0] = (AihcSlot)value;
  roots[1] = (AihcSlot)*continuation_pointer;
  uint64_t root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    if (field_is_pointer[original_count + index]) {
      roots[root_index++] = fields[index];
    }
  }

  /* The apply helper is entered by a tail call, so the static references of
     the function that applied the value are dead: the roots above carry
     everything the application needs. */
  aihc_ensure_heap(capability, words, 2 + pointer_count, roots, NULL);
  value = (AihcValue *)roots[0];
  *value_pointer = value;
  *continuation_pointer = (AihcValue *)roots[1];

  /* The stage that fills the last slot becomes a saturated node and drops the
     applied count, so the result decides the allocator, not the source. */
  AihcValue *copy =
      next_partial
          ? aihc_place_partial(capability, next_info, original_count + count)
          : aihc_place_node(capability, next_info, words);
  const AihcSlot *original_fields = partial ? aihc_partial_fields_const(value)
                                            : aihc_value_fields_const(value);
  AihcSlot *copy_fields =
      next_partial ? aihc_partial_fields(copy) : aihc_value_fields(copy);
  for (uint64_t index = 0; index < original_count; ++index) {
    copy_fields[index] = original_fields[index];
  }
  root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    copy_fields[original_count + index] =
        field_is_pointer[original_count + index] ? roots[root_index++]
                                                 : fields[index];
  }
  return copy;
}

/* A thread record is in the global heap from the start: a ThreadId# can
   move between capabilities, and the record never moves. */
static AihcThread *aihc_thread_new(AihcCapability *capability) {
  AihcThread *thread = (AihcThread *)aihc_gc_allocate_global(
      capability, aihc_record_words(sizeof(*thread)));
  thread->header = (AihcSlot)(uintptr_t)&aihc_thread_info;
  thread->id = __atomic_add_fetch(&capability->machine->next_thread_id, 1,
                                  __ATOMIC_RELAXED);
  thread->resume_kind = AIHC_RESUME_NONE;
  thread->resume_function = NULL;
  thread->resume_continuation = NULL;
  thread->resume_value = 0;
  thread->resume_count = 0;
  thread->next = NULL;
  thread->transaction = NULL;
  thread->queues = NULL;
  thread->capability = capability->index;
  (void)aihc_stack_new(capability, thread);
  return thread;
}

/* The result is a pointer, because the lowering declares the result of this
   call as an address. A pointer is four bytes on a 32-bit target. */
AihcThread *aihc_my_thread_id(AihcCapability *capability) {
  return capability->current_thread;
}

/* Append a list of threads to a queue. Thread records are global, and the
   link replaces a null link, so it needs no barrier: an append into an
   inbox runs under the inbox lock, which a barrier must not wait under. */
static void aihc_queue_append(AihcThread **head, AihcThread **tail,
                              AihcThread *first, AihcThread *last) {
  if (*tail == NULL) {
    __atomic_store_n(head, first, __ATOMIC_SEQ_CST);
  } else {
    (*tail)->next = first;
  }
  *tail = last;
}

/* Put a thread on the run queue of its own capability. A woken thread goes
   back to the capability that ran it, because its stack can name objects of
   the local heap of that capability. Another capability gets the thread in
   its inbox, and a wake-up when it is in the idle wait. The idle wait sets
   its flag before it looks at the inbox for the last time, and the flag and
   the inbox are in this order here. Thus either the wait finds the thread,
   or this function finds the flag. */
static void aihc_enqueue_thread(AihcCapability *capability,
                                AihcThread *thread) {
  if (thread->next != NULL) {
    aihc_fail("attempted to enqueue an already queued thread");
  }
  AihcCapability *target =
      capability->machine->capabilities[thread->capability];
  if (target == capability) {
    aihc_queue_append(&target->run_queue_head, &target->run_queue_tail, thread,
                      thread);
    return;
  }
  aihc_host_lock_acquire(target->inbox_lock);
  aihc_queue_append(&target->inbox_head, &target->inbox_tail, thread, thread);
  aihc_host_lock_release(target->inbox_lock);
  if (__atomic_load_n(&target->idle, __ATOMIC_SEQ_CST)) {
    aihc_host_wake(target);
  }
}

/* Move the inbox of a capability into its run queue. */
static void aihc_take_inbox(AihcCapability *capability) {
  if (capability->inbox_lock == NULL) {
    return;
  }
  aihc_host_lock_acquire(capability->inbox_lock);
  AihcThread *first = capability->inbox_head;
  AihcThread *last = capability->inbox_tail;
  capability->inbox_head = NULL;
  capability->inbox_tail = NULL;
  aihc_host_lock_release(capability->inbox_lock);
  if (first != NULL) {
    aihc_queue_append(&capability->run_queue_head, &capability->run_queue_tail,
                      first, last);
  }
}

/* Whether the inbox of a capability holds a thread. */
static int aihc_inbox_used(AihcCapability *capability) {
  if (capability->inbox_lock == NULL) {
    return 0;
  }
  aihc_host_lock_acquire(capability->inbox_lock);
  int used = capability->inbox_head != NULL;
  aihc_host_lock_release(capability->inbox_lock);
  return used;
}

static AihcThread *aihc_dequeue_thread(AihcCapability *capability) {
  AihcThread *thread = capability->run_queue_head;
  if (thread == NULL) {
    aihc_fail("no runnable threads");
  }
  capability->run_queue_head = thread->next;
  if (capability->run_queue_head == NULL) {
    capability->run_queue_tail = NULL;
  }
  /* The link to the next thread is deleted from an old record. The barrier
     shades it, so a gen2 cycle that took its snapshot with the thread on
     the queue still marks the threads behind it. */
  aihc_write_barrier(capability, (AihcValue *)thread);
  thread->next = NULL;
  return thread;
}

void aihc_set_field(AihcCapability *capability, AihcValue *value,
                    uint64_t index, AihcSlot field) {
  aihc_count_escape(capability, value, field);
  aihc_write_barrier(capability, value);
  __atomic_store_n(&aihc_value_fields(value)[index], field, __ATOMIC_RELEASE);
}

void aihc_set_exit_status(AihcCapability *capability, int64_t status) {
  capability->machine->exit_status = status;
}

int64_t aihc_get_exit_status(const AihcCapability *capability) {
  return capability->machine->exit_status;
}

static AihcMachine aihc_machine;
static AihcCapability aihc_initial_capability;

AihcCapability *aihc_machine_initialize(void) {
  if (aihc_initial_capability.heap_start == NULL) {
    static AihcCapability *initial_capabilities[1] = {&aihc_initial_capability};
    aihc_machine = (AihcMachine){.capabilities = initial_capabilities,
                                 .capability_count = 1};
    aihc_initial_capability = (AihcCapability){.machine = &aihc_machine};
    aihc_gc_init(&aihc_initial_capability);
    aihc_machine.next_stable_name = 1;
    aihc_machine.io_backend = aihc_host_io_backend();
    aihc_process_capability = &aihc_initial_capability;
  }
  return &aihc_initial_capability;
}

uint64_t aihc_get_num_capabilities(void) {
  return aihc_machine_initialize()->machine->capability_count;
}

/* Host imports need fixed addresses. Retained process data can move after
 * import. */
static void aihc_finish_imports(AihcCapability *capability) {
  for (uint64_t index = 0; index < 3; ++index) {
    AihcValue *source = aihc_rts_root(index);
    if (source == NULL) {
      continue;
    }
    int64_t bytes;
    if (index == 0) {
      bytes = aihc_program_arguments_size();
    } else if (index == 1) {
      bytes = aihc_program_environment_size();
    } else {
      bytes = (int64_t)aihc_byte_array_get_size(source);
    }
    if (bytes == 0) {
      aihc_rts_set_root(index, NULL);
      continue;
    }
    aihc_ensure_heap(capability, aihc_byte_array_words(bytes, 0, 1), 0, NULL,
                     NULL);
    source = aihc_rts_root(index);
    void *array = aihc_byte_array_new(capability, bytes);
    memcpy(aihc_byte_array_contents(array), aihc_byte_array_contents(source),
           (size_t)bytes);
    aihc_rts_set_root(index, array);
  }
}

AihcCapability *aihc_machine_new(uint64_t global_count) {
  AihcCapability *capability = aihc_machine_initialize();
  if (capability->machine->program_started) {
    aihc_fail("program capability already started");
  }
  if (capability->root_frames != NULL) {
    aihc_fail("program start requires completed host scopes");
  }
  aihc_finish_imports(capability);
  /* Startup scopes have ended. Collect temporary imports before the limit
   * applies. */
  aihc_gc_collect(capability, 0, 0, NULL, NULL);
  aihc_gc_apply_options(capability);
  capability->machine->heap_max_bytes = aihc_rts_heap_max_bytes();
  capability->machine->heap_limit_enabled = aihc_rts_heap_limit_enabled() != 0;
  aihc_gc_apply_limit(capability);
  aihc_gc_collect(capability, 0, 0, NULL, NULL);
  if (global_count > INT64_MAX / sizeof(*capability->machine->globals)) {
    aihc_fail("global table is too large");
  }
  if (global_count != 0) {
    int64_t bytes =
        (int64_t)(global_count * sizeof(*capability->machine->globals));
    aihc_ensure_heap(capability, aihc_byte_array_words(bytes, 1, 1), 0, NULL,
                     NULL);
    capability->machine->global_array =
        aihc_byte_array_new_pinned(capability, bytes);
    capability->machine->globals =
        aihc_byte_array_contents(capability->machine->global_array);
  }
  capability->machine->global_count = global_count;
  aihc_gc_ensure(capability, aihc_record_words(sizeof(AihcThread)), 0, NULL,
                 NULL);
  capability->current_thread = aihc_thread_new(capability);
  capability->stack_next = aihc_stack_base(capability->machine->stacks);
  capability->machine->program_started = 1;
  if (capability->machine->capability_count > 1) {
    aihc_workers_start(capability);
  }
  return capability;
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

AihcValue *aihc_apply_slow(AihcCapability *capability, AihcValue *function,
                           uint64_t stages, uint64_t count,
                           const AihcSlot *arguments,
                           AihcValue **continuation) {
  if (function == NULL) {
    aihc_fail("attempted to apply null");
  }
  while (aihc_value_kind(function) == AIHC_OBJECT_INDIRECTION) {
    function = (AihcValue *)(uintptr_t)function->fields[0];
    if (function == NULL) {
      aihc_fail("indirection points to null");
    }
  }
  switch (aihc_value_kind(function)) {
  case AIHC_OBJECT_CLOSURE: {
    uint64_t arity = aihc_value_arity(function);
    if (arity <= stages) {
      aihc_fail("closure application does not require the slow path");
    }
    return aihc_copy_with_fields(capability, &function, stages, count,
                                 arguments, continuation);
  }
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR: {
    if (aihc_partial_applied(function) + count > aihc_partial_total(function)) {
      aihc_fail("constructor application overruns the constructor");
    }
    return aihc_copy_with_fields(capability, &function, stages, count,
                                 arguments, continuation);
  }
  case AIHC_OBJECT_NODE:
    aihc_fail("saturated constructor was applied");
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_suspend_apply(AihcThread *thread, AihcValue *function,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_APPLY;
  thread->resume_function = function;
  thread->resume_continuation = continuation;
  aihc_stack_note_top(continuation);
  thread->resume_count = 0;
}

static void aihc_suspend_raise(AihcThread *thread, AihcValue *exception,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_RAISE;
  thread->resume_function = exception;
  thread->resume_continuation = continuation;
  aihc_stack_note_top(continuation);
  thread->resume_count = 0;
}

static void aihc_suspend_continue(AihcThread *thread, AihcValue *continuation,
                                  uint64_t count, AihcSlot value) {
  if (count > 1) {
    aihc_fail("suspended continuation has too many immediate values");
  }
  thread->resume_kind = AIHC_RESUME_CONTINUE;
  thread->resume_function = continuation;
  thread->resume_continuation = NULL;
  aihc_stack_note_top(continuation);
  thread->resume_value = value;
  thread->resume_count = count;
}

static const AihcResume *aihc_select_thread(AihcCapability *capability,
                                            AihcThread *thread) {
  AihcResume *resume = &capability->selected_resume;
  /* The resumption moves from one root of the capability to another, so
     it needs no barrier. */
  resume->kind = thread->resume_kind;
  resume->function = thread->resume_function;
  resume->continuation = thread->resume_continuation;
  resume->value = thread->resume_value;
  resume->count = thread->resume_count;
  thread->resume_kind = AIHC_RESUME_NONE;
  thread->resume_function = NULL;
  thread->resume_continuation = NULL;
  thread->resume_value = 0;
  thread->resume_count = 0;
  capability->current_thread = thread;
  if (resume->kind != AIHC_RESUME_APPLY &&
      resume->kind != AIHC_RESUME_CONTINUE &&
      resume->kind != AIHC_RESUME_RAISE) {
    aihc_fail("thread has no suspended continuation");
  }
  return resume;
}

int64_t aihc_io_error(int error) { return -((int64_t)error) - 1; }

void aihc_resume_io_request(AihcCapability *capability, AihcIoRequest *request,
                            int64_t result) {
  AihcThread *thread = request->thread;
  AihcValue *continuation = request->continuation;
  request->state = AIHC_IO_COMPLETED;
  request->result = result;
  /* The thread and the continuation are deleted from an old record, as in
     aihc_dequeue_thread. */
  aihc_write_barrier(capability, (AihcValue *)request);
  request->thread = NULL;
  request->continuation = NULL;
  request->next = NULL;
  aihc_suspend_continue(thread, continuation, 0, 0);
  aihc_enqueue_thread(capability, thread);
}

/* Take the last thread of the run queue. */
static AihcThread *aihc_dequeue_last_thread(AihcCapability *capability) {
  AihcThread *last = capability->run_queue_tail;
  if (last == capability->run_queue_head) {
    return aihc_dequeue_thread(capability);
  }
  AihcThread *before = capability->run_queue_head;
  while (before->next != last) {
    before = before->next;
  }
  /* As in aihc_dequeue_thread, the link is deleted from an old record. */
  aihc_write_barrier(capability, (AihcValue *)before);
  before->next = NULL;
  capability->run_queue_tail = before;
  return last;
}

/* Whether a capability waits in the idle wait with nothing to run. Only
   its own worker touches its run queue, which is empty in the idle wait. */
static int aihc_capability_available(AihcCapability *capability) {
  return __atomic_load_n(&capability->idle, __ATOMIC_SEQ_CST) &&
         !aihc_inbox_used(capability);
}

/* Push surplus threads to idle capabilities. See "Thread movement" in
   docs/multi-threaded-rts-design.md. A pushed thread can name objects of
   the local heap of this capability. Thus a collection first moves each
   young survivor of the local heap into gen2, and the pushes then need no
   promotion of their own. The capability keeps the first thread of its
   queue and pushes at most half of the others. */
static void aihc_push_threads(AihcCapability *capability) {
  AihcMachine *machine = capability->machine;
  if (__atomic_load_n(&machine->idle_count, __ATOMIC_SEQ_CST) == 0 ||
      capability->run_queue_head == NULL ||
      capability->run_queue_head->next == NULL) {
    return;
  }
  uint64_t surplus = 0;
  for (const AihcThread *thread = capability->run_queue_head->next;
       thread != NULL; thread = thread->next) {
    ++surplus;
  }
  uint64_t available = 0;
  for (uint64_t index = 0; index < machine->capability_count; ++index) {
    available += aihc_capability_available(machine->capabilities[index]);
  }
  uint64_t pushes = (surplus + 1) / 2;
  if (pushes > available) {
    pushes = available;
  }
  if (pushes == 0) {
    return;
  }
  aihc_escape_collect(capability, 0, NULL, NULL);
  for (uint64_t index = 0; index < machine->capability_count && pushes != 0;
       ++index) {
    AihcCapability *target = machine->capabilities[index];
    if (!aihc_capability_available(target)) {
      continue;
    }
    AihcThread *thread = aihc_dequeue_last_thread(capability);
    thread->capability = target->index;
    aihc_stack_move(thread, target);
    aihc_enqueue_thread(capability, thread);
    ++capability->push_count;
    --pushes;
  }
}

/* Whether no capability has work: no IO request, no runnable thread, and
   an empty inbox. A completed IO request puts its thread on the run queue
   before it leaves the count, so a reader that sees no request also sees
   the thread. */
static int aihc_machine_without_work(AihcMachine *machine) {
  for (uint64_t index = 0; index < machine->capability_count; ++index) {
    AihcCapability *each = machine->capabilities[index];
    if (__atomic_load_n(&each->io_request_count, __ATOMIC_SEQ_CST) != 0 ||
        __atomic_load_n(&each->run_queue_head, __ATOMIC_SEQ_CST) != NULL ||
        aihc_inbox_used(each)) {
      return 0;
    }
  }
  return 1;
}

/* Enter the idle wait. The last running capability tests for a deadlock:
   no capability runs, and no capability has work. */
void aihc_idle_enter(AihcCapability *capability) {
  AihcMachine *machine = capability->machine;
  aihc_host_lock_acquire(machine->stop_lock);
  if (machine->running_count == 1 && !machine->stop_requested &&
      aihc_machine_without_work(machine)) {
    aihc_host_lock_release(machine->stop_lock);
    aihc_fail("no runnable threads");
  }
  --machine->running_count;
  aihc_host_condition_broadcast(machine->stop_condition);
  aihc_host_lock_release(machine->stop_lock);
}

/* Leave the idle wait. A capability does not run during a stop. */
void aihc_idle_exit(AihcCapability *capability) {
  AihcMachine *machine = capability->machine;
  aihc_host_lock_acquire(machine->stop_lock);
  while (machine->stop_requested) {
    aihc_host_condition_wait(machine->stop_condition, machine->stop_lock);
  }
  ++machine->running_count;
  aihc_host_lock_release(machine->stop_lock);
}

void aihc_capability_object_lock(AihcCapability *capability) {
  aihc_object_lock(capability->machine);
}

void aihc_capability_object_unlock(AihcCapability *capability) {
  aihc_object_unlock(capability->machine);
}

void aihc_heap_lock(AihcMachine *machine) {
  if (machine->heap_lock != NULL) {
    aihc_host_lock_acquire(machine->heap_lock);
  }
}

void aihc_heap_unlock(AihcMachine *machine) {
  if (machine->heap_lock != NULL) {
    aihc_host_lock_release(machine->heap_lock);
  }
}

void aihc_object_lock(AihcMachine *machine) {
  if (machine->object_lock != NULL) {
    aihc_host_lock_acquire(machine->object_lock);
  }
}

void aihc_object_unlock(AihcMachine *machine) {
  if (machine->object_lock != NULL) {
    aihc_host_lock_release(machine->object_lock);
  }
}

/* Park until the stop ends. The caller holds the stop lock and runs. A
   parked capability still counts as running, because its thread is in no
   queue, and the deadlock test must not miss it. */
static void aihc_park(AihcMachine *machine) {
  ++machine->parked_count;
  aihc_host_condition_broadcast(machine->stop_condition);
  while (machine->stop_requested) {
    aihc_host_condition_wait(machine->stop_condition, machine->stop_lock);
  }
  --machine->parked_count;
}

void aihc_safepoint(AihcCapability *capability, uint64_t root_count,
                    AihcSlot *roots) {
  AihcMachine *machine = capability->machine;
  if (machine->stop_lock == NULL || capability->stop_depth != 0 ||
      !__atomic_load_n(&machine->stop_requested, __ATOMIC_ACQUIRE)) {
    return;
  }
  /* A root frame publishes the roots, so the collection of the leader
     updates them. */
  AihcRootFrame frame;
  aihc_roots_enter(capability, &frame, root_count, roots);
  aihc_host_lock_acquire(machine->stop_lock);
  if (machine->stop_requested) {
    aihc_park(machine);
  }
  aihc_host_lock_release(machine->stop_lock);
  aihc_roots_leave(capability, &frame);
}

void aihc_stop_begin(AihcCapability *capability, uint64_t root_count,
                     AihcSlot *roots) {
  AihcMachine *machine = capability->machine;
  if (machine->stop_lock == NULL || capability->stop_depth++ != 0) {
    return;
  }
  AihcRootFrame frame;
  aihc_roots_enter(capability, &frame, root_count, roots);
  aihc_host_lock_acquire(machine->stop_lock);
  /* Another leader goes first. This capability parks, and then it leads
     its own stop. */
  while (machine->stop_requested) {
    aihc_park(machine);
  }
  __atomic_store_n(&machine->stop_requested, 1, __ATOMIC_RELEASE);
  /* Every running capability except the leader parks. */
  while (machine->parked_count + 1 != machine->running_count) {
    aihc_host_condition_wait(machine->stop_condition, machine->stop_lock);
  }
  aihc_host_lock_release(machine->stop_lock);
  aihc_roots_leave(capability, &frame);
}

void aihc_stop_end(AihcCapability *capability) {
  AihcMachine *machine = capability->machine;
  if (machine->stop_lock == NULL || --capability->stop_depth != 0) {
    return;
  }
  aihc_host_lock_acquire(machine->stop_lock);
  __atomic_store_n(&machine->stop_requested, 0, __ATOMIC_RELEASE);
  aihc_host_condition_broadcast(machine->stop_condition);
  aihc_host_lock_release(machine->stop_lock);
}

static const AihcResume *aihc_schedule(AihcCapability *capability) {
  if (capability->callback_frames != NULL) {
    aihc_fail("a foreign callback cannot suspend");
  }
  AihcMachine *machine = capability->machine;
  for (;;) {
    /* The scheduler is a safepoint: the roots of the capability are in its
       records. */
    aihc_safepoint(capability, 0, NULL);
    aihc_take_inbox(capability);
    (void)machine->io_backend->poll(capability, 0);
    if (capability->run_queue_head != NULL) {
      if (machine->workers_active) {
        aihc_push_threads(capability);
      }
      return aihc_select_thread(capability, aihc_dequeue_thread(capability));
    }
    if (machine->workers_active) {
      /* A push into the inbox, a completed IO request, or a timer ends the
         idle wait. */
      /* An idle capability runs no stack. A thread that it ran can run on
         another capability now, and the collector must not take the old
         stack pointer for the top of that stack. */
      capability->stack_next = NULL;
      __atomic_store_n(&capability->idle, 1, __ATOMIC_SEQ_CST);
      __atomic_add_fetch(&machine->idle_count, 1, __ATOMIC_SEQ_CST);
      if (!aihc_inbox_used(capability)) {
        (void)machine->io_backend->poll(capability, 1);
      }
      __atomic_store_n(&capability->idle, 0, __ATOMIC_SEQ_CST);
      __atomic_sub_fetch(&machine->idle_count, 1, __ATOMIC_SEQ_CST);
      continue;
    }
    if (capability->io_request_count != 0) {
      if (capability->machine->io_backend->poll(capability, 1) ==
          AIHC_IO_POLL_SUSPENDED) {
        return NULL;
      }
      continue;
    }
    aihc_fail("no runnable threads");
  }
}

/* The caller reserves sixteen slots. This operation cannot collect. */
static AihcIoRequest *aihc_io_request_new(AihcCapability *capability) {
  AihcIoRequest *request = (AihcIoRequest *)aihc_gc_allocate_pinned(
      capability, aihc_record_words(sizeof(*request)));
  request->header = (AihcSlot)(uintptr_t)&aihc_io_request_info;
  request->capability = capability;
  return request;
}

const AihcInfo aihc_io_handle_info = {.object_kind = AIHC_OBJECT_IO_HANDLE};

AihcIoHandle *aihc_io_handle_new(AihcCapability *capability) {
  AihcIoHandle *handle = (AihcIoHandle *)aihc_gc_allocate(
      capability, aihc_record_words(sizeof(AihcIoHandle)));
  *handle = (AihcIoHandle){.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info,
                           .closed = 1};
  return handle;
}

/* Retain a pinned owner when the address names its payload.
   External buffers remain the caller's responsibility. */
static int aihc_io_request_buffer_unlocked(AihcIoRequest *request,
                                           uint8_t *buffer, size_t offset,
                                           size_t length) {
  AihcCapability *capability = request->capability;
  uintptr_t address = (uintptr_t)buffer;
  /* A buffer in a region that moves is rejected. A large byte array never
     moves, and the pinned list below finds it. */
  switch (aihc_region_kind(buffer)) {
  case AIHC_REGION_NURSERY:
  case AIHC_REGION_GEN1:
  case AIHC_REGION_GEN2:
  case AIHC_REGION_FROM1:
    return 0;
  default:
    break;
  }
  for (AihcPinnedBlock *block = capability->machine->pinned_blocks;
       block != NULL; block = block->next) {
    /* Another worker can publish a block before it initializes the object.
       Read its header only when the buffer address names that block. */
    uintptr_t object_start = (uintptr_t)block->object;
    if (address < object_start ||
        address - object_start > aihc_pinned_bytes(block)) {
      continue;
    }
    uint64_t payload_bytes = aihc_pinned_bytes(block);
    if (aihc_region_kind(block) != AIHC_REGION_LARGE) {
      payload_bytes -= sizeof(AihcPinnedBlock);
    }
    if (address - object_start > payload_bytes) {
      continue;
    }
    AihcValue *object = (AihcValue *)block->object;
    if (aihc_value_kind(object) != AIHC_OBJECT_BYTE_ARRAY) {
      continue;
    }
    AihcByteArray *array = (AihcByteArray *)object;
    uintptr_t start = (uintptr_t)array->contents;
    if (address >= start && address - start <= array->size) {
      uint64_t available = array->size - (address - start);
      if (offset > available || length > available - offset) {
        return 0;
      }
      aihc_write_barrier(capability, (AihcValue *)request);
      request->buffer_owner = object;
      break;
    }
  }
  request->buffer = buffer;
  request->offset = offset;
  request->length = length;
  return 1;
}

static int aihc_io_request_buffer(AihcIoRequest *request, uint8_t *buffer,
                                  size_t offset, size_t length) {
  AihcMachine *machine = request->capability->machine;
  aihc_heap_lock(machine);
  int result = aihc_io_request_buffer_unlocked(request, buffer, offset, length);
  aihc_heap_unlock(machine);
  return result;
}

static AihcIoRequest *aihc_io_submit(AihcCapability *capability,
                                     AihcIoKind kind, AihcIoHandle *handle,
                                     uint8_t *buffer, int64_t offset,
                                     int64_t length) {
  AihcIoRequest *request = aihc_io_request_new(capability);
  request->kind = kind;
  request->state = AIHC_IO_SUBMITTED;
  aihc_write_barrier(capability, (AihcValue *)request);
  request->handle = handle;
  if (handle == NULL || handle->closed) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
    return request;
  }
  if (offset < 0 || length < 0 || (uint64_t)offset > SIZE_MAX ||
      (uint64_t)length > SIZE_MAX - (size_t)offset ||
      (buffer == NULL && length != 0)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    return request;
  }
  uint32_t required_capability =
      kind == AIHC_IO_READ ? AIHC_IO_READABLE : AIHC_IO_WRITABLE;
  if ((handle->capabilities & required_capability) == 0) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
    return request;
  }
  if (!aihc_io_request_buffer(request, buffer, (size_t)offset,
                              (size_t)length)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  return request;
}

static AihcIoRequest *aihc_io_submit_open_request(AihcCapability *capability,
                                                  uint8_t *path,
                                                  int64_t requested_length,
                                                  int64_t requested_mode) {
  AihcIoRequest *request = aihc_io_request_new(capability);
  request->kind = AIHC_IO_OPEN;
  AihcIoHandle *open_handle = aihc_io_handle_new(capability);
  aihc_write_barrier(capability, (AihcValue *)request);
  request->handle = open_handle;
  request->state = AIHC_IO_SUBMITTED;
  if (requested_length < 0 || (uint64_t)requested_length > SIZE_MAX ||
      (path == NULL && requested_length != 0) || requested_mode < 0 ||
      requested_mode > 3) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    return request;
  }
  if (!aihc_io_request_buffer(request, path, 0, (size_t)requested_length)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  request->mode = requested_mode;
  return request;
}

int64_t aihc_io_open_result_error(void *result) {
  return ((AihcIoHandle *)result)->error;
}

int64_t aihc_memory_write_byte(void *opaque_buffer, int64_t offset,
                               int64_t value) {
  if (opaque_buffer == NULL || offset < 0 || value < 0 || value > UINT8_MAX) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  ((uint8_t *)opaque_buffer)[(size_t)offset] = (uint8_t)value;
  return 0;
}

int64_t aihc_memory_read_byte(const void *opaque_buffer, int64_t offset) {
  if (opaque_buffer == NULL || offset < 0) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  const uint8_t *buffer = opaque_buffer;
  return buffer[offset];
}

/* errno is a macro over a thread-local location, so a foreign import cannot
   name it. Foreign.C.Error reads and clears it through these instead. The
   setter reports the previous value so that it has a result to return. */
int64_t aihc_errno_get(void) { return (int64_t)errno; }

int64_t aihc_errno_set(int64_t value) {
  int previous = errno;
  errno = (int)value;
  return (int64_t)previous;
}

void *aihc_io_submit_read(AihcCapability *capability, void *opaque_handle,
                          void *opaque_buffer, int64_t offset, int64_t length) {
  return aihc_io_submit(capability, AIHC_IO_READ, opaque_handle, opaque_buffer,
                        offset, length);
}

void *aihc_io_submit_write(AihcCapability *capability, void *opaque_handle,
                           void *opaque_buffer, int64_t offset,
                           int64_t length) {
  return aihc_io_submit(capability, AIHC_IO_WRITE, opaque_handle, opaque_buffer,
                        offset, length);
}

void *aihc_io_submit_open(AihcCapability *capability, void *opaque_path,
                          int64_t length, int64_t mode) {
  return aihc_io_submit_open_request(capability, opaque_path, length, mode);
}

int64_t aihc_io_take_result(void *opaque_request) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL || request->state != AIHC_IO_COMPLETED) {
    aihc_fail("attempted to consume an incomplete IO request");
  }
  int64_t result = request->result;
  request->state = AIHC_IO_CONSUMED;
  request->buffer_owner = NULL;
  return result;
}

void *aihc_io_take_open_result(void *opaque_request) {
  AihcIoRequest *request = opaque_request;
  int64_t result = aihc_io_take_result(request);
  request->handle->error = result < 0 ? -result - 1 : 0;
  return request->handle;
}

static const AihcResume *aihc_resume_current(AihcCapability *capability,
                                             AihcValue *continuation) {
  aihc_suspend_continue(capability->current_thread, continuation, 0, 0);
  return aihc_select_thread(capability, capability->current_thread);
}

static const AihcResume *aihc_resume_current_value(AihcCapability *capability,
                                                   AihcValue *continuation,
                                                   AihcSlot value) {
  aihc_suspend_continue(capability->current_thread, continuation, 1, value);
  return aihc_select_thread(capability, capability->current_thread);
}

static AihcMVarWaiter *aihc_mvar_waiter_new(AihcCapability *capability,
                                            const AihcMVar *mvar,
                                            AihcValue *continuation,
                                            AihcSlot value) {
  AihcMVarWaiter *waiter = (AihcMVarWaiter *)aihc_record_allocate(
      capability, (const AihcValue *)mvar, aihc_record_words(sizeof(*waiter)));
  waiter->header = (AihcSlot)(uintptr_t)&aihc_mvar_waiter_info;
  waiter->next = NULL;
  waiter->thread = capability->current_thread;
  waiter->continuation = continuation;
  aihc_stack_note_top(continuation);
  aihc_thread_pause(capability, continuation);
  waiter->value = value;
  /* A global waiter names a frame, which the remembered set keeps reached
     until its chunk is old. A young waiter takes no entry. */
  aihc_write_barrier(capability, (AihcValue *)waiter);
  return waiter;
}

static void aihc_mvar_append_waiter(AihcCapability *capability,
                                    AihcMVarWaiter **head,
                                    AihcMVarWaiter **tail,
                                    AihcMVarWaiter *waiter) {
  if (*tail == NULL) {
    *head = waiter;
  } else {
    aihc_write_barrier(capability, (AihcValue *)*tail);
    (*tail)->next = waiter;
  }
  *tail = waiter;
}

static AihcMVarWaiter *aihc_mvar_pop_waiter(AihcCapability *capability,
                                            AihcMVarWaiter **head,
                                            AihcMVarWaiter **tail) {
  AihcMVarWaiter *waiter = *head;
  if (waiter == NULL) {
    return NULL;
  }
  *head = waiter->next;
  if (*head == NULL) {
    *tail = NULL;
  }
  /* The link to the next waiter is deleted from an old record, as in
     aihc_dequeue_thread. */
  aihc_write_barrier(capability, (AihcValue *)waiter);
  waiter->next = NULL;
  return waiter;
}

/* A woken waiter gives its continuation to its thread. The waiter can stay
   in the remembered set after this, and the frame is popped later, so the
   waiter keeps no pointer to it. */
static void aihc_mvar_wake(AihcCapability *capability, AihcMVarWaiter *waiter,
                           uint64_t count, AihcSlot value) {
  AihcValue *continuation = waiter->continuation;
  waiter->continuation = NULL;
  aihc_suspend_continue(waiter->thread, continuation, count, value);
  aihc_enqueue_thread(capability, waiter->thread);
}

static AihcMVar *aihc_checked_mvar(void *opaque_mvar) {
  AihcMVar *mvar = opaque_mvar;
  if (mvar == NULL) {
    aihc_fail("attempted an operation on a null MVar");
  }
  return mvar;
}

void *aihc_mvar_new(AihcCapability *capability) {
  AihcMVar *mvar = (AihcMVar *)aihc_gc_allocate(
      capability, aihc_record_words(sizeof(*mvar)));
  mvar->header = (AihcSlot)(uintptr_t)&aihc_mvar_info;
  __atomic_store_n(&mvar->full, (uint8_t)0, __ATOMIC_RELEASE);
  mvar->value = 0;
  mvar->readers_head = NULL;
  mvar->readers_tail = NULL;
  mvar->takers_head = NULL;
  mvar->takers_tail = NULL;
  mvar->putters_head = NULL;
  mvar->putters_tail = NULL;
  return mvar;
}

/* The object lock guards the state and the waiters of every MVar. A
   blocked thread schedules after the lock is free: a wake from another
   capability before the schedule puts the thread into the inbox, and the
   schedule takes it from there. */
const AihcResume *aihc_mvar_read(AihcCapability *capability, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  aihc_write_barrier(capability, (AihcValue *)mvar);
  if (mvar->full) {
    AihcSlot value = mvar->value;
    aihc_object_unlock(machine);
    return aihc_resume_current_value(capability, continuation, value);
  }
  AihcMVarWaiter *waiter =
      aihc_mvar_waiter_new(capability, mvar, continuation, 0);
  aihc_mvar_append_waiter(capability, &mvar->readers_head, &mvar->readers_tail,
                          waiter);
  aihc_object_unlock(machine);
  return aihc_schedule(capability);
}

const AihcResume *aihc_mvar_take(AihcCapability *capability, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  aihc_write_barrier(capability, (AihcValue *)mvar);
  if (!mvar->full) {
    AihcMVarWaiter *waiter =
        aihc_mvar_waiter_new(capability, mvar, continuation, 0);
    aihc_mvar_append_waiter(capability, &mvar->takers_head, &mvar->takers_tail,
                            waiter);
    aihc_object_unlock(machine);
    return aihc_schedule(capability);
  }

  AihcSlot value = mvar->value;
  AihcMVarWaiter *putter = aihc_mvar_pop_waiter(capability, &mvar->putters_head,
                                                &mvar->putters_tail);
  if (putter == NULL) {
    __atomic_store_n(&mvar->full, (uint8_t)0, __ATOMIC_RELEASE);
    mvar->value = 0;
  } else {
    mvar->value = putter->value;
    aihc_mvar_wake(capability, putter, 0, 0);
  }
  aihc_object_unlock(machine);
  return aihc_resume_current_value(capability, continuation, value);
}

/* With several capabilities, a put into a global MVar promotes the value
   first. The promotion can collect, and a collection does not move a global
   MVar. */
const AihcResume *aihc_mvar_put(AihcCapability *capability, void *opaque_mvar,
                                AihcSlot value, AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  /* With several capabilities, the put pauses the thread first. A put that
     is an escape then copies an unclaimed thunk safely, and the pause does
     not depend on the age of the MVar. */
  if (capability->machine->capability_count > 1) {
    aihc_thread_pause(capability, continuation);
  }
  /* The promotion comes before the lock, because it can collect. */
  value = aihc_escape_in_operation(capability, (const AihcValue *)mvar, value,
                                   continuation);
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  aihc_write_barrier(capability, (AihcValue *)mvar);
  if (mvar->full) {
    AihcMVarWaiter *waiter =
        aihc_mvar_waiter_new(capability, mvar, continuation, value);
    aihc_mvar_append_waiter(capability, &mvar->putters_head,
                            &mvar->putters_tail, waiter);
    aihc_object_unlock(machine);
    return aihc_schedule(capability);
  }

  AihcMVarWaiter *reader;
  while ((reader = aihc_mvar_pop_waiter(capability, &mvar->readers_head,
                                        &mvar->readers_tail)) != NULL) {
    aihc_mvar_wake(capability, reader, 1, value);
  }
  AihcMVarWaiter *taker =
      aihc_mvar_pop_waiter(capability, &mvar->takers_head, &mvar->takers_tail);
  if (taker == NULL) {
    aihc_count_escape(capability, (AihcValue *)mvar, value);
    __atomic_store_n(&mvar->full, (uint8_t)1, __ATOMIC_RELEASE);
    mvar->value = value;
  } else {
    aihc_mvar_wake(capability, taker, 1, value);
  }
  aihc_object_unlock(machine);
  return aihc_resume_current(capability, continuation);
}

/* The non-blocking MVar operations. None of them suspends the caller, so each
   one is an ordinary runtime call rather than a CPS call. Compiled code
   reads the full flag of isEmptyMVar# itself, at the offset asserted above.
   The take and the read give the value, or zero for an empty variable, so
   the flag and the value come from one state of the variable. */
AihcSlot aihc_mvar_try_take(AihcCapability *capability, void *opaque_mvar) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  aihc_write_barrier(capability, (AihcValue *)mvar);
  if (!mvar->full) {
    aihc_object_unlock(machine);
    return 0;
  }
  AihcSlot value = mvar->value;
  AihcMVarWaiter *putter = aihc_mvar_pop_waiter(capability, &mvar->putters_head,
                                                &mvar->putters_tail);
  if (putter == NULL) {
    __atomic_store_n(&mvar->full, (uint8_t)0, __ATOMIC_RELEASE);
    mvar->value = 0;
  } else {
    mvar->value = putter->value;
    aihc_mvar_wake(capability, putter, 0, 0);
  }
  aihc_object_unlock(machine);
  return value;
}

AihcSlot aihc_mvar_try_read(AihcCapability *capability, void *opaque_mvar) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  aihc_object_lock(capability->machine);
  AihcSlot value = mvar->full ? mvar->value : 0;
  aihc_object_unlock(capability->machine);
  return value;
}

/* The result is two when the put is an escape whose promotion needs a
   collection. The caller then collects at its store safepoint and tries
   again. */
uint64_t aihc_mvar_try_put(AihcCapability *capability, void *opaque_mvar,
                           AihcSlot value) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (mvar->full) {
    capability->pause_top = NULL;
    return 0;
  }
  value = aihc_escape(capability, (const AihcValue *)mvar, value);
  if (value == AIHC_ESCAPE_COLLECT) {
    return 2;
  }
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  if (mvar->full) {
    aihc_object_unlock(machine);
    return 0;
  }
  aihc_write_barrier(capability, (AihcValue *)mvar);
  AihcMVarWaiter *reader;
  while ((reader = aihc_mvar_pop_waiter(capability, &mvar->readers_head,
                                        &mvar->readers_tail)) != NULL) {
    aihc_mvar_wake(capability, reader, 1, value);
  }
  AihcMVarWaiter *taker =
      aihc_mvar_pop_waiter(capability, &mvar->takers_head, &mvar->takers_tail);
  if (taker == NULL) {
    aihc_count_escape(capability, (AihcValue *)mvar, value);
    __atomic_store_n(&mvar->full, (uint8_t)1, __ATOMIC_RELEASE);
    mvar->value = value;
  } else {
    aihc_mvar_wake(capability, taker, 1, value);
  }
  aihc_object_unlock(machine);
  return 1;
}

const AihcResume *aihc_await_io(AihcCapability *capability,
                                void *opaque_request, AihcValue *continuation) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL) {
    aihc_fail("attempted to await a null IO request");
  }
  if (request->state == AIHC_IO_COMPLETED) {
    return aihc_resume_current(capability, continuation);
  }
  if (request->state != AIHC_IO_SUBMITTED) {
    aihc_fail("attempted to await an IO request more than once");
  }

  AihcSlot roots[] = {(AihcSlot)(uintptr_t)request,
                      (AihcSlot)(uintptr_t)continuation};
  AihcRootFrame frame;
  aihc_roots_enter(capability, &frame, 2, roots);
  int error = capability->machine->io_backend->prepare(request);
  if (error != 0) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(error);
    aihc_roots_leave(capability, &frame);
    return aihc_resume_current(capability, (AihcValue *)(uintptr_t)roots[1]);
  }

  int64_t result = 0;
  if (capability->machine->io_backend->try_request(request, &result)) {
    request->state = AIHC_IO_COMPLETED;
    request->result =
        capability->machine->io_backend->finish_request(request, result);
    aihc_roots_leave(capability, &frame);
    return aihc_resume_current(capability, (AihcValue *)(uintptr_t)roots[1]);
  }

  continuation = (AihcValue *)(uintptr_t)roots[1];
  aihc_roots_leave(capability, &frame);
  request->state = AIHC_IO_PENDING;
  aihc_write_barrier(capability, (AihcValue *)request);
  request->thread = capability->current_thread;
  request->continuation = continuation;
  aihc_stack_note_top(continuation);
  aihc_thread_pause(capability, continuation);
  if (capability->io_requests_tail == NULL) {
    capability->io_requests_head = request;
  } else {
    capability->io_requests_tail->next = request;
  }
  capability->io_requests_tail = request;
  __atomic_add_fetch(&capability->io_request_count, 1, __ATOMIC_SEQ_CST);
  return aihc_schedule(capability);
}

/* Field zero of a thunk with bit zero names its owner thread, a blackhole
   queue of its owner, or the value. A thread record and a queue have kinds
   that no Haskell value has. */
static AihcThread *aihc_thunk_owner(const AihcValue *thunk,
                                    AihcBlackholeQueue **queue) {
  /* C promotes a dependent load to an acquire load. */
  AihcValue *word = (AihcValue *)(uintptr_t)__atomic_load_n(&thunk->fields[0],
                                                            __ATOMIC_ACQUIRE);
  *queue = NULL;
  switch (aihc_value_info_table(word)->object_kind) {
  case AIHC_OBJECT_THREAD:
    return (AihcThread *)word;
  case AIHC_OBJECT_BLACKHOLE_QUEUE:
    *queue = (AihcBlackholeQueue *)word;
    return (*queue)->owner;
  default:
    return NULL;
  }
}

/* Remove a queue from the list of its owner. The list is a root of the
   capability, and a link between two queues takes the write barrier. */
static void aihc_unlink_queue(AihcCapability *capability,
                              AihcBlackholeQueue *queue) {
  AihcBlackholeQueue **link = &queue->owner->queues;
  AihcBlackholeQueue *previous = NULL;
  while (*link != NULL && *link != queue) {
    previous = *link;
    link = &previous->next;
  }
  if (*link == NULL) {
    aihc_fail("blackhole queue is not on the list of its owner");
  }
  if (previous != NULL) {
    aihc_write_barrier(capability, (AihcValue *)previous);
  }
  *link = queue->next;
  aihc_write_barrier(capability, (AihcValue *)queue);
  queue->next = NULL;
}

/* Wake the waiters of a queue with a value, or with an exception. */
static void aihc_wake_queue(AihcCapability *capability,
                            AihcBlackholeQueue *queue, AihcSlot value,
                            int raise) {
  AihcBlackholeWaiter *waiter = queue->head;
  while (waiter != NULL) {
    AihcBlackholeWaiter *next = waiter->next;
    AihcValue *continuation = waiter->continuation;
    /* As for an MVar waiter: the frame is popped later. */
    waiter->continuation = NULL;
    if (raise) {
      aihc_suspend_raise(waiter->thread, (AihcValue *)(uintptr_t)value,
                         continuation);
    } else {
      aihc_suspend_continue(waiter->thread, continuation, 1, value);
    }
    aihc_enqueue_thread(capability, waiter->thread);
    waiter = next;
  }
}

/* A thread finds a claimed thunk. On one capability the owner runs on this
   capability, so the thread attaches itself to the queue of the thunk at
   once. The first waiter makes the queue. The eval helper reserved the
   waiter and the queue before it entered the C runtime. A collection on that
   path can follow an evaluated thunk to its value, so a value continues the
   thread. */
static void aihc_attach_waiter(AihcCapability *capability, AihcValue *value,
                               AihcThread *owner, AihcBlackholeQueue *queue,
                               AihcValue *continuation);

const AihcResume *aihc_block_on_blackhole(AihcCapability *capability,
                                          AihcValue *value,
                                          AihcValue *continuation) {
  AihcThread *current = capability->current_thread;
  if (value == NULL) {
    aihc_fail("attempted to block on null");
  }
  /* The pause claims the thunks of this thread before it waits. The object
     lock covers the look at the owner and the attach, so an update on
     another capability cannot miss the waiter. */
  aihc_stack_note_top(continuation);
  aihc_thread_pause(capability, continuation);
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  AihcBlackholeQueue *queue = NULL;
  AihcThread *owner = aihc_value_kind(value) == AIHC_OBJECT_BLACKHOLE
                          ? aihc_thunk_owner(value, &queue)
                          : NULL;
  if (owner == NULL) {
    AihcSlot result = aihc_value_kind(value) == AIHC_OBJECT_BLACKHOLE
                          ? value->fields[0]
                          : (AihcSlot)(uintptr_t)value;
    aihc_object_unlock(machine);
    aihc_suspend_continue(current, continuation, 1, result);
    return aihc_select_thread(capability, current);
  }
  if (owner == current) {
    aihc_fail("blackholed thunk re-entered");
  }
  aihc_attach_waiter(capability, value, owner, queue, continuation);
  aihc_object_unlock(machine);
  return aihc_schedule(capability);
}

/* Attach the current thread to the queue of a thunk that another thread
   claimed. The first waiter makes the queue. The caller holds the object
   lock, and it reserved the waiter and the queue. */
static void aihc_attach_waiter(AihcCapability *capability, AihcValue *value,
                               AihcThread *owner, AihcBlackholeQueue *queue,
                               AihcValue *continuation) {
  AihcThread *current = capability->current_thread;
  /* A waiter of a global thunk is global. */
  const AihcValue *container = queue == NULL ? value : (const AihcValue *)queue;
  AihcBlackholeWaiter *waiter = (AihcBlackholeWaiter *)aihc_record_allocate(
      capability, container, aihc_record_words(sizeof(*waiter)));
  waiter->header = (AihcSlot)(uintptr_t)&aihc_blackhole_waiter_info;
  waiter->thread = current;
  waiter->continuation = continuation;
  waiter->next = NULL;
  /* A global waiter names a frame, which the remembered set keeps reached
     until its chunk is old. A young waiter takes no entry. */
  aihc_write_barrier(capability, (AihcValue *)waiter);
  if (queue == NULL) {
    /* A queue of a global thunk is global. */
    queue = (AihcBlackholeQueue *)aihc_record_allocate(
        capability, value, aihc_record_words(sizeof(*queue)));
    queue->header = (AihcSlot)(uintptr_t)&aihc_blackhole_queue_info;
    queue->owner = owner;
    queue->thunk = value;
    queue->head = waiter;
    queue->tail = waiter;
    queue->next = owner->queues;
    owner->queues = queue;
    /* A global queue can name a younger queue of its owner and the waiter,
       so it goes into the remembered set. A young queue takes no entry. */
    aihc_write_barrier(capability, (AihcValue *)queue);
    aihc_write_barrier(capability, value);
    __atomic_store_n(&value->fields[0], (AihcSlot)(uintptr_t)queue,
                     __ATOMIC_RELEASE);
  } else {
    aihc_write_barrier(capability, (AihcValue *)queue->tail);
    queue->tail->next = waiter;
    aihc_write_barrier(capability, (AihcValue *)queue);
    queue->tail = waiter;
  }
}

/* noDuplicate#: the evaluations of the thread must not run twice. The walk
   claims the thunk of each update frame that no pause processed, from the
   top, as a pause does. A thunk that another thread claimed, or that
   another evaluation finished, means that this evaluation is a duplicate
   from that frame on. The thread then gives up the frames above the lowest
   such frame and gives back the claims that the walk made above it. It
   waits for that thunk with the parent continuation of the frame, or it
   continues there with the value. On one capability every suspension
   pauses, so no duplicate exists, but the walk is the same. See
   "noDuplicate#" in docs/multi-threaded-rts-design.md. */
const AihcResume *aihc_no_duplicate(AihcCapability *capability,
                                    AihcValue *continuation) {
  AihcThread *current = capability->current_thread;
  if (current == NULL) {
    aihc_fail("noDuplicate# without a running thread");
  }
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  AihcValue *lowest = NULL;
  for (AihcValue *frame = continuation;
       frame != NULL && aihc_region_kind(frame) == AIHC_REGION_STACK;
       frame = (AihcValue *)(uintptr_t)frame->fields[0]) {
    const AihcInfo *info = aihc_value_info_table(frame);
    if (info->frame_kind == AIHC_FRAME_STOP || info->field_count == 0) {
      break;
    }
    if (info->frame_kind != AIHC_FRAME_UPDATE) {
      continue;
    }
    if ((frame->header & AIHC_FRAME_PROCESSED) != 0) {
      break;
    }
    AihcValue *thunk = (AihcValue *)(uintptr_t)frame->fields[1];
    AihcSlot header = __atomic_load_n(&thunk->header, __ATOMIC_ACQUIRE);
    const AihcInfo *thunk_info =
        (const AihcInfo *)(uintptr_t)(header & ~AIHC_HEADER_TAG_MASK);
    if (thunk_info->object_kind == AIHC_OBJECT_INDIRECTION) {
      lowest = frame;
    } else if ((header & AIHC_HEADER_TAG_MASK) == 0) {
      aihc_write_barrier(capability, thunk);
      aihc_thunk_claim(thunk, current);
    } else {
      AihcBlackholeQueue *queue = NULL;
      if (aihc_thunk_owner(thunk, &queue) != current) {
        lowest = frame;
      }
    }
    frame->header |= AIHC_FRAME_PROCESSED;
  }
  if (lowest == NULL) {
    aihc_object_unlock(machine);
    return aihc_resume_current(capability, continuation);
  }
  /* The claims above the lowest duplicate frame are new: this walk made
     them, and no waiter can have come since, because the walk holds the
     lock. They go back to the state Thunk. */
  for (AihcValue *frame = continuation; frame != lowest;
       frame = (AihcValue *)(uintptr_t)frame->fields[0]) {
    if (aihc_value_info_table(frame)->frame_kind != AIHC_FRAME_UPDATE) {
      continue;
    }
    AihcValue *thunk = (AihcValue *)(uintptr_t)frame->fields[1];
    AihcBlackholeQueue *queue = NULL;
    AihcSlot header = __atomic_load_n(&thunk->header, __ATOMIC_ACQUIRE);
    if ((header & AIHC_HEADER_EVALUATING) != 0 &&
        aihc_thunk_owner(thunk, &queue) == current && queue == NULL) {
      aihc_write_barrier(capability, thunk);
      __atomic_store_n(&thunk->header, header & ~AIHC_HEADER_TAG_MASK,
                       __ATOMIC_RELEASE);
    }
  }
  AihcValue *thunk = (AihcValue *)(uintptr_t)lowest->fields[1];
  AihcValue *parent = (AihcValue *)(uintptr_t)lowest->fields[0];
  AihcBlackholeQueue *queue = NULL;
  AihcThread *owner = aihc_thunk_owner(thunk, &queue);
  if (owner == NULL) {
    /* Another evaluation finished the thunk: field zero is the value. */
    AihcSlot value = __atomic_load_n(&thunk->fields[0], __ATOMIC_ACQUIRE);
    aihc_object_unlock(machine);
    aihc_suspend_continue(current, parent, 1, value);
    return aihc_select_thread(capability, current);
  }
  aihc_stack_note_top(parent);
  aihc_attach_waiter(capability, thunk, owner, queue, parent);
  aihc_object_unlock(machine);
  return aihc_schedule(capability);
}
const AihcResume *aihc_complete_io(AihcCapability *capability, int64_t result) {
  AihcIoRequest *request = capability->io_requests_head;
  if (request == NULL) {
    aihc_fail("IO completion has no pending request");
  }
  capability->io_requests_head = request->next;
  --capability->io_request_count;
  int64_t request_result =
      capability->machine->io_backend->finish_request(request, result);
  aihc_resume_io_request(capability, request, request_result);

  while (capability->io_requests_head != NULL) {
    request = capability->io_requests_head;
    if (!capability->machine->io_backend->try_request(request,
                                                      &request_result)) {
      break;
    }
    request_result = capability->machine->io_backend->finish_request(
        request, request_result);
    capability->io_requests_head = request->next;
    --capability->io_request_count;
    aihc_resume_io_request(capability, request, request_result);
  }
  if (capability->io_requests_head == NULL) {
    capability->io_requests_tail = NULL;
  }
  return aihc_schedule(capability);
}

AihcValue *aihc_update(AihcCapability *capability, AihcValue *object,
                       AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  AihcSlot stored = aihc_escape(capability, object, (AihcSlot)(uintptr_t)value);
  if (stored == AIHC_ESCAPE_COLLECT) {
    return NULL;
  }
  value = (AihcValue *)(uintptr_t)stored;
  aihc_write_barrier_update(capability, object);
  /* An indirection has two words, and the rest of the thunk is slop. The
     collector never walks it: it follows an indirection of a generation it
     copies and leaves an older one in place. A heap walker, such as the
     collector fuzz driver, does walk old generations, so the slop reads as
     zero words, which no object header is. */
  uint64_t words = aihc_value_words(object);
  object->fields[0] = (AihcSlot)value;
  object->header = (AihcSlot)(uintptr_t)&aihc_indirection_info;
  for (uint64_t index = 1; index < words - 1; ++index) {
    object->fields[index] = 0;
  }
  return value;
}

/* The update of a thunk at the end of its evaluation. The value goes into
   field zero, and the header keeps the thunk table with bit zero. A thunk
   that no pause claimed is the usual case. A claimed thunk can have a queue
   of this thread, whose waiters wake. The collector follows an evaluated
   thunk to its value, as it follows an indirection. */
AihcValue *aihc_update_blackhole(AihcCapability *capability, AihcValue *object,
                                 AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  AihcSlot observed = __atomic_load_n(&object->header, __ATOMIC_ACQUIRE);
  const AihcInfo *info =
      (const AihcInfo *)(uintptr_t)(observed & ~AIHC_HEADER_TAG_MASK);
  AihcObjectKind kind = info->object_kind;
  if (kind == AIHC_OBJECT_INDIRECTION) {
    /* Another evaluation finished, and a collection converted the thunk.
       This duplicate result needs no store. */
    capability->pause_top = NULL;
    return value;
  }
  if (kind != AIHC_OBJECT_THUNK) {
    aihc_fail("attempted to update a cell that is not a thunk");
  }
  /* The promotion comes before the object lock, because it can collect. */
  AihcSlot stored = aihc_escape(capability, object, (AihcSlot)(uintptr_t)value);
  if (stored == AIHC_ESCAPE_COLLECT) {
    return NULL;
  }
  AihcMachine *machine = capability->machine;
  aihc_object_lock(machine);
  AihcSlot header = __atomic_load_n(&object->header, __ATOMIC_ACQUIRE);
  if ((header & AIHC_HEADER_TAG_MASK) == AIHC_HEADER_TAG_MASK) {
    aihc_fail("attempted to update a thunk in the state Claiming");
  }
  /* The update deletes the owner or the queue in field zero and every free
     variable, so the barrier shades them. */
  aihc_count_escape(capability, object, (AihcSlot)(uintptr_t)value);
  value = (AihcValue *)(uintptr_t)stored;
  aihc_write_barrier_update(capability, object);
  AihcBlackholeQueue *queue = NULL;
  if ((header & AIHC_HEADER_EVALUATING) != 0) {
    (void)aihc_thunk_owner(object, &queue);
  }
  /* Both stores are release stores: the value is in field zero before a
     reader can see bit zero. */
  __atomic_store_n(&object->fields[0], (AihcSlot)(uintptr_t)value,
                   __ATOMIC_RELEASE);
  __atomic_store_n(&object->header, header | AIHC_HEADER_EVALUATING,
                   __ATOMIC_RELEASE);
  if (queue != NULL) {
    /* The queue of the owner, or the queue of another thread when this
       evaluation is a duplicate. The value stands, and the waiters get it
       now. */
    aihc_unlink_queue(capability, queue);
    aihc_wake_queue(capability, queue, (AihcSlot)(uintptr_t)value, 0);
  }
  aihc_object_unlock(machine);
  return value;
}
/* An exception unwinds an update frame. A thunk that this thread claimed
   goes back to the state Thunk, and its waiters get the exception. A thunk
   that no pause claimed needs nothing: the entry wrote nothing into it, and
   its free variables are intact. A thunk that another evaluation finished
   keeps its value. */
static void aihc_abandon_blackhole_locked(AihcCapability *capability,
                                          AihcValue *object,
                                          AihcValue *exception);

static void aihc_abandon_blackhole(AihcCapability *capability,
                                   AihcValue *object, AihcValue *exception) {
  aihc_object_lock(capability->machine);
  aihc_abandon_blackhole_locked(capability, object, exception);
  aihc_object_unlock(capability->machine);
}

static void aihc_abandon_blackhole_locked(AihcCapability *capability,
                                          AihcValue *object,
                                          AihcValue *exception) {
  if (object != NULL &&
      aihc_value_info_table(object)->object_kind == AIHC_OBJECT_INDIRECTION) {
    return;
  }
  if (object == NULL ||
      aihc_value_info_table(object)->object_kind != AIHC_OBJECT_THUNK) {
    aihc_fail("exception update frame does not contain a thunk");
  }
  AihcSlot header = __atomic_load_n(&object->header, __ATOMIC_ACQUIRE);
  if ((header & AIHC_HEADER_EVALUATING) == 0) {
    return;
  }
  AihcBlackholeQueue *queue = NULL;
  AihcThread *owner = aihc_thunk_owner(object, &queue);
  if (owner != capability->current_thread) {
    return;
  }
  /* Field zero stops being a pointer, so the barrier shades the owner or
     the queue. */
  aihc_write_barrier(capability, object);
  __atomic_store_n(&object->header, header & ~AIHC_HEADER_TAG_MASK,
                   __ATOMIC_RELEASE);
  if (queue != NULL) {
    aihc_unlink_queue(capability, queue);
    aihc_wake_queue(capability, queue, (AihcSlot)(uintptr_t)exception, 1);
  }
}
const AihcResume *aihc_raise(AihcCapability *capability, AihcValue *exception,
                             AihcValue *continuation) {
  if (exception == NULL) {
    aihc_fail("attempted to raise a null exception");
  }
  for (;;) {
    if (continuation == NULL ||
        aihc_value_kind(continuation) != AIHC_OBJECT_CLOSURE) {
      aihc_fail("exception chain contains a non-continuation value");
    }
    /* The walk pops the frame, so the marking of an active gen2 cycle
       scans it first. */
    aihc_gc_frame_read(capability, continuation);
    const AihcInfo *info = aihc_value_info_table(continuation);
    const AihcSlot *fields = aihc_value_fields_const(continuation);
    switch (info->frame_kind) {
    case AIHC_FRAME_FORWARD:
    case AIHC_FRAME_NORMAL:
      if (info->field_count < 1) {
        aihc_fail("normal continuation has no parent");
      }
      continuation = (AihcValue *)(uintptr_t)fields[0];
      break;
    case AIHC_FRAME_CATCH: {
      if (info->field_count < 2) {
        aihc_fail("catch continuation has an invalid layout");
      }
      AihcResume *resume = &capability->selected_resume;
      resume->kind = AIHC_RESUME_APPLY;
      resume->function = (AihcValue *)(uintptr_t)fields[1];
      resume->continuation = (AihcValue *)(uintptr_t)fields[0];
      resume->value = (AihcSlot)(uintptr_t)exception;
      resume->count = 1;
      return resume;
    }
    case AIHC_FRAME_UPDATE:
      if (info->field_count < 2) {
        aihc_fail("update continuation has an invalid layout");
      }
      aihc_abandon_blackhole(capability, (AihcValue *)(uintptr_t)fields[1],
                             exception);
      continuation = (AihcValue *)(uintptr_t)fields[0];
      break;
    case AIHC_FRAME_PROMPT:
      /* An exception passes through a prompt as GHC's does: the prompt
         delimits control0#, not raise#. */
      if (info->field_count < 2) {
        aihc_fail("prompt continuation has an invalid layout");
      }
      continuation = (AihcValue *)(uintptr_t)fields[0];
      break;
    case AIHC_FRAME_STOP:
      aihc_fail("uncaught Haskell exception");
    case AIHC_FRAME_RESTORE_MASK:
      aihc_fail("restore-mask continuation is not implemented");
    default:
      aihc_fail("exception chain contains a non-frame closure");
    }
  }
}

/* What control0# hands its function: the topmost captured frame. The
   frames between the top and the prompt are the continuation. They are on
   the stack, and the stack reuses their bytes when the prompt returns, so
   the capture copies them to the managed heap. The copies keep their info
   tables and captures. Each copy links to the copy below it, and the lowest
   copy has a null parent. A resume never writes the copies, so it can
   resume the same record any number of times. */
static const uint8_t aihc_continuation_field_is_pointer[] = {1};
static const AihcInfo aihc_continuation_info = {
    .field_count = 1,
    .field_is_pointer = aihc_continuation_field_is_pointer,
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_NODE,
};

/* The parent of a frame in a chain that control0# or a resume walks. Both
   walks stop at a prompt frame, so the frames they cross are the ones an
   exception would cross too, minus the ones a capture must refuse: an update
   frame, because the thunk it belongs to is not reentrant and the frame
   marks the start of another state thread, and a stop frame, because the
   chain has ended without a prompt. */
static AihcValue *aihc_captured_frame_parent(AihcValue *frame, int capturing) {
  if (frame == NULL || aihc_value_kind(frame) != AIHC_OBJECT_CLOSURE) {
    aihc_fail("continuation chain contains a non-continuation value");
  }
  const AihcInfo *info = aihc_value_info_table(frame);
  switch (info->frame_kind) {
  case AIHC_FRAME_FORWARD:
  case AIHC_FRAME_NORMAL:
  case AIHC_FRAME_CATCH:
  case AIHC_FRAME_PROMPT:
    if (info->field_count < 1) {
      aihc_fail("continuation frame has no parent");
    }
    return (AihcValue *)(uintptr_t)aihc_value_fields_const(frame)[0];
  case AIHC_FRAME_UPDATE:
    aihc_fail(capturing ? "control0# cannot capture a continuation that "
                          "contains a thunk under evaluation"
                        : "captured continuation contains an update frame");
  case AIHC_FRAME_STOP:
    aihc_fail(capturing ? "control0# found no prompt frame with its tag"
                        : "captured continuation reached a stop frame");
  case AIHC_FRAME_RESTORE_MASK:
    aihc_fail("restore-mask continuation is not implemented");
  default:
    aihc_fail("continuation chain contains a non-frame closure");
  }
}

static int aihc_is_prompt_frame(const AihcValue *frame, const AihcValue *tag) {
  const AihcInfo *info = aihc_value_info_table(frame);
  return info->frame_kind == AIHC_FRAME_PROMPT && info->field_count >= 2 &&
         (const AihcValue *)(uintptr_t)aihc_value_fields_const(frame)[1] == tag;
}

const AihcResume *aihc_control0(AihcCapability *capability, AihcValue *tag,
                                AihcValue *function, AihcValue *continuation) {
  if (tag == NULL) {
    aihc_fail("control0# received a null prompt tag");
  }
  uint64_t words = 1 + aihc_continuation_info.field_count;
  AihcValue *prompt = continuation;
  while (!(aihc_value_kind(prompt) == AIHC_OBJECT_CLOSURE &&
           aihc_is_prompt_frame(prompt, tag))) {
    /* The capture copies the frame and pops it, so the marking of an
       active gen2 cycle scans it first. */
    aihc_gc_frame_read(capability, prompt);
    words += aihc_value_words(prompt);
    prompt = aihc_captured_frame_parent(prompt, 1);
  }
  aihc_gc_frame_read(capability, prompt);

  /* Compiled code transfers to the resumption straight after this call, so
     the calling function's static references are dead here. Frames do not
     move. The continuation is a root so that the collector scans the frames
     before this call copies them. */
  AihcSlot roots[2] = {(AihcSlot)(uintptr_t)function,
                       (AihcSlot)(uintptr_t)continuation};
  aihc_heap_collect(capability, words, 2, roots, NULL);
  function = (AihcValue *)(uintptr_t)roots[0];

  AihcValue *captured = aihc_place_node(capability, &aihc_continuation_info,
                                        1 + aihc_continuation_info.field_count);
  aihc_value_fields(captured)[0] = 0;
  AihcValue *previous = NULL;
  for (AihcValue *frame = continuation; frame != prompt;
       frame = aihc_captured_frame_parent(frame, 1)) {
    uint64_t frame_words = aihc_value_words(frame);
    AihcValue *copy = aihc_gc_allocate(capability, frame_words);
    memcpy(copy, frame, frame_words * sizeof(AihcSlot));
    aihc_value_fields(copy)[0] = 0;
    if (previous == NULL) {
      aihc_value_fields(captured)[0] = (AihcSlot)(uintptr_t)copy;
    } else {
      aihc_value_fields(previous)[0] = (AihcSlot)(uintptr_t)copy;
    }
    previous = copy;
  }

  /* The prompt frame is popped with the frames above it: the function runs
     under the prompt's parent. */
  AihcResume *resume = &capability->selected_resume;
  resume->kind = AIHC_RESUME_APPLY;
  resume->function = function;
  resume->continuation =
      (AihcValue *)(uintptr_t)aihc_value_fields_const(prompt)[0];
  resume->value = (AihcSlot)(uintptr_t)captured;
  resume->count = 1;
  return resume;
}

const AihcResume *aihc_continuation_resume(AihcCapability *capability,
                                           AihcValue *captured,
                                           AihcValue *action,
                                           AihcValue *continuation) {
  if (captured == NULL ||
      aihc_value_info_table(captured) != &aihc_continuation_info) {
    aihc_fail("resumed value is not a captured continuation");
  }
  /* The copies are linked from the top down, and a push must go from the
     bottom up. Thus one walk collects the copies in an array, and the pushes
     read the array from its end. */
  uint64_t count = 0;
  uint64_t capacity = 0;
  AihcValue **frames = NULL;
  for (AihcValue *frame =
           (AihcValue *)(uintptr_t)aihc_value_fields_const(captured)[0];
       frame != NULL;
       frame = (AihcValue *)(uintptr_t)aihc_value_fields_const(frame)[0]) {
    if (count == capacity) {
      capacity = capacity == 0 ? 16 : capacity * 2;
      AihcValue **grown = realloc(frames, capacity * sizeof(*frames));
      if (grown == NULL) {
        aihc_fail("out of memory");
      }
      frames = grown;
    }
    frames[count++] = frame;
  }

  /* The resumer's continuation is the top of the stack. Each pushed copy
     links to the frame below it, and the lowest one links to the resumer's
     continuation, where the prompt frame used to be. Pushes do not collect,
     so the heap copies stay where they are. */
  aihc_stack_resume_after(capability, continuation);
  AihcValue *resumed = continuation;
  while (count != 0) {
    AihcValue *source = frames[--count];
    uint64_t frame_words = aihc_value_words(source);
    AihcValue *copy = aihc_stack_push(capability, frame_words);
    memcpy(copy, source, frame_words * sizeof(AihcSlot));
    aihc_value_fields(copy)[0] = (AihcSlot)(uintptr_t)resumed;
    resumed = copy;
  }
  free(frames);

  AihcResume *resume = &capability->selected_resume;
  resume->kind = AIHC_RESUME_APPLY;
  resume->function = action;
  resume->continuation = resumed;
  resume->value = 0;
  resume->count = 0;
  return resume;
}

AihcSlot aihc_fork(AihcCapability *capability, AihcValue *action) {
  if (capability->machine->thread_done_info == NULL) {
    aihc_fail("thread completion continuation is not initialized");
  }
  AihcThread *child = aihc_thread_new(capability);
  /* The new stack starts with the frame that ends the thread. */
  AihcValue *done = (AihcValue *)aihc_stack_base(capability->machine->stacks);
  done->header = (AihcSlot)(uintptr_t)capability->machine->thread_done_info;
  aihc_suspend_apply(child, action, done);
  aihc_enqueue_thread(capability, child);
  return (AihcSlot)child;
}

const AihcResume *aihc_yield(AihcCapability *capability,
                             AihcValue *continuation) {
  AihcThread *current = capability->current_thread;
  aihc_suspend_continue(current, continuation, 0, 0);
  aihc_thread_pause(capability, continuation);
  aihc_enqueue_thread(capability, current);
  return aihc_schedule(capability);
}

const AihcResume *aihc_thread_done(AihcCapability *capability) {
  /* The thread entered the frame at the bottom of its stack, so no frame of
     the stack is live. */
  AihcStack *stack = aihc_stack_of(capability->stack_next - 1);
  if (stack->thread != capability->current_thread) {
    aihc_fail("finished thread does not own the current stack");
  }
  aihc_stack_release(capability, stack);
  capability->stack_next = NULL;
  return aihc_schedule(capability);
}

void aihc_set_thread_done_continuation(AihcCapability *capability,
                                       AihcValue *thread_done_continuation) {
  if (thread_done_continuation == NULL ||
      aihc_value_kind(thread_done_continuation) != AIHC_OBJECT_CLOSURE ||
      aihc_value_arity(thread_done_continuation) != 1 ||
      aihc_value_info_table(thread_done_continuation)->field_count != 0) {
    aihc_fail("invalid thread completion continuation");
  }
  capability->machine->thread_done_info =
      aihc_value_info_table(thread_done_continuation);
}

AihcEntry aihc_halt(AihcCapability *capability) {
  if (capability->index != 0) {
    /* The main thread ran on a worker, which did not start the program.
       The process ends here, as main ends it after its exit function. */
    aihc_exit_process(0);
  }
  return capability->exit_code;
}

extern void aihc_lir_worker_enter(AihcCapability *capability,
                                  const AihcResume *resume);

_Noreturn void aihc_worker_run(AihcCapability *capability) {
  aihc_lir_worker_enter(capability, aihc_schedule(capability));
  aihc_fail("a worker returned");
}

/* Give each capability except the first a worker. The first capability
   waits until every worker is in its idle wait, so the first push of the
   program does not depend on the speed of the OS threads. */
static void aihc_workers_start(AihcCapability *capability) {
  AihcMachine *machine = capability->machine;
  machine->heap_lock = aihc_host_recursive_lock_new();
  aihc_regions_set_lock(machine->heap_lock);
  /* One recursive lock serves as both: a promotion under the heap lock
     claims thunks, and an MVar operation under the object lock allocates,
     so two locks would need opposite orders. */
  machine->object_lock = machine->heap_lock;
  machine->stop_lock = aihc_host_lock_new();
  machine->stop_condition = aihc_host_condition_new();
  machine->running_count = machine->capability_count;
  machine->workers_active = 1;
  aihc_host_workers_start(capability);
  while (__atomic_load_n(&machine->idle_count, __ATOMIC_SEQ_CST) + 1 <
         machine->capability_count) {
    aihc_host_sleep_ns(100000);
  }
}

static void aihc_stm_expire_timers(AihcCapability *capability);

/* Each nested transaction keeps its own reverse write log. */
uint64_t aihc_stm_begin(AihcCapability *capability) {
  if (capability->current_thread->transaction == NULL) {
    aihc_stm_expire_timers(capability);
  }
  uint64_t words = aihc_record_words(sizeof(AihcTransaction));
  AihcTransaction *transaction =
      (AihcTransaction *)aihc_gc_allocate(capability, words);
  transaction->header = (AihcSlot)(uintptr_t)&aihc_transaction_info;
  transaction->writes = NULL;
  transaction->reads = NULL;
  transaction->parent = capability->current_thread->transaction;
  capability->current_thread->transaction = transaction;
  return 0;
}

uint64_t aihc_stm_active(AihcCapability *capability) {
  return capability->current_thread->transaction != NULL;
}

/* Add an entry to a log of a transaction. The caller reserved it. */
static void aihc_log_add(AihcCapability *capability,
                         AihcTransaction *transaction,
                         AihcTransactionWrite **log, AihcValue *variable,
                         AihcSlot value) {
  uint64_t words = aihc_record_words(sizeof(AihcTransactionWrite));
  AihcTransactionWrite *entry =
      (AihcTransactionWrite *)aihc_gc_allocate(capability, words);
  entry->header = (AihcSlot)(uintptr_t)&aihc_transaction_write_info;
  entry->variable = variable;
  entry->value = value;
  entry->next = *log;
  aihc_write_barrier(capability, (AihcValue *)transaction);
  *log = entry;
}

uint64_t aihc_tvar_write(AihcCapability *capability, AihcValue *variable,
                         AihcSlot value) {
  AihcTransaction *transaction = capability->current_thread->transaction;
  if (transaction == NULL) {
    aihc_fail("TVar write outside a transaction");
  }
  aihc_log_add(capability, transaction, &transaction->writes, variable, value);
  return 0;
}

/* Whether every read of a transaction and of its parents still sees the
   value that it saw. */
static int aihc_transaction_valid(const AihcTransaction *transaction) {
  for (; transaction != NULL; transaction = transaction->parent) {
    for (const AihcTransactionWrite *read = transaction->reads; read != NULL;
         read = read->next) {
      if (__atomic_load_n(&read->variable->fields[1], __ATOMIC_ACQUIRE) !=
          read->value) {
        return 0;
      }
    }
  }
  return 1;
}

uint64_t aihc_stm_abort(AihcCapability *capability) {
  AihcTransaction *transaction = capability->current_thread->transaction;
  if (transaction == NULL) {
    aihc_fail("STM abort outside a transaction");
  }
  aihc_object_lock(capability->machine);
  int valid = aihc_transaction_valid(transaction);
  aihc_object_unlock(capability->machine);
  capability->current_thread->transaction = transaction->parent;
  return (uint64_t)valid;
}

/* Put a log in front of another one. */
static void aihc_log_prepend(AihcCapability *capability, AihcTransaction *owner,
                             AihcTransactionWrite **log,
                             AihcTransactionWrite *first) {
  if (first == NULL) {
    return;
  }
  AihcTransactionWrite *last = first;
  while (last->next != NULL) {
    last = last->next;
  }
  aihc_write_barrier(capability, (AihcValue *)last);
  last->next = *log;
  aihc_write_barrier(capability, (AihcValue *)owner);
  *log = first;
}

/* The commit gives 1 when it applied the writes, 0 when a read no longer
   holds and the transaction must run again, and 2 when the promotion of a
   written value needs a collection. The caller then collects at its store
   safepoint and commits again. A nested commit gives its logs to its
   parent. */
static uint64_t aihc_stm_commit_locked(AihcCapability *capability);

/* The object lock makes the check of the reads and the writes one step for
   the other capabilities. */
uint64_t aihc_stm_commit(AihcCapability *capability) {
  aihc_object_lock(capability->machine);
  uint64_t result = aihc_stm_commit_locked(capability);
  aihc_object_unlock(capability->machine);
  return result;
}

static uint64_t aihc_stm_commit_locked(AihcCapability *capability) {
  AihcValue *top = capability->pause_top;
  capability->pause_top = NULL;
  AihcTransaction *transaction = capability->current_thread->transaction;
  if (transaction == NULL) {
    aihc_fail("STM commit outside a transaction");
  }
  AihcTransaction *parent = transaction->parent;
  if (parent != NULL) {
    aihc_log_prepend(capability, parent, &parent->writes, transaction->writes);
    aihc_log_prepend(capability, parent, &parent->reads, transaction->reads);
    capability->current_thread->transaction = parent;
    return 1;
  }
  if (!aihc_transaction_valid(transaction)) {
    capability->current_thread->transaction = NULL;
    return 0;
  }
  /* A global variable gets a global value. The promotions come first, so a
     promotion that needs a collection leaves every variable as it was. */
  uint64_t count = 0;
  for (AihcTransactionWrite *write = transaction->writes; write != NULL;
       write = write->next) {
    capability->pause_top = top;
    AihcSlot stored = aihc_escape(capability, write->variable, write->value);
    if (stored == AIHC_ESCAPE_COLLECT) {
      capability->pause_top = NULL;
      return 2;
    }
    if (stored != write->value) {
      aihc_write_barrier(capability, (AihcValue *)write);
      write->value = stored;
    }
    ++count;
  }
  capability->pause_top = NULL;
  /* The log has the newest write first, and the newest write of a variable
     must be its final value. */
  AihcTransactionWrite **writes = NULL;
  if (count != 0) {
    writes = malloc(count * sizeof(*writes));
    if (writes == NULL) {
      aihc_fail("out of memory");
    }
  }
  uint64_t index = count;
  for (AihcTransactionWrite *write = transaction->writes; write != NULL;
       write = write->next) {
    writes[--index] = write;
  }
  for (index = 0; index < count; ++index) {
    AihcTransactionWrite *write = writes[index];
    aihc_count_escape(capability, write->variable, write->value);
    aihc_write_barrier(capability, write->variable);
    __atomic_store_n(&write->variable->fields[1], write->value,
                     __ATOMIC_RELEASE);
  }
  free(writes);
  capability->current_thread->transaction = NULL;
  return 1;
}

AihcValue *aihc_tvar_delay(AihcCapability *capability, int64_t delay,
                           AihcSlot initial, AihcSlot final) {
  uint64_t timer_words = aihc_record_words(sizeof(AihcTransactionTimer));
  /* The caller reserves the variable and timer together. */
  AihcValue *variable =
      aihc_mutvar_new(capability, delay <= 0 ? final : initial);
  if (delay > 0) {
    AihcTransactionTimer *timer =
        (AihcTransactionTimer *)aihc_gc_allocate(capability, timer_words);
    timer->header = (AihcSlot)(uintptr_t)&aihc_transaction_timer_info;
    timer->variable = variable;
    timer->final = final;
    uint64_t now = aihc_host_monotonic_ns();
    uint64_t maximum = UINT64_MAX - now;
    timer->deadline = (uint64_t)delay > maximum / 1000
                          ? UINT64_MAX
                          : now + (uint64_t)delay * 1000;
    aihc_object_lock(capability->machine);
    timer->next = capability->machine->transaction_timers;
    capability->machine->transaction_timers = timer;
    aihc_object_unlock(capability->machine);
  }
  return variable;
}

static void aihc_stm_expire_timers(AihcCapability *capability) {
  if (__atomic_load_n(&capability->machine->transaction_timers,
                      __ATOMIC_ACQUIRE) == NULL) {
    return;
  }
  aihc_object_lock(capability->machine);
  uint64_t now = aihc_host_monotonic_ns();
  AihcTransactionTimer **link = &capability->machine->transaction_timers;
  while (*link != NULL) {
    AihcTransactionTimer *timer = *link;
    if (timer->deadline <= now) {
      aihc_write_barrier(capability, timer->variable);
      __atomic_store_n(&timer->variable->fields[1], timer->final,
                       __ATOMIC_RELEASE);
      *link = timer->next;
    } else {
      link = &timer->next;
    }
  }
  aihc_object_unlock(capability->machine);
}

/* A read inside a transaction sees the newest write of the transaction and
   of its parents. Another read goes into the read log, which the caller
   reserved. A transaction observes one timer state until it commits or
   retries. */
AihcSlot aihc_tvar_read(AihcCapability *capability, AihcValue *variable) {
  AihcTransaction *transaction = capability->current_thread->transaction;
  if (transaction == NULL) {
    return aihc_tvar_read_io(capability, variable);
  }
  for (const AihcTransaction *each = transaction; each != NULL;
       each = each->parent) {
    for (const AihcTransactionWrite *write = each->writes; write != NULL;
         write = write->next) {
      if (write->variable == variable) {
        return write->value;
      }
    }
  }
  AihcSlot value = __atomic_load_n(&variable->fields[1], __ATOMIC_ACQUIRE);
  aihc_log_add(capability, transaction, &transaction->reads, variable, value);
  return value;
}

AihcSlot aihc_tvar_read_io(AihcCapability *capability, AihcValue *variable) {
  if (!aihc_stm_active(capability)) {
    aihc_stm_expire_timers(capability);
  }
  return __atomic_load_n(&variable->fields[1], __ATOMIC_ACQUIRE);
}

void *aihc_stm_wait_request(AihcCapability *capability) {
  AihcIoRequest *request = aihc_io_request_new(capability);
  request->kind = AIHC_IO_TIMER;
  request->state = AIHC_IO_COMPLETED;
  aihc_object_lock(capability->machine);
  /* Another capability or a suspended IO operation can change a TVar.
     A short timer yields the capability before the transaction tries again.
     The absence of a registerDelay timer does not prove a deadlock. */
  int may_change = capability->machine->capability_count > 1 ||
                   capability->run_queue_head != NULL ||
                   capability->io_request_count != 0 ||
                   aihc_inbox_used(capability);
  if (!may_change && capability->machine->transaction_timers == NULL) {
    aihc_object_unlock(capability->machine);
    return request;
  }
  uint64_t now = aihc_host_monotonic_ns();
  request->deadline = capability->machine->transaction_timers == NULL &&
                              may_change &&
                              now <= UINT64_MAX - UINT64_C(10000000)
                          ? now + UINT64_C(10000000)
                          : UINT64_MAX;
  for (AihcTransactionTimer *timer = capability->machine->transaction_timers;
       timer != NULL; timer = timer->next) {
    if (timer->deadline < request->deadline) {
      request->deadline = timer->deadline;
    }
  }
  aihc_object_unlock(capability->machine);
  request->state = AIHC_IO_SUBMITTED;
  return request;
}

int64_t aihc_stm_wait_result(AihcCapability *capability, void *request) {
  int64_t result = aihc_io_take_result(request);
  if (result < 0) {
    aihc_fail("STM timer wait failed");
  }
  aihc_stm_expire_timers(capability);
  return result;
}

/* GHC's RTS API, as far as package C code uses it. unix, process, posix-pty
   and rawfilepath stop the interval timer and block the user signals around
   fork, so that the child inherits neither. aihc runs no interval timer, since
   its scheduler is cooperative, and installs no signal handlers, so each call
   has nothing to undo. rtsSupportsBoundThreads answers for a runtime without
   OS threads. */
void startTimer(void) {}

void stopTimer(void) {}

void blockUserSignals(void) {}

void unblockUserSignals(void) {}

int rtsSupportsBoundThreads(void) { return 0; }

/* A non-zero value tells stg_sig_install to give SA_NOCLDSTOP to a SIGCHLD
   action, so that a stopped child does not send SIGCHLD. The type is HsInt,
   because System.Posix.Signals reads and writes it as a Ptr Int. */
int64_t nocldstop = 0;

uint64_t aihc_clock_monotonic_ns(void) { return aihc_host_monotonic_ns(); }
