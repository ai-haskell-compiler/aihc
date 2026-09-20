#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

#ifndef AIHC_SEMISPACE_BYTES
#define AIHC_SEMISPACE_BYTES (UINT64_C(1024) * UINT64_C(1024))
#endif

enum {
  AIHC_OBJECT_NODE,
  AIHC_OBJECT_CLOSURE,
  AIHC_OBJECT_THUNK,
  AIHC_OBJECT_PARTIAL_CONSTRUCTOR,
  AIHC_OBJECT_INDIRECTION,
  AIHC_OBJECT_BLACKHOLE,
  AIHC_OBJECT_ARRAY,
  AIHC_OBJECT_THREAD,
  /* Byte arrays and stable names remain outside the managed heap.
     Each object has a header because compiled code stores it in pointer fields.
     The root visitor traces stable-name referents through the machine list. */
  AIHC_OBJECT_RUNTIME,
  AIHC_OBJECT_TRANSACTION,
  AIHC_OBJECT_TRANSACTION_WRITE,
  AIHC_OBJECT_TRANSACTION_TIMER,
  AIHC_OBJECT_MVAR,
  AIHC_OBJECT_MVAR_WAITER,
};
typedef uint8_t AihcObjectKind;

typedef struct AihcValue AihcValue;
typedef struct AihcMachine AihcMachine;
typedef struct AihcTransaction AihcTransaction;
typedef struct AihcTransactionTimer AihcTransactionTimer;
typedef struct AihcInfo AihcInfo;
typedef struct AihcSrt AihcSrt;
typedef struct AihcThread AihcThread;
typedef struct AihcBlackhole AihcBlackhole;
typedef struct AihcIoHandle AihcIoHandle;
typedef struct AihcIoRequest AihcIoRequest;
typedef struct AihcIoBackend AihcIoBackend;
typedef struct AihcMVar AihcMVar;
typedef struct AihcStableName AihcStableName;
typedef uint64_t AihcSlot;
/* The portable entry of an info table. Reserved: Lir stores null until the
   runtime moves to Lir. The exit code of a machine has this type. */
typedef void (*AihcEntry)(AihcMachine *machine);
/* The backend entry is a Lir function with the signature
   (machine, object, continuation, supplied values...). Common runtime code
   preserves it but never calls it. */
typedef void (*AihcBackendEntry)(void);

enum {
  AIHC_RESUME_NONE,
  AIHC_RESUME_APPLY,
  AIHC_RESUME_CONTINUE,
  AIHC_RESUME_RAISE,
};
typedef uint64_t AihcResumeKind;

typedef struct {
  AihcResumeKind kind;
  AihcValue *function;
  AihcValue *continuation;
  AihcSlot value;
  uint64_t count;
} AihcResume;

enum {
  AIHC_FRAME_NONE = 0,
  AIHC_FRAME_NORMAL = 1,
  AIHC_FRAME_CATCH = 2,
  AIHC_FRAME_UPDATE = 3,
  AIHC_FRAME_RESTORE_MASK = 4,
  AIHC_FRAME_STOP = 5,
  /* The delimiter that prompt# pushes: [parent, tag]. Exception unwinding
     passes through it; control0# captures the frames above it. */
  AIHC_FRAME_PROMPT = 6,
};
typedef uint8_t AihcFrameKind;

/* A static reference table names the static objects one function reaches
   without going through a heap object. Tables are chained: a table names the
   tables of the functions its code calls directly, so the collector walks the
   graph instead of the compiler flattening it.

   Backends emit one word-uniform record per table: the walk link, the two
   counts, then object_count static object addresses followed by child_count
   table addresses. The link is mutable, so tables belong in a writable
   section even though info tables themselves are read-only. */
struct AihcSrt {
  /* Null until a collection walks this table, then the next walked table.
     Recursive functions make the table graph cyclic, so the collector links
     every table it walks and clears the whole list when the collection
     ends. */
  AihcSrt *walked;
  uintptr_t object_count;
  uintptr_t child_count;
  uintptr_t entries[];
};

/* Five word-wide fields followed by four byte-wide ones, so Lir addresses
   word field k at offset k words and byte field j at offset 5 words + j on
   every target. See the "Info tables" section of docs/lir.md. */
struct AihcInfo {
  uintptr_t identity;
  /* One byte per slot of the saturated object. A partial constructor indexes
     the same array as the finished one: the slots it has filled are a prefix
     of the slots the saturated constructor holds. */
  const uint8_t *field_is_pointer;
  /* The info table of the saturated constructor, for a partial constructor.
     Null for every other kind. */
  const AihcInfo *next;
  /* Backend-owned dynamic entry. Lir gives this word its own callable
     type. */
  AihcBackendEntry backend_entry;
  /* The static objects this object's code reaches, or null when it reaches
     none. The collector marks them whenever it traces the object. */
  const AihcSrt *srt;
  /* The slots an object of this info table holds. A partial constructor is
     the exception: every stage of one constructor shares a single info table,
     so the count of the slots filled so far lives in the object and this byte
     is zero. See aihc_value_count. The lowering rejects an object with more
     than 255 slots. */
  uint8_t field_count;
  /* The lowering rejects a function that takes more than 255 arguments. */
  uint8_t remaining_arity;
  /* Continuation closures have their parent in field zero. This kind is
     backend-independent so the runtime can unwind them uniformly. */
  AihcFrameKind frame_kind;
  AihcObjectKind object_kind;
};

struct AihcValue {
  /* Ordinarily an unmodified info-table pointer. During semispace collection,
     a forwarded from-space object temporarily holds its to-space address. */
  AihcSlot header;
  /* A partial constructor and a boxed array both spend field zero on a count
     and hold their payload from field one. Reach that payload through
     aihc_partial_fields and aihc_array_elements rather than by hand. */
  AihcSlot fields[];
};

struct AihcMachine {
  AihcSlot *globals;
  uint64_t global_count;
  AihcEntry exit_code;
  uint8_t *heap_next;
  uint8_t *heap_limit;
  uint8_t *heap_start;
  uint8_t *other_space;
  uint64_t semispace_bytes;
  uint64_t heap_max_bytes;
  uint64_t heap_allocated_bytes;
  uint8_t heap_limit_enabled;
  AihcValue *thread_done_continuation;
  AihcThread *current_thread;
  AihcThread *run_queue_head;
  AihcThread *run_queue_tail;
  AihcBlackhole *blackholes;
  AihcStableName *stable_names;
  uint64_t next_stable_name;
  /* The number of the next new thread. The counter starts at one, it gives the
     main thread the number one, and it does not give a number again. */
  uint64_t next_thread_id;
  AihcIoRequest *io_requests_head;
  AihcIoRequest *io_requests_tail;
  uint64_t io_request_count;
  const AihcIoBackend *io_backend;
  uint64_t allocation_count;
  AihcResume selected_resume;
  int64_t exit_status;
  uint64_t other_space_bytes;
  /* The runtime statistics. heap_allocated_bytes above counts every byte
     compiled code has taken from the managed heap. Compiled code reserves
     and bumps the heap pointer itself and reports nothing, so the runtime
     reads the total off the bump pointer instead of counting reservations:
     heap_alloc_base is where the mutator started filling the current space,
     and heap_next minus that base is what it has taken since. The collector
     adds that span before it flips, and aihc_heap_account adds it again
     whenever the total is read. heap_peak_bytes is the most the current space
     ever held: the live data after a collection plus the allocations since,
     sampled before each collection and when the statistics are reported. The
     collector counts its runs and their monotonic time. */
  uint8_t *heap_alloc_base;
  uint64_t heap_peak_bytes;
  uint64_t gc_count;
  uint64_t gc_time_ns;
  AihcTransactionTimer *transaction_timers;
};

_Static_assert(sizeof(AihcValue) == sizeof(AihcSlot),
               "AIHC objects must have a one-word base header");

/* Transfer a scheduler record to fixed-width Lir slots. */
void aihc_lir_take_resume(AihcResume *resume, uint64_t *slots);

static inline const AihcInfo *aihc_value_info_table(const AihcValue *value) {
  return (const AihcInfo *)(uintptr_t)value->header;
}

static inline AihcObjectKind aihc_value_kind(const AihcValue *value) {
  return aihc_value_info_table(value)->object_kind;
}

static inline uintptr_t aihc_value_info(const AihcValue *value) {
  return aihc_value_info_table(value)->identity;
}

static inline uint64_t aihc_value_arity(const AihcValue *value) {
  return aihc_value_info_table(value)->remaining_arity;
}

/* The slots a partial constructor has filled. It lives in the object because
   every stage of one constructor shares a single info table. */
static inline uint64_t aihc_partial_applied(const AihcValue *value) {
  return value->fields[0];
}

/* The slots a partial constructor still needs. The saturated info table names
   the full width. */
static inline uint64_t aihc_partial_total(const AihcValue *value) {
  return aihc_value_info_table(value)->next->field_count;
}

static inline AihcSlot *aihc_partial_fields(AihcValue *value) {
  return value->fields + 1;
}

static inline const AihcSlot *
aihc_partial_fields_const(const AihcValue *value) {
  return value->fields + 1;
}

static inline uint64_t aihc_value_count(const AihcValue *value) {
  if (aihc_value_kind(value) == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return aihc_partial_applied(value);
  }
  return aihc_value_info_table(value)->field_count;
}

static inline AihcSlot *aihc_value_fields(AihcValue *value) {
  return value->fields;
}

static inline const AihcSlot *aihc_value_fields_const(const AihcValue *value) {
  return value->fields;
}

/* Reserve heap for the objects that follow. Compiled code then takes each
   object by bumping the heap pointer itself, so the runtime exports no
   allocator. */
/* Collect for a caller that has already compared the bump pointer against the
   end of the space and found the words do not fit. Compiled code takes this
   entry point; aihc_ensure_heap is for a caller that has compared nothing.

   The table is the static reference table of the code that requests the
   collection, and the collector marks everything it names. A running function
   has no heap object to carry its table, so the safepoint of a compiled
   function passes the function's own table. Every other request passes NULL:
   a runtime helper is reached by a tail call, or by a call whose only
   continuation is a transfer to heap objects, so the static references of the
   function that called it are dead by the time it collects. Suspended code
   needs no entry either: its continuation closure carries the table in its
   info table. */
void aihc_heap_collect(AihcMachine *machine, uint64_t words,
                       uint64_t root_count, AihcSlot *roots,
                       const AihcSrt *srt);
void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots, const AihcSrt *srt);
AihcMachine *aihc_machine_new(uint64_t global_count);
uint64_t aihc_allocation_count(const AihcMachine *machine);
void aihc_reset_allocation_count(AihcMachine *machine);
/* The bytes compiled code has taken from the managed heap. Compiled code
   bumps the heap pointer itself, so both of these account for the span since
   the last account before they read or clear the total. */
uint64_t aihc_heap_allocated_bytes(AihcMachine *machine);
void aihc_reset_heap_allocated_bytes(AihcMachine *machine);
void aihc_no_match(void);
void aihc_unsupported_primitive(void);
/* The runtime removes RTS options before the Haskell machine starts. argv[0]
   stays because getProgName and withProgName use the same mutable vector.
   aihc_program_arguments_initialize flattens argv into one buffer of
   zero-terminated strings; the parser and the argument store behind the other
   four functions live in core-libs/aihc-rts/native/aihc_runtime_options.lir. */
void aihc_program_arguments_initialize(int argc, char *const argv[]);
int64_t aihc_runtime_arguments_initialize(const void *buffer, int64_t length);
/* The runtime settings that come from the environment. The host reads the
   process environment and flattens it like argv; the parser in
   aihc_runtime_options.lir keeps the AIHC_RTS_STATS value, and keeps the
   flattened buffer itself for lookupEnv and getEnvironment. The WASI P3 host
   reads no environment: see aihc_host_wasip3.c. */
void aihc_program_environment_initialize(void);
int64_t aihc_runtime_environment_initialize(const void *buffer, int64_t length);
/* Write the runtime statistics to the AIHC_RTS_STATS file, once, when the
   environment names one. The generated main calls this when the machine
   halts, and aihc_exit_process calls it before the process exits. */
void aihc_runtime_statistics_report(void);
int64_t aihc_program_environment_size(void);
int64_t aihc_program_environment_copy(void *buffer, int64_t capacity);
int64_t aihc_program_arguments_size(void);
int64_t aihc_program_arguments_copy(void *buffer, int64_t capacity);
int64_t aihc_program_arguments_replace(const void *buffer, int64_t length);
void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field);
/* Boxed arrays are contiguous managed objects. GrinEnsureHeap reserves their
   length-dependent storage before this initializer advances the heap.

   The boxed arrays, the mutable references, the stable names, and the byte
   arrays below are defined by the Lir runtime units in
   core-libs/aihc-rts/native. See the "Runtime units" section of docs/lir.md. */
AihcValue *aihc_array_new(AihcMachine *machine, int64_t count,
                          AihcSlot initial);
/* The caller reserves heap for delay creation, transaction creation, and
   transaction writes. These operations and their callees must not collect.
   Aihc.Grin.Primitive defines the maximum slot counts. */
AihcValue *aihc_tvar_delay(AihcMachine *machine, int64_t delay,
                           AihcSlot initial, AihcSlot final);
AihcSlot aihc_tvar_read(AihcMachine *machine, AihcValue *variable);
void *aihc_stm_wait_request(AihcMachine *machine);
int64_t aihc_stm_wait_result(AihcMachine *machine, void *request);
uint64_t aihc_stm_begin(AihcMachine *machine);
uint64_t aihc_stm_commit(AihcMachine *machine);
uint64_t aihc_stm_abort(AihcMachine *machine);
uint64_t aihc_stm_active(AihcMachine *machine);
uint64_t aihc_tvar_write(AihcMachine *machine, AihcValue *variable,
                         AihcSlot value);

AihcSlot aihc_array_copy(AihcValue *source, int64_t source_offset,
                         AihcValue *target, int64_t target_offset,
                         int64_t count);
AihcValue *aihc_mutvar_new(AihcMachine *machine, AihcSlot initial);
/* Stable-name handles are auxiliary, non-moving objects. The machine-owned
   table keeps their referents synchronized with a moving collector. */
void *aihc_stable_name_make(AihcMachine *machine, AihcValue *value);
/* State and allocation helpers used by native code. None of these functions
   transfers control to a generated user function. */
AihcValue *aihc_apply_slow(AihcMachine *machine, AihcValue *function,
                           uint64_t count, const AihcSlot *arguments,
                           AihcValue **continuation);
void aihc_begin_blackhole(AihcMachine *machine, AihcValue *value);
const AihcResume *aihc_block_on_blackhole(AihcMachine *machine,
                                          AihcValue *value,
                                          AihcValue *continuation);
void aihc_update(AihcValue *object, AihcValue *value);
void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value);
/* Delimited continuations. See "Delimited continuations" in
   docs/exceptions.md. A prompt tag is a heap node without fields, compared
   by address. control0# walks the continuation to the nearest prompt frame
   with its tag, records the frames above that prompt in a heap node, and
   resumes by applying the function to that node in the prompt's context.
   Applying the captured continuation copies the recorded frames onto the
   caller's continuation, so a capture can be resumed any number of times. */
/* aihc_prompt_tag.lir defines this operation. The caller reserves one heap
   slot. This operation must not collect. */
AihcValue *aihc_prompt_tag_new(AihcMachine *machine);
const AihcResume *aihc_control0(AihcMachine *machine, AihcValue *tag,
                                AihcValue *function, AihcValue *continuation);
const AihcResume *aihc_continuation_resume(AihcMachine *machine,
                                           AihcValue *captured,
                                           AihcValue *action,
                                           AihcValue *continuation);
const AihcResume *aihc_raise(AihcMachine *machine, AihcValue *exception,
                             AihcValue *continuation);
AihcSlot aihc_fork(AihcMachine *machine, AihcValue *action);
AihcThread *aihc_my_thread_id(AihcMachine *machine);
void *aihc_mvar_new(AihcMachine *machine);
const AihcResume *aihc_mvar_read(AihcMachine *machine, void *mvar,
                                 AihcValue *continuation);
const AihcResume *aihc_mvar_take(AihcMachine *machine, void *mvar,
                                 AihcValue *continuation);
uint64_t aihc_mvar_try_take(AihcMachine *machine, void *mvar);
uint64_t aihc_mvar_try_put(AihcMachine *machine, void *mvar, AihcSlot value);
const AihcResume *aihc_mvar_put(AihcMachine *machine, void *mvar,
                                AihcSlot value, AihcValue *continuation);
const AihcResume *aihc_yield(AihcMachine *machine, AihcValue *continuation);
const AihcResume *aihc_await_io(AihcMachine *machine, void *request,
                                AihcValue *continuation);
const AihcResume *aihc_thread_done(AihcMachine *machine);
void *aihc_io_stdin(void);
void *aihc_io_stdout(void);
void *aihc_io_stderr(void);
/* The access mode of a descriptor the program already has, numbered as an
   open request numbers its mode, or a negative aihc_io_error. */
int64_t aihc_io_descriptor_mode(int64_t descriptor);
/* An IO handle over a descriptor the program already has, or an open error.
   The host that has no descriptors to adopt reports one. */
void *aihc_io_adopt(int64_t descriptor, int64_t mode);
int64_t aihc_io_handle_descriptor(void *handle);
int64_t aihc_io_open_result_error(void *result);
int64_t aihc_io_close(void *handle);
int64_t aihc_errno_get(void);
int64_t aihc_errno_set(int64_t value);
int64_t aihc_memory_write_byte(void *buffer, int64_t offset, int64_t value);
int64_t aihc_memory_read_byte(const void *buffer, int64_t offset);
_Noreturn int64_t aihc_io_raise_error(int64_t error);
uint64_t aihc_byte_array_copy_to_addr(void *opaque_array, int64_t offset,
                                      void *destination, int64_t length);
uint64_t aihc_byte_array_compare(void *opaque_left, int64_t left_offset,
                                 void *opaque_right, int64_t right_offset,
                                 int64_t length);
/* Proof-of-concept byte arrays use stable auxiliary allocations and are not
   released. Freeze and thaw are representation-preserving compiler
   primitives. */
void *aihc_byte_array_new(int64_t size);
void *aihc_byte_array_new_pinned(int64_t size);
void *aihc_byte_array_new_aligned_pinned(int64_t size, int64_t alignment);
uint64_t aihc_byte_array_is_pinned(void *array);
void *aihc_byte_array_contents(void *array);
uint64_t aihc_byte_array_shrink(void *array, int64_t size);
void *aihc_byte_array_resize(void *array, int64_t size);
uint64_t aihc_byte_array_get_size(void *array);
uint64_t aihc_byte_array_copy_from_addr(void *source, void *array,
                                        int64_t offset, int64_t length);
uint64_t aihc_byte_array_fetch_add_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_sub_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_and_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_nand_word(void *array, int64_t index,
                                         uint64_t value);
uint64_t aihc_byte_array_fetch_or_word(void *array, int64_t index,
                                       uint64_t value);
uint64_t aihc_byte_array_fetch_xor_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_compare_and_swap_word(void *array, int64_t index,
                                               uint64_t expected,
                                               uint64_t replacement);
uint64_t aihc_byte_array_copy(void *source, int64_t source_offset,
                              void *destination, int64_t destination_offset,
                              int64_t length);
uint64_t aihc_byte_array_set(void *array, int64_t offset, int64_t length,
                             uint64_t value);
void *aihc_io_submit_read(void *handle, void *buffer, int64_t offset,
                          int64_t length);
void *aihc_io_submit_write(void *handle, void *buffer, int64_t offset,
                           int64_t length);
void *aihc_io_submit_open(void *path, int64_t length, int64_t mode);
int64_t aihc_io_take_result(void *request);
void *aihc_io_take_open_result(void *request);
void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation);
void aihc_set_exit_status(AihcMachine *machine, int64_t status);
int64_t aihc_get_exit_status(const AihcMachine *machine);
_Noreturn void aihc_exit_process(int64_t status);
AihcEntry aihc_halt(AihcMachine *machine);
/* The entry points of GHC's RTS API that package C code calls, declared for
   it by the Rts.h that Aihc.Hackage.Headers writes. Their names are GHC's,
   because the callers are not aihc's to rename. */
void startTimer(void);
void stopTimer(void);
void blockUserSignals(void);
void unblockUserSignals(void);
int rtsSupportsBoundThreads(void);
#endif
