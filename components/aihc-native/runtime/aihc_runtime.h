#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

#define AIHC_GC_CALLOC 0
#define AIHC_GC_SEMISPACE 1

#ifndef AIHC_GC
#define AIHC_GC AIHC_GC_CALLOC
#endif

#ifndef AIHC_SEMISPACE_BYTES
#define AIHC_SEMISPACE_BYTES (1024U * 1024U)
#endif

enum {
  AIHC_TAG_NODE = 0,
  AIHC_TAG_CLOSURE = 1,
  AIHC_TAG_THUNK = 2,
  AIHC_TAG_PARTIAL_CONSTRUCTOR = 3,
  AIHC_TAG_INDIRECTION = 4,
  AIHC_TAG_BLACKHOLE = 5,
  AIHC_TAG_THREAD = 6,
  AIHC_TAG_FORWARDING = 7,
};

#define AIHC_TAG_BITS 3
#define AIHC_TAG_MASK ((uintptr_t)((1U << AIHC_TAG_BITS) - 1U))

typedef struct AihcValue AihcValue;
typedef struct AihcMachine AihcMachine;
typedef struct AihcInfo AihcInfo;
typedef struct AihcThread AihcThread;
typedef struct AihcBlackhole AihcBlackhole;
typedef struct AihcIoHandle AihcIoHandle;
typedef struct AihcIoRequest AihcIoRequest;
typedef struct AihcIoBackend AihcIoBackend;
typedef uint64_t AihcSlot;
typedef void (*AihcEntry)(AihcSlot *arguments);

/* Only the portable-C trampoline expands a transfer into an argument array.
   Native backends enter generated code with their register convention. */
typedef struct {
  AihcEntry entry;
  AihcSlot *arguments;
} AihcPortableTransfer;

enum {
  AIHC_RESUME_NONE,
  AIHC_RESUME_APPLY,
  AIHC_RESUME_CONTINUE,
};
typedef uint64_t AihcResumeKind;

typedef struct {
  AihcResumeKind kind;
  AihcValue *function;
  AihcValue *continuation;
  AihcSlot value;
  uint64_t count;
} AihcResume;

struct AihcInfo {
  uintptr_t identity;
  AihcEntry entry;
  uint64_t field_count;
  uint64_t remaining_arity;
  const uint8_t *field_is_pointer;
  const AihcInfo *next;
  /* Backend-owned dynamic entry. Native entries unpack captured fields into
     registers; portable C leaves this null and uses entry plus its buffer. */
  AihcEntry enter_entry;
};

struct AihcValue {
  /* Low AIHC_TAG_BITS select the runtime state. Remaining bits point to the
     static info table, or to the copied object for a forwarding header. */
  AihcSlot header;
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
  AihcValue *thread_done_continuation;
  AihcThread *current_thread;
  AihcThread *run_queue_head;
  AihcThread *run_queue_tail;
  AihcBlackhole *blackholes;
  AihcIoRequest *io_requests_head;
  AihcIoRequest *io_requests_tail;
  uint64_t io_request_count;
  const AihcIoBackend *io_backend;
  uint64_t allocation_count;
  AihcSlot *locals;
  uint64_t locals_capacity;
  AihcResume selected_resume;
};

_Static_assert(sizeof(AihcValue) == sizeof(AihcSlot),
               "AIHC objects must have a one-word base header");
_Static_assert(_Alignof(AihcInfo) >= (1U << AIHC_TAG_BITS),
               "AIHC info tables must leave the tag bits clear");

static inline uint64_t aihc_value_tag(const AihcValue *value) {
  return value->header & AIHC_TAG_MASK;
}

static inline const AihcInfo *aihc_value_info_table(const AihcValue *value) {
  return (const AihcInfo *)(value->header & ~AIHC_TAG_MASK);
}

static inline uintptr_t aihc_value_info(const AihcValue *value) {
  return aihc_value_info_table(value)->identity;
}

static inline AihcEntry aihc_value_entry(const AihcValue *value) {
  return aihc_value_info_table(value)->entry;
}

static inline uint64_t aihc_value_arity(const AihcValue *value) {
  return aihc_value_info_table(value)->remaining_arity;
}

static inline uint64_t aihc_value_count(const AihcValue *value) {
  return aihc_value_info_table(value)->field_count;
}

static inline AihcSlot *aihc_value_fields(AihcValue *value) {
  return value->fields;
}

static inline const AihcSlot *aihc_value_fields_const(const AihcValue *value) {
  return value->fields;
}

AihcValue *aihc_make_node(AihcMachine *machine, uint64_t tag,
                          const AihcInfo *info);
AihcValue *aihc_make_node_unchecked(AihcMachine *machine, uint64_t tag,
                                    const AihcInfo *info);
void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots);
AihcMachine *aihc_machine_new(uint64_t global_count);
uint64_t aihc_allocation_count(const AihcMachine *machine);
void aihc_reset_allocation_count(AihcMachine *machine);
AihcSlot *aihc_alloc_locals(AihcMachine *machine, uint64_t count);
void aihc_no_match(void);
void aihc_unsupported_primitive(void);
void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field);
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
AihcSlot aihc_fork(AihcMachine *machine, AihcValue *action);
const AihcResume *aihc_yield(AihcMachine *machine, AihcValue *continuation);
const AihcResume *aihc_await_io(AihcMachine *machine, void *request,
                                AihcValue *continuation);
const AihcResume *aihc_thread_done(AihcMachine *machine);
void *aihc_io_stdin(void);
void *aihc_io_stdout(void);
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
uint64_t aihc_byte_array_index_word(void *array, int64_t index);
uint64_t aihc_byte_array_read_word(void *array, int64_t index);
uint64_t aihc_byte_array_write_word(void *array, int64_t index, uint64_t value);
uint64_t aihc_byte_array_copy(void *source, int64_t source_offset,
                              void *destination, int64_t destination_offset,
                              int64_t length);
uint64_t aihc_word_clz(uint64_t value);
uint64_t aihc_word_ctz(uint64_t value);
uint64_t aihc_word_popcount(uint64_t value);
void *aihc_io_submit_read(void *handle, void *buffer, int32_t offset,
                          int32_t length);
void *aihc_io_submit_write(void *handle, void *buffer, int32_t offset,
                           int32_t length);
int32_t aihc_io_take_result(void *request);
void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation);
AihcEntry aihc_halt(AihcMachine *machine);

/* Portable-C control operations. The generated backend owns and supplies the
   reusable buffer; the machine and native backends never contain one. */
AihcPortableTransfer
aihc_portable_apply_cps(AihcMachine *machine, AihcSlot *buffer,
                        AihcValue *function, uint64_t count,
                        const AihcSlot *arguments, AihcValue *continuation);
AihcPortableTransfer aihc_portable_eval_cps(AihcMachine *machine,
                                            AihcSlot *buffer, AihcValue *value,
                                            uint64_t result_is_lifted,
                                            AihcValue *continuation,
                                            AihcValue *update_continuation);
AihcPortableTransfer aihc_portable_continue_values(AihcMachine *machine,
                                                   AihcSlot *buffer,
                                                   AihcValue *continuation,
                                                   uint64_t count,
                                                   const AihcSlot *values);
AihcPortableTransfer aihc_portable_fork_cps(AihcMachine *machine,
                                            AihcSlot *buffer, AihcValue *action,
                                            AihcValue *continuation);
AihcPortableTransfer aihc_portable_yield_cps(AihcMachine *machine,
                                             AihcSlot *buffer,
                                             AihcValue *continuation);
AihcPortableTransfer aihc_portable_await_io_cps(AihcMachine *machine,
                                                AihcSlot *buffer, void *request,
                                                AihcValue *continuation);
AihcPortableTransfer aihc_portable_thread_done(AihcMachine *machine,
                                               AihcSlot *buffer);
AihcPortableTransfer
aihc_portable_start(AihcMachine *machine, AihcSlot *buffer, AihcValue *root,
                    AihcValue *continuation, AihcValue *update_continuation,
                    AihcValue *thread_done_continuation, AihcEntry exit_code);

typedef enum {
  AIHC_SNAPSHOT_POINTER,
  AIHC_SNAPSHOT_INT,
  AIHC_SNAPSHOT_INT8,
  AIHC_SNAPSHOT_INT16,
  AIHC_SNAPSHOT_INT32,
  AIHC_SNAPSHOT_INT64,
  AIHC_SNAPSHOT_WORD,
  AIHC_SNAPSHOT_WORD8,
  AIHC_SNAPSHOT_WORD16,
  AIHC_SNAPSHOT_WORD32,
  AIHC_SNAPSHOT_WORD64,
  AIHC_SNAPSHOT_ADDR,
  AIHC_SNAPSHOT_FLOAT,
  AIHC_SNAPSHOT_DOUBLE,
} AihcSnapshotRep;

typedef struct {
  uintptr_t info;
  const char *name;
  uint64_t field_count;
  const AihcSnapshotRep *field_reps;
} AihcSnapshotConstructor;

typedef struct {
  uintptr_t info;
  const char *name;
  uint64_t parameter_count;
  const AihcSnapshotRep *parameter_reps;
} AihcSnapshotFunction;

void aihc_snapshot_dump(uint64_t result_count, const AihcSlot *results,
                        const AihcSnapshotRep *result_reps,
                        uint64_t allocation_count, uint64_t constructor_count,
                        const AihcSnapshotConstructor *constructors,
                        uint64_t function_count,
                        const AihcSnapshotFunction *functions);

#endif
