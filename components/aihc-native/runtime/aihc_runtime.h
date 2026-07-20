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
typedef void (*AihcEntry)(void);

struct AihcInfo {
  uintptr_t identity;
  AihcEntry entry;
  uint64_t field_count;
  uint64_t remaining_arity;
  const uint8_t *field_is_pointer;
  const AihcInfo *next;
};

struct AihcValue {
  /* Low AIHC_TAG_BITS select the runtime state. Remaining bits point to the
     static info table, or to the copied object for a forwarding header. */
  AihcSlot header;
  AihcSlot fields[];
};

struct AihcMachine {
  AihcSlot *args;
  AihcSlot *globals;
  uint64_t global_count;
  AihcEntry exit_code;
  uint8_t *heap_next;
  uint8_t *heap_limit;
  uint8_t *heap_start;
  uint8_t *other_space;
  uint64_t semispace_bytes;
  const AihcInfo *args_info;
  uint64_t args_trailing_pointers;
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
AihcEntry aihc_apply_cps(AihcMachine *machine, AihcValue *function,
                         uint64_t count, const AihcSlot *arguments,
                         AihcValue *continuation);
AihcEntry aihc_eval_cps(AihcMachine *machine, AihcValue *value,
                        uint64_t result_is_lifted, AihcValue *continuation,
                        AihcValue *update_continuation);
AihcEntry aihc_continue_values(AihcMachine *machine, AihcValue *continuation,
                               uint64_t count, const AihcSlot *values);
void aihc_update(AihcValue *object, AihcValue *value);
void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value);
AihcEntry aihc_fork_cps(AihcMachine *machine, AihcValue *action,
                        AihcValue *continuation);
AihcEntry aihc_yield_cps(AihcMachine *machine, AihcValue *continuation);
AihcEntry aihc_await_io_cps(AihcMachine *machine, void *request,
                            AihcValue *continuation);
AihcEntry aihc_thread_done(AihcMachine *machine);
void *aihc_io_stdin(void);
void *aihc_io_stdout(void);
void *aihc_io_submit_read(void *handle);
void *aihc_io_submit_write(void *handle, int32_t byte);
int32_t aihc_io_take_result(void *request);
void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation);
AihcEntry aihc_halt(AihcMachine *machine);
AihcEntry aihc_start(AihcMachine *machine, AihcValue *root,
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
