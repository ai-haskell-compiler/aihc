#ifndef AIHC_RUNTIME_INTERNAL_H
#define AIHC_RUNTIME_INTERNAL_H

#include "aihc_runtime.h"

#include <stddef.h>

typedef struct AihcBlackholeWaiter AihcBlackholeWaiter;
typedef struct AihcMVarWaiter AihcMVarWaiter;

typedef enum {
  AIHC_IO_READ,
  AIHC_IO_WRITE,
  AIHC_IO_OPEN,
  AIHC_IO_TIMER,
} AihcIoKind;

typedef enum {
  AIHC_IO_READABLE = 1U << 0,
  AIHC_IO_WRITABLE = 1U << 1,
} AihcIoCapability;

typedef enum {
  AIHC_IO_ERROR_IO = 5,
  AIHC_IO_ERROR_BAD_DESCRIPTOR = 9,
  AIHC_IO_ERROR_INVALID_ARGUMENT = 22,
  AIHC_IO_ERROR_NOT_SUPPORTED = 38,
} AihcIoError;

typedef enum {
  AIHC_IO_SUBMITTED,
  AIHC_IO_PENDING,
  AIHC_IO_COMPLETED,
  AIHC_IO_CONSUMED,
} AihcIoState;

typedef enum {
  AIHC_IO_POLL_PROGRESS,
  AIHC_IO_POLL_SUSPENDED,
} AihcIoPollOutcome;

struct AihcThread {
  AihcSlot header;
  AihcResumeKind resume_kind;
  AihcValue *resume_function;
  AihcValue *resume_continuation;
  AihcSlot resume_value;
  uint64_t resume_count;
  AihcThread *next;
  AihcTransaction *transaction;
};

typedef struct AihcTransactionWrite AihcTransactionWrite;

struct AihcTransactionWrite {
  AihcValue *variable;
  AihcSlot previous;
  AihcTransactionWrite *next;
};

struct AihcTransactionTimer {
  AihcValue *variable;
  AihcSlot final;
  uint64_t deadline;
  AihcTransactionTimer *next;
};

struct AihcTransaction {
  AihcTransactionWrite *writes;
  AihcTransaction *parent;
};

struct AihcBlackholeWaiter {
  AihcThread *thread;
  AihcValue *continuation;
  AihcBlackholeWaiter *next;
};

struct AihcBlackhole {
  AihcInfo info;
  const AihcInfo *original_info;
  AihcValue *object;
  AihcThread *owner;
  AihcBlackholeWaiter *waiters_head;
  AihcBlackholeWaiter *waiters_tail;
  AihcBlackhole *next;
};

struct AihcMVarWaiter {
  AihcThread *thread;
  AihcValue *continuation;
  AihcSlot value;
  AihcMVarWaiter *next;
};

struct AihcMVar {
  AihcSlot header;
  uint8_t full;
  AihcSlot value;
  AihcMVarWaiter *readers_head;
  AihcMVarWaiter *readers_tail;
  AihcMVarWaiter *takers_head;
  AihcMVarWaiter *takers_tail;
  AihcMVarWaiter *putters_head;
  AihcMVarWaiter *putters_tail;
  AihcMVar *next;
};

/* aihc_stable_name.lir allocates and links these, so every field starts one
   eight-byte slot after the last on every target. The static assertions in
   aihc_runtime.c hold the two descriptions together. */
struct AihcStableName {
  AihcSlot header;
  AihcValue *value;
  uint64_t hash;
  AihcStableName *next;
};

struct AihcIoHandle {
  uintptr_t backend_token;
  uint64_t position;
  uint32_t capabilities;
  uint8_t append;
  uint8_t closed;
};

struct AihcIoRequest {
  AihcIoKind kind;
  AihcIoState state;
  AihcIoHandle *handle;
  uint8_t *buffer;
  size_t offset;
  size_t length;
  int64_t mode;
  uint64_t deadline;
  AihcThread *thread;
  AihcValue *continuation;
  int64_t result;
  AihcIoRequest *next;
};

struct AihcIoBackend {
  int (*prepare)(AihcIoRequest *request);
  int (*try_request)(AihcIoRequest *request, int64_t *result);
  int64_t (*finish_request)(AihcIoRequest *request, int64_t result);
  AihcIoPollOutcome (*poll)(AihcMachine *machine, int may_block);
};

typedef AihcSlot (*AihcRootVisitor)(AihcSlot root, void *context);

_Noreturn void aihc_fail(const char *message);
void aihc_record_allocation(AihcMachine *machine);
void *aihc_allocate_zeroed(uint64_t bytes);
void *aihc_allocate_auxiliary(AihcMachine *machine, uint64_t bytes);
void aihc_memory_copy(void *destination, const void *source, uint64_t length);
void aihc_memory_move(void *destination, const void *source, uint64_t length);
void aihc_memory_set(void *destination, uint64_t byte, uint64_t length);
void aihc_memory_free(void *pointer);
/* The machine fields that aihc_stable_name.lir needs. Their offsets in
   AihcMachine follow the target word size, so a Lir unit reaches them through
   these accessors instead of by offset. */
AihcStableName **aihc_stable_names(AihcMachine *machine);
uint64_t aihc_stable_name_take_hash(AihcMachine *machine);
/* The RTS options of the process, parsed by aihc_runtime_options.lir from
   the arguments the host passed. */
uint64_t aihc_rts_heap_max_bytes(void);
uint64_t aihc_rts_heap_limit_enabled(void);
/* The zero-terminated path of the statistics file, or null when the
   environment names none. */
const char *aihc_rts_stats_path(void);
/* Flatten one null-terminated list of NAME=VALUE strings for the parser. */
void aihc_environment_initialize(char *const envp[]);
uint64_t aihc_object_words(const AihcInfo *info);
uint64_t aihc_value_words(const AihcValue *value);
AihcSlot *aihc_array_elements(AihcValue *array);
uint64_t aihc_array_length(const AihcValue *array);
const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                           uint64_t supplied_count);
const AihcInfo *aihc_applied_constructor_info(const AihcInfo *info,
                                              uint64_t applied);
int64_t aihc_io_error(int error);
void *aihc_io_open_error(int error);
void aihc_resume_io_request(AihcMachine *machine, AihcIoRequest *request,
                            int64_t result);
const AihcResume *aihc_complete_io(AihcMachine *machine, int64_t result);
void aihc_visit_roots(AihcMachine *machine, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context);
/* The header every runtime object outside the managed heap carries. */
extern const AihcInfo aihc_runtime_object_info;

void aihc_gc_init(AihcMachine *machine);
void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots);
AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words);
/* Raise heap_peak_bytes to what the current space holds now. */
void aihc_gc_record_peak(AihcMachine *machine);

_Noreturn void aihc_host_fail(const char *message);
const AihcIoBackend *aihc_host_io_backend(void);
/* A monotonic clock in nanoseconds, or zero on a host without one. */
uint64_t aihc_host_monotonic_ns(void);
void aihc_host_sleep_ns(uint64_t duration);
/* Replace the file at path with the given bytes. The result is zero, or an
   errno value when the host cannot write the file. */
int aihc_host_write_file(const char *path, const void *bytes, size_t length);

#endif
