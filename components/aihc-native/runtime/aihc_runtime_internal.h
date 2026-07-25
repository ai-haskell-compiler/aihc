#ifndef AIHC_RUNTIME_INTERNAL_H
#define AIHC_RUNTIME_INTERNAL_H

#include "aihc_runtime.h"

#include <stddef.h>

typedef struct AihcBlackholeWaiter AihcBlackholeWaiter;
typedef struct AihcMVarWaiter AihcMVarWaiter;

typedef enum {
  AIHC_IO_READ,
  AIHC_IO_WRITE,
} AihcIoKind;

typedef enum {
  AIHC_IO_READABLE = 1U << 0,
  AIHC_IO_WRITABLE = 1U << 1,
} AihcIoCapability;

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
};

struct AihcBlackholeWaiter {
  AihcThread *thread;
  AihcValue *continuation;
  AihcBlackholeWaiter *next;
};

struct AihcBlackhole {
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

struct AihcIoHandle {
  uintptr_t backend_token;
  uint32_t capabilities;
};

typedef struct {
  size_t size;
  uint8_t *contents;
  uint8_t pinned;
  size_t alignment;
} AihcByteArray;

struct AihcIoRequest {
  AihcIoKind kind;
  AihcIoState state;
  AihcIoHandle *handle;
  uint8_t *buffer;
  size_t offset;
  size_t length;
  AihcThread *thread;
  AihcValue *continuation;
  int32_t result;
  AihcIoRequest *next;
};

struct AihcIoBackend {
  int (*prepare)(AihcIoRequest *request);
  int (*try_request)(AihcIoRequest *request, int32_t *result);
  AihcIoPollOutcome (*poll)(AihcMachine *machine, int may_block);
};

typedef AihcSlot (*AihcRootVisitor)(AihcSlot root, void *context);

_Noreturn void aihc_fail(const char *message);
void aihc_record_allocation(AihcMachine *machine);
void *aihc_allocate_zeroed(size_t bytes);
void *aihc_allocate_auxiliary(AihcMachine *machine, size_t bytes);
AihcSlot *aihc_reserve_slots(AihcMachine *machine, AihcSlot **slots,
                             uint64_t *capacity, uint64_t count);
uint64_t aihc_object_words(uint64_t tag, const AihcInfo *info);
const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                           uint64_t supplied_count);
int32_t aihc_io_error(int error);
void aihc_resume_io_request(AihcMachine *machine, AihcIoRequest *request,
                            int32_t result);
const AihcResume *aihc_complete_io(AihcMachine *machine, int32_t result);
void aihc_visit_roots(AihcMachine *machine, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context);

void aihc_gc_init(AihcMachine *machine);
void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots);
AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words);

_Noreturn void aihc_host_fail(const char *message);
const AihcIoBackend *aihc_host_io_backend(void);

#endif
