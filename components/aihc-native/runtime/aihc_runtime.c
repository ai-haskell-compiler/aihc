#include "aihc_runtime.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct AihcBlackholeWaiter AihcBlackholeWaiter;

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
  void (*poll)(AihcMachine *machine, int may_block);
};

static AihcIoHandle aihc_standard_input = {(uintptr_t)STDIN_FILENO,
                                           AIHC_IO_READABLE};
static AihcIoHandle aihc_standard_output = {(uintptr_t)STDOUT_FILENO,
                                            AIHC_IO_WRITABLE};

#if UINTPTR_MAX == UINT64_MAX
_Static_assert(offsetof(AihcMachine, globals) == 0, "machine globals ABI");
_Static_assert(offsetof(AihcMachine, heap_next) == 24, "machine heap-next ABI");
_Static_assert(offsetof(AihcMachine, heap_limit) == 32,
               "machine heap-limit ABI");
_Static_assert(offsetof(AihcInfo, enter_entry) == 48,
               "info-table enter-entry ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 16,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 24, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 32, "resume count ABI");
#endif

static _Noreturn void aihc_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
}

static const AihcResume *aihc_schedule(AihcMachine *machine);
static const AihcIoBackend *aihc_default_io_backend(void);

void aihc_unsupported_primitive(void) {
  aihc_fail("primitive is not implemented by the native runtime");
}

static void aihc_record_allocation(AihcMachine *machine) {
  if (machine->allocation_count == UINT64_MAX) {
    aihc_fail("allocation counter overflow");
  }
  ++machine->allocation_count;
}

static void *aihc_allocate_zeroed(size_t bytes) {
  void *pointer = calloc(1, bytes);
  if (pointer == NULL) {
    aihc_fail("out of memory");
  }
  return pointer;
}

static void *aihc_allocate_auxiliary(AihcMachine *machine, size_t bytes) {
  void *pointer = aihc_allocate_zeroed(bytes);
  aihc_record_allocation(machine);
  return pointer;
}

static AihcSlot *aihc_reserve_slots(AihcMachine *machine, AihcSlot **slots,
                                    uint64_t *capacity, uint64_t count) {
  uint64_t required = count == 0 ? 1 : count;
  if (required <= *capacity) {
    return *slots;
  }
  if (required > SIZE_MAX / sizeof(**slots)) {
    aihc_fail("runtime slot area is too large");
  }
  size_t old_bytes = (size_t)*capacity * sizeof(**slots);
  size_t new_bytes = (size_t)required * sizeof(**slots);
  AihcSlot *resized = realloc(*slots, new_bytes);
  if (resized == NULL) {
    aihc_fail("out of memory");
  }
  memset((uint8_t *)resized + old_bytes, 0, new_bytes - old_bytes);
  *slots = resized;
  *capacity = required;
  aihc_record_allocation(machine);
  return resized;
}

static AihcSlot aihc_make_header(uint64_t tag, const AihcInfo *info) {
  uintptr_t address = (uintptr_t)info;
  if (info == NULL || (address & AIHC_TAG_MASK) != 0) {
    aihc_fail("info table is null or insufficiently aligned");
  }
  switch (tag) {
  case AIHC_TAG_CLOSURE:
  case AIHC_TAG_THUNK:
  case AIHC_TAG_NODE:
  case AIHC_TAG_PARTIAL_CONSTRUCTOR:
    return address | tag;
  default:
    aihc_fail("attempted to allocate an invalid object tag");
  }
}

static uint64_t aihc_object_words(uint64_t tag, const AihcInfo *info) {
  uint64_t field_words = info->field_count;
  if (field_words == 0 && (tag == AIHC_TAG_THUNK || tag == AIHC_TAG_BLACKHOLE ||
                           tag == AIHC_TAG_INDIRECTION)) {
    field_words = 1;
  }
  return 1 + field_words;
}

#if AIHC_GC == AIHC_GC_SEMISPACE
static int aihc_in_space(const uint8_t *start, uint64_t bytes,
                         const AihcValue *value) {
  uintptr_t address = (uintptr_t)value;
  uintptr_t first = (uintptr_t)start;
  return address >= first && address - first < bytes;
}

static AihcValue *aihc_forward(AihcMachine *machine, uint8_t *from_start,
                               AihcValue *value) {
  if (value == NULL ||
      !aihc_in_space(from_start, machine->semispace_bytes, value)) {
    return value;
  }
  if (aihc_value_tag(value) == AIHC_TAG_FORWARDING) {
    return (AihcValue *)(value->header & ~AIHC_TAG_MASK);
  }

  uint64_t tag = aihc_value_tag(value);
  const AihcInfo *info = aihc_value_info_table(value);
  uint64_t words = aihc_object_words(tag, info);
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_fail("live data exceeds semispace");
  }
  AihcValue *copy = (AihcValue *)machine->heap_next;
  memcpy(copy, value, bytes);
  machine->heap_next += bytes;
  value->header = (uintptr_t)copy | AIHC_TAG_FORWARDING;
  return copy;
}

static void aihc_forward_slot(AihcMachine *machine, uint8_t *from_start,
                              AihcSlot *slot) {
  *slot = (AihcSlot)aihc_forward(machine, from_start, (AihcValue *)*slot);
}

static void aihc_forward_value(AihcMachine *machine, uint8_t *from_start,
                               AihcValue **value) {
  *value = aihc_forward(machine, from_start, *value);
}

static void aihc_forward_thread(AihcMachine *machine, uint8_t *from_start,
                                AihcThread *thread) {
  if (thread != NULL) {
    aihc_forward_value(machine, from_start, &thread->resume_function);
    aihc_forward_value(machine, from_start, &thread->resume_continuation);
    if (thread->resume_kind == AIHC_RESUME_CONTINUE &&
        thread->resume_count == 1) {
      aihc_forward_slot(machine, from_start, &thread->resume_value);
    }
  }
}

static void aihc_collect(AihcMachine *machine, uint64_t required_words,
                         uint64_t root_count, AihcSlot *roots) {
  uint8_t *from_start = machine->heap_start;
  uint8_t *to_start = machine->other_space;
  machine->heap_start = to_start;
  machine->heap_next = to_start;
  machine->heap_limit = to_start + machine->semispace_bytes;

  for (uint64_t index = 0; index < machine->global_count; ++index) {
    aihc_forward_slot(machine, from_start, &machine->globals[index]);
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    aihc_forward_slot(machine, from_start, &roots[index]);
  }
  aihc_forward_value(machine, from_start, &machine->thread_done_continuation);
  aihc_forward_value(machine, from_start, &machine->selected_resume.function);
  aihc_forward_value(machine, from_start,
                     &machine->selected_resume.continuation);
  if (machine->selected_resume.kind == AIHC_RESUME_CONTINUE &&
      machine->selected_resume.count == 1) {
    aihc_forward_slot(machine, from_start, &machine->selected_resume.value);
  }
  aihc_forward_thread(machine, from_start, machine->current_thread);
  for (AihcThread *thread = machine->run_queue_head; thread != NULL;
       thread = thread->next) {
    aihc_forward_thread(machine, from_start, thread);
  }
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    aihc_forward_value(machine, from_start, &blackhole->object);
    for (AihcBlackholeWaiter *waiter = blackhole->waiters_head; waiter != NULL;
         waiter = waiter->next) {
      aihc_forward_value(machine, from_start, &waiter->continuation);
      aihc_forward_thread(machine, from_start, waiter->thread);
    }
  }
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    aihc_forward_value(machine, from_start, &request->continuation);
    aihc_forward_thread(machine, from_start, request->thread);
  }

  uint8_t *scan = to_start;
  while (scan < machine->heap_next) {
    AihcValue *object = (AihcValue *)scan;
    uint64_t tag = aihc_value_tag(object);
    const AihcInfo *info = aihc_value_info_table(object);
    uint64_t count = info->field_count;
    if (tag == AIHC_TAG_INDIRECTION) {
      aihc_forward_slot(machine, from_start, &object->fields[0]);
    } else if (tag == AIHC_TAG_NODE || tag == AIHC_TAG_CLOSURE ||
               tag == AIHC_TAG_THUNK || tag == AIHC_TAG_PARTIAL_CONSTRUCTOR ||
               tag == AIHC_TAG_BLACKHOLE) {
      for (uint64_t index = 0; index < count; ++index) {
        if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
          aihc_forward_slot(machine, from_start, &object->fields[index]);
        }
      }
    } else {
      aihc_fail("collector encountered an invalid object tag");
    }
    scan += sizeof(AihcSlot) * aihc_object_words(tag, info);
  }

  machine->other_space = from_start;
  size_t required_bytes = sizeof(AihcSlot) * required_words;
  if (machine->heap_next + required_bytes > machine->heap_limit) {
    aihc_fail("insufficient heap after collection");
  }
}
#endif

void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots) {
#if AIHC_GC == AIHC_GC_CALLOC
  (void)machine;
  (void)words;
  (void)root_count;
  (void)roots;
#elif AIHC_GC == AIHC_GC_SEMISPACE
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_collect(machine, words, root_count, roots);
  }
#else
#error "unknown AIHC_GC selection"
#endif
}

AihcValue *aihc_make_node_unchecked(AihcMachine *machine, uint64_t tag,
                                    const AihcInfo *info) {
  uint64_t words = aihc_object_words(tag, info);
  AihcValue *value;
#if AIHC_GC == AIHC_GC_CALLOC
  value = aihc_allocate_zeroed(sizeof(AihcSlot) * words);
#elif AIHC_GC == AIHC_GC_SEMISPACE
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  value = (AihcValue *)machine->heap_next;
  machine->heap_next += bytes;
  memset(value, 0, bytes);
#else
#error "unknown AIHC_GC selection"
#endif
  aihc_record_allocation(machine);
  value->header = aihc_make_header(tag, info);
  return value;
}

AihcValue *aihc_make_node(AihcMachine *machine, uint64_t tag,
                          const AihcInfo *info) {
  uint64_t words = aihc_object_words(tag, info);
  aihc_ensure_heap(machine, words, 0, NULL);
  return aihc_make_node_unchecked(machine, tag, info);
}

uint64_t aihc_allocation_count(const AihcMachine *machine) {
  return machine->allocation_count;
}

void aihc_reset_allocation_count(AihcMachine *machine) {
  machine->allocation_count = 0;
}

static const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                                  uint64_t supplied_count) {
  const AihcInfo *next = info->next;
  if (info->remaining_arity == 0 || next == NULL ||
      next->remaining_arity + 1 != info->remaining_arity ||
      next->field_count < info->field_count ||
      next->field_count - info->field_count != supplied_count) {
    aihc_fail("application does not match static info-table transition");
  }
  return next;
}

static AihcValue *aihc_copy_with_fields(AihcMachine *machine,
                                        AihcValue **value_pointer,
                                        uint64_t result_tag, uint64_t count,
                                        const AihcSlot *fields,
                                        AihcValue **continuation_pointer) {
  AihcValue *value = *value_pointer;
  const AihcInfo *info = aihc_value_info_table(value);
  const AihcInfo *next_info = aihc_next_application_info(info, count);
  uint64_t original_count = info->field_count;

  uint64_t pointer_count = 0;
  for (uint64_t index = 0; index < count; ++index) {
    if (next_info->field_is_pointer[original_count + index]) {
      ++pointer_count;
    }
  }
  AihcSlot roots[2 + pointer_count];
  roots[0] = (AihcSlot)value;
  roots[1] = (AihcSlot)*continuation_pointer;
  uint64_t root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    if (next_info->field_is_pointer[original_count + index]) {
      roots[root_index++] = fields[index];
    }
  }

  aihc_ensure_heap(machine, aihc_object_words(result_tag, next_info),
                   2 + pointer_count, roots);
  value = (AihcValue *)roots[0];
  *value_pointer = value;
  *continuation_pointer = (AihcValue *)roots[1];

  AihcValue *copy = aihc_make_node_unchecked(machine, result_tag, next_info);
  AihcSlot *original_fields = aihc_value_fields(value);
  AihcSlot *copy_fields = aihc_value_fields(copy);
  for (uint64_t index = 0; index < original_count; ++index) {
    copy_fields[index] = original_fields[index];
  }
  root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    copy_fields[original_count + index] =
        next_info->field_is_pointer[original_count + index]
            ? roots[root_index++]
            : fields[index];
  }
  return copy;
}

static AihcSlot *aihc_portable_arguments(AihcMachine *machine,
                                         AihcValue *function, uint64_t count,
                                         const AihcSlot *values,
                                         AihcValue *continuation) {
  uint64_t field_count = aihc_value_info_table(function)->field_count;
  size_t maximum_count = SIZE_MAX / sizeof(AihcSlot);
  if (field_count > maximum_count || count > maximum_count - field_count) {
    aihc_fail("runtime argument vector is too large");
  }
  size_t total = (size_t)field_count + (size_t)count;
  if (continuation != NULL) {
    if (total == maximum_count) {
      aihc_fail("runtime argument vector is too large");
    }
    ++total;
  }
  AihcSlot *buffer =
      aihc_reserve_slots(machine, &machine->portable_arguments,
                         &machine->portable_arguments_capacity, total);
  if (count != 0) {
    memmove(buffer + field_count, values, sizeof(*values) * (size_t)count);
  }
  const AihcSlot *function_fields = aihc_value_fields_const(function);
  if (field_count != 0) {
    memcpy(buffer, function_fields,
           sizeof(*function_fields) * (size_t)field_count);
  }
  if (continuation != NULL) {
    buffer[(size_t)field_count + (size_t)count] = (AihcSlot)continuation;
  }
  return buffer;
}

static AihcPortableTransfer aihc_portable_transfer(AihcEntry entry,
                                                   AihcSlot *arguments) {
  return (AihcPortableTransfer){entry, arguments};
}

AihcPortableTransfer aihc_portable_call(AihcMachine *machine, AihcEntry entry,
                                        uint64_t count,
                                        const AihcSlot *arguments) {
  AihcSlot *buffer =
      aihc_reserve_slots(machine, &machine->portable_arguments,
                         &machine->portable_arguments_capacity, count);
  if (count != 0) {
    memmove(buffer, arguments, sizeof(*arguments) * (size_t)count);
  }
  return aihc_portable_transfer(entry, buffer);
}

static AihcThread *aihc_thread_new(AihcMachine *machine) {
  AihcThread *thread = aihc_allocate_auxiliary(machine, sizeof(*thread));
  thread->header = AIHC_TAG_THREAD;
  return thread;
}

static void aihc_enqueue_thread(AihcMachine *machine, AihcThread *thread) {
  if (thread->next != NULL) {
    aihc_fail("attempted to enqueue an already queued thread");
  }
  if (machine->run_queue_tail == NULL) {
    machine->run_queue_head = thread;
  } else {
    machine->run_queue_tail->next = thread;
  }
  machine->run_queue_tail = thread;
}

static AihcThread *aihc_dequeue_thread(AihcMachine *machine) {
  AihcThread *thread = machine->run_queue_head;
  if (thread == NULL) {
    aihc_fail("no runnable threads");
  }
  machine->run_queue_head = thread->next;
  if (machine->run_queue_head == NULL) {
    machine->run_queue_tail = NULL;
  }
  thread->next = NULL;
  return thread;
}

static AihcBlackhole *aihc_find_blackhole(AihcMachine *machine,
                                          AihcValue *object) {
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    if (blackhole->object == object) {
      return blackhole;
    }
  }
  AihcBlackhole *blackhole =
      aihc_allocate_auxiliary(machine, sizeof(*blackhole));
  blackhole->object = object;
  blackhole->owner = machine->current_thread;
  blackhole->next = machine->blackholes;
  machine->blackholes = blackhole;
  return blackhole;
}

static void aihc_add_blackhole_waiter(AihcMachine *machine, AihcValue *object,
                                      AihcValue *continuation) {
  AihcBlackhole *blackhole = aihc_find_blackhole(machine, object);
  if (blackhole->owner == machine->current_thread) {
    aihc_fail("blackholed thunk re-entered");
  }
  AihcBlackholeWaiter *waiter =
      aihc_allocate_auxiliary(machine, sizeof(*waiter));
  waiter->thread = machine->current_thread;
  waiter->continuation = continuation;
  if (blackhole->waiters_tail == NULL) {
    blackhole->waiters_head = waiter;
  } else {
    blackhole->waiters_tail->next = waiter;
  }
  blackhole->waiters_tail = waiter;
}

static AihcBlackhole *aihc_remove_blackhole(AihcMachine *machine,
                                            AihcValue *object) {
  AihcBlackhole **link = &machine->blackholes;
  while (*link != NULL && (*link)->object != object) {
    link = &(*link)->next;
  }
  if (*link == NULL) {
    return NULL;
  }
  AihcBlackhole *blackhole = *link;
  *link = blackhole->next;
  return blackhole;
}

void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field) {
  aihc_value_fields(value)[index] = field;
}

AihcMachine *aihc_machine_new(uint64_t global_count) {
  AihcMachine *machine = aihc_allocate_zeroed(sizeof(*machine));
  machine->allocation_count = 1;
  machine->global_count = global_count;
  machine->globals = aihc_allocate_auxiliary(
      machine,
      sizeof(*machine->globals) * (global_count == 0 ? 1 : global_count));
#if AIHC_GC == AIHC_GC_CALLOC
  machine->heap_next = NULL;
  machine->heap_limit = (uint8_t *)UINTPTR_MAX;
#elif AIHC_GC == AIHC_GC_SEMISPACE
  machine->semispace_bytes = AIHC_SEMISPACE_BYTES;
  machine->heap_start =
      aihc_allocate_auxiliary(machine, machine->semispace_bytes);
  machine->other_space =
      aihc_allocate_auxiliary(machine, machine->semispace_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->semispace_bytes;
#endif
  machine->current_thread = aihc_thread_new(machine);
  machine->io_backend = aihc_default_io_backend();
  return machine;
}

AihcSlot *aihc_alloc_locals(AihcMachine *machine, uint64_t count) {
  return aihc_reserve_slots(machine, &machine->locals,
                            &machine->locals_capacity, count);
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

static AihcPortableTransfer
aihc_portable_continue_values_now(AihcMachine *machine, AihcValue *continuation,
                                  uint64_t count, const AihcSlot *values) {
  if (continuation == NULL ||
      aihc_value_tag(continuation) != AIHC_TAG_CLOSURE) {
    aihc_fail("attempted to invoke a non-continuation value");
  }
  if (aihc_value_arity(continuation) != 1) {
    aihc_fail("continuation closure does not accept exactly one result");
  }
  (void)aihc_next_application_info(aihc_value_info_table(continuation), count);
  return aihc_portable_transfer(
      aihc_value_entry(continuation),
      aihc_portable_arguments(machine, continuation, count, values, NULL));
}

AihcPortableTransfer aihc_portable_continue_values(AihcMachine *machine,
                                                   AihcValue *continuation,
                                                   uint64_t count,
                                                   const AihcSlot *values) {
  return aihc_portable_continue_values_now(machine, continuation, count,
                                           values);
}

static AihcPortableTransfer
aihc_portable_continue_value(AihcMachine *machine, AihcValue *continuation,
                             AihcSlot value) {
  return aihc_portable_continue_values_now(machine, continuation, 1, &value);
}

AihcValue *aihc_apply_slow(AihcMachine *machine, AihcValue *function,
                           uint64_t count, const AihcSlot *arguments,
                           AihcValue **continuation) {
  if (function == NULL) {
    aihc_fail("attempted to apply null");
  }
  switch (aihc_value_tag(function)) {
  case AIHC_TAG_CLOSURE: {
    uint64_t arity = aihc_value_arity(function);
    if (arity <= 1) {
      aihc_fail("closure application does not require the slow path");
    }
    return aihc_copy_with_fields(machine, &function, AIHC_TAG_CLOSURE, count,
                                 arguments, continuation);
  }
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    uint64_t result_tag =
        arity > 1 ? AIHC_TAG_PARTIAL_CONSTRUCTOR : AIHC_TAG_NODE;
    return aihc_copy_with_fields(machine, &function, result_tag, count,
                                 arguments, continuation);
  }
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

AihcPortableTransfer aihc_portable_apply_cps(AihcMachine *machine,
                                             AihcValue *function,
                                             uint64_t count,
                                             const AihcSlot *arguments,
                                             AihcValue *continuation) {
  if (function == NULL) {
    aihc_fail("attempted to apply null");
  }
  if (aihc_value_tag(function) == AIHC_TAG_CLOSURE &&
      aihc_value_arity(function) == 1) {
    (void)aihc_next_application_info(aihc_value_info_table(function), count);
    return aihc_portable_transfer(aihc_value_entry(function),
                                  aihc_portable_arguments(machine, function,
                                                          count, arguments,
                                                          continuation));
  }
  AihcValue *applied =
      aihc_apply_slow(machine, function, count, arguments, &continuation);
  return aihc_portable_continue_value(machine, continuation, (AihcSlot)applied);
}

static void aihc_suspend_apply(AihcThread *thread, AihcValue *function,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_APPLY;
  thread->resume_function = function;
  thread->resume_continuation = continuation;
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
  thread->resume_value = value;
  thread->resume_count = count;
}

static const AihcResume *aihc_select_thread(AihcMachine *machine,
                                            AihcThread *thread) {
  AihcResume *resume = &machine->selected_resume;
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
  machine->current_thread = thread;
  if (resume->kind != AIHC_RESUME_APPLY &&
      resume->kind != AIHC_RESUME_CONTINUE) {
    aihc_fail("thread has no suspended continuation");
  }
  return resume;
}

static int32_t aihc_io_error(int error) {
  return (int32_t)(-((int64_t)error) - 1);
}

static int aihc_posix_descriptor(const AihcIoHandle *handle) {
  return (int)handle->backend_token;
}

static int aihc_posix_prepare(AihcIoRequest *request) {
  int descriptor = aihc_posix_descriptor(request->handle);
  int flags = fcntl(descriptor, F_GETFL);
  if (flags == -1) {
    return errno;
  }
  if ((flags & O_NONBLOCK) == 0 &&
      fcntl(descriptor, F_SETFL, flags | O_NONBLOCK) == -1) {
    return errno;
  }
  return 0;
}

static int aihc_posix_try_request(AihcIoRequest *request, int32_t *result) {
  for (;;) {
    ssize_t transferred;
    uint8_t *bytes = request->buffer + request->offset;
    if (request->kind == AIHC_IO_READ) {
      transferred =
          read(aihc_posix_descriptor(request->handle), bytes, request->length);
    } else {
      transferred =
          write(aihc_posix_descriptor(request->handle), bytes, request->length);
    }
    if (transferred >= 0) {
      *result = (int32_t)transferred;
      return 1;
    }
    if (errno == EINTR) {
      continue;
    }
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return 0;
    }
    *result = aihc_io_error(errno);
    return 1;
  }
}

static void aihc_resume_io_request(AihcMachine *machine, AihcIoRequest *request,
                                   int32_t result) {
  AihcThread *thread = request->thread;
  AihcValue *continuation = request->continuation;
  request->state = AIHC_IO_COMPLETED;
  request->result = result;
  request->thread = NULL;
  request->continuation = NULL;
  request->next = NULL;
  aihc_suspend_continue(thread, continuation, 0, 0);
  aihc_enqueue_thread(machine, thread);
}

static void aihc_complete_all_io_with_error(AihcMachine *machine, int error) {
  AihcIoRequest *request = machine->io_requests_head;
  machine->io_requests_head = NULL;
  machine->io_requests_tail = NULL;
  machine->io_request_count = 0;
  while (request != NULL) {
    AihcIoRequest *next = request->next;
    aihc_resume_io_request(machine, request, aihc_io_error(error));
    request = next;
  }
}

static void aihc_posix_poll(AihcMachine *machine, int may_block) {
  if (machine->io_request_count == 0) {
    return;
  }
  size_t count = (size_t)machine->io_request_count;
  struct pollfd *descriptors =
      aihc_allocate_auxiliary(machine, sizeof(*descriptors) * count);
  size_t index = 0;
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    descriptors[index].fd = aihc_posix_descriptor(request->handle);
    descriptors[index].events =
        request->kind == AIHC_IO_READ ? POLLIN : POLLOUT;
    ++index;
  }
  int ready = poll(descriptors, count, may_block ? -1 : 0);
  if (ready == -1) {
    int error = errno;
    free(descriptors);
    if (error != EINTR) {
      aihc_complete_all_io_with_error(machine, error);
    }
    return;
  }
  if (ready == 0) {
    free(descriptors);
    return;
  }

  AihcIoRequest **link = &machine->io_requests_head;
  AihcIoRequest *tail = NULL;
  index = 0;
  while (*link != NULL) {
    AihcIoRequest *request = *link;
    short events = descriptors[index++].revents;
    int32_t result = 0;
    int complete = 0;
    if ((events & POLLNVAL) != 0) {
      result = aihc_io_error(EBADF);
      complete = 1;
    } else if (events != 0) {
      complete = aihc_posix_try_request(request, &result);
    }
    if (complete) {
      *link = request->next;
      --machine->io_request_count;
      aihc_resume_io_request(machine, request, result);
    } else {
      tail = request;
      link = &request->next;
    }
  }
  machine->io_requests_tail = tail;
  free(descriptors);
}

static const AihcIoBackend aihc_posix_io_backend = {
    aihc_posix_prepare,
    aihc_posix_try_request,
    aihc_posix_poll,
};

static const AihcIoBackend *aihc_default_io_backend(void) {
  return &aihc_posix_io_backend;
}

static const AihcResume *aihc_schedule(AihcMachine *machine) {
  for (;;) {
    machine->io_backend->poll(machine, 0);
    if (machine->run_queue_head != NULL) {
      return aihc_select_thread(machine, aihc_dequeue_thread(machine));
    }
    if (machine->io_request_count != 0) {
      machine->io_backend->poll(machine, 1);
      continue;
    }
    aihc_fail("no runnable threads");
  }
}

static AihcIoRequest *aihc_io_submit(AihcIoKind kind, AihcIoHandle *handle,
                                     uint8_t *buffer, int32_t offset,
                                     int32_t length) {
  if (handle == NULL) {
    aihc_fail("attempted IO with a null handle");
  }
  if (offset < 0 || length < 0 || (buffer == NULL && length != 0)) {
    aihc_fail("invalid IO buffer range");
  }
  uint32_t required_capability =
      kind == AIHC_IO_READ ? AIHC_IO_READABLE : AIHC_IO_WRITABLE;
  if ((handle->capabilities & required_capability) == 0) {
    aihc_fail("attempted unsupported operation on IO handle");
  }
  AihcIoRequest *request = aihc_allocate_zeroed(sizeof(*request));
  request->kind = kind;
  request->state = AIHC_IO_SUBMITTED;
  request->handle = handle;
  request->buffer = buffer;
  request->offset = (size_t)offset;
  request->length = (size_t)length;
  return request;
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

static size_t aihc_byte_array_size(int64_t requested_size) {
  if (requested_size < 0 || (uint64_t)requested_size > SIZE_MAX) {
    aihc_fail("invalid byte array size");
  }
  return (size_t)requested_size;
}

static size_t aihc_byte_array_alignment(int64_t requested_alignment) {
  if (requested_alignment <= 0 || (uint64_t)requested_alignment > SIZE_MAX ||
      ((uint64_t)requested_alignment &
       ((uint64_t)requested_alignment - UINT64_C(1))) != 0) {
    aihc_fail("invalid byte array alignment");
  }
  return (size_t)requested_alignment;
}

static AihcByteArray *aihc_byte_array_allocate(int64_t requested_size,
                                               uint8_t pinned,
                                               int64_t requested_alignment) {
  size_t size = aihc_byte_array_size(requested_size);
  size_t alignment = aihc_byte_array_alignment(requested_alignment);
  size_t allocation_size = size == 0 ? 1 : size;
  if (allocation_size > SIZE_MAX - (alignment - 1)) {
    aihc_fail("byte array allocation is too large");
  }
  AihcByteArray *array = aihc_allocate_zeroed(sizeof(*array));
  uint8_t *raw = aihc_allocate_zeroed(allocation_size + alignment - 1);
  uintptr_t aligned = ((uintptr_t)raw + alignment - 1) & ~(alignment - 1);
  array->size = size;
  array->contents = (uint8_t *)aligned;
  array->pinned = pinned;
  array->alignment = alignment;
  return array;
}

void *aihc_byte_array_new(int64_t size) {
  return aihc_byte_array_allocate(size, 0, (int64_t)sizeof(uintptr_t));
}

void *aihc_byte_array_new_pinned(int64_t size) {
  return aihc_byte_array_allocate(size, 1, (int64_t)sizeof(uintptr_t));
}

void *aihc_byte_array_new_aligned_pinned(int64_t size, int64_t alignment) {
  return aihc_byte_array_allocate(size, 1, alignment);
}

uint64_t aihc_byte_array_is_pinned(void *opaque_array) {
  AihcByteArray *array = opaque_array;
  if (array == NULL) {
    aihc_fail("attempted to inspect a null byte array");
  }
  return array->pinned;
}

void *aihc_byte_array_contents(void *opaque_array) {
  AihcByteArray *array = opaque_array;
  if (array == NULL) {
    aihc_fail("attempted to inspect a null byte array");
  }
  return array->contents;
}

uint64_t aihc_byte_array_shrink(void *opaque_array, int64_t requested_size) {
  AihcByteArray *array = opaque_array;
  size_t size = aihc_byte_array_size(requested_size);
  if (array == NULL || size > array->size) {
    aihc_fail("invalid byte array shrink");
  }
  array->size = size;
  return 0;
}

void *aihc_byte_array_resize(void *opaque_array, int64_t requested_size) {
  AihcByteArray *array = opaque_array;
  if (array == NULL) {
    aihc_fail("attempted to resize a null byte array");
  }
  AihcByteArray *resized = aihc_byte_array_allocate(
      requested_size, array->pinned, (int64_t)array->alignment);
  size_t copy_size = array->size < resized->size ? array->size : resized->size;
  memcpy(resized->contents, array->contents, copy_size);
  return resized;
}

uint64_t aihc_byte_array_get_size(void *opaque_array) {
  AihcByteArray *array = opaque_array;
  if (array == NULL) {
    aihc_fail("attempted to inspect a null byte array");
  }
  return (uint64_t)array->size;
}

uint64_t aihc_byte_array_copy_from_addr(void *source, void *opaque_array,
                                        int64_t requested_offset,
                                        int64_t requested_length) {
  AihcByteArray *array = opaque_array;
  size_t offset = aihc_byte_array_size(requested_offset);
  size_t length = aihc_byte_array_size(requested_length);
  if (array == NULL || offset > array->size || length > array->size - offset ||
      (source == NULL && length != 0)) {
    aihc_fail("invalid byte array copy");
  }
  if (length != 0) {
    memcpy(array->contents + offset, source, length);
  }
  return 0;
}

static size_t aihc_byte_array_word_offset(AihcByteArray *array,
                                          int64_t requested_index) {
  if (array == NULL || requested_index < 0 ||
      (uint64_t)requested_index > SIZE_MAX / sizeof(uint64_t)) {
    aihc_fail("invalid byte array word index");
  }
  size_t offset = (size_t)requested_index * sizeof(uint64_t);
  if (offset > array->size || sizeof(uint64_t) > array->size - offset) {
    aihc_fail("byte array word index out of bounds");
  }
  return offset;
}

uint64_t aihc_byte_array_index_word(void *opaque_array, int64_t index) {
  AihcByteArray *array = opaque_array;
  size_t offset = aihc_byte_array_word_offset(array, index);
  uint64_t value;
  memcpy(&value, array->contents + offset, sizeof(value));
  return value;
}

uint64_t aihc_byte_array_read_word(void *opaque_array, int64_t index) {
  return aihc_byte_array_index_word(opaque_array, index);
}

uint64_t aihc_byte_array_write_word(void *opaque_array, int64_t index,
                                    uint64_t value) {
  AihcByteArray *array = opaque_array;
  size_t offset = aihc_byte_array_word_offset(array, index);
  memcpy(array->contents + offset, &value, sizeof(value));
  return 0;
}

uint64_t aihc_byte_array_copy(void *opaque_source,
                              int64_t requested_source_offset,
                              void *opaque_destination,
                              int64_t requested_destination_offset,
                              int64_t requested_length) {
  AihcByteArray *source = opaque_source;
  AihcByteArray *destination = opaque_destination;
  size_t source_offset = aihc_byte_array_size(requested_source_offset);
  size_t destination_offset =
      aihc_byte_array_size(requested_destination_offset);
  size_t length = aihc_byte_array_size(requested_length);
  if (source == NULL || destination == NULL || source_offset > source->size ||
      length > source->size - source_offset ||
      destination_offset > destination->size ||
      length > destination->size - destination_offset) {
    aihc_fail("invalid byte array copy");
  }
  memmove(destination->contents + destination_offset,
          source->contents + source_offset, length);
  return 0;
}

uint64_t aihc_word_clz(uint64_t value) {
  return value == 0 ? 64 : (uint64_t)__builtin_clzll(value);
}

uint64_t aihc_word_ctz(uint64_t value) {
  return value == 0 ? 64 : (uint64_t)__builtin_ctzll(value);
}

uint64_t aihc_word_popcount(uint64_t value) {
  return (uint64_t)__builtin_popcountll(value);
}

void *aihc_io_submit_read(void *opaque_handle, void *opaque_buffer,
                          int32_t offset, int32_t length) {
  return aihc_io_submit(AIHC_IO_READ, opaque_handle, opaque_buffer, offset,
                        length);
}

void *aihc_io_submit_write(void *opaque_handle, void *opaque_buffer,
                           int32_t offset, int32_t length) {
  return aihc_io_submit(AIHC_IO_WRITE, opaque_handle, opaque_buffer, offset,
                        length);
}

int32_t aihc_io_take_result(void *opaque_request) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL || request->state != AIHC_IO_COMPLETED) {
    aihc_fail("attempted to consume an incomplete IO request");
  }
  int32_t result = request->result;
  request->state = AIHC_IO_CONSUMED;
  free(request);
  return result;
}

static const AihcResume *aihc_resume_current(AihcMachine *machine,
                                             AihcValue *continuation) {
  aihc_suspend_continue(machine->current_thread, continuation, 0, 0);
  return aihc_select_thread(machine, machine->current_thread);
}

const AihcResume *aihc_await_io(AihcMachine *machine, void *opaque_request,
                                AihcValue *continuation) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL) {
    aihc_fail("attempted to await a null IO request");
  }
  if (request->state == AIHC_IO_COMPLETED) {
    return aihc_resume_current(machine, continuation);
  }
  if (request->state != AIHC_IO_SUBMITTED) {
    aihc_fail("attempted to await an IO request more than once");
  }

  int error = machine->io_backend->prepare(request);
  if (error != 0) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(error);
    return aihc_resume_current(machine, continuation);
  }

  int32_t result = 0;
  if (machine->io_backend->try_request(request, &result)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = result;
    return aihc_resume_current(machine, continuation);
  }

  request->state = AIHC_IO_PENDING;
  request->thread = machine->current_thread;
  request->continuation = continuation;
  if (machine->io_requests_tail == NULL) {
    machine->io_requests_head = request;
  } else {
    machine->io_requests_tail->next = request;
  }
  machine->io_requests_tail = request;
  ++machine->io_request_count;
  return aihc_schedule(machine);
}

void aihc_begin_blackhole(AihcMachine *machine, AihcValue *value) {
  if (value == NULL || aihc_value_tag(value) != AIHC_TAG_THUNK) {
    aihc_fail("attempted to blackhole a non-thunk value");
  }
  value->header = (value->header & ~AIHC_TAG_MASK) | AIHC_TAG_BLACKHOLE;
  aihc_find_blackhole(machine, value);
}

const AihcResume *aihc_block_on_blackhole(AihcMachine *machine,
                                          AihcValue *value,
                                          AihcValue *continuation) {
  if (value == NULL || aihc_value_tag(value) != AIHC_TAG_BLACKHOLE) {
    aihc_fail("attempted to block on a value that is not blackholed");
  }
  aihc_add_blackhole_waiter(machine, value, continuation);
  return aihc_schedule(machine);
}

static AihcPortableTransfer aihc_portable_resume(AihcMachine *machine,
                                                 const AihcResume *resume) {
  AihcPortableTransfer transfer;
  switch (resume->kind) {
  case AIHC_RESUME_APPLY:
    transfer = aihc_portable_apply_cps(machine, resume->function, 0, NULL,
                                       resume->continuation);
    break;
  case AIHC_RESUME_CONTINUE:
    transfer = aihc_portable_continue_values_now(
        machine, resume->function, resume->count,
        resume->count == 0 ? NULL : &resume->value);
    break;
  default:
    aihc_fail("invalid suspended continuation");
  }
  machine->selected_resume = (AihcResume){0};
  return transfer;
}

AihcPortableTransfer aihc_portable_eval_cps(AihcMachine *machine,
                                            AihcValue *value,
                                            uint64_t result_is_lifted,
                                            AihcValue *continuation,
                                            AihcValue *update_continuation) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  switch (aihc_value_tag(value)) {
  case AIHC_TAG_THUNK: {
    AihcSlot *arguments =
        aihc_portable_arguments(machine, value, 0, NULL, update_continuation);
    AihcEntry entry = aihc_value_entry(value);
    aihc_begin_blackhole(machine, value);
    return aihc_portable_transfer(entry, arguments);
  }
  case AIHC_TAG_INDIRECTION:
    if (result_is_lifted) {
      return aihc_portable_eval_cps(machine, (AihcValue *)value->fields[0], 1,
                                    continuation, update_continuation);
    }
    return aihc_portable_continue_value(machine, continuation,
                                        value->fields[0]);
  case AIHC_TAG_BLACKHOLE:
    return aihc_portable_resume(
        machine, aihc_block_on_blackhole(machine, value, continuation));
  default:
    return aihc_portable_continue_value(machine, continuation, (AihcSlot)value);
  }
}

void aihc_update(AihcValue *object, AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  object->fields[0] = (AihcSlot)value;
  object->header = (object->header & ~AIHC_TAG_MASK) | AIHC_TAG_INDIRECTION;
}

void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value) {
  if (object == NULL || aihc_value_tag(object) != AIHC_TAG_BLACKHOLE) {
    aihc_fail("attempted to update a cell that is not blackholed");
  }
  aihc_update(object, value);
  AihcBlackhole *blackhole = aihc_remove_blackhole(machine, object);
  if (blackhole == NULL) {
    return;
  }
  AihcBlackholeWaiter *waiter = blackhole->waiters_head;
  while (waiter != NULL) {
    AihcBlackholeWaiter *next = waiter->next;
    aihc_suspend_continue(waiter->thread, waiter->continuation, 1,
                          (AihcSlot)value);
    aihc_enqueue_thread(machine, waiter->thread);
    free(waiter);
    waiter = next;
  }
  free(blackhole);
}

AihcSlot aihc_fork(AihcMachine *machine, AihcValue *action) {
  if (machine->thread_done_continuation == NULL) {
    aihc_fail("thread completion continuation is not initialized");
  }
  AihcThread *child = aihc_thread_new(machine);
  aihc_suspend_apply(child, action, machine->thread_done_continuation);
  aihc_enqueue_thread(machine, child);
  return (AihcSlot)child;
}

const AihcResume *aihc_yield(AihcMachine *machine, AihcValue *continuation) {
  AihcThread *current = machine->current_thread;
  aihc_suspend_continue(current, continuation, 0, 0);
  aihc_enqueue_thread(machine, current);
  return aihc_schedule(machine);
}

const AihcResume *aihc_thread_done(AihcMachine *machine) {
  return aihc_schedule(machine);
}

void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation) {
  if (thread_done_continuation == NULL ||
      aihc_value_tag(thread_done_continuation) != AIHC_TAG_CLOSURE ||
      aihc_value_arity(thread_done_continuation) != 1) {
    aihc_fail("invalid thread completion continuation");
  }
  machine->thread_done_continuation = thread_done_continuation;
}

AihcEntry aihc_halt(AihcMachine *machine) { return machine->exit_code; }

AihcPortableTransfer aihc_portable_fork_cps(AihcMachine *machine,
                                            AihcValue *action,
                                            AihcValue *continuation) {
  AihcSlot thread_id = aihc_fork(machine, action);
  return aihc_portable_continue_value(machine, continuation, thread_id);
}

AihcPortableTransfer aihc_portable_yield_cps(AihcMachine *machine,
                                             AihcValue *continuation) {
  return aihc_portable_resume(machine, aihc_yield(machine, continuation));
}

AihcPortableTransfer aihc_portable_await_io_cps(AihcMachine *machine,
                                                void *request,
                                                AihcValue *continuation) {
  return aihc_portable_resume(machine,
                              aihc_await_io(machine, request, continuation));
}

AihcPortableTransfer aihc_portable_thread_done(AihcMachine *machine) {
  return aihc_portable_resume(machine, aihc_thread_done(machine));
}

AihcPortableTransfer aihc_portable_start(AihcMachine *machine, AihcValue *root,
                                         AihcValue *continuation,
                                         AihcValue *update_continuation,
                                         AihcValue *thread_done_continuation,
                                         AihcEntry exit_code) {
  machine->exit_code = exit_code;
  aihc_set_thread_done_continuation(machine, thread_done_continuation);
  return aihc_portable_eval_cps(machine, root, 1, continuation,
                                update_continuation);
}
