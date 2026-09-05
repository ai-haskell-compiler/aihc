#include "aihc_runtime.h"
#include "aihc_runtime_internal.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if UINTPTR_MAX == UINT64_MAX
_Static_assert(offsetof(AihcMachine, globals) == 0, "machine globals ABI");
_Static_assert(offsetof(AihcMachine, heap_next) == 24, "machine heap-next ABI");
_Static_assert(offsetof(AihcMachine, heap_limit) == 32,
               "machine heap-limit ABI");
_Static_assert(offsetof(AihcMachine, exit_code) == 16, "machine exit-code ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 24,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 48,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 56,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 64,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, srt) == 72, "info-table SRT ABI");
_Static_assert(sizeof(AihcInfo) == 80, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 8, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 16, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 24, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 16,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 24, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 32, "resume count ABI");
#elif UINTPTR_MAX == UINT32_MAX
_Static_assert(offsetof(AihcMachine, exit_code) == 16, "machine exit-code ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 12,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 24,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 28,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 32,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, srt) == 36, "info-table SRT ABI");
_Static_assert(sizeof(AihcInfo) == 40, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 4, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 8, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 12, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 12,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 16, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 24, "resume count ABI");
#endif

const AihcSrt *aihc_current_srt = NULL;

_Noreturn void aihc_fail(const char *message) { aihc_host_fail(message); }

static const AihcResume *aihc_schedule(AihcMachine *machine);

static const uint8_t aihc_indirection_field_is_pointer[] = {1};
static const AihcInfo aihc_indirection_info = {
    .field_count = 1,
    .field_is_pointer = aihc_indirection_field_is_pointer,
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_INDIRECTION,
};
static const AihcInfo aihc_thread_info = {
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_THREAD,
};
const AihcInfo aihc_runtime_object_info = {
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_RUNTIME,
};

void aihc_unsupported_primitive(void) {
  aihc_fail("primitive is not implemented by the native runtime");
}

void aihc_record_allocation(AihcMachine *machine) {
  if (machine->allocation_count == UINT64_MAX) {
    aihc_fail("allocation counter overflow");
  }
  ++machine->allocation_count;
}

void *aihc_allocate_zeroed(size_t bytes) {
  void *pointer = calloc(1, bytes);
  if (pointer == NULL) {
    aihc_fail("out of memory");
  }
  return pointer;
}

void *aihc_allocate_auxiliary(AihcMachine *machine, size_t bytes) {
  void *pointer = aihc_allocate_zeroed(bytes);
  aihc_record_allocation(machine);
  return pointer;
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
  if (aihc_value_kind(value) == AIHC_OBJECT_ARRAY) {
    return 2 + aihc_array_length(value);
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

/* aihc_array_new, aihc_array_index, aihc_array_write, aihc_array_same, and
   the info table they share live in compiler/native/runtime/aihc_array.lir.
   aihc_array_length and aihc_array_elements stay here: the collector walks
   arrays through them, including the ones the GC fuzz harness builds with
   info tables of its own. */

AihcValue *aihc_mutvar_new(AihcMachine *machine, AihcSlot initial) {
  return aihc_array_new(machine, 1, initial);
}

AihcSlot aihc_mutvar_read(AihcValue *mutvar) {
  return aihc_array_index(mutvar, 0);
}

AihcSlot aihc_mutvar_write(AihcValue *mutvar, AihcSlot value) {
  return aihc_array_write(mutvar, 0, value);
}

uint64_t aihc_mutvar_compare_and_swap(AihcValue *mutvar, AihcSlot expected,
                                      AihcSlot replacement) {
  AihcSlot *current = &aihc_array_elements(mutvar)[0];
  if (*current != expected) {
    return 1;
  }
  *current = replacement;
  return 0;
}

uint64_t aihc_mutvar_same(AihcValue *left, AihcValue *right) {
  return aihc_array_same(left, right);
}

void *aihc_stable_name_make(AihcMachine *machine, AihcValue *value) {
  if (value == NULL) {
    aihc_fail("stable-name primitive received null");
  }
  for (AihcStableName *name = machine->stable_names; name != NULL;
       name = name->next) {
    if (name->value == value) {
      return name;
    }
  }
  if (machine->next_stable_name > (uint64_t)INT64_MAX) {
    aihc_fail("stable-name counter overflow");
  }
  AihcStableName *name = aihc_allocate_auxiliary(machine, sizeof(*name));
  name->header = (AihcSlot)(uintptr_t)&aihc_runtime_object_info;
  name->value = value;
  name->hash = machine->next_stable_name++;
  name->next = machine->stable_names;
  machine->stable_names = name;
  return name;
}

uint64_t aihc_stable_name_equal(const void *left, const void *right) {
  return left == right;
}

int64_t aihc_stable_name_hash(const void *opaque_name) {
  if (opaque_name == NULL) {
    aihc_fail("stable-name hash received null");
  }
  const AihcStableName *name = opaque_name;
  return (int64_t)name->hash;
}

static void aihc_visit_value(AihcValue **value, AihcRootVisitor visitor,
                             void *context) {
  *value =
      (AihcValue *)(uintptr_t)visitor((AihcSlot)(uintptr_t)*value, context);
}

static void aihc_visit_thread(AihcThread *thread, AihcRootVisitor visitor,
                              void *context) {
  if (thread == NULL) {
    return;
  }
  aihc_visit_value(&thread->resume_function, visitor, context);
  aihc_visit_value(&thread->resume_continuation, visitor, context);
  if ((thread->resume_kind == AIHC_RESUME_CONTINUE ||
       thread->resume_kind == AIHC_RESUME_APPLY) &&
      thread->resume_count == 1) {
    thread->resume_value = visitor(thread->resume_value, context);
  }
}

/* Static objects are not visited here. They never move, so a collector marks
   and scans the ones it finds reachable instead of treating all of them as
   roots. */
void aihc_visit_roots(AihcMachine *machine, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context) {
  for (uint64_t index = 0; index < machine->global_count; ++index) {
    machine->globals[index] = visitor(machine->globals[index], context);
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    roots[index] = visitor(roots[index], context);
  }
  aihc_visit_value(&machine->thread_done_continuation, visitor, context);
  aihc_visit_value(&machine->selected_resume.function, visitor, context);
  aihc_visit_value(&machine->selected_resume.continuation, visitor, context);
  if ((machine->selected_resume.kind == AIHC_RESUME_CONTINUE ||
       machine->selected_resume.kind == AIHC_RESUME_APPLY) &&
      machine->selected_resume.count == 1) {
    machine->selected_resume.value =
        visitor(machine->selected_resume.value, context);
  }
  aihc_visit_thread(machine->current_thread, visitor, context);
  for (AihcThread *thread = machine->run_queue_head; thread != NULL;
       thread = thread->next) {
    aihc_visit_thread(thread, visitor, context);
  }
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    aihc_visit_value(&blackhole->object, visitor, context);
    for (AihcBlackholeWaiter *waiter = blackhole->waiters_head; waiter != NULL;
         waiter = waiter->next) {
      aihc_visit_value(&waiter->continuation, visitor, context);
      aihc_visit_thread(waiter->thread, visitor, context);
    }
  }
  for (AihcMVar *mvar = machine->mvars; mvar != NULL; mvar = mvar->next) {
    if (mvar->full) {
      mvar->value = visitor(mvar->value, context);
    }
    AihcMVarWaiter *waiter_lists[] = {mvar->readers_head, mvar->takers_head,
                                      mvar->putters_head};
    for (size_t list = 0; list < 3; ++list) {
      for (AihcMVarWaiter *waiter = waiter_lists[list]; waiter != NULL;
           waiter = waiter->next) {
        if (list == 2) {
          waiter->value = visitor(waiter->value, context);
        }
        aihc_visit_value(&waiter->continuation, visitor, context);
        aihc_visit_thread(waiter->thread, visitor, context);
      }
    }
  }
  for (AihcStableName *name = machine->stable_names; name != NULL;
       name = name->next) {
    aihc_visit_value(&name->value, visitor, context);
  }
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    aihc_visit_value(&request->continuation, visitor, context);
    aihc_visit_thread(request->thread, visitor, context);
  }
}

void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots) {
  aihc_gc_ensure(machine, words, root_count, roots);
}

AihcValue *aihc_make_node_unchecked(AihcMachine *machine,
                                    const AihcInfo *info) {
  uint64_t words = aihc_object_words(info);
  AihcValue *value = aihc_gc_allocate(machine, words);
  aihc_record_allocation(machine);
  value->header = aihc_make_header(info);
  return value;
}

AihcValue *aihc_make_node(AihcMachine *machine, const AihcInfo *info) {
  uint64_t words = aihc_object_words(info);
  aihc_ensure_heap(machine, words, 0, NULL);
  return aihc_make_node_unchecked(machine, info);
}

uint64_t aihc_allocation_count(const AihcMachine *machine) {
  return machine->allocation_count;
}

void aihc_reset_allocation_count(AihcMachine *machine) {
  machine->allocation_count = 0;
}

const AihcInfo *aihc_next_application_info(const AihcInfo *info,
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
                                        uint64_t count, const AihcSlot *fields,
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

  aihc_ensure_heap(machine, aihc_object_words(next_info), 2 + pointer_count,
                   roots);
  value = (AihcValue *)roots[0];
  *value_pointer = value;
  *continuation_pointer = (AihcValue *)roots[1];

  AihcValue *copy = aihc_make_node_unchecked(machine, next_info);
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

static AihcThread *aihc_thread_new(AihcMachine *machine) {
  AihcThread *thread = aihc_allocate_auxiliary(machine, sizeof(*thread));
  thread->header = (AihcSlot)(uintptr_t)&aihc_thread_info;
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

void aihc_set_exit_status(AihcMachine *machine, int64_t status) {
  machine->exit_status = status;
}

int64_t aihc_get_exit_status(const AihcMachine *machine) {
  return machine->exit_status;
}

AihcMachine *aihc_machine_new(uint64_t global_count) {
  AihcMachine *machine = aihc_allocate_zeroed(sizeof(*machine));
  const AihcRtsConfig *rts_config = aihc_rts_config();
  machine->allocation_count = 1;
  machine->heap_max_bytes = rts_config->heap_max_bytes;
  machine->heap_limit_enabled = rts_config->heap_limit_enabled;
  machine->global_count = global_count;
  machine->globals = aihc_allocate_auxiliary(
      machine,
      sizeof(*machine->globals) * (global_count == 0 ? 1 : global_count));
  machine->next_stable_name = 1;
  aihc_gc_init(machine);
  machine->current_thread = aihc_thread_new(machine);
  machine->io_backend = aihc_host_io_backend();
  return machine;
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

AihcValue *aihc_apply_slow(AihcMachine *machine, AihcValue *function,
                           uint64_t count, const AihcSlot *arguments,
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
    if (arity <= 1) {
      aihc_fail("closure application does not require the slow path");
    }
    return aihc_copy_with_fields(machine, &function, count, arguments,
                                 continuation);
  }
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    return aihc_copy_with_fields(machine, &function, count, arguments,
                                 continuation);
  }
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_suspend_apply(AihcThread *thread, AihcValue *function,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_APPLY;
  thread->resume_function = function;
  thread->resume_continuation = continuation;
  thread->resume_count = 0;
}

static void aihc_suspend_raise(AihcThread *thread, AihcValue *exception,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_RAISE;
  thread->resume_function = exception;
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
      resume->kind != AIHC_RESUME_CONTINUE &&
      resume->kind != AIHC_RESUME_RAISE) {
    aihc_fail("thread has no suspended continuation");
  }
  return resume;
}

int64_t aihc_io_error(int error) { return -((int64_t)error) - 1; }

void aihc_resume_io_request(AihcMachine *machine, AihcIoRequest *request,
                            int64_t result) {
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

static const AihcResume *aihc_schedule(AihcMachine *machine) {
  for (;;) {
    (void)machine->io_backend->poll(machine, 0);
    if (machine->run_queue_head != NULL) {
      return aihc_select_thread(machine, aihc_dequeue_thread(machine));
    }
    if (machine->io_request_count != 0) {
      if (machine->io_backend->poll(machine, 1) == AIHC_IO_POLL_SUSPENDED) {
        return NULL;
      }
      continue;
    }
    aihc_fail("no runnable threads");
  }
}

static AihcIoRequest *aihc_io_submit(AihcIoKind kind, AihcIoHandle *handle,
                                     uint8_t *buffer, int64_t offset,
                                     int64_t length) {
  AihcIoRequest *request = aihc_allocate_zeroed(sizeof(*request));
  request->kind = kind;
  request->state = AIHC_IO_SUBMITTED;
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
  request->buffer = buffer;
  request->offset = (size_t)offset;
  request->length = (size_t)length;
  return request;
}

static AihcIoRequest *aihc_io_submit_open_request(uint8_t *path,
                                                  int64_t requested_length,
                                                  int64_t requested_mode) {
  AihcIoRequest *request = aihc_allocate_zeroed(sizeof(*request));
  request->kind = AIHC_IO_OPEN;
  request->state = AIHC_IO_SUBMITTED;
  if (requested_length < 0 || (uint64_t)requested_length > SIZE_MAX ||
      (path == NULL && requested_length != 0) || requested_mode < 0 ||
      requested_mode > 3) {
    request->state = AIHC_IO_COMPLETED;
    request->result =
        (int64_t)(uintptr_t)aihc_io_open_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    return request;
  }
  request->buffer = path;
  request->length = (size_t)requested_length;
  request->mode = requested_mode;
  return request;
}

void *aihc_io_open_error(int error) {
  return (void *)((((uintptr_t)error) << 1) | (uintptr_t)1);
}

int64_t aihc_io_open_result_error(void *result) {
  uintptr_t encoded = (uintptr_t)result;
  return (encoded & (uintptr_t)1) == 0 ? 0 : (int64_t)(encoded >> 1);
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
  array->header = (AihcSlot)(uintptr_t)&aihc_runtime_object_info;
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

uint64_t aihc_byte_array_copy_to_addr(void *opaque_array,
                                      int64_t requested_offset,
                                      void *destination,
                                      int64_t requested_length) {
  AihcByteArray *array = opaque_array;
  size_t offset = aihc_byte_array_size(requested_offset);
  size_t length = aihc_byte_array_size(requested_length);
  if (array == NULL || offset > array->size || length > array->size - offset ||
      (destination == NULL && length != 0)) {
    aihc_fail("invalid byte array copy");
  }
  if (length != 0) {
    memcpy(destination, array->contents + offset, length);
  }
  return 0;
}

uint64_t aihc_byte_array_compare(void *opaque_left, int64_t left_requested,
                                 void *opaque_right, int64_t right_requested,
                                 int64_t requested_length) {
  AihcByteArray *left = opaque_left;
  AihcByteArray *right = opaque_right;
  size_t left_offset = aihc_byte_array_size(left_requested);
  size_t right_offset = aihc_byte_array_size(right_requested);
  size_t length = aihc_byte_array_size(requested_length);
  if (left == NULL || right == NULL || left_offset > left->size ||
      length > left->size - left_offset || right_offset > right->size ||
      length > right->size - right_offset) {
    aihc_fail("invalid byte array comparison");
  }
  const uint8_t *left_bytes = left->contents + left_offset;
  const uint8_t *right_bytes = right->contents + right_offset;
  for (size_t index = 0; index < length; index += 1) {
    if (left_bytes[index] != right_bytes[index]) {
      return left_bytes[index] < right_bytes[index] ? (uint64_t)-1 : 1;
    }
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

static size_t aihc_byte_array_byte_offset(AihcByteArray *array,
                                          int64_t requested_offset,
                                          size_t element_size) {
  if (array == NULL || requested_offset < 0) {
    aihc_fail("invalid byte array byte offset");
  }
  size_t offset = (size_t)requested_offset;
  if (offset > array->size || element_size > array->size - offset) {
    aihc_fail("byte array byte offset out of bounds");
  }
  return offset;
}

uint64_t aihc_byte_array_index_byte_word8(void *opaque_array, int64_t offset) {
  AihcByteArray *array = opaque_array;
  size_t start = aihc_byte_array_byte_offset(array, offset, sizeof(uint8_t));
  uint8_t value;
  memcpy(&value, array->contents + start, sizeof(value));
  return value;
}

uint64_t aihc_byte_array_index_byte_word16(void *opaque_array, int64_t offset) {
  AihcByteArray *array = opaque_array;
  size_t start = aihc_byte_array_byte_offset(array, offset, sizeof(uint16_t));
  uint16_t value;
  memcpy(&value, array->contents + start, sizeof(value));
  return value;
}

uint64_t aihc_byte_array_index_byte_word32(void *opaque_array, int64_t offset) {
  AihcByteArray *array = opaque_array;
  size_t start = aihc_byte_array_byte_offset(array, offset, sizeof(uint32_t));
  uint32_t value;
  memcpy(&value, array->contents + start, sizeof(value));
  return value;
}

uint64_t aihc_byte_array_index_byte_word64(void *opaque_array, int64_t offset) {
  AihcByteArray *array = opaque_array;
  size_t start = aihc_byte_array_byte_offset(array, offset, sizeof(uint64_t));
  uint64_t value;
  memcpy(&value, array->contents + start, sizeof(value));
  return value;
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
                          int64_t offset, int64_t length) {
  return aihc_io_submit(AIHC_IO_READ, opaque_handle, opaque_buffer, offset,
                        length);
}

void *aihc_io_submit_write(void *opaque_handle, void *opaque_buffer,
                           int64_t offset, int64_t length) {
  return aihc_io_submit(AIHC_IO_WRITE, opaque_handle, opaque_buffer, offset,
                        length);
}

void *aihc_io_submit_open(void *opaque_path, int64_t length, int64_t mode) {
  return aihc_io_submit_open_request(opaque_path, length, mode);
}

int64_t aihc_io_take_result(void *opaque_request) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL || request->state != AIHC_IO_COMPLETED) {
    aihc_fail("attempted to consume an incomplete IO request");
  }
  int64_t result = request->result;
  request->state = AIHC_IO_CONSUMED;
  free(request);
  return result;
}

void *aihc_io_take_open_result(void *opaque_request) {
  return (void *)(uintptr_t)aihc_io_take_result(opaque_request);
}

static const AihcResume *aihc_resume_current(AihcMachine *machine,
                                             AihcValue *continuation) {
  aihc_suspend_continue(machine->current_thread, continuation, 0, 0);
  return aihc_select_thread(machine, machine->current_thread);
}

static const AihcResume *aihc_resume_current_value(AihcMachine *machine,
                                                   AihcValue *continuation,
                                                   AihcSlot value) {
  aihc_suspend_continue(machine->current_thread, continuation, 1, value);
  return aihc_select_thread(machine, machine->current_thread);
}

static AihcMVarWaiter *aihc_mvar_waiter_new(AihcMachine *machine,
                                            AihcValue *continuation,
                                            AihcSlot value) {
  AihcMVarWaiter *waiter = aihc_allocate_auxiliary(machine, sizeof(*waiter));
  waiter->thread = machine->current_thread;
  waiter->continuation = continuation;
  waiter->value = value;
  return waiter;
}

static void aihc_mvar_append_waiter(AihcMVarWaiter **head,
                                    AihcMVarWaiter **tail,
                                    AihcMVarWaiter *waiter) {
  if (*tail == NULL) {
    *head = waiter;
  } else {
    (*tail)->next = waiter;
  }
  *tail = waiter;
}

static AihcMVarWaiter *aihc_mvar_pop_waiter(AihcMVarWaiter **head,
                                            AihcMVarWaiter **tail) {
  AihcMVarWaiter *waiter = *head;
  if (waiter == NULL) {
    return NULL;
  }
  *head = waiter->next;
  if (*head == NULL) {
    *tail = NULL;
  }
  waiter->next = NULL;
  return waiter;
}

static void aihc_mvar_wake(AihcMachine *machine, AihcMVarWaiter *waiter,
                           uint64_t count, AihcSlot value) {
  aihc_suspend_continue(waiter->thread, waiter->continuation, count, value);
  aihc_enqueue_thread(machine, waiter->thread);
  free(waiter);
}

static AihcMVar *aihc_checked_mvar(void *opaque_mvar) {
  AihcMVar *mvar = opaque_mvar;
  if (mvar == NULL) {
    aihc_fail("attempted an operation on a null MVar");
  }
  return mvar;
}

void *aihc_mvar_new(AihcMachine *machine) {
  AihcMVar *mvar = aihc_allocate_auxiliary(machine, sizeof(*mvar));
  mvar->header = (AihcSlot)(uintptr_t)&aihc_runtime_object_info;
  mvar->next = machine->mvars;
  machine->mvars = mvar;
  return mvar;
}

const AihcResume *aihc_mvar_read(AihcMachine *machine, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (mvar->full) {
    return aihc_resume_current_value(machine, continuation, mvar->value);
  }
  AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, 0);
  aihc_mvar_append_waiter(&mvar->readers_head, &mvar->readers_tail, waiter);
  return aihc_schedule(machine);
}

const AihcResume *aihc_mvar_take(AihcMachine *machine, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (!mvar->full) {
    AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, 0);
    aihc_mvar_append_waiter(&mvar->takers_head, &mvar->takers_tail, waiter);
    return aihc_schedule(machine);
  }

  AihcSlot value = mvar->value;
  AihcMVarWaiter *putter =
      aihc_mvar_pop_waiter(&mvar->putters_head, &mvar->putters_tail);
  if (putter == NULL) {
    mvar->full = 0;
    mvar->value = 0;
  } else {
    mvar->value = putter->value;
    aihc_mvar_wake(machine, putter, 0, 0);
  }
  return aihc_resume_current_value(machine, continuation, value);
}

const AihcResume *aihc_mvar_put(AihcMachine *machine, void *opaque_mvar,
                                AihcSlot value, AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (mvar->full) {
    AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, value);
    aihc_mvar_append_waiter(&mvar->putters_head, &mvar->putters_tail, waiter);
    return aihc_schedule(machine);
  }

  AihcMVarWaiter *reader;
  while ((reader = aihc_mvar_pop_waiter(&mvar->readers_head,
                                        &mvar->readers_tail)) != NULL) {
    aihc_mvar_wake(machine, reader, 1, value);
  }
  AihcMVarWaiter *taker =
      aihc_mvar_pop_waiter(&mvar->takers_head, &mvar->takers_tail);
  if (taker == NULL) {
    mvar->full = 1;
    mvar->value = value;
  } else {
    aihc_mvar_wake(machine, taker, 1, value);
  }
  return aihc_resume_current(machine, continuation);
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

  int64_t result = 0;
  if (machine->io_backend->try_request(request, &result)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = machine->io_backend->finish_request(request, result);
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
  if (value == NULL || aihc_value_kind(value) != AIHC_OBJECT_THUNK) {
    aihc_fail("attempted to blackhole a non-thunk value");
  }
  const AihcInfo *original_info = aihc_value_info_table(value);
  AihcBlackhole *blackhole = aihc_find_blackhole(machine, value);
  blackhole->original_info = original_info;
  blackhole->info = *original_info;
  blackhole->info.object_kind = AIHC_OBJECT_BLACKHOLE;
  value->header = (AihcSlot)(uintptr_t)&blackhole->info;
}

const AihcResume *aihc_block_on_blackhole(AihcMachine *machine,
                                          AihcValue *value,
                                          AihcValue *continuation) {
  if (value == NULL || aihc_value_kind(value) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("attempted to block on a value that is not blackholed");
  }
  aihc_add_blackhole_waiter(machine, value, continuation);
  return aihc_schedule(machine);
}

const AihcResume *aihc_complete_io(AihcMachine *machine, int64_t result) {
  AihcIoRequest *request = machine->io_requests_head;
  if (request == NULL) {
    aihc_fail("IO completion has no pending request");
  }
  machine->io_requests_head = request->next;
  --machine->io_request_count;
  int64_t request_result = machine->io_backend->finish_request(request, result);
  aihc_resume_io_request(machine, request, request_result);

  while (machine->io_requests_head != NULL) {
    request = machine->io_requests_head;
    if (!machine->io_backend->try_request(request, &request_result)) {
      break;
    }
    request_result =
        machine->io_backend->finish_request(request, request_result);
    machine->io_requests_head = request->next;
    --machine->io_request_count;
    aihc_resume_io_request(machine, request, request_result);
  }
  if (machine->io_requests_head == NULL) {
    machine->io_requests_tail = NULL;
  }
  return aihc_schedule(machine);
}

void aihc_update(AihcValue *object, AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  object->fields[0] = (AihcSlot)value;
  object->header = (AihcSlot)(uintptr_t)&aihc_indirection_info;
  aihc_gc_note_update(object);
}

void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value) {
  if (object == NULL || aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("attempted to update a cell that is not blackholed");
  }
  AihcBlackhole *blackhole = aihc_remove_blackhole(machine, object);
  if (blackhole == NULL) {
    aihc_fail("blackholed object has no scheduler record");
  }
  aihc_update(object, value);
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

static void aihc_abandon_blackhole(AihcMachine *machine, AihcValue *object,
                                   AihcValue *exception) {
  if (object == NULL || aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("exception update frame does not contain a blackhole");
  }
  AihcBlackhole *blackhole = aihc_remove_blackhole(machine, object);
  if (blackhole == NULL) {
    aihc_fail("blackholed object has no scheduler record");
  }
  object->header = (AihcSlot)(uintptr_t)blackhole->original_info;
  AihcBlackholeWaiter *waiter = blackhole->waiters_head;
  while (waiter != NULL) {
    AihcBlackholeWaiter *next = waiter->next;
    aihc_suspend_raise(waiter->thread, exception, waiter->continuation);
    aihc_enqueue_thread(machine, waiter->thread);
    free(waiter);
    waiter = next;
  }
  free(blackhole);
}

const AihcResume *aihc_raise(AihcMachine *machine, AihcValue *exception,
                             AihcValue *continuation) {
  if (exception == NULL) {
    aihc_fail("attempted to raise a null exception");
  }
  for (;;) {
    if (continuation == NULL ||
        aihc_value_kind(continuation) != AIHC_OBJECT_CLOSURE) {
      aihc_fail("exception chain contains a non-continuation value");
    }
    const AihcInfo *info = aihc_value_info_table(continuation);
    const AihcSlot *fields = aihc_value_fields_const(continuation);
    switch (info->frame_kind) {
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
      AihcResume *resume = &machine->selected_resume;
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
      aihc_abandon_blackhole(machine, (AihcValue *)(uintptr_t)fields[1],
                             exception);
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
      aihc_value_kind(thread_done_continuation) != AIHC_OBJECT_CLOSURE ||
      aihc_value_arity(thread_done_continuation) != 1) {
    aihc_fail("invalid thread completion continuation");
  }
  machine->thread_done_continuation = thread_done_continuation;
}

AihcEntry aihc_halt(AihcMachine *machine) { return machine->exit_code; }

/* Floating point functions of the Floating class. The runtime is freestanding
   on some targets, so the functions do not use libm. A Double# arrives as
   its IEEE 754 bit pattern in a 64-bit word, and a Float# arrives as its
   bit pattern in the low 32 bits. The results have the same form. The
   implementations reduce the argument and sum a short series, so their
   error stays within a few units in the last place for ordinary arguments. */

static double aihc_double_from_bits(uint64_t bits) {
  double value;
  memcpy(&value, &bits, sizeof value);
  return value;
}

static uint64_t aihc_double_to_bits(double value) {
  uint64_t bits;
  memcpy(&bits, &value, sizeof bits);
  return bits;
}

static float aihc_float_from_bits(uint64_t bits) {
  uint32_t low = (uint32_t)bits;
  float value;
  memcpy(&value, &low, sizeof value);
  return value;
}

static uint64_t aihc_float_to_bits(float value) {
  uint32_t low;
  memcpy(&low, &value, sizeof low);
  return (uint64_t)low;
}

static const uint64_t AIHC_DOUBLE_NAN_BITS = 0x7ff8000000000000ULL;
static const uint64_t AIHC_DOUBLE_INFINITY_BITS = 0x7ff0000000000000ULL;
static const double AIHC_LN2_HIGH = 6.93147180369123816490e-01;
static const double AIHC_LN2_LOW = 1.90821492927058770002e-10;
static const double AIHC_LN2 = 6.93147180559945309417e-01;
static const double AIHC_PI = 3.14159265358979323846;
static const double AIHC_HALF_PI_HIGH = 1.57079632673412561417e+00;
static const double AIHC_HALF_PI_LOW = 6.07710050650619224932e-11;
static const double AIHC_EXP_OVERFLOW = 709.782712893384;
static const double AIHC_EXP_UNDERFLOW = -745.1332191019412;
static const double AIHC_SQRT_HALF = 0.70710678118654752440;
static const double AIHC_TAN_EIGHTH_PI = 0.41421356237309503;

static double aihc_double_nan(void) {
  return aihc_double_from_bits(AIHC_DOUBLE_NAN_BITS);
}

static double aihc_double_infinity(void) {
  return aihc_double_from_bits(AIHC_DOUBLE_INFINITY_BITS);
}

static int aihc_double_is_nan(double value) { return value != value; }

static int aihc_double_is_infinite(double value) {
  return !aihc_double_is_nan(value) && aihc_double_is_nan(value - value);
}

/* value * 2^power, with two steps when the power is large. */
static double aihc_double_scale(double value, int power) {
  while (power > 1000) {
    value *= aihc_double_from_bits((uint64_t)(1023 + 1000) << 52);
    power -= 1000;
  }
  while (power < -1000) {
    value *= aihc_double_from_bits((uint64_t)(1023 - 1000) << 52);
    power += 1000;
  }
  return value * aihc_double_from_bits((uint64_t)(1023 + power) << 52);
}

/* The mantissa in [1, 2) and the exponent of a positive finite value. */
static double aihc_double_split(double value, int *power) {
  uint64_t bits = aihc_double_to_bits(value);
  int exponent_field = (int)((bits >> 52) & 0x7ff);
  if (exponent_field == 0) {
    value *= aihc_double_from_bits((uint64_t)(1023 + 54) << 52);
    bits = aihc_double_to_bits(value);
    exponent_field = (int)((bits >> 52) & 0x7ff) - 54;
  }
  *power = exponent_field - 1023;
  bits = (bits & 0x000fffffffffffffULL) | ((uint64_t)1023 << 52);
  return aihc_double_from_bits(bits);
}

/* The largest integer value that is not above the value. */
static double aihc_math_floor_value(double value) {
  if (aihc_double_is_nan(value) || aihc_double_is_infinite(value)) {
    return value;
  }
  if (value > 4503599627370496.0 || value < -4503599627370496.0) {
    return value;
  }
  double truncated = (double)(long long)value;
  return truncated > value ? truncated - 1.0 : truncated;
}

static double aihc_math_exp(double x) {
  if (aihc_double_is_nan(x)) {
    return x;
  }
  if (x > AIHC_EXP_OVERFLOW) {
    return aihc_double_infinity();
  }
  if (x < AIHC_EXP_UNDERFLOW) {
    return 0.0;
  }
  double quotient = x / AIHC_LN2;
  int k = (int)(quotient < 0.0 ? quotient - 0.5 : quotient + 0.5);
  double r = (x - (double)k * AIHC_LN2_HIGH) - (double)k * AIHC_LN2_LOW;
  double term = 1.0;
  double sum = 1.0;
  for (int n = 1; n <= 18; n++) {
    term *= r / (double)n;
    sum += term;
  }
  return aihc_double_scale(sum, k);
}

static double aihc_math_log(double x) {
  if (aihc_double_is_nan(x) || x < 0.0) {
    return aihc_double_nan();
  }
  if (x == 0.0) {
    return -aihc_double_infinity();
  }
  if (aihc_double_is_infinite(x)) {
    return x;
  }
  int power;
  double m = aihc_double_split(x, &power);
  if (m > 1.0 / AIHC_SQRT_HALF) {
    m *= 0.5;
    power += 1;
  }
  double s = (m - 1.0) / (m + 1.0);
  double s2 = s * s;
  double term = s;
  double sum = 0.0;
  for (int n = 1; n <= 31; n += 2) {
    sum += term / (double)n;
    term *= s2;
  }
  return (double)power * AIHC_LN2_HIGH +
         (2.0 * sum + (double)power * AIHC_LN2_LOW);
}

static int aihc_double_is_integer(double value) {
  return !aihc_double_is_infinite(value) && !aihc_double_is_nan(value) &&
         aihc_math_floor_value(value) == value;
}

static double aihc_math_pow(double x, double y) {
  if (y == 0.0 || x == 1.0) {
    return 1.0;
  }
  if (aihc_double_is_nan(x) || aihc_double_is_nan(y)) {
    return aihc_double_nan();
  }
  if (x == 0.0) {
    if (y < 0.0) {
      return aihc_double_infinity();
    }
    return 0.0;
  }
  if (x < 0.0) {
    if (!aihc_double_is_integer(y)) {
      return aihc_double_nan();
    }
    double magnitude = aihc_math_exp(y * aihc_math_log(-x));
    double half = y * 0.5;
    int odd = aihc_math_floor_value(half) != half;
    return odd ? -magnitude : magnitude;
  }
  return aihc_math_exp(y * aihc_math_log(x));
}

/* sin and cos of a value in [-pi/4, pi/4]. */
static double aihc_math_sin_reduced(double r) {
  double r2 = r * r;
  double term = r;
  double sum = r;
  for (int n = 1; n <= 9; n++) {
    term *= -r2 / (double)((2 * n) * (2 * n + 1));
    sum += term;
  }
  return sum;
}

static double aihc_math_cos_reduced(double r) {
  double r2 = r * r;
  double term = 1.0;
  double sum = 1.0;
  for (int n = 1; n <= 9; n++) {
    term *= -r2 / (double)((2 * n - 1) * (2 * n));
    sum += term;
  }
  return sum;
}

/* The quadrant of a value and its remainder after removing quadrants. */
static double aihc_math_reduce_quadrant(double x, int *quadrant) {
  double quotient = x / (2.0 * AIHC_HALF_PI_HIGH);
  double k = quotient < 0.0 ? (double)(long long)(quotient - 0.5)
                            : (double)(long long)(quotient + 0.5);
  *quadrant = (int)(((long long)k % 4 + 4) % 4);
  return (x - k * AIHC_HALF_PI_HIGH) - k * AIHC_HALF_PI_LOW;
}

static double aihc_math_sin(double x) {
  if (aihc_double_is_nan(x) || aihc_double_is_infinite(x)) {
    return aihc_double_nan();
  }
  int quadrant;
  double r = aihc_math_reduce_quadrant(x, &quadrant);
  switch (quadrant) {
  case 0:
    return aihc_math_sin_reduced(r);
  case 1:
    return aihc_math_cos_reduced(r);
  case 2:
    return -aihc_math_sin_reduced(r);
  default:
    return -aihc_math_cos_reduced(r);
  }
}

static double aihc_math_cos(double x) {
  if (aihc_double_is_nan(x) || aihc_double_is_infinite(x)) {
    return aihc_double_nan();
  }
  int quadrant;
  double r = aihc_math_reduce_quadrant(x, &quadrant);
  switch (quadrant) {
  case 0:
    return aihc_math_cos_reduced(r);
  case 1:
    return -aihc_math_sin_reduced(r);
  case 2:
    return -aihc_math_cos_reduced(r);
  default:
    return aihc_math_sin_reduced(r);
  }
}

static double aihc_math_tan(double x) {
  return aihc_math_sin(x) / aihc_math_cos(x);
}

static double aihc_math_atan(double x) {
  if (aihc_double_is_nan(x)) {
    return x;
  }
  double sign = x < 0.0 ? -1.0 : 1.0;
  double t = x < 0.0 ? -x : x;
  if (aihc_double_is_infinite(t)) {
    return sign * AIHC_PI * 0.5;
  }
  double offset = 0.0;
  if (t > 1.0) {
    t = 1.0 / t;
    offset = AIHC_PI * 0.5;
    sign = -sign;
  }
  double extra = 0.0;
  if (t > AIHC_TAN_EIGHTH_PI) {
    t = (t - 1.0) / (t + 1.0);
    extra = AIHC_PI * 0.25;
  }
  double t2 = t * t;
  double term = t;
  double sum = 0.0;
  for (int n = 1; n <= 45; n += 2) {
    sum += term / (double)n;
    term *= -t2;
  }
  double result = sum + extra;
  if (offset != 0.0) {
    return -sign * (offset - result);
  }
  return sign * result;
}

static double aihc_math_asin(double x) {
  if (aihc_double_is_nan(x) || x > 1.0 || x < -1.0) {
    return aihc_double_nan();
  }
  if (x == 1.0 || x == -1.0) {
    return x * AIHC_PI * 0.5;
  }
  return aihc_math_atan(x / __builtin_sqrt(1.0 - x * x));
}

static double aihc_math_acos(double x) {
  return AIHC_PI * 0.5 - aihc_math_asin(x);
}

static double aihc_math_sinh(double x) {
  return (aihc_math_exp(x) - aihc_math_exp(-x)) * 0.5;
}

static double aihc_math_cosh(double x) {
  return (aihc_math_exp(x) + aihc_math_exp(-x)) * 0.5;
}

static double aihc_math_tanh(double x) {
  if (x > 20.0) {
    return 1.0;
  }
  if (x < -20.0) {
    return -1.0;
  }
  double e = aihc_math_exp(2.0 * x);
  return (e - 1.0) / (e + 1.0);
}

static double aihc_math_asinh(double x) {
  double magnitude = x < 0.0 ? -x : x;
  double result =
      aihc_math_log(magnitude + __builtin_sqrt(magnitude * magnitude + 1.0));
  return x < 0.0 ? -result : result;
}

static double aihc_math_acosh(double x) {
  if (aihc_double_is_nan(x) || x < 1.0) {
    return aihc_double_nan();
  }
  return aihc_math_log(x + __builtin_sqrt(x * x - 1.0));
}

static double aihc_math_atanh(double x) {
  if (aihc_double_is_nan(x) || x > 1.0 || x < -1.0) {
    return aihc_double_nan();
  }
  if (x == 1.0 || x == -1.0) {
    return x * aihc_double_infinity();
  }
  return 0.5 * aihc_math_log((1.0 + x) / (1.0 - x));
}

#define AIHC_DEFINE_DOUBLE_UNARY(name)                                         \
  uint64_t aihc_double_##name(uint64_t bits) {                                 \
    return aihc_double_to_bits(aihc_math_##name(aihc_double_from_bits(bits))); \
  }                                                                            \
  uint64_t aihc_float_##name(uint64_t bits) {                                  \
    return aihc_float_to_bits(                                                 \
        (float)aihc_math_##name((double)aihc_float_from_bits(bits)));          \
  }

AIHC_DEFINE_DOUBLE_UNARY(exp)
AIHC_DEFINE_DOUBLE_UNARY(log)
AIHC_DEFINE_DOUBLE_UNARY(sin)
AIHC_DEFINE_DOUBLE_UNARY(cos)
AIHC_DEFINE_DOUBLE_UNARY(tan)
AIHC_DEFINE_DOUBLE_UNARY(asin)
AIHC_DEFINE_DOUBLE_UNARY(acos)
AIHC_DEFINE_DOUBLE_UNARY(atan)
AIHC_DEFINE_DOUBLE_UNARY(sinh)
AIHC_DEFINE_DOUBLE_UNARY(cosh)
AIHC_DEFINE_DOUBLE_UNARY(tanh)
AIHC_DEFINE_DOUBLE_UNARY(asinh)
AIHC_DEFINE_DOUBLE_UNARY(acosh)
AIHC_DEFINE_DOUBLE_UNARY(atanh)

uint64_t aihc_double_pow(uint64_t left, uint64_t right) {
  return aihc_double_to_bits(
      aihc_math_pow(aihc_double_from_bits(left), aihc_double_from_bits(right)));
}

uint64_t aihc_float_pow(uint64_t left, uint64_t right) {
  return aihc_float_to_bits((float)aihc_math_pow(
      (double)aihc_float_from_bits(left), (double)aihc_float_from_bits(right)));
}
