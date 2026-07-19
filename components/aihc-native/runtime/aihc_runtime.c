#include "aihc_runtime.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct AihcBlackholeWaiter AihcBlackholeWaiter;

struct AihcThread {
  uintptr_t header;
  void *entry;
  AihcSlot *args;
  const AihcInfo *args_info;
  uint64_t args_trailing_pointers;
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

_Static_assert(offsetof(AihcMachine, args) == 0, "machine args ABI");
_Static_assert(offsetof(AihcMachine, globals) == 8, "machine globals ABI");
_Static_assert(offsetof(AihcMachine, heap_next) == 32, "machine heap-next ABI");
_Static_assert(offsetof(AihcMachine, heap_limit) == 40,
               "machine heap-limit ABI");

static _Noreturn void aihc_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
}

void aihc_unsupported_primitive(void) {
  aihc_fail("primitive is not implemented by the native runtime");
}

static void *aihc_allocate_auxiliary(size_t bytes) {
  void *pointer = calloc(1, bytes);
  if (pointer == NULL) {
    aihc_fail("out of memory");
  }
  return pointer;
}

static uintptr_t aihc_make_header(uint64_t tag, const AihcInfo *info) {
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

static void aihc_forward_arguments(AihcMachine *machine, uint8_t *from_start,
                                   AihcSlot *arguments, const AihcInfo *info,
                                   uint64_t trailing_pointers) {
  if (arguments == NULL || info == NULL) {
    return;
  }
  for (uint64_t index = 0; index < info->field_count; ++index) {
    if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
      aihc_forward_slot(machine, from_start, &arguments[index]);
    }
  }
  for (uint64_t index = 0; index < trailing_pointers; ++index) {
    aihc_forward_slot(machine, from_start,
                      &arguments[info->field_count + index]);
  }
}

static void aihc_forward_thread(AihcMachine *machine, uint8_t *from_start,
                                AihcThread *thread) {
  if (thread != NULL) {
    aihc_forward_arguments(machine, from_start, thread->args, thread->args_info,
                           thread->args_trailing_pointers);
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
  aihc_forward_arguments(machine, from_start, machine->args, machine->args_info,
                         machine->args_trailing_pointers);
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
  (void)machine;
  value = aihc_allocate_auxiliary(sizeof(AihcSlot) * words);
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
  value->header = aihc_make_header(tag, info);
  return value;
}

AihcValue *aihc_make_node(AihcMachine *machine, uint64_t tag,
                          const AihcInfo *info) {
  uint64_t words = aihc_object_words(tag, info);
  aihc_ensure_heap(machine, words, 0, NULL);
  return aihc_make_node_unchecked(machine, tag, info);
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

static AihcSlot *aihc_arguments(AihcValue *function, uint64_t count,
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
  AihcSlot *arguments =
      aihc_allocate_auxiliary(sizeof(*arguments) * (total == 0 ? 1 : total));
  const AihcSlot *function_fields = aihc_value_fields_const(function);
  for (size_t index = 0; index < (size_t)field_count; ++index) {
    arguments[index] = function_fields[index];
  }
  for (size_t index = 0; index < (size_t)count; ++index) {
    arguments[(size_t)field_count + index] = values[index];
  }
  if (continuation != NULL) {
    arguments[(size_t)field_count + (size_t)count] = (AihcSlot)continuation;
  }
  return arguments;
}

static void *aihc_prepare_entry(AihcMachine *machine, void *entry,
                                AihcSlot *arguments,
                                const AihcInfo *arguments_info,
                                uint64_t trailing_pointers) {
  machine->args = arguments;
  machine->args_info = arguments_info;
  machine->args_trailing_pointers = trailing_pointers;
  return entry;
}

static AihcThread *aihc_thread_new(void) {
  AihcThread *thread = aihc_allocate_auxiliary(sizeof(*thread));
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

static void *aihc_select_thread(AihcMachine *machine, AihcThread *thread) {
  machine->current_thread = thread;
  machine->args = thread->args;
  machine->args_info = thread->args_info;
  machine->args_trailing_pointers = thread->args_trailing_pointers;
  return thread->entry;
}

static AihcBlackhole *aihc_find_blackhole(AihcMachine *machine,
                                          AihcValue *object) {
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    if (blackhole->object == object) {
      return blackhole;
    }
  }
  AihcBlackhole *blackhole = aihc_allocate_auxiliary(sizeof(*blackhole));
  blackhole->object = object;
  blackhole->owner = machine->current_thread;
  blackhole->next = machine->blackholes;
  machine->blackholes = blackhole;
  return blackhole;
}

static void aihc_block_on_blackhole(AihcMachine *machine, AihcValue *object,
                                    AihcValue *continuation) {
  AihcBlackhole *blackhole = aihc_find_blackhole(machine, object);
  if (blackhole->owner == machine->current_thread) {
    aihc_fail("blackholed thunk re-entered");
  }
  AihcBlackholeWaiter *waiter = aihc_allocate_auxiliary(sizeof(*waiter));
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
  AihcMachine *machine = aihc_allocate_auxiliary(sizeof(*machine));
  machine->global_count = global_count;
  machine->globals = aihc_allocate_auxiliary(
      sizeof(*machine->globals) * (global_count == 0 ? 1 : global_count));
#if AIHC_GC == AIHC_GC_CALLOC
  machine->heap_next = NULL;
  machine->heap_limit = (uint8_t *)UINTPTR_MAX;
#elif AIHC_GC == AIHC_GC_SEMISPACE
  machine->semispace_bytes = AIHC_SEMISPACE_BYTES;
  machine->heap_start = aihc_allocate_auxiliary(machine->semispace_bytes);
  machine->other_space = aihc_allocate_auxiliary(machine->semispace_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->semispace_bytes;
#endif
  machine->current_thread = aihc_thread_new();
  return machine;
}

AihcSlot *aihc_alloc_locals(uint64_t count) {
  return aihc_allocate_auxiliary(sizeof(AihcSlot) * (count == 0 ? 1 : count));
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

void *aihc_continue_values(AihcMachine *machine, AihcValue *continuation,
                           uint64_t count, const AihcSlot *values) {
  if (continuation == NULL ||
      aihc_value_tag(continuation) != AIHC_TAG_CLOSURE) {
    aihc_fail("attempted to invoke a non-continuation value");
  }
  if (aihc_value_arity(continuation) != 1) {
    aihc_fail("continuation closure does not accept exactly one result");
  }
  const AihcInfo *arguments_info =
      aihc_next_application_info(aihc_value_info_table(continuation), count);
  return aihc_prepare_entry(machine, (void *)aihc_value_info(continuation),
                            aihc_arguments(continuation, count, values, NULL),
                            arguments_info, 0);
}

static void *aihc_continue_value(AihcMachine *machine, AihcValue *continuation,
                                 AihcSlot value) {
  return aihc_continue_values(machine, continuation, 1, &value);
}

void *aihc_apply_cps(AihcMachine *machine, AihcValue *function, uint64_t count,
                     const AihcSlot *arguments, AihcValue *continuation) {
  if (function == NULL) {
    aihc_fail("attempted to apply null");
  }
  switch (aihc_value_tag(function)) {
  case AIHC_TAG_CLOSURE: {
    uint64_t arity = aihc_value_arity(function);
    if (arity > 1) {
      AihcValue *applied =
          aihc_copy_with_fields(machine, &function, AIHC_TAG_CLOSURE, count,
                                arguments, &continuation);
      return aihc_continue_value(machine, continuation, (AihcSlot)applied);
    }
    if (arity == 0) {
      aihc_fail("saturated closure was applied");
    }
    const AihcInfo *arguments_info =
        aihc_next_application_info(aihc_value_info_table(function), count);
    return aihc_prepare_entry(
        machine, (void *)aihc_value_info(function),
        aihc_arguments(function, count, arguments, continuation),
        arguments_info, continuation == NULL ? 0 : 1);
  }
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity > 1) {
      AihcValue *applied = aihc_copy_with_fields(
          machine, &function, AIHC_TAG_PARTIAL_CONSTRUCTOR, count, arguments,
          &continuation);
      return aihc_continue_value(machine, continuation, (AihcSlot)applied);
    }
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    AihcValue *applied = aihc_copy_with_fields(
        machine, &function, AIHC_TAG_NODE, count, arguments, &continuation);
    return aihc_continue_value(machine, continuation, (AihcSlot)applied);
  }
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

void *aihc_eval_cps(AihcMachine *machine, AihcValue *value,
                    uint64_t result_is_lifted, AihcValue *continuation,
                    AihcValue *update_continuation) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  switch (aihc_value_tag(value)) {
  case AIHC_TAG_THUNK: {
    AihcSlot *arguments = aihc_arguments(value, 0, NULL, update_continuation);
    void *entry = (void *)aihc_value_info(value);
    const AihcInfo *arguments_info = aihc_value_info_table(value);
    value->header = (value->header & ~AIHC_TAG_MASK) | AIHC_TAG_BLACKHOLE;
    aihc_find_blackhole(machine, value);
    return aihc_prepare_entry(machine, entry, arguments, arguments_info, 1);
  }
  case AIHC_TAG_INDIRECTION:
    if (result_is_lifted) {
      return aihc_eval_cps(machine, (AihcValue *)value->fields[0], 1,
                           continuation, update_continuation);
    }
    return aihc_continue_value(machine, continuation, value->fields[0]);
  case AIHC_TAG_BLACKHOLE:
    aihc_block_on_blackhole(machine, value, continuation);
    return aihc_select_thread(machine, aihc_dequeue_thread(machine));
  default:
    return aihc_continue_value(machine, continuation, (AihcSlot)value);
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
    waiter->thread->entry =
        aihc_continue_value(machine, waiter->continuation, (AihcSlot)value);
    waiter->thread->args = machine->args;
    waiter->thread->args_info = machine->args_info;
    waiter->thread->args_trailing_pointers = machine->args_trailing_pointers;
    aihc_enqueue_thread(machine, waiter->thread);
    free(waiter);
    waiter = next;
  }
  free(blackhole);
}

void *aihc_fork_cps(AihcMachine *machine, AihcValue *action,
                    AihcValue *continuation) {
  if (machine->thread_done_continuation == NULL) {
    aihc_fail("thread completion continuation is not initialized");
  }
  AihcThread *child = aihc_thread_new();
  child->entry = aihc_apply_cps(machine, action, 0, NULL,
                                machine->thread_done_continuation);
  child->args = machine->args;
  child->args_info = machine->args_info;
  child->args_trailing_pointers = machine->args_trailing_pointers;
  aihc_enqueue_thread(machine, child);
  AihcSlot thread_id = (AihcSlot)child;
  return aihc_continue_values(machine, continuation, 1, &thread_id);
}

void *aihc_yield_cps(AihcMachine *machine, AihcValue *continuation) {
  AihcThread *current = machine->current_thread;
  current->entry = aihc_continue_values(machine, continuation, 0, NULL);
  current->args = machine->args;
  current->args_info = machine->args_info;
  current->args_trailing_pointers = machine->args_trailing_pointers;
  aihc_enqueue_thread(machine, current);
  return aihc_select_thread(machine, aihc_dequeue_thread(machine));
}

void *aihc_thread_done(AihcMachine *machine) {
  return aihc_select_thread(machine, aihc_dequeue_thread(machine));
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

void *aihc_halt(AihcMachine *machine) { return machine->exit_code; }

void *aihc_start(AihcMachine *machine, AihcValue *root, AihcValue *continuation,
                 AihcValue *update_continuation,
                 AihcValue *thread_done_continuation, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_set_thread_done_continuation(machine, thread_done_continuation);
  return aihc_eval_cps(machine, root, 1, continuation, update_continuation);
}
