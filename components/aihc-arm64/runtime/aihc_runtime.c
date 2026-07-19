#include "aihc_runtime.h"

#include <stdio.h>
#include <stdlib.h>

typedef struct {
  AihcSlot *args;
  AihcSlot *globals;
  void *exit_code;
} AihcMachine;

static _Noreturn void aihc_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
}

void aihc_unsupported_primitive(void) {
  aihc_fail("primitive is not implemented by the native runtime");
}

static void *aihc_allocate(size_t bytes) {
  void *pointer = calloc(1, bytes);
  if (pointer == NULL) {
    aihc_fail("out of memory");
  }
  return pointer;
}

static uintptr_t aihc_make_header(uint64_t tag, uintptr_t info) {
  switch (tag) {
  case AIHC_TAG_CLOSURE:
  case AIHC_TAG_THUNK:
    if ((info & AIHC_TAG_MASK) != 0) {
      aihc_fail("function entry is not aligned for pointer tagging");
    }
    return info | tag;
  case AIHC_TAG_NODE:
  case AIHC_TAG_PARTIAL_CONSTRUCTOR:
    return (info << AIHC_TAG_BITS) | tag;
  default:
    aihc_fail("attempted to allocate an invalid object tag");
  }
  return 0;
}

static AihcSlot aihc_make_shape(uint64_t arity, uint64_t count) {
  if (arity > UINT32_MAX || count > UINT32_MAX) {
    aihc_fail("object shape exceeds 32-bit limits");
  }
  return (arity << AIHC_SHAPE_ARITY_SHIFT) | count;
}

AihcValue *aihc_make_node(uint64_t tag, uintptr_t info, uint64_t arity,
                          uint64_t count) {
  uint64_t shape_words = tag == AIHC_TAG_CLOSURE || tag == AIHC_TAG_THUNK ||
                                 tag == AIHC_TAG_PARTIAL_CONSTRUCTOR
                             ? 1
                             : 0;
  uint64_t object_words = 1 + shape_words + count;
  if (object_words < 2) {
    object_words = 2;
  }
  AihcValue *value = aihc_allocate(sizeof(AihcSlot) * object_words);
  value->header = aihc_make_header(tag, info);
  if (shape_words != 0) {
    value->fields[0] = aihc_make_shape(arity, count);
  } else if (arity != 0) {
    aihc_fail("only functions and partial applications may carry shape");
  }
  return value;
}

static AihcValue *aihc_copy_with_fields(AihcValue *value, uint64_t result_tag,
                                        uint64_t result_arity, uint64_t count,
                                        const AihcSlot *fields) {
  uint64_t original_count = aihc_value_count(value);
  AihcValue *copy = aihc_make_node(result_tag, aihc_value_info(value),
                                   result_arity, original_count + count);
  AihcSlot *original_fields = aihc_value_fields(value);
  AihcSlot *copy_fields = aihc_value_fields(copy);
  for (uint64_t index = 0; index < original_count; ++index) {
    copy_fields[index] = original_fields[index];
  }
  for (uint64_t index = 0; index < count; ++index) {
    copy_fields[original_count + index] = fields[index];
  }
  return copy;
}

static AihcSlot *aihc_arguments(AihcValue *function, uint64_t count,
                                const AihcSlot *values,
                                AihcValue *continuation) {
  uint64_t field_count = aihc_value_count(function);
  uint64_t continuation_count = continuation == NULL ? 0 : 1;
  uint64_t total = field_count + count + continuation_count;
  AihcSlot *arguments =
      aihc_allocate(sizeof(*arguments) * (total == 0 ? 1 : total));
  AihcSlot *function_fields = aihc_value_fields(function);
  for (uint64_t index = 0; index < field_count; ++index) {
    arguments[index] = function_fields[index];
  }
  for (uint64_t index = 0; index < count; ++index) {
    arguments[field_count + index] = values[index];
  }
  if (continuation != NULL) {
    arguments[field_count + count] = (AihcSlot)continuation;
  }
  return arguments;
}

static void *aihc_prepare_entry(AihcMachine *machine, void *entry,
                                AihcSlot *arguments) {
  machine->args = arguments;
  return entry;
}

void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field) {
  aihc_value_fields(value)[index] = field;
}

AihcMachine *aihc_machine_new(uint64_t global_count) {
  AihcMachine *machine = aihc_allocate(sizeof(*machine));
  machine->globals = aihc_allocate(sizeof(*machine->globals) * global_count);
  return machine;
}

AihcSlot *aihc_alloc_locals(uint64_t count) {
  return aihc_allocate(sizeof(AihcSlot) * (count == 0 ? 1 : count));
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
  return aihc_prepare_entry(machine, (void *)aihc_value_info(continuation),
                            aihc_arguments(continuation, count, values, NULL));
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
      AihcValue *applied = aihc_copy_with_fields(function, AIHC_TAG_CLOSURE,
                                                 arity - 1, count, arguments);
      return aihc_continue_value(machine, continuation, (AihcSlot)applied);
    }
    if (arity == 0) {
      aihc_fail("saturated closure was applied");
    }
    return aihc_prepare_entry(
        machine, (void *)aihc_value_info(function),
        aihc_arguments(function, count, arguments, continuation));
  }
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity > 1) {
      AihcValue *applied = aihc_copy_with_fields(
          function, AIHC_TAG_PARTIAL_CONSTRUCTOR, arity - 1, count, arguments);
      return aihc_continue_value(machine, continuation, (AihcSlot)applied);
    }
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    AihcValue *applied =
        aihc_copy_with_fields(function, AIHC_TAG_NODE, 0, count, arguments);
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
    value->header = AIHC_TAG_BLACKHOLE;
    return aihc_prepare_entry(machine, entry, arguments);
  }
  case AIHC_TAG_INDIRECTION:
    if (result_is_lifted) {
      return aihc_eval_cps(machine, (AihcValue *)value->fields[0], 1,
                           continuation, update_continuation);
    } else {
      return aihc_continue_value(machine, continuation, value->fields[0]);
    }
  case AIHC_TAG_BLACKHOLE:
    aihc_fail("blackholed thunk re-entered");
  default:
    return aihc_continue_value(machine, continuation, (AihcSlot)value);
  }
}

void aihc_update(AihcValue *object, AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  object->fields[0] = (AihcSlot)value;
  object->header = AIHC_TAG_INDIRECTION;
}

void aihc_update_blackhole(AihcValue *object, AihcValue *value) {
  if (object == NULL || aihc_value_tag(object) != AIHC_TAG_BLACKHOLE) {
    aihc_fail("attempted to update a cell that is not blackholed");
  }
  aihc_update(object, value);
}

void *aihc_halt(AihcMachine *machine) { return machine->exit_code; }

void *aihc_start(AihcMachine *machine, AihcValue *root, AihcValue *continuation,
                 AihcValue *update_continuation, void *exit_code) {
  machine->exit_code = exit_code;
  return aihc_eval_cps(machine, root, 1, continuation, update_continuation);
}
