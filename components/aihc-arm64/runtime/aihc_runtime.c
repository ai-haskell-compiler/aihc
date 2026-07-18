#include "aihc_runtime.h"

#include <stdio.h>
#include <stdlib.h>

enum {
  AIHC_CONT_NORMAL = 0,
  AIHC_CONT_UPDATE = 1,
  AIHC_CONT_TOP = 2,
  AIHC_CONT_FINAL = 3,
};

typedef struct AihcContinuation AihcContinuation;

struct AihcContinuation {
  uint64_t kind;
  AihcContinuation *parent;
  union {
    struct {
      void *code;
      AihcSlot *locals;
      uint64_t slot;
      uint64_t count;
    } normal;
    struct {
      AihcValue *object;
    } update;
  } payload;
};

typedef struct {
  void *next;
  AihcSlot *args;
  AihcContinuation *continuation;
  AihcSlot *globals;
  AihcSlot *locals;
  void *exit_code;
} AihcMachine;

static void aihc_fail(const char *message) {
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

static AihcContinuation *aihc_push_special(AihcMachine *machine,
                                            uint64_t kind) {
  AihcContinuation *continuation = aihc_allocate(sizeof(*continuation));
  continuation->kind = kind;
  continuation->parent = machine->continuation;
  machine->continuation = continuation;
  return continuation;
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
  uint64_t shape_words =
      tag == AIHC_TAG_CLOSURE || tag == AIHC_TAG_PARTIAL_CONSTRUCTOR ? 1 : 0;
  uint64_t object_words = 1 + shape_words + count;
  if (object_words < 2) {
    object_words = 2;
  }
  AihcValue *value = aihc_allocate(sizeof(AihcSlot) * object_words);
  value->header = aihc_make_header(tag, info);
  if (shape_words != 0) {
    value->fields[0] = aihc_make_shape(arity, count);
  } else if (arity != 0) {
    aihc_fail("only partial applications may carry dynamic arity");
  }
  return value;
}

static AihcValue *aihc_copy_with_fields(AihcValue *value, uint64_t result_tag,
                                        uint64_t result_arity,
                                        uint64_t count,
                                        const AihcSlot *fields) {
  uint64_t original_count = aihc_value_count(value);
  AihcValue *copy =
      aihc_make_node(result_tag, aihc_value_info(value), result_arity,
                     original_count + count);
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

static AihcSlot *aihc_arguments_with_fields(AihcValue *function,
                                             uint64_t count,
                                             const AihcSlot *fields) {
  uint64_t field_count = aihc_value_count(function);
  uint64_t total = field_count + count;
  AihcSlot *arguments =
      aihc_allocate(sizeof(*arguments) * (total == 0 ? 1 : total));
  AihcSlot *function_fields = aihc_value_fields(function);
  for (uint64_t index = 0; index < field_count; ++index) {
    arguments[index] = function_fields[index];
  }
  for (uint64_t index = 0; index < count; ++index) {
    arguments[field_count + index] = fields[index];
  }
  return arguments;
}

static void aihc_return_values_internal(AihcMachine *machine, uint64_t count,
                                        const AihcSlot *values);
static void aihc_return_value(AihcMachine *machine, AihcSlot value);
static void aihc_eval_value(AihcMachine *machine, AihcValue *value,
                            uint64_t result_is_lifted);

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

void aihc_push_normal(AihcMachine *machine, void *code, AihcSlot *locals,
                      uint64_t slot, uint64_t count) {
  AihcContinuation *continuation = aihc_push_special(machine, AIHC_CONT_NORMAL);
  continuation->payload.normal.code = code;
  continuation->payload.normal.locals = locals;
  continuation->payload.normal.slot = slot;
  continuation->payload.normal.count = count;
}

static void aihc_schedule_function(AihcMachine *machine, AihcValue *function,
                                   AihcSlot *arguments) {
  machine->next = (void *)aihc_value_info(function);
  machine->args = arguments;
  machine->locals = NULL;
}

static void aihc_apply_forced(AihcMachine *machine, AihcValue *function,
                              uint64_t count,
                              const AihcSlot *arguments) {
  switch (aihc_value_tag(function)) {
  case AIHC_TAG_CLOSURE: {
    uint64_t arity = aihc_value_arity(function);
    if (arity > 1) {
      AihcValue *applied = aihc_copy_with_fields(
          function, AIHC_TAG_CLOSURE, arity - 1, count, arguments);
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (arity == 0) {
      aihc_fail("saturated closure was applied");
    }
    aihc_schedule_function(machine, function,
                           aihc_arguments_with_fields(function, count,
                                                      arguments));
    return;
  }
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity > 1) {
      AihcValue *applied = aihc_copy_with_fields(
          function, AIHC_TAG_PARTIAL_CONSTRUCTOR, arity - 1, count,
          arguments);
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    AihcValue *applied = aihc_copy_with_fields(
        function, AIHC_TAG_NODE, 0, count, arguments);
    aihc_return_value(machine, (AihcSlot)applied);
    return;
  }
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_eval_value(AihcMachine *machine, AihcValue *value,
                            uint64_t result_is_lifted) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  switch (aihc_value_tag(value)) {
  case AIHC_TAG_THUNK: {
    uintptr_t entry = aihc_value_info(value);
    AihcSlot *arguments = aihc_value_fields(value);
    value->header = AIHC_TAG_BLACKHOLE;
    AihcContinuation *continuation =
        aihc_push_special(machine, AIHC_CONT_UPDATE);
    continuation->payload.update.object = value;
    machine->next = (void *)entry;
    machine->args = arguments;
    machine->locals = NULL;
    return;
  }
  case AIHC_TAG_INDIRECTION:
    if (result_is_lifted) {
      aihc_eval_value(machine, (AihcValue *)value->fields[0], 1);
    } else {
      aihc_return_value(machine, value->fields[0]);
    }
    return;
  case AIHC_TAG_BLACKHOLE:
    aihc_fail("blackholed thunk re-entered");
  default:
    break;
  }
  aihc_return_value(machine, (AihcSlot)value);
}

static void aihc_run_io(AihcMachine *machine, AihcValue *value) {
  aihc_push_special(machine, AIHC_CONT_FINAL);
  aihc_apply_forced(machine, value, 0, NULL);
}

static void aihc_return_values_internal(AihcMachine *machine, uint64_t count,
                                        const AihcSlot *values) {
  AihcContinuation *continuation = machine->continuation;
  if (continuation == NULL) {
    aihc_fail("returned without a continuation");
  }
  machine->continuation = continuation->parent;
  switch (continuation->kind) {
  case AIHC_CONT_NORMAL: {
    if (count != continuation->payload.normal.count) {
      aihc_fail("continuation received the wrong number of values");
    }
    for (uint64_t index = 0; index < count; ++index) {
      continuation->payload.normal.locals
          [continuation->payload.normal.slot + index] = values[index];
    }
    machine->locals = continuation->payload.normal.locals;
    machine->next = continuation->payload.normal.code;
    return;
  }
  case AIHC_CONT_UPDATE:
    if (count != 1) {
      aihc_fail("attempted to update a thunk with multiple values");
    }
    continuation->payload.update.object->fields[0] = values[0];
    continuation->payload.update.object->header = AIHC_TAG_INDIRECTION;
    /* Continue forcing through the freshly installed indirection so the
     * awaiting continuation receives the result in weak-head normal form. */
    aihc_eval_value(machine, (AihcValue *)values[0], 1);
    return;
  case AIHC_CONT_TOP:
    if (count != 1) {
      aihc_fail("top-level evaluation returned multiple values");
    }
    aihc_run_io(machine, (AihcValue *)values[0]);
    return;
  case AIHC_CONT_FINAL:
    if (count != 1) {
      aihc_fail("IO action returned the wrong number of values");
    }
    machine->locals = NULL;
    machine->next = machine->exit_code;
    return;
  default:
    aihc_fail("unknown continuation kind");
  }
}

static void aihc_return_value(AihcMachine *machine, AihcSlot value) {
  aihc_return_values_internal(machine, 1, &value);
}

void aihc_return(AihcMachine *machine, AihcSlot value) {
  aihc_return_value(machine, value);
}

void aihc_return_values(AihcMachine *machine, uint64_t count,
                        const AihcSlot *values) {
  aihc_return_values_internal(machine, count, values);
}

void aihc_eval(AihcMachine *machine, AihcValue *value,
               uint64_t result_is_lifted) {
  aihc_eval_value(machine, value, result_is_lifted);
}

void aihc_apply(AihcMachine *machine, AihcValue *function,
                AihcSlot argument) {
  aihc_apply_forced(machine, function, 1, &argument);
}

void aihc_apply_values(AihcMachine *machine, AihcValue *function,
                       uint64_t count, const AihcSlot *arguments) {
  aihc_apply_forced(machine, function, count, arguments);
}

void aihc_start(AihcMachine *machine, AihcValue *root, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_push_special(machine, AIHC_CONT_TOP);
  aihc_eval_value(machine, root, 1);
}
