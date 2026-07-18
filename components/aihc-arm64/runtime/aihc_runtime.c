#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum {
  AIHC_LITERAL_INT = 0,
  AIHC_CONSTRUCTOR = 1,
  AIHC_CLOSURE = 2,
  AIHC_THUNK = 3,
  AIHC_PRIMITIVE = 4,
  AIHC_CELL = 5,
  AIHC_STATE_TOKEN = 6,
};

enum {
  AIHC_CELL_SUSPENDED = 0,
  AIHC_CELL_VALUE = 1,
  AIHC_CELL_BLACKHOLE = 2,
};

enum {
  AIHC_CONT_NORMAL = 0,
  AIHC_CONT_UPDATE = 1,
  AIHC_CONT_APPLY = 2,
  AIHC_CONT_TOP = 3,
  AIHC_CONT_FINAL = 4,
};

enum {
  AIHC_PRIM_REAL_WORLD = 1,
};

typedef struct AihcValue AihcValue;
typedef struct AihcContinuation AihcContinuation;
/* Every runtime location is one machine word. Static RuntimeRep metadata says
 * whether that word is a heap pointer or an unboxed payload. */
typedef uintptr_t AihcSlot;

struct AihcValue {
  uint64_t kind;
  uintptr_t info;
  uint64_t arity;
  uint64_t count;
  AihcSlot fields[];
};

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
      AihcValue *cell;
    } update;
    struct {
      AihcSlot *arguments;
      uint64_t count;
    } apply;
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

static AihcValue *aihc_copy_with_fields(AihcValue *value, uint64_t count,
                                        const AihcSlot *fields) {
  AihcValue *copy = aihc_allocate(
      sizeof(*copy) + sizeof(AihcSlot) * (value->count + count));
  copy->kind = value->kind;
  copy->info = value->info;
  copy->arity = value->arity;
  copy->count = value->count + count;
  for (uint64_t index = 0; index < value->count; ++index) {
    copy->fields[index] = value->fields[index];
  }
  for (uint64_t index = 0; index < count; ++index) {
    copy->fields[value->count + index] = fields[index];
  }
  return copy;
}

static AihcSlot *aihc_arguments_with_fields(AihcValue *function,
                                             uint64_t count,
                                             const AihcSlot *fields) {
  uint64_t total = function->count + count;
  AihcSlot *arguments =
      aihc_allocate(sizeof(*arguments) * (total == 0 ? 1 : total));
  for (uint64_t index = 0; index < function->count; ++index) {
    arguments[index] = function->fields[index];
  }
  for (uint64_t index = 0; index < count; ++index) {
    arguments[function->count + index] = fields[index];
  }
  return arguments;
}

static void aihc_return_values_internal(AihcMachine *machine, uint64_t count,
                                        const AihcSlot *values);
static void aihc_return_value(AihcMachine *machine, AihcSlot value);
static void aihc_eval_value(AihcMachine *machine, AihcValue *value,
                            uint64_t result_is_lifted);
static void aihc_apply_values_internal(AihcMachine *machine,
                                       AihcValue *function, uint64_t count,
                                       const AihcSlot *arguments);

AihcValue *aihc_make_node(uint64_t kind, uintptr_t info, uint64_t arity,
                          uint64_t count) {
  AihcValue *value = aihc_allocate(sizeof(*value) + sizeof(AihcSlot) * count);
  value->kind = kind;
  value->info = info;
  value->arity = arity;
  value->count = count;
  return value;
}

AihcValue *aihc_make_cell(void) {
  return aihc_make_node(AIHC_CELL, AIHC_CELL_SUSPENDED, 1, 1);
}

void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field) {
  if (index >= value->count) {
    aihc_fail("field index out of bounds");
  }
  value->fields[index] = field;
}

void aihc_set_cell(AihcValue *cell, AihcValue *value) {
  if (cell->kind != AIHC_CELL) {
    aihc_fail("attempted to initialize a non-cell");
  }
  cell->info =
      value->kind == AIHC_THUNK ? AIHC_CELL_SUSPENDED : AIHC_CELL_VALUE;
  cell->fields[0] = (AihcSlot)value;
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
  machine->next = (void *)function->info;
  machine->args = arguments;
  machine->locals = NULL;
}

static void aihc_apply_forced(AihcMachine *machine, AihcValue *function,
                              uint64_t count,
                              const AihcSlot *arguments) {
  switch (function->kind) {
  case AIHC_CLOSURE:
    if (count < function->arity) {
      AihcValue *applied = aihc_copy_with_fields(function, count, arguments);
      applied->arity -= count;
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (count > function->arity) {
      aihc_fail("closure received the wrong number of values");
    }
    aihc_schedule_function(machine, function,
                           aihc_arguments_with_fields(function, count,
                                                      arguments));
    return;
  case AIHC_CONSTRUCTOR: {
    AihcValue *applied = aihc_copy_with_fields(function, count, arguments);
    if (applied->count < applied->arity) {
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (applied->count > applied->arity) {
      aihc_fail("overapplication is not implemented");
    }
    aihc_return_value(machine, (AihcSlot)applied);
    return;
  }
  case AIHC_PRIMITIVE:
    aihc_fail("primitive is not implemented by the native runtime");
    return;
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_eval_value(AihcMachine *machine, AihcValue *value,
                            uint64_t result_is_lifted) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  if (value->kind == AIHC_CELL) {
    switch (value->info) {
    case AIHC_CELL_SUSPENDED: {
      AihcValue *thunk = (AihcValue *)value->fields[0];
      if (thunk == NULL || thunk->kind != AIHC_THUNK) {
        aihc_fail("suspended cell does not contain a thunk");
      }
      value->info = AIHC_CELL_BLACKHOLE;
      AihcContinuation *continuation =
          aihc_push_special(machine, AIHC_CONT_UPDATE);
      continuation->payload.update.cell = value;
      aihc_schedule_function(machine, thunk, thunk->fields);
      return;
    }
    case AIHC_CELL_VALUE:
      if (result_is_lifted) {
        aihc_eval_value(machine, (AihcValue *)value->fields[0], 1);
      } else {
        aihc_return_value(machine, value->fields[0]);
      }
      return;
    case AIHC_CELL_BLACKHOLE:
      aihc_fail("blackholed thunk re-entered");
    default:
      aihc_fail("unknown cell state");
    }
  }
  if (value->kind == AIHC_PRIMITIVE && value->arity == 0) {
    if (value->info == AIHC_PRIM_REAL_WORLD) {
      aihc_return_value(machine, 0);
      return;
    }
    aihc_fail("unknown zero-arity primitive");
  }
  aihc_return_value(machine, (AihcSlot)value);
}

static void aihc_apply_values_internal(AihcMachine *machine,
                                       AihcValue *function, uint64_t count,
                                       const AihcSlot *arguments) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_APPLY);
  continuation->payload.apply.arguments = (AihcSlot *)arguments;
  continuation->payload.apply.count = count;
  aihc_eval_value(machine, function, 1);
}

static void aihc_run_io(AihcMachine *machine, AihcValue *value) {
  aihc_push_special(machine, AIHC_CONT_FINAL);
  aihc_apply_values_internal(machine, value, 0, NULL);
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
    continuation->payload.update.cell->info = AIHC_CELL_VALUE;
    continuation->payload.update.cell->fields[0] = values[0];
    /* A stored constructor is itself represented by a value cell. Continue
     * forcing the thunk result so the awaiting continuation receives the
     * constructor node rather than that intermediate heap location. */
    aihc_eval_value(machine, (AihcValue *)values[0], 1);
    return;
  case AIHC_CONT_APPLY:
    if (count != 1) {
      aihc_fail("function evaluation returned multiple values");
    }
    aihc_apply_forced(machine, (AihcValue *)values[0],
                      continuation->payload.apply.count,
                      continuation->payload.apply.arguments);
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
  aihc_apply_values_internal(machine, function, 1, &argument);
}

void aihc_apply_values(AihcMachine *machine, AihcValue *function,
                       uint64_t count, const AihcSlot *arguments) {
  aihc_apply_values_internal(machine, function, count, arguments);
}

void aihc_start(AihcMachine *machine, AihcValue *root, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_push_special(machine, AIHC_CONT_TOP);
  aihc_eval_value(machine, root, 1);
}
