#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum {
  AIHC_LITERAL_INT = 0,
  AIHC_CONSTRUCTOR = 1,
  AIHC_CLOSURE = 2,
  AIHC_THUNK = 3,
  AIHC_PRIMITIVE = 4,
  AIHC_FOREIGN = 5,
  AIHC_FOREIGN_IO = 6,
  AIHC_DICTIONARY = 7,
  AIHC_CELL = 8,
  AIHC_STATE_TOKEN = 9,
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
  AIHC_CONT_FOREIGN_CINT = 5,
  AIHC_CONT_SELECT = 6,
};

enum {
  AIHC_PRIM_REAL_WORLD = 1,
};

typedef struct AihcValue AihcValue;
typedef struct AihcContinuation AihcContinuation;
/* Every runtime location is one machine word. Static RuntimeRep metadata says
 * whether that word is a heap pointer or an unboxed payload. */
typedef uintptr_t AihcSlot;

typedef struct {
  void *function;
  uint64_t is_io;
  uint64_t int32_constructor;
  uint64_t tuple_constructor;
} AihcForeign;

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
    } normal;
    struct {
      AihcValue *cell;
    } update;
    struct {
      AihcSlot argument;
    } apply;
    struct {
      AihcSlot state;
      AihcForeign *descriptor;
    } foreign_call;
    struct {
      uint64_t index;
      uint64_t result_is_lifted;
    } select;
  } payload;
};

typedef struct {
  void *next;
  AihcSlot *args;
  AihcContinuation *continuation;
  AihcSlot *globals;
  AihcSlot *locals;
  void *exit_code;
  uint64_t tuple_constructor;
} AihcMachine;

static void aihc_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
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

static AihcValue *aihc_copy_with_field(AihcValue *value, AihcSlot field) {
  AihcValue *copy = aihc_allocate(sizeof(*copy) + sizeof(AihcSlot) * (value->count + 1));
  copy->kind = value->kind;
  copy->info = value->info;
  copy->arity = value->arity;
  copy->count = value->count + 1;
  for (uint64_t index = 0; index < value->count; ++index) {
    copy->fields[index] = value->fields[index];
  }
  copy->fields[value->count] = field;
  return copy;
}

static AihcSlot *aihc_arguments_with_field(AihcValue *function,
                                            AihcSlot argument) {
  AihcSlot *arguments = aihc_allocate(sizeof(*arguments) * (function->count + 1));
  for (uint64_t index = 0; index < function->count; ++index) {
    arguments[index] = function->fields[index];
  }
  arguments[function->count] = argument;
  return arguments;
}

static void aihc_return_value(AihcMachine *machine, AihcSlot value);
static void aihc_eval_value(AihcMachine *machine, AihcValue *value,
                            uint64_t result_is_lifted);
static void aihc_apply_value(AihcMachine *machine, AihcValue *function,
                             AihcSlot argument);

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
  cell->info = AIHC_CELL_SUSPENDED;
  cell->fields[0] = (AihcSlot)value;
}

AihcMachine *aihc_machine_new(uint64_t global_count,
                              uint64_t tuple_constructor) {
  AihcMachine *machine = aihc_allocate(sizeof(*machine));
  machine->globals = aihc_allocate(sizeof(*machine->globals) * global_count);
  machine->tuple_constructor = tuple_constructor;
  return machine;
}

AihcSlot *aihc_alloc_locals(uint64_t count) {
  return aihc_allocate(sizeof(AihcSlot) * (count == 0 ? 1 : count));
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

void aihc_push_normal(AihcMachine *machine, void *code, AihcSlot *locals,
                      uint64_t slot) {
  AihcContinuation *continuation = aihc_push_special(machine, AIHC_CONT_NORMAL);
  continuation->payload.normal.code = code;
  continuation->payload.normal.locals = locals;
  continuation->payload.normal.slot = slot;
}

static void aihc_schedule_function(AihcMachine *machine, AihcValue *function,
                                   AihcSlot *arguments) {
  machine->next = (void *)function->info;
  machine->args = arguments;
  machine->locals = NULL;
}

static AihcValue *aihc_constructor(uint64_t constructor, uint64_t arity,
                                   uint64_t count) {
  return aihc_make_node(AIHC_CONSTRUCTOR, constructor, arity, count);
}

static void aihc_complete_foreign(AihcMachine *machine, AihcValue *foreign,
                                  AihcSlot state) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_FOREIGN_CINT);
  continuation->payload.foreign_call.state = state;
  continuation->payload.foreign_call.descriptor =
      (AihcForeign *)foreign->info;
  aihc_eval_value(machine, (AihcValue *)foreign->fields[0], 1);
}

static void aihc_apply_forced(AihcMachine *machine, AihcValue *function,
                              AihcSlot argument) {
  switch (function->kind) {
  case AIHC_CLOSURE:
    aihc_schedule_function(machine, function,
                           aihc_arguments_with_field(function, argument));
    return;
  case AIHC_CONSTRUCTOR:
  case AIHC_PRIMITIVE:
  case AIHC_FOREIGN: {
    AihcValue *applied = aihc_copy_with_field(function, argument);
    if (applied->count < applied->arity) {
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (applied->count > applied->arity) {
      aihc_fail("overapplication is not implemented");
    }
    if (applied->kind == AIHC_FOREIGN) {
      AihcForeign *descriptor = (AihcForeign *)applied->info;
      if (!descriptor->is_io) {
        aihc_fail("pure foreign calls are not implemented");
      }
      AihcValue *action = aihc_make_node(AIHC_FOREIGN_IO, applied->info,
                                         applied->arity, applied->count);
      for (uint64_t index = 0; index < applied->count; ++index) {
        action->fields[index] = applied->fields[index];
      }
      aihc_return_value(machine, (AihcSlot)action);
      return;
    }
    aihc_return_value(machine, (AihcSlot)applied);
    return;
  }
  case AIHC_FOREIGN_IO:
    aihc_complete_foreign(machine, function, argument);
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

static void aihc_apply_value(AihcMachine *machine, AihcValue *function,
                             AihcSlot argument) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_APPLY);
  continuation->payload.apply.argument = argument;
  aihc_eval_value(machine, function, 1);
}

static void aihc_run_io(AihcMachine *machine, AihcValue *value) {
  aihc_push_special(machine, AIHC_CONT_FINAL);
  aihc_apply_value(machine, value, 0);
}

static void aihc_return_value(AihcMachine *machine, AihcSlot value) {
  AihcContinuation *continuation = machine->continuation;
  if (continuation == NULL) {
    aihc_fail("returned without a continuation");
  }
  machine->continuation = continuation->parent;
  switch (continuation->kind) {
  case AIHC_CONT_NORMAL:
    continuation->payload.normal.locals
        [continuation->payload.normal.slot] = value;
    machine->locals = continuation->payload.normal.locals;
    machine->next = continuation->payload.normal.code;
    return;
  case AIHC_CONT_UPDATE:
    continuation->payload.update.cell->info = AIHC_CELL_VALUE;
    continuation->payload.update.cell->fields[0] = value;
    aihc_return_value(machine, value);
    return;
  case AIHC_CONT_APPLY:
    aihc_apply_forced(machine, (AihcValue *)value,
                      continuation->payload.apply.argument);
    return;
  case AIHC_CONT_TOP:
    aihc_run_io(machine, (AihcValue *)value);
    return;
  case AIHC_CONT_FINAL: {
    AihcValue *tuple = (AihcValue *)value;
    if (tuple->kind != AIHC_CONSTRUCTOR ||
        tuple->info != machine->tuple_constructor || tuple->count != 2) {
      aihc_fail("IO action returned an invalid state/result tuple");
    }
    machine->locals = NULL;
    machine->next = machine->exit_code;
    return;
  }
  case AIHC_CONT_FOREIGN_CINT: {
    AihcForeign *descriptor = continuation->payload.foreign_call.descriptor;
    AihcValue *int32 = (AihcValue *)value;
    if (int32->kind != AIHC_CONSTRUCTOR ||
        int32->info != descriptor->int32_constructor || int32->count != 1) {
      aihc_fail("foreign CInt argument has invalid Int32 constructor");
    }
    int argument = (int)(int32_t)(uint32_t)int32->fields[0];
    int result = ((int (*)(int))descriptor->function)(argument);
    AihcValue *result_int32 =
        aihc_constructor(descriptor->int32_constructor, 1, 1);
    result_int32->fields[0] = (AihcSlot)(intptr_t)(int32_t)result;
    AihcValue *tuple = aihc_constructor(descriptor->tuple_constructor, 2, 2);
    tuple->fields[0] = continuation->payload.foreign_call.state;
    tuple->fields[1] = (AihcSlot)result_int32;
    aihc_return_value(machine, (AihcSlot)tuple);
    return;
  }
  case AIHC_CONT_SELECT: {
    AihcValue *dictionary = (AihcValue *)value;
    if (dictionary->kind != AIHC_DICTIONARY ||
        continuation->payload.select.index >= dictionary->count) {
      aihc_fail("invalid dictionary selection");
    }
    AihcSlot selected =
        dictionary->fields[continuation->payload.select.index];
    if (continuation->payload.select.result_is_lifted) {
      aihc_eval_value(machine, (AihcValue *)selected, 1);
    } else {
      aihc_return_value(machine, selected);
    }
    return;
  }
  default:
    aihc_fail("unknown continuation kind");
  }
}

void aihc_return(AihcMachine *machine, AihcSlot value) {
  aihc_return_value(machine, value);
}

void aihc_eval(AihcMachine *machine, AihcValue *value,
               uint64_t result_is_lifted) {
  aihc_eval_value(machine, value, result_is_lifted);
}

void aihc_apply(AihcMachine *machine, AihcValue *function,
                AihcSlot argument) {
  aihc_apply_value(machine, function, argument);
}

void aihc_select(AihcMachine *machine, AihcValue *dictionary,
                 uint64_t index, uint64_t result_is_lifted) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_SELECT);
  continuation->payload.select.index = index;
  continuation->payload.select.result_is_lifted = result_is_lifted;
  aihc_eval_value(machine, dictionary, 1);
}

void aihc_start(AihcMachine *machine, AihcValue *root, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_push_special(machine, AIHC_CONT_TOP);
  aihc_eval_value(machine, root, 1);
}
