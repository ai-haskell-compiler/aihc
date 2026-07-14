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
  AIHC_CONT_FOREIGN_INT32 = 6,
  AIHC_CONT_FOREIGN_LITERAL = 7,
  AIHC_CONT_SELECT = 8,
};

enum {
  AIHC_PRIM_REAL_WORLD = 1,
};

typedef struct AihcValue AihcValue;
typedef struct AihcContinuation AihcContinuation;

typedef struct {
  void *function;
  uint64_t is_io;
  uint64_t io_constructor;
  uint64_t cint_constructor;
  uint64_t int32_constructor;
  uint64_t tuple_constructor;
} AihcForeign;

struct AihcValue {
  uint64_t kind;
  uintptr_t info;
  uint64_t arity;
  uint64_t count;
  AihcValue *fields[];
};

struct AihcContinuation {
  uint64_t kind;
  AihcContinuation *parent;
  union {
    struct {
      void *code;
      AihcValue **locals;
      uint64_t slot;
    } normal;
    struct {
      AihcValue *cell;
    } update;
    struct {
      AihcValue *argument;
    } apply;
    struct {
      AihcValue *state;
      AihcForeign *descriptor;
    } foreign_call;
    struct {
      uint64_t index;
    } select;
  } payload;
};

typedef struct {
  void *next;
  AihcValue **args;
  AihcContinuation *continuation;
  AihcValue **globals;
  AihcValue **locals;
  void *exit_code;
  uint64_t io_constructor;
  uint64_t tuple_constructor;
  AihcValue *state_token;
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

static AihcValue *aihc_copy_with_field(AihcValue *value, AihcValue *field) {
  AihcValue *copy = aihc_allocate(sizeof(*copy) + sizeof(AihcValue *) * (value->count + 1));
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

static AihcValue **aihc_arguments_with_field(AihcValue *function,
                                              AihcValue *argument) {
  AihcValue **arguments = aihc_allocate(sizeof(*arguments) * (function->count + 1));
  for (uint64_t index = 0; index < function->count; ++index) {
    arguments[index] = function->fields[index];
  }
  arguments[function->count] = argument;
  return arguments;
}

static void aihc_return_value(AihcMachine *machine, AihcValue *value);
static void aihc_eval_value(AihcMachine *machine, AihcValue *value);
static void aihc_apply_value(AihcMachine *machine, AihcValue *function,
                             AihcValue *argument);

AihcValue *aihc_make_node(uint64_t kind, uintptr_t info, uint64_t arity,
                          uint64_t count) {
  AihcValue *value = aihc_allocate(sizeof(*value) + sizeof(AihcValue *) * count);
  value->kind = kind;
  value->info = info;
  value->arity = arity;
  value->count = count;
  return value;
}

AihcValue *aihc_make_literal(int64_t integer) {
  return aihc_make_node(AIHC_LITERAL_INT, (uintptr_t)integer, 0, 0);
}

AihcValue *aihc_make_cell(void) {
  return aihc_make_node(AIHC_CELL, AIHC_CELL_SUSPENDED, 1, 1);
}

void aihc_set_field(AihcValue *value, uint64_t index, AihcValue *field) {
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
  cell->fields[0] = value;
}

AihcMachine *aihc_machine_new(uint64_t global_count, uint64_t io_constructor,
                              uint64_t tuple_constructor) {
  AihcMachine *machine = aihc_allocate(sizeof(*machine));
  machine->globals = aihc_allocate(sizeof(*machine->globals) * global_count);
  machine->io_constructor = io_constructor;
  machine->tuple_constructor = tuple_constructor;
  machine->state_token = aihc_make_node(AIHC_STATE_TOKEN, 0, 0, 0);
  return machine;
}

AihcValue **aihc_alloc_locals(uint64_t count) {
  return aihc_allocate(sizeof(AihcValue *) * (count == 0 ? 1 : count));
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

void aihc_push_normal(AihcMachine *machine, void *code, AihcValue **locals,
                      uint64_t slot) {
  AihcContinuation *continuation = aihc_push_special(machine, AIHC_CONT_NORMAL);
  continuation->payload.normal.code = code;
  continuation->payload.normal.locals = locals;
  continuation->payload.normal.slot = slot;
}

static void aihc_schedule_function(AihcMachine *machine, AihcValue *function,
                                   AihcValue **arguments) {
  machine->next = (void *)function->info;
  machine->args = arguments;
  machine->locals = NULL;
}

static AihcValue *aihc_constructor(uint64_t constructor, uint64_t arity,
                                   uint64_t count) {
  return aihc_make_node(AIHC_CONSTRUCTOR, constructor, arity, count);
}

static void aihc_complete_foreign(AihcMachine *machine, AihcValue *foreign,
                                  AihcValue *state) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_FOREIGN_CINT);
  continuation->payload.foreign_call.state = state;
  continuation->payload.foreign_call.descriptor =
      (AihcForeign *)foreign->info;
  aihc_eval_value(machine, foreign->fields[0]);
}

static void aihc_apply_forced(AihcMachine *machine, AihcValue *function,
                              AihcValue *argument) {
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
      aihc_return_value(machine, applied);
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
      AihcValue *io = aihc_constructor(descriptor->io_constructor, 1, 1);
      io->fields[0] = action;
      aihc_return_value(machine, io);
      return;
    }
    aihc_return_value(machine, applied);
    return;
  }
  case AIHC_FOREIGN_IO:
    aihc_complete_foreign(machine, function, argument);
    return;
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_eval_value(AihcMachine *machine, AihcValue *value) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  if (value->kind == AIHC_CELL) {
    switch (value->info) {
    case AIHC_CELL_SUSPENDED: {
      AihcValue *thunk = value->fields[0];
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
      aihc_eval_value(machine, value->fields[0]);
      return;
    case AIHC_CELL_BLACKHOLE:
      aihc_fail("blackholed thunk re-entered");
    default:
      aihc_fail("unknown cell state");
    }
  }
  if (value->kind == AIHC_PRIMITIVE && value->arity == 0) {
    if (value->info == AIHC_PRIM_REAL_WORLD) {
      aihc_return_value(machine, machine->state_token);
      return;
    }
    aihc_fail("unknown zero-arity primitive");
  }
  aihc_return_value(machine, value);
}

static void aihc_apply_value(AihcMachine *machine, AihcValue *function,
                             AihcValue *argument) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_APPLY);
  continuation->payload.apply.argument = argument;
  aihc_eval_value(machine, function);
}

static void aihc_run_io(AihcMachine *machine, AihcValue *value) {
  if (value->kind != AIHC_CONSTRUCTOR || value->info != machine->io_constructor ||
      value->count != 1) {
    aihc_fail("program result is not an IO value");
  }
  aihc_push_special(machine, AIHC_CONT_FINAL);
  aihc_apply_value(machine, value->fields[0], machine->state_token);
}

static void aihc_return_value(AihcMachine *machine, AihcValue *value) {
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
    aihc_apply_forced(machine, value, continuation->payload.apply.argument);
    return;
  case AIHC_CONT_TOP:
    aihc_run_io(machine, value);
    return;
  case AIHC_CONT_FINAL:
    if (value->kind != AIHC_CONSTRUCTOR ||
        value->info != machine->tuple_constructor || value->count != 2) {
      aihc_fail("IO action returned an invalid state/result tuple");
    }
    machine->locals = NULL;
    machine->next = machine->exit_code;
    return;
  case AIHC_CONT_FOREIGN_CINT: {
    AihcForeign *descriptor = continuation->payload.foreign_call.descriptor;
    if (value->kind != AIHC_CONSTRUCTOR ||
        value->info != descriptor->cint_constructor || value->count != 1) {
      aihc_fail("foreign CInt argument has invalid outer constructor");
    }
    AihcContinuation *next =
        aihc_push_special(machine, AIHC_CONT_FOREIGN_INT32);
    next->payload.foreign_call = continuation->payload.foreign_call;
    aihc_eval_value(machine, value->fields[0]);
    return;
  }
  case AIHC_CONT_FOREIGN_INT32: {
    AihcForeign *descriptor = continuation->payload.foreign_call.descriptor;
    if (value->kind != AIHC_CONSTRUCTOR ||
        value->info != descriptor->int32_constructor || value->count != 1) {
      aihc_fail("foreign CInt argument has invalid Int32 constructor");
    }
    AihcContinuation *next =
        aihc_push_special(machine, AIHC_CONT_FOREIGN_LITERAL);
    next->payload.foreign_call = continuation->payload.foreign_call;
    aihc_eval_value(machine, value->fields[0]);
    return;
  }
  case AIHC_CONT_FOREIGN_LITERAL: {
    AihcForeign *descriptor = continuation->payload.foreign_call.descriptor;
    if (value->kind != AIHC_LITERAL_INT) {
      aihc_fail("foreign CInt argument is not an integer literal");
    }
    int result = ((int (*)(int))descriptor->function)((int)value->info);
    AihcValue *literal = aihc_make_literal(result);
    AihcValue *int32 = aihc_constructor(descriptor->int32_constructor, 1, 1);
    int32->fields[0] = literal;
    AihcValue *cint = aihc_constructor(descriptor->cint_constructor, 1, 1);
    cint->fields[0] = int32;
    AihcValue *tuple = aihc_constructor(descriptor->tuple_constructor, 2, 2);
    tuple->fields[0] = continuation->payload.foreign_call.state;
    tuple->fields[1] = cint;
    aihc_return_value(machine, tuple);
    return;
  }
  case AIHC_CONT_SELECT:
    if (value->kind != AIHC_DICTIONARY ||
        continuation->payload.select.index >= value->count) {
      aihc_fail("invalid dictionary selection");
    }
    aihc_eval_value(machine,
                    value->fields[continuation->payload.select.index]);
    return;
  default:
    aihc_fail("unknown continuation kind");
  }
}

void aihc_return(AihcMachine *machine, AihcValue *value) {
  aihc_return_value(machine, value);
}

void aihc_eval(AihcMachine *machine, AihcValue *value) {
  aihc_eval_value(machine, value);
}

void aihc_apply(AihcMachine *machine, AihcValue *function,
                AihcValue *argument) {
  aihc_apply_value(machine, function, argument);
}

void aihc_select(AihcMachine *machine, AihcValue *dictionary,
                 uint64_t index) {
  AihcContinuation *continuation =
      aihc_push_special(machine, AIHC_CONT_SELECT);
  continuation->payload.select.index = index;
  aihc_eval_value(machine, dictionary);
}

void aihc_start(AihcMachine *machine, AihcValue *root, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_push_special(machine, AIHC_CONT_TOP);
  aihc_eval_value(machine, root);
}
