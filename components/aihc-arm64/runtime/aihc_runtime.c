#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

enum {
  AIHC_LITERAL_INT = 0,
  AIHC_CONSTRUCTOR = 1,
  AIHC_CLOSURE = 2,
  AIHC_THUNK = 3,
  AIHC_PRIMITIVE = 4,
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
  AIHC_CONT_SELECT = 5,
};

enum {
  AIHC_PRIM_REAL_WORLD = 1,
  AIHC_PRIM_FORK = 2,
  AIHC_PRIM_YIELD = 3,
  AIHC_PRIM_NEW_MVAR = 4,
  AIHC_PRIM_TAKE_MVAR = 5,
  AIHC_PRIM_PUT_MVAR = 6,
  AIHC_PRIM_DELAY = 7,
};

enum {
  AIHC_FIBER_RUNNING = 0,
  AIHC_FIBER_RUNNABLE = 1,
  AIHC_FIBER_BLOCKED_TAKE = 2,
  AIHC_FIBER_BLOCKED_PUT = 3,
  AIHC_FIBER_SLEEPING = 4,
  AIHC_FIBER_DEAD = 5,
};

typedef struct AihcValue AihcValue;
typedef struct AihcContinuation AihcContinuation;
typedef struct AihcFiber AihcFiber;
typedef struct AihcMVar AihcMVar;
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
    } normal;
    struct {
      AihcValue *cell;
    } update;
    struct {
      AihcSlot argument;
    } apply;
    struct {
      uint64_t index;
      uint64_t result_is_lifted;
    } select;
  } payload;
};

typedef struct {
  uint64_t serial;
} AihcThreadId;

struct AihcFiber {
  void *next;
  AihcSlot *args;
  AihcContinuation *continuation;
  AihcSlot *locals;

  uint64_t state;
  bool is_main;
  bool bootstrap;
  bool has_pending_result;
  AihcValue *bootstrap_action;
  AihcSlot bootstrap_state;
  AihcSlot pending_result;

  AihcFiber *run_next;
  AihcFiber *wait_next;
  AihcFiber *timer_next;

  AihcMVar *blocked_mvar;
  AihcSlot blocked_state;
  AihcSlot blocked_value;
  uint64_t deadline_ns;
  uint64_t timer_sequence;
};

typedef struct {
  AihcFiber *head;
  AihcFiber *tail;
} AihcFiberQueue;

struct AihcMVar {
  bool full;
  AihcSlot value;
  AihcFiberQueue takers;
  AihcFiberQueue putters;
};

typedef struct {
  void *next;
  AihcSlot *args;
  AihcContinuation *continuation;
  AihcSlot *globals;
  AihcSlot *locals;
  void *exit_code;
  uint64_t tuple_constructor;
  AihcFiber *current_fiber;
  AihcFiber *main_fiber;
  AihcFiberQueue runnable;
  AihcFiber *timers;
  uint64_t next_thread_id;
  uint64_t next_timer_sequence;
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
static void aihc_schedule_next(AihcMachine *machine);
static void aihc_execute_primitive(AihcMachine *machine,
                                   AihcValue *primitive);

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
  machine->next_thread_id = 1;
  AihcFiber *main_fiber = aihc_allocate(sizeof(*main_fiber));
  main_fiber->state = AIHC_FIBER_RUNNING;
  main_fiber->is_main = true;
  machine->current_fiber = main_fiber;
  machine->main_fiber = main_fiber;
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

static AihcValue *aihc_tuple2(AihcMachine *machine, AihcSlot first,
                              AihcSlot second) {
  AihcValue *tuple = aihc_constructor(machine->tuple_constructor, 2, 2);
  tuple->fields[0] = first;
  tuple->fields[1] = second;
  return tuple;
}

static void aihc_run_queue_push(AihcFiberQueue *queue, AihcFiber *fiber) {
  fiber->run_next = NULL;
  if (queue->tail == NULL) {
    queue->head = fiber;
    queue->tail = fiber;
  } else {
    queue->tail->run_next = fiber;
    queue->tail = fiber;
  }
}

static AihcFiber *aihc_run_queue_pop(AihcFiberQueue *queue) {
  AihcFiber *fiber = queue->head;
  if (fiber == NULL) {
    return NULL;
  }
  queue->head = fiber->run_next;
  if (queue->head == NULL) {
    queue->tail = NULL;
  }
  fiber->run_next = NULL;
  return fiber;
}

static void aihc_wait_queue_push(AihcFiberQueue *queue, AihcFiber *fiber) {
  fiber->wait_next = NULL;
  if (queue->tail == NULL) {
    queue->head = fiber;
    queue->tail = fiber;
  } else {
    queue->tail->wait_next = fiber;
    queue->tail = fiber;
  }
}

static AihcFiber *aihc_wait_queue_pop(AihcFiberQueue *queue) {
  AihcFiber *fiber = queue->head;
  if (fiber == NULL) {
    return NULL;
  }
  queue->head = fiber->wait_next;
  if (queue->head == NULL) {
    queue->tail = NULL;
  }
  fiber->wait_next = NULL;
  return fiber;
}

static void aihc_save_current_fiber(AihcMachine *machine) {
  AihcFiber *fiber = machine->current_fiber;
  fiber->next = machine->next;
  fiber->args = machine->args;
  fiber->continuation = machine->continuation;
  fiber->locals = machine->locals;
}

static void aihc_load_fiber(AihcMachine *machine, AihcFiber *fiber) {
  machine->current_fiber = fiber;
  machine->next = fiber->next;
  machine->args = fiber->args;
  machine->continuation = fiber->continuation;
  machine->locals = fiber->locals;
  fiber->state = AIHC_FIBER_RUNNING;
}

static void aihc_enqueue_fiber(AihcMachine *machine, AihcFiber *fiber) {
  fiber->state = AIHC_FIBER_RUNNABLE;
  aihc_run_queue_push(&machine->runnable, fiber);
}

static void aihc_wake_fiber(AihcMachine *machine, AihcFiber *fiber,
                            AihcSlot result) {
  fiber->has_pending_result = true;
  fiber->pending_result = result;
  fiber->blocked_mvar = NULL;
  aihc_enqueue_fiber(machine, fiber);
}

static uint64_t aihc_monotonic_ns(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
    aihc_fail("could not read monotonic clock");
  }
  return (uint64_t)now.tv_sec * UINT64_C(1000000000) + (uint64_t)now.tv_nsec;
}

static void aihc_wait_until_ns(uint64_t deadline) {
  for (;;) {
    uint64_t now = aihc_monotonic_ns();
    if (now >= deadline) {
      return;
    }
    uint64_t remaining = deadline - now;
    struct timespec sleep_time = {
        .tv_sec = (time_t)(remaining / UINT64_C(1000000000)),
        .tv_nsec = (long)(remaining % UINT64_C(1000000000)),
    };
    if (nanosleep(&sleep_time, NULL) == 0) {
      return;
    }
    if (errno != EINTR) {
      aihc_fail("timer wait failed");
    }
  }
}

static void aihc_insert_timer(AihcMachine *machine, AihcFiber *fiber) {
  AihcFiber **cursor = &machine->timers;
  while (*cursor != NULL &&
         ((*cursor)->deadline_ns < fiber->deadline_ns ||
          ((*cursor)->deadline_ns == fiber->deadline_ns &&
           (*cursor)->timer_sequence < fiber->timer_sequence))) {
    cursor = &(*cursor)->timer_next;
  }
  fiber->timer_next = *cursor;
  *cursor = fiber;
}

static void aihc_drain_timers(AihcMachine *machine) {
  uint64_t now = aihc_monotonic_ns();
  while (machine->timers != NULL && machine->timers->deadline_ns <= now) {
    AihcFiber *fiber = machine->timers;
    machine->timers = fiber->timer_next;
    fiber->timer_next = NULL;
    aihc_enqueue_fiber(machine, fiber);
  }
}

static void aihc_schedule_next(AihcMachine *machine) {
  aihc_save_current_fiber(machine);
  for (;;) {
    aihc_drain_timers(machine);
    AihcFiber *next = aihc_run_queue_pop(&machine->runnable);
    if (next != NULL) {
      aihc_load_fiber(machine, next);
      if (next->bootstrap) {
        next->bootstrap = false;
        aihc_push_special(machine, AIHC_CONT_FINAL);
        aihc_apply_value(machine, next->bootstrap_action,
                         next->bootstrap_state);
      } else if (next->has_pending_result) {
        next->has_pending_result = false;
        aihc_return_value(machine, next->pending_result);
      }
      return;
    }
    if (machine->timers != NULL) {
      aihc_wait_until_ns(machine->timers->deadline_ns);
      continue;
    }
    if (machine->main_fiber->state == AIHC_FIBER_DEAD) {
      machine->locals = NULL;
      machine->next = machine->exit_code;
      return;
    }
    aihc_fail("deadlock: no runnable green threads or timers");
  }
}

static void aihc_apply_forced(AihcMachine *machine, AihcValue *function,
                              AihcSlot argument) {
  switch (function->kind) {
  case AIHC_CLOSURE:
    aihc_schedule_function(machine, function,
                           aihc_arguments_with_field(function, argument));
    return;
  case AIHC_CONSTRUCTOR:
  case AIHC_PRIMITIVE: {
    AihcValue *applied = aihc_copy_with_field(function, argument);
    if (applied->count < applied->arity) {
      aihc_return_value(machine, (AihcSlot)applied);
      return;
    }
    if (applied->count > applied->arity) {
      aihc_fail("overapplication is not implemented");
    }
    if (applied->kind == AIHC_PRIMITIVE) {
      aihc_execute_primitive(machine, applied);
      return;
    }
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
    AihcFiber *fiber = machine->current_fiber;
    fiber->state = AIHC_FIBER_DEAD;
    if (fiber->is_main) {
      machine->locals = NULL;
      machine->next = machine->exit_code;
    } else {
      aihc_schedule_next(machine);
    }
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

void aihc_prim_fork(AihcMachine *machine, AihcValue *action,
                    AihcSlot state) {
  AihcFiber *child = aihc_allocate(sizeof(*child));
  AihcThreadId *thread_id = aihc_allocate(sizeof(*thread_id));
  thread_id->serial = machine->next_thread_id++;
  child->bootstrap = true;
  child->bootstrap_action = action;
  child->bootstrap_state = state;
  aihc_enqueue_fiber(machine, child);
  aihc_return_value(
      machine,
      (AihcSlot)aihc_tuple2(machine, state, (AihcSlot)thread_id));
}

void aihc_prim_yield(AihcMachine *machine, AihcSlot state) {
  aihc_return_value(machine, state);
  aihc_enqueue_fiber(machine, machine->current_fiber);
  aihc_schedule_next(machine);
}

void aihc_prim_new_mvar(AihcMachine *machine, AihcSlot state) {
  AihcMVar *mvar = aihc_allocate(sizeof(*mvar));
  aihc_return_value(
      machine, (AihcSlot)aihc_tuple2(machine, state, (AihcSlot)mvar));
}

void aihc_prim_take_mvar(AihcMachine *machine, AihcMVar *mvar,
                         AihcSlot state) {
  if (mvar == NULL) {
    aihc_fail("takeMVar# received a null MVar");
  }
  if (!mvar->full) {
    AihcFiber *fiber = machine->current_fiber;
    fiber->state = AIHC_FIBER_BLOCKED_TAKE;
    fiber->blocked_mvar = mvar;
    fiber->blocked_state = state;
    aihc_wait_queue_push(&mvar->takers, fiber);
    aihc_schedule_next(machine);
    return;
  }

  AihcSlot value = mvar->value;
  AihcFiber *putter = aihc_wait_queue_pop(&mvar->putters);
  if (putter == NULL) {
    mvar->full = false;
    mvar->value = 0;
  } else {
    mvar->value = putter->blocked_value;
    aihc_wake_fiber(machine, putter, putter->blocked_state);
  }
  aihc_return_value(machine,
                    (AihcSlot)aihc_tuple2(machine, state, value));
}

void aihc_prim_put_mvar(AihcMachine *machine, AihcMVar *mvar,
                        AihcSlot value, AihcSlot state) {
  if (mvar == NULL) {
    aihc_fail("putMVar# received a null MVar");
  }
  AihcFiber *taker = aihc_wait_queue_pop(&mvar->takers);
  if (taker != NULL) {
    AihcValue *result = aihc_tuple2(machine, taker->blocked_state, value);
    aihc_wake_fiber(machine, taker, (AihcSlot)result);
    aihc_return_value(machine, state);
    return;
  }
  if (!mvar->full) {
    mvar->full = true;
    mvar->value = value;
    aihc_return_value(machine, state);
    return;
  }

  AihcFiber *fiber = machine->current_fiber;
  fiber->state = AIHC_FIBER_BLOCKED_PUT;
  fiber->blocked_mvar = mvar;
  fiber->blocked_state = state;
  fiber->blocked_value = value;
  aihc_wait_queue_push(&mvar->putters, fiber);
  aihc_schedule_next(machine);
}

void aihc_prim_delay(AihcMachine *machine, intptr_t microseconds,
                     AihcSlot state) {
  if (microseconds <= 0) {
    aihc_return_value(machine, state);
    return;
  }
  uint64_t duration;
  if ((uint64_t)microseconds > UINT64_MAX / UINT64_C(1000)) {
    duration = UINT64_MAX;
  } else {
    duration = (uint64_t)microseconds * UINT64_C(1000);
  }
  uint64_t now = aihc_monotonic_ns();
  uint64_t deadline = duration > UINT64_MAX - now ? UINT64_MAX : now + duration;
  AihcFiber *fiber = machine->current_fiber;
  fiber->state = AIHC_FIBER_SLEEPING;
  fiber->has_pending_result = true;
  fiber->pending_result = state;
  fiber->deadline_ns = deadline;
  fiber->timer_sequence = machine->next_timer_sequence++;
  aihc_insert_timer(machine, fiber);
  aihc_schedule_next(machine);
}

static void aihc_execute_primitive(AihcMachine *machine,
                                   AihcValue *primitive) {
  switch (primitive->info) {
  case AIHC_PRIM_FORK:
    aihc_prim_fork(machine, (AihcValue *)primitive->fields[0],
                   primitive->fields[1]);
    return;
  case AIHC_PRIM_YIELD:
    aihc_prim_yield(machine, primitive->fields[0]);
    return;
  case AIHC_PRIM_NEW_MVAR:
    aihc_prim_new_mvar(machine, primitive->fields[0]);
    return;
  case AIHC_PRIM_TAKE_MVAR:
    aihc_prim_take_mvar(machine, (AihcMVar *)primitive->fields[0],
                        primitive->fields[1]);
    return;
  case AIHC_PRIM_PUT_MVAR:
    aihc_prim_put_mvar(machine, (AihcMVar *)primitive->fields[0],
                       primitive->fields[1], primitive->fields[2]);
    return;
  case AIHC_PRIM_DELAY:
    aihc_prim_delay(machine, (intptr_t)primitive->fields[0],
                    primitive->fields[1]);
    return;
  default:
    aihc_fail("unsupported saturated primitive");
  }
}

void aihc_start(AihcMachine *machine, AihcValue *root, void *exit_code) {
  machine->exit_code = exit_code;
  aihc_push_special(machine, AIHC_CONT_TOP);
  aihc_eval_value(machine, root, 1);
}
