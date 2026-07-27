#include "aihc_runtime_trampoline.h"
#include "aihc_runtime_internal.h"

#include <stddef.h>
#include <string.h>

typedef struct {
  AihcSlot *arguments;
  uint64_t capacity;
} AihcTrampolineState;

static AihcTrampolineState *aihc_trampoline_state(AihcMachine *machine) {
  if (machine->trampoline_state == NULL) {
    machine->trampoline_state =
        aihc_allocate_zeroed(sizeof(AihcTrampolineState));
  }
  return machine->trampoline_state;
}

static AihcSlot *aihc_trampoline_reserve(AihcMachine *machine, uint64_t count) {
  AihcTrampolineState *state = aihc_trampoline_state(machine);
  return aihc_reserve_slots(machine, &state->arguments, &state->capacity,
                            count);
}

static AihcSlot *aihc_trampoline_arguments(AihcMachine *machine,
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
  AihcSlot *buffer = aihc_trampoline_reserve(machine, total);
  if (count != 0) {
    memmove(buffer + field_count, values, sizeof(*values) * (size_t)count);
  }
  const AihcSlot *function_fields = aihc_value_fields_const(function);
  if (field_count != 0) {
    memcpy(buffer, function_fields,
           sizeof(*function_fields) * (size_t)field_count);
  }
  if (continuation != NULL) {
    buffer[(size_t)field_count + (size_t)count] =
        (AihcSlot)(uintptr_t)continuation;
  }
  return buffer;
}

static AihcTrampolineTransfer aihc_trampoline_transfer(AihcEntry entry,
                                                       AihcSlot *arguments) {
  return (AihcTrampolineTransfer){entry, arguments};
}

AihcTrampolineTransfer aihc_trampoline_call(AihcMachine *machine,
                                            AihcEntry entry, uint64_t count,
                                            const AihcSlot *arguments) {
  AihcSlot *buffer = aihc_trampoline_reserve(machine, count);
  if (count != 0) {
    memmove(buffer, arguments, sizeof(*arguments) * (size_t)count);
  }
  return aihc_trampoline_transfer(entry, buffer);
}

static AihcTrampolineTransfer
aihc_trampoline_continue_values_now(AihcMachine *machine,
                                    AihcValue *continuation, uint64_t count,
                                    const AihcSlot *values) {
  if (continuation == NULL ||
      aihc_value_tag(continuation) != AIHC_TAG_CLOSURE) {
    aihc_fail("attempted to invoke a non-continuation value");
  }
  if (aihc_value_arity(continuation) != 1) {
    aihc_fail("continuation closure does not accept exactly one result");
  }
  (void)aihc_next_application_info(aihc_value_info_table(continuation), count);
  return aihc_trampoline_transfer(
      aihc_value_entry(continuation),
      aihc_trampoline_arguments(machine, continuation, count, values, NULL));
}

AihcTrampolineTransfer aihc_trampoline_continue_values(AihcMachine *machine,
                                                       AihcValue *continuation,
                                                       uint64_t count,
                                                       const AihcSlot *values) {
  return aihc_trampoline_continue_values_now(machine, continuation, count,
                                             values);
}

static AihcTrampolineTransfer
aihc_trampoline_continue_value(AihcMachine *machine, AihcValue *continuation,
                               AihcSlot value) {
  return aihc_trampoline_continue_values_now(machine, continuation, 1, &value);
}

AihcTrampolineTransfer aihc_trampoline_apply_cps(AihcMachine *machine,
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
    return aihc_trampoline_transfer(aihc_value_entry(function),
                                    aihc_trampoline_arguments(machine, function,
                                                              count, arguments,
                                                              continuation));
  }
  AihcValue *applied =
      aihc_apply_slow(machine, function, count, arguments, &continuation);
  return aihc_trampoline_continue_value(machine, continuation,
                                        (AihcSlot)(uintptr_t)applied);
}

AihcTrampolineTransfer aihc_trampoline_resume(AihcMachine *machine,
                                              const AihcResume *resume) {
  if (resume == NULL) {
    return (AihcTrampolineTransfer){0};
  }
  AihcTrampolineTransfer transfer;
  switch (resume->kind) {
  case AIHC_RESUME_APPLY:
    transfer = aihc_trampoline_apply_cps(
        machine, resume->function, resume->count,
        resume->count == 0 ? NULL : &resume->value, resume->continuation);
    break;
  case AIHC_RESUME_CONTINUE:
    transfer = aihc_trampoline_continue_values_now(
        machine, resume->function, resume->count,
        resume->count == 0 ? NULL : &resume->value);
    break;
  case AIHC_RESUME_RAISE:
    transfer = aihc_trampoline_raise_cps(machine, resume->function,
                                         resume->continuation);
    break;
  default:
    aihc_fail("invalid suspended continuation");
  }
  machine->selected_resume = (AihcResume){0};
  return transfer;
}

AihcTrampolineTransfer aihc_trampoline_raise_cps(AihcMachine *machine,
                                                 AihcValue *exception,
                                                 AihcValue *continuation) {
  return aihc_trampoline_resume(machine,
                                aihc_raise(machine, exception, continuation));
}

AihcTrampolineTransfer
aihc_trampoline_eval_cps(AihcMachine *machine, AihcValue *value,
                         uint64_t result_is_lifted, AihcValue *continuation,
                         AihcValue *update_continuation) {
  if (value == NULL) {
    aihc_fail("attempted to evaluate null");
  }
  switch (aihc_value_tag(value)) {
  case AIHC_TAG_THUNK: {
    AihcSlot *arguments =
        aihc_trampoline_arguments(machine, value, 0, NULL, update_continuation);
    AihcEntry entry = aihc_value_entry(value);
    aihc_begin_blackhole(machine, value);
    return aihc_trampoline_transfer(entry, arguments);
  }
  case AIHC_TAG_INDIRECTION:
    if (result_is_lifted) {
      return aihc_trampoline_eval_cps(machine,
                                      (AihcValue *)(uintptr_t)value->fields[0],
                                      1, continuation, update_continuation);
    }
    return aihc_trampoline_continue_value(machine, continuation,
                                          value->fields[0]);
  case AIHC_TAG_BLACKHOLE:
    return aihc_trampoline_resume(
        machine, aihc_block_on_blackhole(machine, value, continuation));
  default:
    return aihc_trampoline_continue_value(machine, continuation,
                                          (AihcSlot)(uintptr_t)value);
  }
}

AihcTrampolineTransfer aihc_trampoline_fork_cps(AihcMachine *machine,
                                                AihcValue *action,
                                                AihcValue *continuation) {
  return aihc_trampoline_continue_value(machine, continuation,
                                        aihc_fork(machine, action));
}

AihcTrampolineTransfer aihc_trampoline_new_mvar_cps(AihcMachine *machine,
                                                    AihcValue *continuation) {
  return aihc_trampoline_continue_value(
      machine, continuation, (AihcSlot)(uintptr_t)aihc_mvar_new(machine));
}

AihcTrampolineTransfer aihc_trampoline_read_mvar_cps(AihcMachine *machine,
                                                     void *mvar,
                                                     AihcValue *continuation) {
  return aihc_trampoline_resume(machine,
                                aihc_mvar_read(machine, mvar, continuation));
}

AihcTrampolineTransfer aihc_trampoline_take_mvar_cps(AihcMachine *machine,
                                                     void *mvar,
                                                     AihcValue *continuation) {
  return aihc_trampoline_resume(machine,
                                aihc_mvar_take(machine, mvar, continuation));
}

AihcTrampolineTransfer aihc_trampoline_put_mvar_cps(AihcMachine *machine,
                                                    void *mvar,
                                                    AihcValue *value,
                                                    AihcValue *continuation) {
  return aihc_trampoline_resume(
      machine,
      aihc_mvar_put(machine, mvar, (AihcSlot)(uintptr_t)value, continuation));
}

AihcTrampolineTransfer aihc_trampoline_yield_cps(AihcMachine *machine,
                                                 AihcValue *continuation) {
  return aihc_trampoline_resume(machine, aihc_yield(machine, continuation));
}

AihcTrampolineTransfer aihc_trampoline_await_io_cps(AihcMachine *machine,
                                                    void *request,
                                                    AihcValue *continuation) {
  return aihc_trampoline_resume(machine,
                                aihc_await_io(machine, request, continuation));
}

AihcTrampolineTransfer aihc_trampoline_thread_done(AihcMachine *machine) {
  return aihc_trampoline_resume(machine, aihc_thread_done(machine));
}

AihcTrampolineTransfer
aihc_trampoline_start(AihcMachine *machine, AihcValue *root,
                      AihcValue *continuation, AihcValue *update_continuation,
                      AihcValue *thread_done_continuation,
                      AihcEntry exit_code) {
  machine->exit_code = exit_code;
  aihc_set_thread_done_continuation(machine, thread_done_continuation);
  return aihc_trampoline_eval_cps(machine, root, 1, continuation,
                                  update_continuation);
}
