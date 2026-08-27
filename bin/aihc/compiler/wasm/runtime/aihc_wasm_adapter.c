#include "aihc_runtime_internal.h"
#include "aihc_runtime_trampoline.h"
#include "aihc_wasm_internal.h"

#include <stddef.h>

typedef void (*AihcWasmEnterEntry)(AihcMachine *machine, AihcSlot object,
                                   const AihcSlot *supplied,
                                   AihcSlot continuation);

typedef struct {
  AihcWasmEnterEntry entry;
  AihcMachine *machine;
  AihcSlot object;
  const AihcSlot *supplied;
  AihcSlot continuation;
} AihcWasmObjectTransfer;

AihcMachine *aihc_machine;
AihcTrampolineTransfer aihc_next_transfer;

static AihcWasmObjectTransfer aihc_next_object_transfer;
static int aihc_wasm_finished;

uint64_t aihc_wasm_times_word2_high(uint64_t left, uint64_t right) {
  uint64_t left_low = (uint32_t)left;
  uint64_t left_high = left >> 32;
  uint64_t right_low = (uint32_t)right;
  uint64_t right_high = right >> 32;
  uint64_t product00 = left_low * right_low;
  uint64_t product01 = left_low * right_high;
  uint64_t product10 = left_high * right_low;
  uint64_t product11 = left_high * right_high;
  uint64_t low_partial = product00 + (product01 << 32);
  uint64_t low_result = low_partial + (product10 << 32);
  return product11 + (product01 >> 32) + (product10 >> 32) +
         (low_partial < product00) + (low_result < low_partial);
}

uint64_t aihc_wasm_quot_rem_word2_quotient(uint64_t high, uint64_t low,
                                           uint64_t divisor) {
  if (divisor == 0) {
    __builtin_trap();
  }
  uint64_t quotient = 0;
  uint64_t remainder = high;
  for (unsigned int count = 0; count < 64; ++count) {
    uint64_t overflow = remainder >> 63;
    uint64_t next_bit = low >> 63;
    remainder = (remainder << 1) | next_bit;
    low <<= 1;
    quotient <<= 1;
    if (overflow != 0 || remainder >= divisor) {
      remainder -= divisor;
      quotient |= 1;
    }
  }
  return quotient;
}

static AihcValue *aihc_value(AihcSlot slot) {
  return (AihcValue *)(uintptr_t)slot;
}

AihcSlot aihc_wasm_make_node(AihcMachine *machine, const AihcInfo *info) {
  return (AihcSlot)(uintptr_t)aihc_make_node(machine, info);
}

AihcSlot aihc_wasm_make_node_unchecked(AihcMachine *machine,
                                       const AihcInfo *info) {
  return (AihcSlot)(uintptr_t)aihc_make_node_unchecked(machine, info);
}

void aihc_wasm_set_field(AihcSlot value, uint64_t index, AihcSlot field) {
  aihc_set_field(aihc_value(value), index, field);
}

void aihc_wasm_update(AihcSlot object, AihcSlot value) {
  aihc_update(aihc_value(object), aihc_value(value));
}

void aihc_wasm_update_blackhole(AihcMachine *machine, AihcSlot object,
                                AihcSlot value) {
  aihc_update_blackhole(machine, aihc_value(object), aihc_value(value));
}

AihcSlot aihc_wasm_global_get(AihcMachine *machine, uint64_t index) {
  return machine->globals[index];
}

void aihc_wasm_global_set(AihcMachine *machine, uint64_t index,
                          AihcSlot value) {
  machine->globals[index] = value;
}

AihcSlot aihc_wasm_value_field(AihcSlot value, uint64_t index) {
  return aihc_value(value)->fields[index];
}

AihcSlot aihc_wasm_value_info(AihcSlot value) {
  return aihc_value_info_table(aihc_value(value))->identity;
}

static void aihc_set_transfer(AihcTrampolineTransfer transfer) {
  aihc_next_transfer = transfer;
}

static AihcWasmEnterEntry aihc_wasm_backend_entry(const AihcInfo *info) {
  return (AihcWasmEnterEntry)info->backend_entry;
}

static void aihc_set_object_transfer(AihcMachine *machine, AihcValue *object,
                                     const AihcSlot *supplied,
                                     AihcValue *continuation) {
  const AihcInfo *info = aihc_value_info_table(object);
  AihcWasmEnterEntry entry = aihc_wasm_backend_entry(info);
  if (entry == NULL) {
    aihc_fail("WebAssembly object has no backend entry");
  }
  aihc_next_object_transfer =
      (AihcWasmObjectTransfer){entry, machine, (AihcSlot)(uintptr_t)object,
                               supplied, (AihcSlot)(uintptr_t)continuation};
}

static int aihc_can_enter_saturated(const AihcValue *object, uint64_t count) {
  const AihcInfo *info = aihc_value_info_table(object);
  const AihcInfo *next = info->next;
  return info->backend_entry != NULL && info->remaining_arity == 1 &&
         next != NULL && next->remaining_arity == 0 &&
         next->field_count >= info->field_count &&
         next->field_count - info->field_count == count;
}

void aihc_wasm_transfer_direct(AihcMachine *machine, AihcEntry entry,
                               uint64_t count, const AihcSlot *arguments) {
  if (entry == NULL) {
    aihc_next_transfer = (AihcTrampolineTransfer){0};
    aihc_wasm_finished = 1;
    return;
  }
  aihc_set_transfer(aihc_trampoline_call(machine, entry, count, arguments));
}

void aihc_wasm_transfer_eval(AihcMachine *machine, AihcSlot value,
                             uint64_t lifted, AihcSlot continuation,
                             AihcSlot update_continuation) {
  AihcValue *object = aihc_value(value);
  if (object != NULL && aihc_value_kind(object) == AIHC_OBJECT_THUNK &&
      aihc_value_info_table(object)->backend_entry != NULL) {
    aihc_set_object_transfer(machine, object, NULL,
                             aihc_value(update_continuation));
    aihc_begin_blackhole(machine, object);
    return;
  }
  aihc_set_transfer(aihc_trampoline_eval_cps(machine, aihc_value(value), lifted,
                                             aihc_value(continuation),
                                             aihc_value(update_continuation)));
}

void aihc_wasm_transfer_apply(AihcMachine *machine, AihcSlot function,
                              uint64_t count, const AihcSlot *arguments,
                              AihcSlot continuation) {
  AihcValue *object = aihc_value(function);
  if (object != NULL && aihc_value_kind(object) == AIHC_OBJECT_CLOSURE &&
      aihc_can_enter_saturated(object, count)) {
    aihc_set_object_transfer(machine, object, arguments,
                             aihc_value(continuation));
    return;
  }
  aihc_set_transfer(aihc_trampoline_apply_cps(machine, aihc_value(function),
                                              count, arguments,
                                              aihc_value(continuation)));
}

void aihc_wasm_transfer_continue(AihcMachine *machine, AihcSlot continuation,
                                 uint64_t count, const AihcSlot *values) {
  AihcValue *object = aihc_value(continuation);
  if (object != NULL && aihc_value_kind(object) == AIHC_OBJECT_CLOSURE &&
      aihc_can_enter_saturated(object, count)) {
    aihc_set_object_transfer(machine, object, values, NULL);
    return;
  }
  aihc_set_transfer(aihc_trampoline_continue_values(
      machine, aihc_value(continuation), count, values));
}

void aihc_wasm_transfer_raise(AihcMachine *machine, AihcSlot exception,
                              AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_raise_cps(machine, aihc_value(exception),
                                              aihc_value(continuation)));
}

void aihc_wasm_transfer_fork(AihcMachine *machine, AihcSlot action,
                             AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_fork_cps(machine, aihc_value(action),
                                             aihc_value(continuation)));
}

void aihc_wasm_transfer_yield(AihcMachine *machine, AihcSlot continuation) {
  aihc_set_transfer(
      aihc_trampoline_yield_cps(machine, aihc_value(continuation)));
}

void aihc_wasm_transfer_await_io(AihcMachine *machine, AihcSlot request,
                                 AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_await_io_cps(
      machine, (void *)(uintptr_t)request, aihc_value(continuation)));
}

void aihc_wasm_transfer_new_mvar(AihcMachine *machine, AihcSlot continuation) {
  aihc_set_transfer(
      aihc_trampoline_new_mvar_cps(machine, aihc_value(continuation)));
}

void aihc_wasm_transfer_read_mvar(AihcMachine *machine, AihcSlot mvar,
                                  AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_read_mvar_cps(
      machine, (void *)(uintptr_t)mvar, aihc_value(continuation)));
}

void aihc_wasm_transfer_take_mvar(AihcMachine *machine, AihcSlot mvar,
                                  AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_take_mvar_cps(
      machine, (void *)(uintptr_t)mvar, aihc_value(continuation)));
}

void aihc_wasm_transfer_put_mvar(AihcMachine *machine, AihcSlot mvar,
                                 AihcSlot value, AihcSlot continuation) {
  aihc_set_transfer(aihc_trampoline_put_mvar_cps(
      machine, (void *)(uintptr_t)mvar, aihc_value(value),
      aihc_value(continuation)));
}

void aihc_wasm_transfer_thread_done(AihcMachine *machine) {
  aihc_set_transfer(aihc_trampoline_thread_done(machine));
}

void aihc_wasm_transfer_halt(AihcMachine *machine) {
  aihc_set_transfer((AihcTrampolineTransfer){aihc_halt(machine), NULL});
}

void aihc_wasm_transfer_start(AihcMachine *machine, AihcSlot root,
                              AihcSlot continuation,
                              AihcSlot update_continuation,
                              AihcSlot thread_done_continuation,
                              AihcEntry exit_code) {
  aihc_set_transfer(
      aihc_trampoline_start(machine, aihc_value(root), aihc_value(continuation),
                            aihc_value(update_continuation),
                            aihc_value(thread_done_continuation), exit_code));
}

void aihc_wasm_complete_io(int64_t result) {
  aihc_set_transfer(aihc_trampoline_resume(
      aihc_machine, aihc_complete_io(aihc_machine, result)));
}

int aihc_wasm_pump_transfers(void) {
  while (aihc_next_object_transfer.entry != NULL ||
         aihc_next_transfer.entry != NULL) {
    if (aihc_next_object_transfer.entry != NULL) {
      AihcWasmObjectTransfer transfer = aihc_next_object_transfer;
      aihc_next_object_transfer = (AihcWasmObjectTransfer){0};
      transfer.entry(transfer.machine, transfer.object, transfer.supplied,
                     transfer.continuation);
    } else {
      AihcTrampolineTransfer transfer = aihc_next_transfer;
      aihc_next_transfer = (AihcTrampolineTransfer){0};
      transfer.entry(transfer.arguments);
    }
  }
  return aihc_wasm_finished;
}
