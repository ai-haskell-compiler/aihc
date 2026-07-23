#include "aihc_runtime.h"
#include "command.h"

#include <stddef.h>
#include <stdint.h>

extern unsigned char aihc_heap_base __asm__("__heap_base");
extern void aihc_wasm_program_initialize(void);
extern AihcSlot aihc_arguments[];

AihcMachine *aihc_machine;
AihcPortableTransfer aihc_next_transfer;

static uintptr_t aihc_heap_next;
static int aihc_wasm_finished;
static struct {
  const unsigned char *bytes;
  size_t length;
  size_t offset;
  command_waitable_set_t wait_set;
  wasi_cli_stdout_stream_u8_writer_t stream;
  wasi_cli_stdout_future_result_void_error_code_t future;
  int active;
  int stream_closed;
} aihc_output;

static uintptr_t aihc_align(uintptr_t value, uintptr_t alignment) {
  return (value + alignment - 1U) & ~(alignment - 1U);
}

void *malloc(size_t size) {
  if (aihc_heap_next == 0) {
    aihc_heap_next = (uintptr_t)&aihc_heap_base;
  }
  size_t requested = size == 0 ? 1 : size;
  uintptr_t header = aihc_align(aihc_heap_next, 16);
  uintptr_t end = header + sizeof(size_t) + requested;
  if (end < header) {
    return NULL;
  }
  size_t memory_size = __builtin_wasm_memory_size(0) * 65536U;
  if (end > memory_size) {
    size_t pages = (end - memory_size + 65535U) / 65536U;
    if (__builtin_wasm_memory_grow(0, pages) == (size_t)-1) {
      return NULL;
    }
  }
  *(size_t *)header = requested;
  aihc_heap_next = end;
  return (void *)(header + sizeof(size_t));
}

void free(void *pointer) { (void)pointer; }

_Noreturn void abort(void) { __builtin_trap(); }

void *memset(void *destination, int byte, size_t length) {
  unsigned char *output = destination;
  for (size_t index = 0; index < length; ++index) {
    output[index] = (unsigned char)byte;
  }
  return destination;
}

void *memcpy(void *destination, const void *source, size_t length) {
  unsigned char *output = destination;
  const unsigned char *input = source;
  for (size_t index = 0; index < length; ++index) {
    output[index] = input[index];
  }
  return destination;
}

void *memmove(void *destination, const void *source, size_t length) {
  unsigned char *output = destination;
  const unsigned char *input = source;
  if (output < input) {
    return memcpy(destination, source, length);
  }
  for (size_t index = length; index != 0; --index) {
    output[index - 1] = input[index - 1];
  }
  return destination;
}

void *calloc(size_t count, size_t size) {
  if (size != 0 && count > SIZE_MAX / size) {
    return NULL;
  }
  size_t bytes = count * size;
  void *result = malloc(bytes == 0 ? 1 : bytes);
  if (result != NULL) {
    memset(result, 0, bytes);
  }
  return result;
}

void *realloc(void *pointer, size_t size) {
  if (pointer == NULL) {
    return malloc(size);
  }
  void *result = malloc(size);
  if (result == NULL) {
    return NULL;
  }
  size_t old_size = *(size_t *)((uintptr_t)pointer - sizeof(size_t));
  memcpy(result, pointer, old_size < size ? old_size : size);
  return result;
}

static AihcValue *aihc_value(AihcSlot slot) {
  return (AihcValue *)(uintptr_t)slot;
}

AihcSlot aihc_wasm_make_node(AihcMachine *machine, uint64_t tag,
                             const AihcInfo *info) {
  return (AihcSlot)(uintptr_t)aihc_make_node(machine, tag, info);
}

AihcSlot aihc_wasm_make_node_unchecked(AihcMachine *machine, uint64_t tag,
                                       const AihcInfo *info) {
  return (AihcSlot)(uintptr_t)aihc_make_node_unchecked(machine, tag, info);
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

AihcSlot aihc_wasm_slot_get(AihcSlot *slots, uint64_t index) {
  return slots[index];
}

void aihc_wasm_slot_set(AihcSlot *slots, uint64_t index, AihcSlot value) {
  slots[index] = value;
}

AihcSlot *aihc_wasm_slot_address(AihcSlot *slots, uint64_t index) {
  return slots + index;
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

static void aihc_set_transfer(AihcPortableTransfer transfer) {
  aihc_next_transfer = transfer;
}

void aihc_wasm_transfer_direct(AihcEntry entry, AihcSlot *arguments) {
  aihc_next_transfer = (AihcPortableTransfer){entry, arguments};
  if (entry == NULL) {
    aihc_wasm_finished = 1;
  }
}

void aihc_wasm_transfer_eval(AihcMachine *machine, AihcSlot *buffer,
                             AihcSlot value, uint64_t lifted,
                             AihcSlot continuation,
                             AihcSlot update_continuation) {
  aihc_set_transfer(aihc_portable_eval_cps(machine, buffer, aihc_value(value),
                                           lifted, aihc_value(continuation),
                                           aihc_value(update_continuation)));
}

void aihc_wasm_transfer_apply(AihcMachine *machine, AihcSlot *buffer,
                              AihcSlot function, uint64_t count,
                              const AihcSlot *arguments,
                              AihcSlot continuation) {
  aihc_set_transfer(
      aihc_portable_apply_cps(machine, buffer, aihc_value(function), count,
                              arguments, aihc_value(continuation)));
}

void aihc_wasm_transfer_continue(AihcMachine *machine, AihcSlot *buffer,
                                 AihcSlot continuation, uint64_t count,
                                 const AihcSlot *values) {
  aihc_set_transfer(aihc_portable_continue_values(
      machine, buffer, aihc_value(continuation), count, values));
}

void aihc_wasm_transfer_fork(AihcMachine *machine, AihcSlot *buffer,
                             AihcSlot action, AihcSlot continuation) {
  aihc_set_transfer(aihc_portable_fork_cps(machine, buffer, aihc_value(action),
                                           aihc_value(continuation)));
}

void aihc_wasm_transfer_yield(AihcMachine *machine, AihcSlot *buffer,
                              AihcSlot continuation) {
  aihc_set_transfer(
      aihc_portable_yield_cps(machine, buffer, aihc_value(continuation)));
}

void aihc_wasm_transfer_await_io(AihcMachine *machine, AihcSlot *buffer,
                                 AihcSlot request, AihcSlot continuation) {
  aihc_set_transfer(aihc_portable_await_io_cps(
      machine, buffer, (void *)(uintptr_t)request, aihc_value(continuation)));
}

void aihc_wasm_transfer_thread_done(AihcMachine *machine, AihcSlot *buffer) {
  aihc_set_transfer(aihc_portable_thread_done(machine, buffer));
}

void aihc_wasm_transfer_halt(AihcMachine *machine) {
  aihc_set_transfer((AihcPortableTransfer){aihc_halt(machine), NULL});
}

void aihc_wasm_transfer_start(AihcMachine *machine, AihcSlot *buffer,
                              AihcSlot root, AihcSlot continuation,
                              AihcSlot update_continuation,
                              AihcSlot thread_done_continuation,
                              AihcEntry exit_code) {
  aihc_set_transfer(aihc_portable_start(
      machine, buffer, aihc_value(root), aihc_value(continuation),
      aihc_value(update_continuation), aihc_value(thread_done_continuation),
      exit_code));
}

static int32_t aihc_output_progress(void) {
  if (!aihc_output.stream_closed) {
    command_waitable_status_t status = wasi_cli_stdout_stream_u8_write(
        aihc_output.stream, aihc_output.bytes + aihc_output.offset,
        aihc_output.length - aihc_output.offset);
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      command_waitable_join(aihc_output.stream, aihc_output.wait_set);
      return INT32_MIN;
    }
    if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
      return -1;
    }
    aihc_output.offset += COMMAND_WAITABLE_COUNT(status);
    if (aihc_output.offset != aihc_output.length) {
      return INT32_MIN;
    }
    wasi_cli_stdout_stream_u8_drop_writable(aihc_output.stream);
    aihc_output.stream_closed = 1;
  }

  wasi_cli_stdout_result_void_error_code_t result;
  command_waitable_status_t status =
      wasi_cli_stdout_future_result_void_error_code_read(aihc_output.future,
                                                         &result);
  if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
    command_waitable_join(aihc_output.future, aihc_output.wait_set);
    return INT32_MIN;
  }
  int32_t transferred =
      COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED &&
              !result.is_err
          ? (int32_t)aihc_output.length
          : -1;
  wasi_cli_stdout_future_result_void_error_code_drop_readable(
      aihc_output.future);
  command_waitable_set_drop(aihc_output.wait_set);
  aihc_output.active = 0;
  return transferred;
}

int32_t aihc_wasip3_start_write(const unsigned char *bytes, size_t length) {
  if (aihc_output.active) {
    return INT32_MIN;
  }
  aihc_output.bytes = bytes;
  aihc_output.length = length;
  aihc_output.offset = 0;
  aihc_output.wait_set = command_waitable_set_new();
  aihc_output.stream_closed = 0;
  wasi_cli_stdout_stream_u8_t reader =
      wasi_cli_stdout_stream_u8_new(&aihc_output.stream);
  aihc_output.future = wasi_cli_stdout_write_via_stream(reader);
  aihc_output.active = 1;
  return aihc_output_progress();
}

static command_callback_code_t aihc_pump(void) {
  while (aihc_next_transfer.entry != NULL) {
    AihcPortableTransfer transfer = aihc_next_transfer;
    aihc_next_transfer = (AihcPortableTransfer){0};
    transfer.entry(transfer.arguments);
  }
  if (aihc_wasm_finished) {
    exports_wasi_cli_run_result_void_void_t result = {0};
    exports_wasi_cli_run_run_return(result);
    return COMMAND_CALLBACK_CODE_EXIT;
  }
  return COMMAND_CALLBACK_CODE_WAIT(aihc_output.wait_set);
}

command_callback_code_t exports_wasi_cli_run_run(void) {
  aihc_wasm_program_initialize();
  return aihc_pump();
}

command_callback_code_t
exports_wasi_cli_run_run_callback(command_event_t *event) {
  (void)event;
  int32_t result = aihc_output_progress();
  if (result == INT32_MIN) {
    return COMMAND_CALLBACK_CODE_WAIT(aihc_output.wait_set);
  }
  aihc_set_transfer(
      aihc_wasip3_complete_io(aihc_machine, aihc_arguments, result));
  return aihc_pump();
}
