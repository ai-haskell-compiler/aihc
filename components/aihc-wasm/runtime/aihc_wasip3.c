#include "aihc_wasm_internal.h"
#include "command.h"

#include <stddef.h>
#include <stdint.h>

extern void aihc_wasm_program_initialize(void);

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
  if (aihc_wasm_pump_transfers()) {
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
  aihc_wasm_complete_io(result);
  return aihc_pump();
}
