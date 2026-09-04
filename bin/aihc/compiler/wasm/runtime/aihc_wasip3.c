#include "aihc_runtime_internal.h"
#include "aihc_wasm_internal.h"
#include "command.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

AihcMachine *aihc_machine;

typedef enum {
  AIHC_WASI_IO_NONE,
  AIHC_WASI_IO_STDIN_READ,
  AIHC_WASI_IO_STDOUT_WRITE,
  AIHC_WASI_IO_STDERR_WRITE,
  AIHC_WASI_IO_FILE_READ,
  AIHC_WASI_IO_FILE_WRITE,
  AIHC_WASI_IO_FILE_APPEND,
  AIHC_WASI_IO_FILE_OPEN,
} AihcWasiIoKind;

typedef enum {
  AIHC_WASI_PENDING_NONE,
  AIHC_WASI_PENDING_STREAM_READ,
  AIHC_WASI_PENDING_STREAM_WRITE,
  AIHC_WASI_PENDING_FUTURE_READ,
  AIHC_WASI_PENDING_SUBTASK,
} AihcWasiPending;

typedef struct {
  AihcWasiIoKind kind;
  unsigned char *bytes;
  size_t length;
  size_t offset;
  command_waitable_set_t wait_set;
  uint32_t stream;
  uint32_t future;
  AihcWasiPending pending;
  command_waitable_status_t completed_status;
  int has_completed_status;
  command_subtask_t subtask;
  wasi_cli_stdin_result_void_error_code_t stdin_result;
  wasi_cli_stdout_result_void_error_code_t stdout_result;
  wasi_cli_stderr_result_void_error_code_t stderr_result;
  wasi_filesystem_types_result_void_error_code_t filesystem_result;
  wasi_filesystem_types_method_descriptor_open_at_args_t open_arguments;
  wasi_filesystem_types_result_own_descriptor_error_code_t open_result;
  wasi_filesystem_preopens_list_tuple2_own_descriptor_string_t directories;
  int has_directories;
  int subtask_returned;
  int stream_closed;
} AihcWasiIo;

static AihcWasiIo aihc_wasi_io;

static void aihc_wasi_initialize_arguments(void) {
  command_list_string_t arguments = {0};
  wasi_cli_environment_get_arguments(&arguments);
  size_t length = 0;
  for (size_t index = 0; index < arguments.len; ++index) {
    if ((uint64_t)arguments.ptr[index].len >=
        (uint64_t)INT64_MAX - (uint64_t)length) {
      command_list_string_free(&arguments);
      abort();
    }
    length += arguments.ptr[index].len + 1;
  }
  uint8_t *buffer = NULL;
  if (length != 0) {
    buffer = malloc(length);
    if (buffer == NULL) {
      command_list_string_free(&arguments);
      abort();
    }
    size_t offset = 0;
    for (size_t index = 0; index < arguments.len; ++index) {
      command_string_t argument = arguments.ptr[index];
      if (argument.len != 0) {
        memcpy(buffer + offset, argument.ptr, argument.len);
      }
      offset += argument.len;
      buffer[offset++] = 0;
    }
  }
  if (aihc_runtime_arguments_initialize(buffer, (int64_t)length) != 0) {
    free(buffer);
    command_list_string_free(&arguments);
    abort();
  }
  free(buffer);
  command_list_string_free(&arguments);
}

static int64_t aihc_wasi_error(int32_t error) { return -((int64_t)error) - 1; }

static int32_t aihc_cli_error(wasi_cli_types_error_code_t error) {
  switch (error) {
  case WASI_CLI_TYPES_ERROR_CODE_ILLEGAL_BYTE_SEQUENCE:
    return 84;
  case WASI_CLI_TYPES_ERROR_CODE_PIPE:
    return 32;
  default:
    return 5;
  }
}

static int32_t aihc_filesystem_error(wasi_filesystem_types_error_code_t error) {
  static const int32_t errors[] = {
      13, 114, 9,  16, 35, 122, 17, 27,  84, 115, 4, 22, 5, 21, 40, 31, 90, 36,
      19, 2,   37, 12, 28, 20,  39, 131, 95, 25,  6, 75, 1, 32, 30, 29, 26, 18,
  };
  size_t index = (size_t)error.tag;
  return index < sizeof(errors) / sizeof(errors[0]) ? errors[index] : 5;
}

static int64_t aihc_wasi_finish(int64_t result) {
  if (aihc_wasi_io.has_directories) {
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &aihc_wasi_io.directories);
  }
  command_waitable_set_drop(aihc_wasi_io.wait_set);
  aihc_wasi_io = (AihcWasiIo){0};
  return result;
}

static int aihc_wasi_take_completed_status(command_waitable_status_t *status) {
  if (!aihc_wasi_io.has_completed_status) {
    return 0;
  }
  *status = aihc_wasi_io.completed_status;
  aihc_wasi_io.has_completed_status = 0;
  return 1;
}

static int64_t aihc_wasi_block(uint32_t waitable, AihcWasiPending pending) {
  aihc_wasi_io.pending = pending;
  command_waitable_join(waitable, aihc_wasi_io.wait_set);
  return INT64_MIN;
}

static int64_t aihc_wasi_progress_cli_write(void) {
  while (!aihc_wasi_io.stream_closed) {
    command_waitable_status_t status;
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_cli_stdin_stream_u8_write(
          aihc_wasi_io.stream, aihc_wasi_io.bytes + aihc_wasi_io.offset,
          aihc_wasi_io.length - aihc_wasi_io.offset);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.stream,
                             AIHC_WASI_PENDING_STREAM_WRITE);
    }
    if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
      return aihc_wasi_finish(aihc_wasi_error(32));
    }
    uint32_t transferred = COMMAND_WAITABLE_COUNT(status);
    if (transferred == 0 && aihc_wasi_io.offset != aihc_wasi_io.length) {
      return aihc_wasi_finish(aihc_wasi_error(5));
    }
    aihc_wasi_io.offset += transferred;
    if (aihc_wasi_io.offset == aihc_wasi_io.length) {
      wasi_cli_stdin_stream_u8_drop_writable(aihc_wasi_io.stream);
      aihc_wasi_io.stream_closed = 1;
    }
  }

  command_waitable_status_t status;
  int32_t error = 0;
  if (aihc_wasi_io.kind == AIHC_WASI_IO_STDOUT_WRITE) {
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_cli_stdout_future_result_void_error_code_read(
          aihc_wasi_io.future, &aihc_wasi_io.stdout_result);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.future,
                             AIHC_WASI_PENDING_FUTURE_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED &&
        aihc_wasi_io.stdout_result.is_err) {
      error = aihc_cli_error(aihc_wasi_io.stdout_result.val.err);
    }
    wasi_cli_stdout_future_result_void_error_code_drop_readable(
        aihc_wasi_io.future);
  } else {
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_cli_stderr_future_result_void_error_code_read(
          aihc_wasi_io.future, &aihc_wasi_io.stderr_result);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.future,
                             AIHC_WASI_PENDING_FUTURE_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED &&
        aihc_wasi_io.stderr_result.is_err) {
      error = aihc_cli_error(aihc_wasi_io.stderr_result.val.err);
    }
    wasi_cli_stderr_future_result_void_error_code_drop_readable(
        aihc_wasi_io.future);
  }
  if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
    error = 5;
  }
  return aihc_wasi_finish(error == 0 ? (int64_t)aihc_wasi_io.length
                                     : aihc_wasi_error(error));
}

static int64_t aihc_wasi_progress_read(void) {
  if (!aihc_wasi_io.stream_closed) {
    command_waitable_status_t status;
    if (!aihc_wasi_take_completed_status(&status)) {
      if (aihc_wasi_io.kind == AIHC_WASI_IO_STDIN_READ) {
        status = wasi_cli_stdin_stream_u8_read(
            aihc_wasi_io.stream, aihc_wasi_io.bytes, aihc_wasi_io.length);
      } else {
        status = wasi_filesystem_types_stream_u8_read(
            aihc_wasi_io.stream, aihc_wasi_io.bytes, aihc_wasi_io.length);
      }
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.stream,
                             AIHC_WASI_PENDING_STREAM_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED) {
      aihc_wasi_io.offset = COMMAND_WAITABLE_COUNT(status);
    } else if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_DROPPED) {
      return aihc_wasi_finish(aihc_wasi_error(5));
    }
    if (aihc_wasi_io.kind == AIHC_WASI_IO_STDIN_READ) {
      wasi_cli_stdin_stream_u8_drop_readable(aihc_wasi_io.stream);
    } else {
      wasi_filesystem_types_stream_u8_drop_readable(aihc_wasi_io.stream);
    }
    aihc_wasi_io.stream_closed = 1;
  }

  command_waitable_status_t status;
  int32_t error = 0;
  if (aihc_wasi_io.kind == AIHC_WASI_IO_STDIN_READ) {
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_cli_stdin_future_result_void_error_code_read(
          aihc_wasi_io.future, &aihc_wasi_io.stdin_result);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.future,
                             AIHC_WASI_PENDING_FUTURE_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED &&
        aihc_wasi_io.stdin_result.is_err) {
      error = aihc_cli_error(aihc_wasi_io.stdin_result.val.err);
    }
    wasi_cli_stdin_future_result_void_error_code_drop_readable(
        aihc_wasi_io.future);
  } else {
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_filesystem_types_future_result_void_error_code_read(
          aihc_wasi_io.future, &aihc_wasi_io.filesystem_result);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.future,
                             AIHC_WASI_PENDING_FUTURE_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED &&
        aihc_wasi_io.filesystem_result.is_err) {
      error = aihc_filesystem_error(aihc_wasi_io.filesystem_result.val.err);
    }
    wasi_filesystem_types_future_result_void_error_code_drop_readable(
        aihc_wasi_io.future);
  }
  if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
    error = 5;
  }
  return aihc_wasi_finish(error == 0 ? (int64_t)aihc_wasi_io.offset
                                     : aihc_wasi_error(error));
}

static int64_t aihc_wasi_progress_file_write(void) {
  while (!aihc_wasi_io.stream_closed) {
    command_waitable_status_t status;
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_filesystem_types_stream_u8_write(
          aihc_wasi_io.stream, aihc_wasi_io.bytes + aihc_wasi_io.offset,
          aihc_wasi_io.length - aihc_wasi_io.offset);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(aihc_wasi_io.stream,
                             AIHC_WASI_PENDING_STREAM_WRITE);
    }
    if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
      return aihc_wasi_finish(aihc_wasi_error(32));
    }
    uint32_t transferred = COMMAND_WAITABLE_COUNT(status);
    if (transferred == 0 && aihc_wasi_io.offset != aihc_wasi_io.length) {
      return aihc_wasi_finish(aihc_wasi_error(5));
    }
    aihc_wasi_io.offset += transferred;
    if (aihc_wasi_io.offset == aihc_wasi_io.length) {
      wasi_filesystem_types_stream_u8_drop_writable(aihc_wasi_io.stream);
      aihc_wasi_io.stream_closed = 1;
    }
  }

  command_waitable_status_t status;
  if (!aihc_wasi_take_completed_status(&status)) {
    status = wasi_filesystem_types_future_result_void_error_code_read(
        aihc_wasi_io.future, &aihc_wasi_io.filesystem_result);
  }
  if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
    return aihc_wasi_block(aihc_wasi_io.future, AIHC_WASI_PENDING_FUTURE_READ);
  }
  int32_t error =
      COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED
          ? 5
          : (aihc_wasi_io.filesystem_result.is_err
                 ? aihc_filesystem_error(aihc_wasi_io.filesystem_result.val.err)
                 : 0);
  wasi_filesystem_types_future_result_void_error_code_drop_readable(
      aihc_wasi_io.future);
  return aihc_wasi_finish(error == 0 ? (int64_t)aihc_wasi_io.length
                                     : aihc_wasi_error(error));
}

static int64_t aihc_wasi_progress_open(void) {
  if (!aihc_wasi_io.subtask_returned) {
    return INT64_MIN;
  }
  int64_t opened = aihc_wasi_io.open_result.is_err
                       ? aihc_wasi_error(aihc_filesystem_error(
                             aihc_wasi_io.open_result.val.err))
                       : (int64_t)aihc_wasi_io.open_result.val.ok.__handle;
  return aihc_wasi_finish(opened);
}

static int64_t aihc_wasi_progress(void) {
  switch (aihc_wasi_io.kind) {
  case AIHC_WASI_IO_STDIN_READ:
  case AIHC_WASI_IO_FILE_READ:
    return aihc_wasi_progress_read();
  case AIHC_WASI_IO_STDOUT_WRITE:
  case AIHC_WASI_IO_STDERR_WRITE:
    return aihc_wasi_progress_cli_write();
  case AIHC_WASI_IO_FILE_WRITE:
  case AIHC_WASI_IO_FILE_APPEND:
    return aihc_wasi_progress_file_write();
  case AIHC_WASI_IO_FILE_OPEN:
    return aihc_wasi_progress_open();
  default:
    return aihc_wasi_error(5);
  }
}

static int aihc_wasi_start(AihcWasiIoKind kind, unsigned char *bytes,
                           size_t length) {
  if (aihc_wasi_io.kind != AIHC_WASI_IO_NONE) {
    return 0;
  }
  aihc_wasi_io.kind = kind;
  aihc_wasi_io.bytes = bytes;
  aihc_wasi_io.length = length;
  aihc_wasi_io.wait_set = command_waitable_set_new();
  return 1;
}

int64_t aihc_wasip3_start_read(int32_t target, int32_t descriptor,
                               uint64_t offset, unsigned char *bytes,
                               size_t length) {
  AihcWasiIoKind kind =
      target == 0 ? AIHC_WASI_IO_STDIN_READ : AIHC_WASI_IO_FILE_READ;
  if ((target != 0 && target != 3) || !aihc_wasi_start(kind, bytes, length)) {
    return aihc_wasi_error(9);
  }
  if (kind == AIHC_WASI_IO_STDIN_READ) {
    wasi_cli_stdin_tuple2_stream_u8_future_result_void_error_code_t input;
    wasi_cli_stdin_read_via_stream(&input);
    aihc_wasi_io.stream = input.f0;
    aihc_wasi_io.future = input.f1;
  } else {
    wasi_filesystem_types_own_descriptor_t own = {descriptor};
    wasi_filesystem_types_tuple2_stream_u8_future_result_void_error_code_t
        input;
    wasi_filesystem_types_method_descriptor_read_via_stream(
        wasi_filesystem_types_borrow_descriptor(own), offset, &input);
    aihc_wasi_io.stream = input.f0;
    aihc_wasi_io.future = input.f1;
  }
  return aihc_wasi_progress();
}

int64_t aihc_wasip3_start_write(int32_t target, int32_t descriptor,
                                uint64_t offset, int32_t append,
                                const unsigned char *bytes, size_t length) {
  AihcWasiIoKind kind;
  if (target == 1) {
    kind = AIHC_WASI_IO_STDOUT_WRITE;
  } else if (target == 2) {
    kind = AIHC_WASI_IO_STDERR_WRITE;
  } else if (target == 3) {
    kind = append ? AIHC_WASI_IO_FILE_APPEND : AIHC_WASI_IO_FILE_WRITE;
  } else {
    return aihc_wasi_error(9);
  }
  if (!aihc_wasi_start(kind, (unsigned char *)bytes, length)) {
    return aihc_wasi_error(9);
  }

  if (target == 1 || target == 2) {
    wasi_cli_stdin_stream_u8_writer_t writer;
    wasi_cli_stdin_stream_u8_t reader = wasi_cli_stdin_stream_u8_new(&writer);
    aihc_wasi_io.stream = writer;
    aihc_wasi_io.future = target == 1
                              ? wasi_cli_stdout_write_via_stream(reader)
                              : wasi_cli_stderr_write_via_stream(reader);
  } else {
    wasi_filesystem_types_stream_u8_writer_t writer;
    wasi_filesystem_types_stream_u8_t reader =
        wasi_filesystem_types_stream_u8_new(&writer);
    wasi_filesystem_types_own_descriptor_t own = {descriptor};
    wasi_filesystem_types_borrow_descriptor_t borrowed =
        wasi_filesystem_types_borrow_descriptor(own);
    aihc_wasi_io.stream = writer;
    aihc_wasi_io.future =
        append ? wasi_filesystem_types_method_descriptor_append_via_stream(
                     borrowed, reader)
               : wasi_filesystem_types_method_descriptor_write_via_stream(
                     borrowed, reader, offset);
  }
  return aihc_wasi_progress();
}

int64_t aihc_wasip3_start_open(const unsigned char *path, size_t length,
                               int32_t mode) {
  if (!aihc_wasi_start(AIHC_WASI_IO_FILE_OPEN, NULL, 0)) {
    return aihc_wasi_error(9);
  }
  wasi_filesystem_preopens_list_tuple2_own_descriptor_string_t directories;
  wasi_filesystem_preopens_get_directories(&directories);
  if (directories.len == 0) {
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &directories);
    return aihc_wasi_finish(aihc_wasi_error(1));
  }

  size_t directory_index = 0;
  int found_current_directory = 0;
  for (size_t index = 0; index < directories.len; ++index) {
    command_string_t name = directories.ptr[index].f1;
    if (name.len == 1 && name.ptr[0] == '.') {
      directory_index = index;
      found_current_directory = 1;
      break;
    }
  }
  if (!found_current_directory) {
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &directories);
    return aihc_wasi_finish(aihc_wasi_error(1));
  }
  wasi_filesystem_types_open_flags_t open_flags = 0;
  wasi_filesystem_types_descriptor_flags_t descriptor_flags = 0;
  switch (mode) {
  case 0:
    descriptor_flags = WASI_FILESYSTEM_TYPES_DESCRIPTOR_FLAGS_READ;
    break;
  case 1:
    open_flags = WASI_FILESYSTEM_TYPES_OPEN_FLAGS_CREATE |
                 WASI_FILESYSTEM_TYPES_OPEN_FLAGS_TRUNCATE;
    descriptor_flags = WASI_FILESYSTEM_TYPES_DESCRIPTOR_FLAGS_WRITE;
    break;
  case 2:
    open_flags = WASI_FILESYSTEM_TYPES_OPEN_FLAGS_CREATE;
    descriptor_flags = WASI_FILESYSTEM_TYPES_DESCRIPTOR_FLAGS_WRITE;
    break;
  case 3:
    open_flags = WASI_FILESYSTEM_TYPES_OPEN_FLAGS_CREATE;
    descriptor_flags = WASI_FILESYSTEM_TYPES_DESCRIPTOR_FLAGS_READ |
                       WASI_FILESYSTEM_TYPES_DESCRIPTOR_FLAGS_WRITE;
    break;
  default:
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &directories);
    return aihc_wasi_finish(aihc_wasi_error(22));
  }
  aihc_wasi_io.directories = directories;
  aihc_wasi_io.has_directories = 1;
  wasi_filesystem_types_own_descriptor_t directory =
      aihc_wasi_io.directories.ptr[directory_index].f0;
  aihc_wasi_io.open_arguments =
      (wasi_filesystem_types_method_descriptor_open_at_args_t){
          wasi_filesystem_types_borrow_descriptor(directory),
          0,
          {(uint8_t *)path, length},
          open_flags,
          descriptor_flags,
      };
  command_subtask_status_t status =
      wasi_filesystem_types_method_descriptor_open_at(
          &aihc_wasi_io.open_arguments, &aihc_wasi_io.open_result);
  if (COMMAND_SUBTASK_STATE(status) == COMMAND_SUBTASK_RETURNED) {
    aihc_wasi_io.subtask_returned = 1;
  } else {
    aihc_wasi_io.subtask = COMMAND_SUBTASK_HANDLE(status);
    aihc_wasi_io.pending = AIHC_WASI_PENDING_SUBTASK;
    command_waitable_join(aihc_wasi_io.subtask, aihc_wasi_io.wait_set);
  }
  return aihc_wasi_progress();
}

void aihc_wasip3_close(int32_t descriptor) {
  wasi_filesystem_types_own_descriptor_t own = {descriptor};
  wasi_filesystem_types_descriptor_drop_own(own);
}

/* A Lir trap has no synchronous error stream on this host, so the message
   is dropped and the component traps. */
_Noreturn void aihc_lir_trap(const uint8_t *message, uint64_t length) {
  (void)message;
  (void)length;
  __builtin_trap();
}

static command_callback_code_t aihc_pump(int32_t finished) {
  if (finished) {
    exports_wasi_cli_run_result_void_void_t result = {0};
    result.is_err = aihc_get_exit_status(aihc_machine) != 0;
    exports_wasi_cli_run_run_return(result);
    return COMMAND_CALLBACK_CODE_EXIT;
  }
  return COMMAND_CALLBACK_CODE_WAIT(aihc_wasi_io.wait_set);
}

command_callback_code_t exports_wasi_cli_run_run(void) {
  aihc_wasi_initialize_arguments();
  return aihc_pump(aihc_lir_program_start());
}

command_callback_code_t
exports_wasi_cli_run_run_callback(command_event_t *event) {
  if (aihc_wasi_io.pending == AIHC_WASI_PENDING_SUBTASK) {
    if (event->event != COMMAND_EVENT_SUBTASK ||
        event->waitable != aihc_wasi_io.subtask ||
        event->code != COMMAND_SUBTASK_RETURNED) {
      return COMMAND_CALLBACK_CODE_EXIT;
    }
    command_subtask_drop(aihc_wasi_io.subtask);
    aihc_wasi_io.pending = AIHC_WASI_PENDING_NONE;
    aihc_wasi_io.subtask_returned = 1;
  } else {
    command_event_code_t expected_event;
    uint32_t expected_waitable;
    switch (aihc_wasi_io.pending) {
    case AIHC_WASI_PENDING_STREAM_READ:
      expected_event = COMMAND_EVENT_STREAM_READ;
      expected_waitable = aihc_wasi_io.stream;
      break;
    case AIHC_WASI_PENDING_STREAM_WRITE:
      expected_event = COMMAND_EVENT_STREAM_WRITE;
      expected_waitable = aihc_wasi_io.stream;
      break;
    case AIHC_WASI_PENDING_FUTURE_READ:
      expected_event = COMMAND_EVENT_FUTURE_READ;
      expected_waitable = aihc_wasi_io.future;
      break;
    default:
      return COMMAND_CALLBACK_CODE_EXIT;
    }
    if (event->event != expected_event ||
        event->waitable != expected_waitable) {
      return COMMAND_CALLBACK_CODE_EXIT;
    }
    aihc_wasi_io.pending = AIHC_WASI_PENDING_NONE;
    aihc_wasi_io.completed_status = event->code;
    aihc_wasi_io.has_completed_status = 1;
  }
  int64_t result = aihc_wasi_progress();
  if (result == INT64_MIN) {
    return COMMAND_CALLBACK_CODE_WAIT(aihc_wasi_io.wait_set);
  }
  return aihc_pump(
      aihc_lir_program_resume(aihc_complete_io(aihc_machine, result)));
}
