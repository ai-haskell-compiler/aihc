#include "aihc_runtime_internal.h"
#include "aihc_wasm_internal.h"
#include "command.h"

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  AIHC_WASI_IO_NONE,
  AIHC_WASI_IO_STDIN_READ,
  AIHC_WASI_IO_STDOUT_WRITE,
  AIHC_WASI_IO_STDERR_WRITE,
  AIHC_WASI_IO_FILE_READ,
  AIHC_WASI_IO_FILE_WRITE,
  AIHC_WASI_IO_FILE_APPEND,
  AIHC_WASI_IO_FILE_OPEN,
  AIHC_WASI_IO_TIMER,
  AIHC_WASI_IO_HTTP_OPEN,
  AIHC_WASI_IO_HTTP_READ,
} AihcWasiIoKind;

typedef enum {
  AIHC_WASI_PENDING_NONE,
  AIHC_WASI_PENDING_STREAM_READ,
  AIHC_WASI_PENDING_STREAM_WRITE,
  AIHC_WASI_PENDING_FUTURE_READ,
  AIHC_WASI_PENDING_SUBTASK,
} AihcWasiPending;

typedef struct {
  AihcRootFrame roots;
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
  unsigned char *open_path;
  wasi_filesystem_types_result_own_descriptor_error_code_t open_result;
  wasi_filesystem_preopens_list_tuple2_own_descriptor_string_t directories;
  int has_directories;
  int subtask_returned;
  int stream_closed;
  wasi_http_client_result_own_response_error_code_t http_send_result;
  wasi_http_types_future_result_void_error_code_t http_transmit;
  int has_http_transmit;
  wasi_http_types_future_result_option_own_trailers_error_code_writer_t
      http_trailers_writer;
  int has_http_trailers_writer;
  size_t http_slot;
} AihcWasiIo;

/* An open HTTP response is a handle whose token is at least
   AIHC_HTTP_TOKEN_BASE, and reads of it continue one body stream. */
#define AIHC_HTTP_SLOTS 16
/* An HTTP status s outside 200 to 299 fails the open with the error number
   AIHC_HTTP_STATUS_ERRNO_BASE + s. */
#define AIHC_HTTP_STATUS_ERRNO_BASE 10000

typedef struct {
  int used;
  int stream_open;
  int trailers_open;
  int finished;
  wasi_http_types_stream_u8_t stream;
  wasi_http_types_future_result_option_own_trailers_error_code_t trailers;
  wasi_http_types_result_option_own_trailers_error_code_t trailers_result;
  int has_transmit_writer;
  wasi_http_types_future_result_void_error_code_writer_t transmit_writer;
} AihcHttpBody;

static AihcHttpBody aihc_http_bodies[AIHC_HTTP_SLOTS];

/* The values written to the two futures that tell the host a request has no
   trailers and a response needs no more transmission. A zeroed result is the
   ok case, and a zeroed option is none. */
static const wasi_http_types_result_option_own_trailers_error_code_t
    aihc_http_no_trailers;
static const wasi_http_types_result_void_error_code_t aihc_http_transmitted;

static AihcWasiIo aihc_wasi_io;
static AihcRootFrame *aihc_wasi_roots;

/* The canonical ABI can request storage only inside an explicit host scope. */
void *aihc_wasi_allocate(uint64_t bytes) {
  if (aihc_wasi_roots == NULL) {
    aihc_fail("canonical ABI allocation requires a host scope");
  }
  return aihc_byte_array_contents(
      aihc_host_byte_array(&aihc_machine, aihc_wasi_roots, bytes));
}

static void aihc_wasi_initialize_arguments(void) {
  aihc_machine_initialize();
  AihcRootFrame frame;
  aihc_roots_enter(&aihc_machine, &frame, 0, NULL);
  aihc_wasi_roots = &frame;
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
    buffer = aihc_wasi_allocate(length);
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
    command_list_string_free(&arguments);
    abort();
  }
  command_list_string_free(&arguments);
  aihc_roots_leave(&aihc_machine, &frame);
  aihc_wasi_roots = NULL;
}

static int64_t aihc_wasi_error(int32_t error) { return -((int64_t)error) - 1; }

static int32_t aihc_cli_error(wasi_cli_types_error_code_t error) {
  switch (error) {
  case WASI_CLI_TYPES_ERROR_CODE_ILLEGAL_BYTE_SEQUENCE:
    return EILSEQ;
  case WASI_CLI_TYPES_ERROR_CODE_PIPE:
    return EPIPE;
  default:
    return EIO;
  }
}

static int32_t aihc_filesystem_error(wasi_filesystem_types_error_code_t error) {
  switch (error.tag) {
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_ACCESS:
    return EACCES;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_ALREADY:
    return EALREADY;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_BAD_DESCRIPTOR:
    return EBADF;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_BUSY:
    return EBUSY;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_DEADLOCK:
    return EDEADLK;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_QUOTA:
    return EDQUOT;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_EXIST:
    return EEXIST;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_FILE_TOO_LARGE:
    return EFBIG;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_ILLEGAL_BYTE_SEQUENCE:
    return EILSEQ;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_IN_PROGRESS:
    return EINPROGRESS;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_INTERRUPTED:
    return EINTR;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_INVALID:
    return EINVAL;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_IO:
    return EIO;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_IS_DIRECTORY:
    return EISDIR;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_LOOP:
    return ELOOP;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_TOO_MANY_LINKS:
    return EMLINK;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_MESSAGE_SIZE:
    return EMSGSIZE;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NAME_TOO_LONG:
    return ENAMETOOLONG;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NO_DEVICE:
    return ENODEV;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NO_ENTRY:
    return ENOENT;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NO_LOCK:
    return ENOLCK;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_INSUFFICIENT_MEMORY:
    return ENOMEM;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_INSUFFICIENT_SPACE:
    return ENOSPC;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NOT_DIRECTORY:
    return ENOTDIR;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NOT_EMPTY:
    return ENOTEMPTY;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NOT_RECOVERABLE:
    return ENOTRECOVERABLE;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_UNSUPPORTED:
    return ENOTSUP;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NO_TTY:
    return ENOTTY;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NO_SUCH_DEVICE:
    return ENXIO;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_OVERFLOW:
    return EOVERFLOW;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_NOT_PERMITTED:
    return EPERM;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_PIPE:
    return EPIPE;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_READ_ONLY:
    return EROFS;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_INVALID_SEEK:
    return ESPIPE;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_TEXT_FILE_BUSY:
    return ETXTBSY;
  case WASI_FILESYSTEM_TYPES_ERROR_CODE_CROSS_DEVICE:
    return EXDEV;
  default:
    return EIO;
  }
}

static int32_t aihc_http_error(const wasi_http_types_error_code_t *error) {
  switch (error->tag) {
  case WASI_HTTP_TYPES_ERROR_CODE_DNS_TIMEOUT:
  case WASI_HTTP_TYPES_ERROR_CODE_CONNECTION_TIMEOUT:
  case WASI_HTTP_TYPES_ERROR_CODE_CONNECTION_READ_TIMEOUT:
  case WASI_HTTP_TYPES_ERROR_CODE_CONNECTION_WRITE_TIMEOUT:
  case WASI_HTTP_TYPES_ERROR_CODE_HTTP_RESPONSE_TIMEOUT:
    return ETIMEDOUT;
  case WASI_HTTP_TYPES_ERROR_CODE_DNS_ERROR:
  case WASI_HTTP_TYPES_ERROR_CODE_DESTINATION_NOT_FOUND:
    return EHOSTUNREACH;
  case WASI_HTTP_TYPES_ERROR_CODE_DESTINATION_UNAVAILABLE:
  case WASI_HTTP_TYPES_ERROR_CODE_DESTINATION_IP_PROHIBITED:
  case WASI_HTTP_TYPES_ERROR_CODE_DESTINATION_IP_UNROUTABLE:
    return ENETUNREACH;
  case WASI_HTTP_TYPES_ERROR_CODE_CONNECTION_REFUSED:
    return ECONNREFUSED;
  case WASI_HTTP_TYPES_ERROR_CODE_CONNECTION_TERMINATED:
    return ECONNRESET;
  case WASI_HTTP_TYPES_ERROR_CODE_TLS_PROTOCOL_ERROR:
  case WASI_HTTP_TYPES_ERROR_CODE_TLS_CERTIFICATE_ERROR:
  case WASI_HTTP_TYPES_ERROR_CODE_TLS_ALERT_RECEIVED:
    return EPROTO;
  default:
    return EIO;
  }
}

/* Complete a write to a future that the host has not read yet. The write
   either finished or is still pending, and a pending one is cancelled, so
   the writable end is always in a final state when it is dropped. */
static void aihc_http_finish_transmit_writer(
    wasi_http_types_future_result_void_error_code_writer_t writer) {
  wasi_http_types_future_result_void_error_code_cancel_write(writer);
  wasi_http_types_future_result_void_error_code_drop_writable(writer);
}

static void aihc_http_release(size_t slot) {
  AihcHttpBody *body = &aihc_http_bodies[slot];
  if (body->stream_open) {
    command_waitable_join(body->stream, 0);
    wasi_http_types_stream_u8_drop_readable(body->stream);
  }
  if (body->trailers_open) {
    wasi_http_types_future_result_option_own_trailers_error_code_drop_readable(
        body->trailers);
  }
  if (body->has_transmit_writer) {
    aihc_http_finish_transmit_writer(body->transmit_writer);
  }
  *body = (AihcHttpBody){0};
}

static int64_t aihc_wasi_finish(int64_t result) {
  if (aihc_wasi_io.has_http_trailers_writer) {
    wasi_http_types_future_result_option_own_trailers_error_code_cancel_write(
        aihc_wasi_io.http_trailers_writer);
    wasi_http_types_future_result_option_own_trailers_error_code_drop_writable(
        aihc_wasi_io.http_trailers_writer);
  }
  if (aihc_wasi_io.has_http_transmit) {
    wasi_http_types_future_result_void_error_code_drop_readable(
        aihc_wasi_io.http_transmit);
  }
  if (aihc_wasi_io.kind == AIHC_WASI_IO_HTTP_READ) {
    AihcHttpBody *body = &aihc_http_bodies[aihc_wasi_io.http_slot];
    if (body->stream_open) {
      /* The stream outlives this request, so it leaves the wait set that
         is dropped below. */
      command_waitable_join(body->stream, 0);
    }
  }
  if (aihc_wasi_io.has_directories) {
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &aihc_wasi_io.directories);
  }
  command_waitable_set_drop(aihc_wasi_io.wait_set);
  aihc_roots_leave(&aihc_machine, &aihc_wasi_io.roots);
  aihc_wasi_roots = NULL;
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
  /* The callback matches the event against the waitable of the request. An
     HTTP body keeps its handles beside the request, so the request learns
     the one it waits for here. */
  if (pending == AIHC_WASI_PENDING_FUTURE_READ) {
    aihc_wasi_io.future = waitable;
  } else {
    aihc_wasi_io.stream = waitable;
  }
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
      return aihc_wasi_finish(aihc_wasi_error(EPIPE));
    }
    uint32_t transferred = COMMAND_WAITABLE_COUNT(status);
    if (transferred == 0 && aihc_wasi_io.offset != aihc_wasi_io.length) {
      return aihc_wasi_finish(aihc_wasi_error(EIO));
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
      return aihc_wasi_finish(aihc_wasi_error(EIO));
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
      return aihc_wasi_finish(aihc_wasi_error(EPIPE));
    }
    uint32_t transferred = COMMAND_WAITABLE_COUNT(status);
    if (transferred == 0 && aihc_wasi_io.offset != aihc_wasi_io.length) {
      return aihc_wasi_finish(aihc_wasi_error(EIO));
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

static int64_t aihc_wasi_progress_http_open(void) {
  if (!aihc_wasi_io.subtask_returned) {
    return INT64_MIN;
  }
  wasi_http_client_result_own_response_error_code_t *sent =
      &aihc_wasi_io.http_send_result;
  if (sent->is_err) {
    int32_t error = aihc_http_error(&sent->val.err);
    wasi_http_types_error_code_free(&sent->val.err);
    return aihc_wasi_finish(aihc_wasi_error(error));
  }
  wasi_http_types_own_response_t response = sent->val.ok;
  wasi_http_types_status_code_t status =
      wasi_http_types_method_response_get_status_code(
          (wasi_http_types_borrow_response_t){response.__handle});
  if (status < 200 || status > 299) {
    wasi_http_types_response_drop_own(response);
    return aihc_wasi_finish(
        aihc_wasi_error(AIHC_HTTP_STATUS_ERRNO_BASE + (int32_t)status));
  }
  size_t slot = 0;
  while (slot < AIHC_HTTP_SLOTS && aihc_http_bodies[slot].used) {
    ++slot;
  }
  if (slot == AIHC_HTTP_SLOTS) {
    wasi_http_types_response_drop_own(response);
    return aihc_wasi_finish(aihc_wasi_error(EMFILE));
  }
  AihcHttpBody *body = &aihc_http_bodies[slot];
  wasi_http_types_future_result_void_error_code_writer_t writer;
  wasi_http_types_future_result_void_error_code_t reader =
      wasi_http_types_future_result_void_error_code_new(&writer);
  wasi_http_types_tuple2_stream_u8_future_result_option_own_trailers_error_code_t
      consumed;
  wasi_http_types_static_response_consume_body(response, reader, &consumed);
  command_waitable_status_t written =
      wasi_http_types_future_result_void_error_code_write(
          writer, &aihc_http_transmitted);
  body->used = 1;
  body->stream = consumed.f0;
  body->stream_open = 1;
  body->trailers = consumed.f1;
  body->trailers_open = 1;
  if (written == COMMAND_WAITABLE_STATUS_BLOCKED) {
    body->transmit_writer = writer;
    body->has_transmit_writer = 1;
  } else {
    wasi_http_types_future_result_void_error_code_drop_writable(writer);
  }
  return aihc_wasi_finish(AIHC_HTTP_TOKEN_BASE + (int64_t)slot);
}

static int64_t aihc_wasi_progress_http_read(void) {
  AihcHttpBody *body = &aihc_http_bodies[aihc_wasi_io.http_slot];
  if (body->stream_open) {
    command_waitable_status_t status;
    if (!aihc_wasi_take_completed_status(&status)) {
      status = wasi_http_types_stream_u8_read(body->stream, aihc_wasi_io.bytes,
                                              aihc_wasi_io.length);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(body->stream, AIHC_WASI_PENDING_STREAM_READ);
    }
    if (COMMAND_WAITABLE_STATE(status) == COMMAND_WAITABLE_COMPLETED) {
      uint32_t transferred = COMMAND_WAITABLE_COUNT(status);
      if (transferred != 0 || aihc_wasi_io.length == 0) {
        return aihc_wasi_finish((int64_t)transferred);
      }
      /* A body stream that completes no bytes is not at its end yet. */
      status = wasi_http_types_stream_u8_read(body->stream, aihc_wasi_io.bytes,
                                              aihc_wasi_io.length);
      if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
        return aihc_wasi_block(body->stream, AIHC_WASI_PENDING_STREAM_READ);
      }
      aihc_wasi_io.completed_status = status;
      aihc_wasi_io.has_completed_status = 1;
      return aihc_wasi_progress_http_read();
    }
    if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_DROPPED) {
      return aihc_wasi_finish(aihc_wasi_error(EIO));
    }
    command_waitable_join(body->stream, 0);
    wasi_http_types_stream_u8_drop_readable(body->stream);
    body->stream_open = 0;
  }
  if (body->trailers_open) {
    command_waitable_status_t status;
    if (!aihc_wasi_take_completed_status(&status)) {
      status =
          wasi_http_types_future_result_option_own_trailers_error_code_read(
              body->trailers, &body->trailers_result);
    }
    if (status == COMMAND_WAITABLE_STATUS_BLOCKED) {
      return aihc_wasi_block(body->trailers, AIHC_WASI_PENDING_FUTURE_READ);
    }
    int32_t error = 0;
    if (COMMAND_WAITABLE_STATE(status) != COMMAND_WAITABLE_COMPLETED) {
      error = EIO;
    } else if (body->trailers_result.is_err) {
      error = aihc_http_error(&body->trailers_result.val.err);
      wasi_http_types_error_code_free(&body->trailers_result.val.err);
    } else if (body->trailers_result.val.ok.is_some) {
      wasi_http_types_fields_drop_own(body->trailers_result.val.ok.val);
    }
    wasi_http_types_future_result_option_own_trailers_error_code_drop_readable(
        body->trailers);
    body->trailers_open = 0;
    body->finished = 1;
    if (error != 0) {
      return aihc_wasi_finish(aihc_wasi_error(error));
    }
  }
  return aihc_wasi_finish(0);
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
  case AIHC_WASI_IO_HTTP_OPEN:
    return aihc_wasi_progress_http_open();
  case AIHC_WASI_IO_HTTP_READ:
    return aihc_wasi_progress_http_read();
  case AIHC_WASI_IO_TIMER:
    return aihc_wasi_io.subtask_returned ? aihc_wasi_finish(1) : INT64_MIN;
  default:
    return aihc_wasi_error(EIO);
  }
}

static int aihc_wasi_start(AihcWasiIoKind kind, unsigned char *bytes,
                           size_t length) {
  if (aihc_wasi_io.kind != AIHC_WASI_IO_NONE) {
    return 0;
  }
  aihc_roots_enter(&aihc_machine, &aihc_wasi_io.roots, 0, NULL);
  aihc_wasi_roots = &aihc_wasi_io.roots;
  aihc_wasi_io.kind = kind;
  aihc_wasi_io.bytes = bytes;
  aihc_wasi_io.length = length;
  aihc_wasi_io.wait_set = command_waitable_set_new();
  return 1;
}

uint64_t aihc_wasip3_monotonic_ns(void) {
  return wasi_clocks_monotonic_clock_now();
}

int64_t aihc_wasip3_start_timer(uint64_t deadline) {
  if (!aihc_wasi_start(AIHC_WASI_IO_TIMER, NULL, 0)) {
    return INT64_MIN;
  }
  command_subtask_status_t status =
      wasi_clocks_monotonic_clock_wait_until(deadline);
  if (COMMAND_SUBTASK_STATE(status) == COMMAND_SUBTASK_RETURNED) {
    aihc_wasi_io.subtask_returned = 1;
  } else {
    aihc_wasi_io.subtask = COMMAND_SUBTASK_HANDLE(status);
    aihc_wasi_io.pending = AIHC_WASI_PENDING_SUBTASK;
    command_waitable_join(aihc_wasi_io.subtask, aihc_wasi_io.wait_set);
  }
  return aihc_wasi_progress();
}

int64_t aihc_wasip3_start_read(int32_t target, int32_t descriptor,
                               uint64_t offset, unsigned char *bytes,
                               size_t length) {
  AihcWasiIoKind kind = target == 0   ? AIHC_WASI_IO_STDIN_READ
                        : target == 4 ? AIHC_WASI_IO_HTTP_READ
                                      : AIHC_WASI_IO_FILE_READ;
  if ((target != 0 && target != 3 && target != 4) ||
      !aihc_wasi_start(kind, bytes, length)) {
    return aihc_wasi_error(EBADF);
  }
  if (kind == AIHC_WASI_IO_HTTP_READ) {
    aihc_wasi_io.http_slot = (size_t)(descriptor - AIHC_HTTP_TOKEN_BASE);
    return aihc_wasi_progress();
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
    return aihc_wasi_error(EBADF);
  }
  if (!aihc_wasi_start(kind, (unsigned char *)bytes, length)) {
    return aihc_wasi_error(EBADF);
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

static int aihc_http_has_prefix(const unsigned char *text, size_t length,
                                const char *prefix) {
  size_t prefix_length = strlen(prefix);
  return length >= prefix_length && memcmp(text, prefix, prefix_length) == 0;
}

static int aihc_http_is_url(const unsigned char *path, size_t length) {
  return aihc_http_has_prefix(path, length, "http://") ||
         aihc_http_has_prefix(path, length, "https://");
}

/* Send a GET request for the URL. The open completes when the response
   head arrives, and the response body is then read like a file. */
static int64_t aihc_wasip3_start_http_open(const unsigned char *url,
                                           size_t length, int32_t mode) {
  if (mode != 0) {
    return aihc_wasi_finish(aihc_wasi_error(EROFS));
  }
  int secure = aihc_http_has_prefix(url, length, "https://");
  size_t scheme_length = secure ? 8 : 7;
  size_t authority_end = scheme_length;
  while (authority_end < length && url[authority_end] != '/' &&
         url[authority_end] != '?' && url[authority_end] != '#') {
    ++authority_end;
  }
  size_t path_end = authority_end;
  while (path_end < length && url[path_end] != '#') {
    ++path_end;
  }
  if (authority_end == scheme_length) {
    return aihc_wasi_finish(aihc_wasi_error(EINVAL));
  }
  /* A query without a path starts with the path /. */
  unsigned char *path_with_query = NULL;
  size_t path_length = path_end - authority_end;
  const unsigned char *path_start = url + authority_end;
  if (path_length == 0 || path_start[0] == '?') {
    path_with_query = aihc_wasi_allocate(path_length + 1);
    path_with_query[0] = '/';
    if (path_length != 0) {
      memcpy(path_with_query + 1, path_start, path_length);
    }
    path_start = path_with_query;
    path_length += 1;
  }

  wasi_http_types_own_headers_t headers = wasi_http_types_constructor_fields();
  wasi_http_types_future_result_option_own_trailers_error_code_writer_t writer;
  wasi_http_types_future_result_option_own_trailers_error_code_t trailers =
      wasi_http_types_future_result_option_own_trailers_error_code_new(&writer);
  wasi_http_types_tuple2_own_request_future_result_void_error_code_t created;
  wasi_http_types_static_request_new(headers, NULL, trailers, NULL, &created);
  aihc_wasi_io.http_transmit = created.f1;
  aihc_wasi_io.has_http_transmit = 1;
  command_waitable_status_t written =
      wasi_http_types_future_result_option_own_trailers_error_code_write(
          writer, &aihc_http_no_trailers);
  if (written == COMMAND_WAITABLE_STATUS_BLOCKED) {
    aihc_wasi_io.http_trailers_writer = writer;
    aihc_wasi_io.has_http_trailers_writer = 1;
  } else {
    wasi_http_types_future_result_option_own_trailers_error_code_drop_writable(
        writer);
  }

  wasi_http_types_borrow_request_t request =
      (wasi_http_types_borrow_request_t){created.f0.__handle};
  wasi_http_types_scheme_t scheme = {.tag = secure
                                                ? WASI_HTTP_TYPES_SCHEME_HTTPS
                                                : WASI_HTTP_TYPES_SCHEME_HTTP};
  command_string_t authority = {(uint8_t *)(url + scheme_length),
                                authority_end - scheme_length};
  command_string_t query = {(uint8_t *)path_start, path_length};
  if (!wasi_http_types_method_request_set_scheme(request, &scheme) ||
      !wasi_http_types_method_request_set_authority(request, &authority) ||
      !wasi_http_types_method_request_set_path_with_query(request, &query)) {
    wasi_http_types_request_drop_own(created.f0);
    return aihc_wasi_finish(aihc_wasi_error(EINVAL));
  }

  command_subtask_status_t status =
      wasi_http_client_send(created.f0, &aihc_wasi_io.http_send_result);
  aihc_wasi_io.kind = AIHC_WASI_IO_HTTP_OPEN;
  if (COMMAND_SUBTASK_STATE(status) == COMMAND_SUBTASK_RETURNED) {
    aihc_wasi_io.subtask_returned = 1;
  } else {
    aihc_wasi_io.subtask = COMMAND_SUBTASK_HANDLE(status);
    aihc_wasi_io.pending = AIHC_WASI_PENDING_SUBTASK;
    command_waitable_join(aihc_wasi_io.subtask, aihc_wasi_io.wait_set);
  }
  return aihc_wasi_progress();
}

int64_t aihc_wasip3_start_open(const unsigned char *path, size_t length,
                               int32_t mode) {
  if (!aihc_wasi_start(AIHC_WASI_IO_FILE_OPEN, NULL, 0)) {
    return aihc_wasi_error(EBADF);
  }
  if (length == 0) {
    return aihc_wasi_finish(aihc_wasi_error(ENOENT));
  }
  if (memchr(path, 0, length) != NULL) {
    return aihc_wasi_finish(aihc_wasi_error(EINVAL));
  }
  if (aihc_http_is_url(path, length)) {
    return aihc_wasip3_start_http_open(path, length, mode);
  }
  command_string_t cwd = {0};
  if (path[0] != '/' && wasi_cli_environment_get_initial_cwd(&cwd)) {
    if (cwd.len != 0) {
      if (length == SIZE_MAX || cwd.len > SIZE_MAX - length - 1) {
        command_string_free(&cwd);
        return aihc_wasi_finish(aihc_wasi_error(ENAMETOOLONG));
      }
      size_t absolute_length = cwd.len + 1 + length;
      aihc_wasi_io.open_path = aihc_wasi_allocate(absolute_length);
      memcpy(aihc_wasi_io.open_path, cwd.ptr, cwd.len);
      aihc_wasi_io.open_path[cwd.len] = '/';
      memcpy(aihc_wasi_io.open_path + cwd.len + 1, path, length);
      path = aihc_wasi_io.open_path;
      length = absolute_length;
    }
    command_string_free(&cwd);
  }
  wasi_filesystem_preopens_list_tuple2_own_descriptor_string_t directories;
  wasi_filesystem_preopens_get_directories(&directories);
  /* Remove leading ./ components from relative paths. Leave .. for WASI
     to check against the selected directory capability. */
  while (length >= 2 && path[0] == '.' && path[1] == '/') {
    path += 2;
    length -= 2;
  }
  size_t directory_index = SIZE_MAX;
  size_t root_directory_index = SIZE_MAX;
  size_t prefix_length = 0;
  size_t relative_offset = 0;
  for (size_t index = 0; index < directories.len; ++index) {
    command_string_t name = directories.ptr[index].f1;
    while (name.len >= 2 && name.ptr[0] == '.' && name.ptr[1] == '/') {
      name.ptr += 2;
      name.len -= 2;
    }
    while (name.len > 1 && name.ptr[name.len - 1] == '/') {
      --name.len;
    }
    size_t offset;
    size_t matched_length = name.len;
    if (name.len == 0 || (name.len == 1 && name.ptr[0] == '.')) {
      if (length != 0 && path[0] == '/') {
        continue;
      }
      offset = 0;
      matched_length = 0;
    } else if (name.len == 1 && name.ptr[0] == '/') {
      root_directory_index = index;
      if (length == 0 || path[0] != '/') {
        continue;
      }
      offset = 1;
    } else {
      if (length < name.len || memcmp(path, name.ptr, name.len) != 0 ||
          (length > name.len && path[name.len] != '/')) {
        continue;
      }
      offset = name.len;
    }
    if (directory_index == SIZE_MAX || matched_length > prefix_length) {
      directory_index = index;
      prefix_length = matched_length;
      relative_offset = offset;
    }
  }
  /* Without an explicit relative preopen, resolve relative paths under /.
     A configured initial directory has already supplied its prefix. */
  if (directory_index == SIZE_MAX && length != 0 && path[0] != '/') {
    directory_index = root_directory_index;
  }
  if (directory_index == SIZE_MAX) {
    wasi_filesystem_preopens_list_tuple2_own_descriptor_string_free(
        &directories);
    return aihc_wasi_finish(aihc_wasi_error(EPERM));
  }
  while (relative_offset < length && path[relative_offset] == '/') {
    ++relative_offset;
  }
  path += relative_offset;
  length -= relative_offset;
  if (length == 0) {
    path = (const unsigned char *)".";
    length = 1;
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
    return aihc_wasi_finish(aihc_wasi_error(EINVAL));
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
  if (descriptor >= AIHC_HTTP_TOKEN_BASE) {
    aihc_http_release((size_t)(descriptor - AIHC_HTTP_TOKEN_BASE));
    return;
  }
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
    result.is_err = aihc_get_exit_status(&aihc_machine) != 0;
    exports_wasi_cli_run_run_return(result);
    return COMMAND_CALLBACK_CODE_EXIT;
  }
  return COMMAND_CALLBACK_CODE_WAIT(aihc_wasi_io.wait_set);
}

/* The generated command.c pulls in the object wit-bindgen would write beside
   it, which carries the component type of the world, by calling this symbol.
   The bindings are committed without that object and the link embeds the
   type from the world itself (wasm-tools component embed), so the symbol is
   defined here and does nothing. */
// NOLINTNEXTLINE(bugprone-reserved-identifier)
void __component_type_object_force_link_command(void);
// NOLINTNEXTLINE(bugprone-reserved-identifier)
void __component_type_object_force_link_command(void) {}

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
      aihc_lir_program_resume(aihc_complete_io(&aihc_machine, result)));
}
