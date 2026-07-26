#include "aihc_runtime_internal.h"

#include <stddef.h>

extern int64_t aihc_wasip3_start_read(int32_t target, int32_t descriptor,
                                      uint64_t offset, unsigned char *bytes,
                                      size_t length);
extern int64_t aihc_wasip3_start_write(int32_t target, int32_t descriptor,
                                       uint64_t offset, int32_t append,
                                       const unsigned char *bytes,
                                       size_t length);
extern int64_t aihc_wasip3_start_open(const unsigned char *path, size_t length,
                                      int32_t mode);
extern void aihc_wasip3_close(int32_t descriptor);

static AihcIoHandle aihc_standard_input = {(uintptr_t)0, 0, AIHC_IO_READABLE, 0,
                                           0};
static AihcIoHandle aihc_standard_output = {(uintptr_t)1, 0, AIHC_IO_WRITABLE,
                                            0, 0};
static AihcIoHandle aihc_standard_error = {(uintptr_t)2, 0, AIHC_IO_WRITABLE, 0,
                                           0};

_Noreturn void aihc_host_fail(const char *message) {
  (void)message;
  __builtin_trap();
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

void *aihc_io_stderr(void) { return &aihc_standard_error; }

static int aihc_wasip3_prepare(AihcIoRequest *request) {
  (void)request;
  return 0;
}

static int aihc_wasip3_try_request(AihcIoRequest *request, int64_t *result) {
  if (request->kind == AIHC_IO_OPEN) {
    *result = aihc_wasip3_start_open(request->buffer, request->length,
                                     (int32_t)request->mode);
    return *result != INT64_MIN;
  }
  size_t length = request->length;
  if (length > INT32_MAX) {
    length = INT32_MAX;
  }
  int32_t target;
  if (request->handle == &aihc_standard_input) {
    target = 0;
  } else if (request->handle == &aihc_standard_output) {
    target = 1;
  } else if (request->handle == &aihc_standard_error) {
    target = 2;
  } else {
    target = 3;
  }
  if (request->kind == AIHC_IO_READ) {
    *result = aihc_wasip3_start_read(
        target, (int32_t)request->handle->backend_token,
        request->handle->position, request->buffer + request->offset, length);
  } else {
    *result = aihc_wasip3_start_write(
        target, (int32_t)request->handle->backend_token,
        request->handle->position, request->handle->append,
        request->buffer + request->offset, length);
  }
  return *result != INT64_MIN;
}

static int64_t aihc_wasip3_finish_request(AihcIoRequest *request,
                                          int64_t result) {
  if (request->kind == AIHC_IO_OPEN) {
    if (result < 0) {
      return (int64_t)(uintptr_t)aihc_io_open_error((int)(-result - 1));
    }
    uint32_t capabilities;
    switch (request->mode) {
    case 0:
      capabilities = AIHC_IO_READABLE;
      break;
    case 1:
    case 2:
      capabilities = AIHC_IO_WRITABLE;
      break;
    case 3:
      capabilities = AIHC_IO_READABLE | AIHC_IO_WRITABLE;
      break;
    default:
      return (int64_t)(uintptr_t)aihc_io_open_error(
          AIHC_IO_ERROR_INVALID_ARGUMENT);
    }
    AihcIoHandle *handle = aihc_allocate_zeroed(sizeof(*handle));
    handle->backend_token = (uintptr_t)result;
    handle->capabilities = capabilities;
    handle->append = request->mode == 2;
    return (int64_t)(uintptr_t)handle;
  }
  if (result >= 0 && request->handle != &aihc_standard_input &&
      request->handle != &aihc_standard_output &&
      request->handle != &aihc_standard_error) {
    request->handle->position += (uint64_t)result;
  }
  return result;
}

static AihcIoPollOutcome aihc_wasip3_poll(AihcMachine *machine, int may_block) {
  (void)machine;
  return may_block ? AIHC_IO_POLL_SUSPENDED : AIHC_IO_POLL_PROGRESS;
}

static const AihcIoBackend aihc_wasip3_io_backend = {
    aihc_wasip3_prepare,
    aihc_wasip3_try_request,
    aihc_wasip3_finish_request,
    aihc_wasip3_poll,
};

const AihcIoBackend *aihc_host_io_backend(void) {
  return &aihc_wasip3_io_backend;
}

int64_t aihc_io_close(void *opaque_handle) {
  AihcIoHandle *handle = opaque_handle;
  if (handle == NULL || handle->closed) {
    return aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
  }
  handle->closed = 1;
  if (handle != &aihc_standard_input && handle != &aihc_standard_output &&
      handle != &aihc_standard_error) {
    aihc_wasip3_close((int32_t)handle->backend_token);
  }
  return 0;
}

_Noreturn int64_t aihc_io_raise_error(int64_t error) {
  (void)error;
  __builtin_trap();
}
