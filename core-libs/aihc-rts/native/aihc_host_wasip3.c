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
extern uint64_t aihc_wasip3_monotonic_ns(void);
extern int64_t aihc_wasip3_start_timer(uint64_t deadline);

static AihcIoHandle aihc_standard_input = {.backend_token = 0,
                                           .capabilities = AIHC_IO_READABLE};
static AihcIoHandle aihc_standard_output = {.backend_token = 1,
                                            .capabilities = AIHC_IO_WRITABLE};
static AihcIoHandle aihc_standard_error = {.backend_token = 2,
                                           .capabilities = AIHC_IO_WRITABLE};

_Noreturn void aihc_host_fail(const char *message) {
  (void)message;
  __builtin_trap();
}

/* The statistics hook is not implemented on this host. The P3 driver
   reads no environment, so AIHC_RTS_STATS is never seen, and every file
   write on this host is one asynchronous stream that the driver pumps,
   which the exit path cannot wait for. The statistics hook stays documented in
   docs/native-runtime-objects.md. */
void aihc_program_environment_initialize(void) {}

uint64_t aihc_host_monotonic_ns(void) { return aihc_wasip3_monotonic_ns(); }

int aihc_host_write_file(const char *path, const void *bytes, size_t length) {
  (void)path;
  (void)bytes;
  (void)length;
  return AIHC_IO_ERROR_NOT_SUPPORTED;
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

void *aihc_io_stderr(void) { return &aihc_standard_error; }

/* A P3 descriptor is a resource the component owns, not a number the program
   can be handed, so this host has nothing to adopt. Both entry points report
   that rather than making up a handle, and GHC.IO.Handle.FD.fdToHandle raises
   the unsupported-operation error the number stands for. */
int64_t aihc_io_descriptor_mode(int64_t descriptor) {
  (void)descriptor;
  return aihc_io_error(AIHC_IO_ERROR_NOT_SUPPORTED);
}

void *aihc_io_adopt(AihcMachine *machine, int64_t descriptor, int64_t mode) {
  AihcIoHandle *handle = aihc_io_handle_new(machine);
  (void)descriptor;
  (void)mode;
  handle->error = AIHC_IO_ERROR_NOT_SUPPORTED;
  return handle;
}

int64_t aihc_io_handle_descriptor(void *opaque_handle) {
  (void)opaque_handle;
  return -1;
}

static int aihc_wasip3_prepare(AihcIoRequest *request) {
  (void)request;
  return 0;
}

static int aihc_wasip3_try_request(AihcIoRequest *request, int64_t *result) {
  if (request->kind == AIHC_IO_TIMER) {
    *result = aihc_wasip3_start_timer(request->deadline);
    return *result != INT64_MIN;
  }
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
      return result;
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
      return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    }
    AihcIoHandle *handle = request->handle;
    handle->backend_token = (uintptr_t)result;
    handle->capabilities = capabilities;
    handle->closed = 0;
    handle->append = request->mode == 2;
    return 0;
  }
  if (request->kind == AIHC_IO_TIMER) {
    return result;
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
  /* The wasm32 linker cannot extend a pointer relocation into a 64-bit slot. */
  aihc_standard_input.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
  aihc_standard_output.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
  aihc_standard_error.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
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
