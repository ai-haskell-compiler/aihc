#include "aihc_runtime_internal.h"

#include <stddef.h>

extern int32_t aihc_wasip3_start_write(const unsigned char *bytes,
                                       size_t length);

static AihcIoHandle aihc_standard_input = {(uintptr_t)0, AIHC_IO_READABLE};
static AihcIoHandle aihc_standard_output = {(uintptr_t)1, AIHC_IO_WRITABLE};

_Noreturn void aihc_host_fail(const char *message) {
  (void)message;
  __builtin_trap();
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

static int aihc_wasip3_prepare(AihcIoRequest *request) {
  (void)request;
  return 0;
}

static int aihc_wasip3_try_request(AihcIoRequest *request, int32_t *result) {
  if (request->kind != AIHC_IO_WRITE || request->handle->backend_token != 1) {
    *result = aihc_io_error(1);
    return 1;
  }
  int32_t status = aihc_wasip3_start_write(request->buffer + request->offset,
                                           request->length);
  if (status == INT32_MIN) {
    return 0;
  }
  *result = status;
  return 1;
}

static AihcIoPollOutcome aihc_wasip3_poll(AihcMachine *machine, int may_block) {
  (void)machine;
  return may_block ? AIHC_IO_POLL_SUSPENDED : AIHC_IO_POLL_PROGRESS;
}

static const AihcIoBackend aihc_wasip3_io_backend = {
    aihc_wasip3_prepare,
    aihc_wasip3_try_request,
    aihc_wasip3_poll,
};

const AihcIoBackend *aihc_host_io_backend(void) {
  return &aihc_wasip3_io_backend;
}
