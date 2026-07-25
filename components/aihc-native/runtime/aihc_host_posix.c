#include "aihc_runtime_internal.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static AihcIoHandle aihc_standard_input = {(uintptr_t)0, AIHC_IO_READABLE};
static AihcIoHandle aihc_standard_output = {(uintptr_t)1, AIHC_IO_WRITABLE};

_Noreturn void aihc_host_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

static int aihc_posix_descriptor(const AihcIoHandle *handle) {
  return (int)handle->backend_token;
}

static int aihc_posix_prepare(AihcIoRequest *request) {
  int descriptor = aihc_posix_descriptor(request->handle);
  int flags = fcntl(descriptor, F_GETFL);
  if (flags == -1) {
    return errno;
  }
  if ((flags & O_NONBLOCK) == 0 &&
      fcntl(descriptor, F_SETFL, flags | O_NONBLOCK) == -1) {
    return errno;
  }
  return 0;
}

static int aihc_posix_try_request(AihcIoRequest *request, int32_t *result) {
  for (;;) {
    ssize_t transferred;
    uint8_t *bytes = request->buffer + request->offset;
    if (request->kind == AIHC_IO_READ) {
      transferred =
          read(aihc_posix_descriptor(request->handle), bytes, request->length);
    } else {
      transferred =
          write(aihc_posix_descriptor(request->handle), bytes, request->length);
    }
    if (transferred >= 0) {
      *result = (int32_t)transferred;
      return 1;
    }
    if (errno == EINTR) {
      continue;
    }
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      return 0;
    }
    *result = aihc_io_error(errno);
    return 1;
  }
}

static void aihc_complete_all_io_with_error(AihcMachine *machine, int error) {
  AihcIoRequest *request = machine->io_requests_head;
  machine->io_requests_head = NULL;
  machine->io_requests_tail = NULL;
  machine->io_request_count = 0;
  while (request != NULL) {
    AihcIoRequest *next = request->next;
    aihc_resume_io_request(machine, request, aihc_io_error(error));
    request = next;
  }
}

static AihcIoPollOutcome aihc_posix_poll(AihcMachine *machine, int may_block) {
  if (machine->io_request_count == 0) {
    return AIHC_IO_POLL_PROGRESS;
  }
  size_t count = (size_t)machine->io_request_count;
  struct pollfd *descriptors =
      aihc_allocate_auxiliary(machine, sizeof(*descriptors) * count);
  size_t index = 0;
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    descriptors[index].fd = aihc_posix_descriptor(request->handle);
    descriptors[index].events =
        request->kind == AIHC_IO_READ ? POLLIN : POLLOUT;
    ++index;
  }
  int ready = poll(descriptors, count, may_block ? -1 : 0);
  if (ready == -1) {
    int error = errno;
    free(descriptors);
    if (error != EINTR) {
      aihc_complete_all_io_with_error(machine, error);
    }
    return AIHC_IO_POLL_PROGRESS;
  }
  if (ready == 0) {
    free(descriptors);
    return AIHC_IO_POLL_PROGRESS;
  }

  AihcIoRequest **link = &machine->io_requests_head;
  AihcIoRequest *tail = NULL;
  index = 0;
  while (*link != NULL) {
    AihcIoRequest *request = *link;
    short events = descriptors[index++].revents;
    int32_t result = 0;
    int complete = 0;
    if ((events & POLLNVAL) != 0) {
      result = aihc_io_error(EBADF);
      complete = 1;
    } else if (events != 0) {
      complete = aihc_posix_try_request(request, &result);
    }
    if (complete) {
      *link = request->next;
      --machine->io_request_count;
      aihc_resume_io_request(machine, request, result);
    } else {
      tail = request;
      link = &request->next;
    }
  }
  machine->io_requests_tail = tail;
  free(descriptors);
  return AIHC_IO_POLL_PROGRESS;
}

static const AihcIoBackend aihc_posix_io_backend = {
    aihc_posix_prepare,
    aihc_posix_try_request,
    aihc_posix_poll,
};

const AihcIoBackend *aihc_host_io_backend(void) {
  return &aihc_posix_io_backend;
}
