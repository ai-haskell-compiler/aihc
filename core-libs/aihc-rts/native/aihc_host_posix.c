/* clock_gettime and CLOCK_MONOTONIC need a POSIX feature level under
   -std=c11 on glibc. The name is reserved by design: it is the macro the
   standard defines for this purpose. */
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _POSIX_C_SOURCE 200809L
/* MAP_ANONYMOUS is not in POSIX. glibc and Darwin hide it when
   _POSIX_C_SOURCE is set unless these macros ask for the native set as
   well. */
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _DEFAULT_SOURCE
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _DARWIN_C_SOURCE

#include "aihc_runtime_internal.h"

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

extern char **environ;

static AihcIoHandle aihc_standard_input = {.backend_token = 0,
                                           .capabilities = AIHC_IO_READABLE};
static AihcIoHandle aihc_standard_output = {.backend_token = 1,
                                            .capabilities = AIHC_IO_WRITABLE};
static AihcIoHandle aihc_standard_error = {.backend_token = 2,
                                           .capabilities = AIHC_IO_WRITABLE};

_Noreturn void aihc_host_fail(const char *message) {
  fprintf(stderr, "aihc runtime: %s\n", message);
  abort();
}

_Noreturn void aihc_exit_process(int64_t status) {
  aihc_runtime_statistics_report();
  exit((int)status);
}

void aihc_program_environment_initialize(void) {
  aihc_environment_initialize(environ);
}

/* Each run of regions is one private anonymous mapping. The mapping takes
   one extra region, so a region boundary falls inside it, and the slack on
   both sides goes back at once. */
uint8_t *aihc_host_map_regions(size_t count) {
  if (count > (SIZE_MAX >> AIHC_REGION_SHIFT) - 1) {
    return NULL;
  }
  size_t bytes = count << AIHC_REGION_SHIFT;
  void *mapping = mmap(NULL, bytes + AIHC_REGION_BYTES, PROT_READ | PROT_WRITE,
                       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (mapping == MAP_FAILED) {
    return NULL;
  }
  uintptr_t start = (uintptr_t)mapping;
  uintptr_t aligned =
      (start + AIHC_REGION_BYTES - 1) & ~(uintptr_t)(AIHC_REGION_BYTES - 1);
  size_t head = (size_t)(aligned - start);
  if (head != 0 && munmap(mapping, head) != 0) {
    aihc_fail("cannot trim a heap mapping");
  }
  size_t tail = AIHC_REGION_BYTES - head;
  if (tail != 0 && munmap((uint8_t *)aligned + bytes, tail) != 0) {
    aihc_fail("cannot trim a heap mapping");
  }
  return (uint8_t *)aligned;
}

int aihc_host_unmap_regions(void *base, size_t count) {
  if (munmap(base, count << AIHC_REGION_SHIFT) != 0) {
    aihc_fail("cannot unmap heap regions");
  }
  return 0;
}

uint64_t aihc_host_monotonic_ns(void) {
  struct timespec now;
  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
    aihc_fail("monotonic clock is unavailable");
  }
  return (uint64_t)now.tv_sec * UINT64_C(1000000000) + (uint64_t)now.tv_nsec;
}

int aihc_host_write_file(const char *path, const void *bytes, size_t length) {
  int descriptor;
  do {
    descriptor = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  } while (descriptor == -1 && errno == EINTR);
  if (descriptor == -1) {
    return errno;
  }
  const uint8_t *cursor = bytes;
  size_t remaining = length;
  while (remaining != 0) {
    ssize_t written = write(descriptor, cursor, remaining);
    if (written < 0) {
      if (errno == EINTR) {
        continue;
      }
      int error = errno;
      close(descriptor);
      return error;
    }
    cursor += written;
    remaining -= (size_t)written;
  }
  if (close(descriptor) == -1) {
    return errno;
  }
  return 0;
}

void *aihc_io_stdin(void) { return &aihc_standard_input; }

void *aihc_io_stdout(void) { return &aihc_standard_output; }

void *aihc_io_stderr(void) { return &aihc_standard_error; }

/* The capabilities an open mode asks for, or zero when the mode is not one
   of the four an open request numbers. */
static uint32_t aihc_posix_capabilities(int64_t mode) {
  switch (mode) {
  case 0:
    return AIHC_IO_READABLE;
  case 1:
  case 2:
    return AIHC_IO_WRITABLE;
  case 3:
    return AIHC_IO_READABLE | AIHC_IO_WRITABLE;
  default:
    return 0;
  }
}

int64_t aihc_io_descriptor_mode(int64_t descriptor) {
  if (descriptor < 0 || descriptor > INT_MAX) {
    return aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
  }
  int flags = fcntl((int)descriptor, F_GETFL);
  if (flags == -1) {
    return aihc_io_error(errno);
  }
  switch (flags & O_ACCMODE) {
  case O_RDONLY:
    return 0;
  case O_WRONLY:
    return (flags & O_APPEND) != 0 ? 2 : 1;
  case O_RDWR:
    return 3;
  default:
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
}

/* The handle borrows the descriptor rather than opening one, so nothing here
   changes the descriptor's flags; a read or a write sets O_NONBLOCK on it the
   same way it does for a descriptor the runtime opened. Closing the handle
   closes the descriptor, which is what a Handle over it promises. */
void *aihc_io_adopt(AihcMachine *machine, int64_t descriptor, int64_t mode) {
  AihcIoHandle *handle = aihc_io_handle_new(machine);
  if (descriptor < 0 || descriptor > INT_MAX) {
    handle->error = AIHC_IO_ERROR_BAD_DESCRIPTOR;
    return handle;
  }
  uint32_t capabilities = aihc_posix_capabilities(mode);
  if (capabilities == 0) {
    handle->error = AIHC_IO_ERROR_INVALID_ARGUMENT;
    return handle;
  }
  if (fcntl((int)descriptor, F_GETFD) == -1) {
    handle->error = errno;
    return handle;
  }
  handle->backend_token = (uintptr_t)descriptor;
  handle->capabilities = capabilities;
  handle->closed = 0;
  handle->append = mode == 2;
  return handle;
}

static int aihc_posix_descriptor(const AihcIoHandle *handle) {
  return (int)handle->backend_token;
}

int64_t aihc_io_handle_descriptor(void *opaque_handle) {
  const AihcIoHandle *handle = opaque_handle;
  return aihc_posix_descriptor(handle);
}

int64_t aihc_io_handle_position(void *opaque_handle) {
  (void)opaque_handle;
  return -1;
}

int64_t aihc_io_handle_set_position(void *opaque_handle, int64_t position) {
  (void)opaque_handle;
  (void)position;
  return -1;
}

static int64_t aihc_posix_open(AihcIoRequest *request) {
  void *opaque_path = request->buffer;
  int64_t requested_length = (int64_t)request->length;
  int64_t requested_mode = request->mode;
  if (requested_length < 0 || (uint64_t)requested_length >= SIZE_MAX ||
      (opaque_path == NULL && requested_length != 0)) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  size_t length = (size_t)requested_length;
  if (length != 0 && memchr(opaque_path, 0, length) != NULL) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  AihcRootFrame frame;
  aihc_roots_enter(request->machine, &frame, 0, NULL);
  char *path = aihc_byte_array_contents(
      aihc_host_byte_array(request->machine, &frame, length + 1));
  if (length != 0) {
    memcpy(path, opaque_path, length);
  }

  int flags;
  uint32_t capabilities;
  switch (requested_mode) {
  case 0:
    flags = O_RDONLY;
    capabilities = AIHC_IO_READABLE;
    break;
  case 1:
    flags = O_WRONLY | O_CREAT | O_TRUNC;
    capabilities = AIHC_IO_WRITABLE;
    break;
  case 2:
    flags = O_WRONLY | O_CREAT | O_APPEND;
    capabilities = AIHC_IO_WRITABLE;
    break;
  case 3:
    flags = O_RDWR | O_CREAT;
    capabilities = AIHC_IO_READABLE | AIHC_IO_WRITABLE;
    break;
  default:
    aihc_roots_leave(request->machine, &frame);
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }

  int descriptor;
  do {
    descriptor = open(path, flags | O_NONBLOCK, 0666);
  } while (descriptor == -1 && errno == EINTR);
  int open_error = errno;
  aihc_roots_leave(request->machine, &frame);
  if (descriptor == -1) {
    return aihc_io_error(open_error);
  }
  AihcIoHandle *handle = request->handle;
  handle->backend_token = (uintptr_t)descriptor;
  handle->capabilities = capabilities;
  handle->closed = 0;
  return 0;
}

static int aihc_posix_prepare(AihcIoRequest *request) {
  if (request->kind == AIHC_IO_OPEN || request->kind == AIHC_IO_TIMER) {
    return 0;
  }
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

static int aihc_posix_try_request(AihcIoRequest *request, int64_t *result) {
  /* A timer that has not expired stays pending, so that every other green
     thread keeps running while it waits. aihc_posix_poll bounds its wait by
     the earliest deadline and comes back here to collect the expired ones. */
  if (request->kind == AIHC_IO_TIMER) {
    if (aihc_host_monotonic_ns() < request->deadline) {
      return 0;
    }
    *result = 1;
    return 1;
  }
  if (request->kind == AIHC_IO_OPEN) {
    *result = aihc_posix_open(request);
    return 1;
  }
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
      *result = (int64_t)transferred;
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

static int64_t aihc_posix_finish_request(AihcIoRequest *request,
                                         int64_t result) {
  (void)request;
  return result;
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

/* How long poll may wait: nothing when the caller may not block, the
   earliest timer deadline when one is pending, and forever otherwise.
   poll takes whole milliseconds, so the deadline rounds up; a wait that
   returns early only costs one more round of the scheduler loop. */
static int aihc_posix_poll_timeout(int may_block, int has_timer,
                                   uint64_t earliest) {
  if (!may_block) {
    return 0;
  }
  if (!has_timer) {
    return -1;
  }
  uint64_t now = aihc_host_monotonic_ns();
  if (earliest <= now) {
    return 0;
  }
  uint64_t milliseconds = (earliest - now + 999999) / 1000000;
  if (milliseconds > (uint64_t)INT_MAX) {
    return INT_MAX;
  }
  return (int)milliseconds;
}

static AihcIoPollOutcome aihc_posix_poll(AihcMachine *machine, int may_block) {
  if (machine->io_request_count == 0) {
    return AIHC_IO_POLL_PROGRESS;
  }
  size_t count = 0;
  int has_timer = 0;
  uint64_t earliest = UINT64_MAX;
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    if (request->kind == AIHC_IO_TIMER) {
      has_timer = 1;
      if (request->deadline < earliest) {
        earliest = request->deadline;
      }
    } else {
      ++count;
    }
  }
  /* Timer requests need no descriptor slot. */
  if (count > SIZE_MAX / sizeof(struct pollfd)) {
    aihc_fail("poll buffer is too large");
  }
  AihcRootFrame frame;
  aihc_roots_enter(machine, &frame, 0, NULL);
  struct pollfd *descriptors =
      count == 0 ? NULL
                 : aihc_byte_array_contents(aihc_host_byte_array(
                       machine, &frame, sizeof(*descriptors) * count));
  aihc_record_allocation(machine);
  size_t index = 0;
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    if (request->kind == AIHC_IO_TIMER) {
      continue;
    }
    if (descriptors == NULL || index >= count) {
      aihc_fail("IO poll descriptor count changed");
    }
    descriptors[index].fd = aihc_posix_descriptor(request->handle);
    descriptors[index].events =
        request->kind == AIHC_IO_READ ? POLLIN : POLLOUT;
    ++index;
  }
  int ready = poll(descriptors, (nfds_t)count,
                   aihc_posix_poll_timeout(may_block, has_timer, earliest));
  if (ready == -1) {
    int error = errno;
    aihc_roots_leave(machine, &frame);
    if (error != EINTR) {
      aihc_complete_all_io_with_error(machine, error);
    }
    return AIHC_IO_POLL_PROGRESS;
  }

  AihcIoRequest **link = &machine->io_requests_head;
  AihcIoRequest *tail = NULL;
  index = 0;
  while (*link != NULL) {
    AihcIoRequest *request = *link;
    int64_t result = 0;
    int complete = 0;
    if (request->kind == AIHC_IO_TIMER) {
      complete = aihc_posix_try_request(request, &result);
    } else {
      if (descriptors == NULL || index >= count) {
        aihc_fail("IO poll descriptor count changed");
      }
      short events = descriptors[index++].revents;
      if ((events & POLLNVAL) != 0) {
        result = aihc_io_error(EBADF);
        complete = 1;
      } else if (events != 0) {
        complete = aihc_posix_try_request(request, &result);
      }
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
  aihc_roots_leave(machine, &frame);
  return AIHC_IO_POLL_PROGRESS;
}

static const AihcIoBackend aihc_posix_io_backend = {
    aihc_posix_prepare,
    aihc_posix_try_request,
    aihc_posix_finish_request,
    aihc_posix_poll,
};

const AihcIoBackend *aihc_host_io_backend(void) {
  /* The wasm32 linker cannot extend a pointer relocation into a 64-bit slot. */
  aihc_standard_input.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
  aihc_standard_output.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
  aihc_standard_error.header = (AihcSlot)(uintptr_t)&aihc_io_handle_info;
  return &aihc_posix_io_backend;
}

int64_t aihc_io_close(void *opaque_handle) {
  AihcIoHandle *handle = opaque_handle;
  if (handle == NULL || handle->closed) {
    return aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
  }
  handle->closed = 1;
  if (close(aihc_posix_descriptor(handle)) == -1) {
    return aihc_io_error(errno);
  }
  return 0;
}

_Noreturn int64_t aihc_io_raise_error(int64_t error) {
  fprintf(stderr, "aihc runtime: IO error %" PRId64 "\n", error);
  abort();
}

void aihc_host_sleep_ns(uint64_t duration) {
  struct timespec remaining = {.tv_sec = (time_t)(duration / 1000000000),
                               .tv_nsec = (long)(duration % 1000000000)};
  while (nanosleep(&remaining, &remaining) != 0) {
    if (errno != EINTR) {
      aihc_fail("STM timer wait failed");
    }
  }
}

/* The action codes of GHC's rts/Signals.h. Aihc.Hackage.Headers writes the
   same values into the rts/Signals.h that package C code includes. */
enum {
  AIHC_SIGNAL_DEFAULT = -1,
  AIHC_SIGNAL_IGNORE = -2,
  AIHC_SIGNAL_ERROR = -3,
  AIHC_SIGNAL_HANDLER = -4,
  AIHC_SIGNAL_HANDLER_ONCE = -5
};

/* GHC's stg_sig_install, which System.Posix.Signals.installHandler calls.
   The runtime sets the default and the ignore actions through sigaction.

   The runtime refuses a Haskell handler (the codes AIHC_SIGNAL_HANDLER and
   AIHC_SIGNAL_HANDLER_ONCE) and returns AIHC_SIGNAL_ERROR. GHC runs a Haskell
   handler in a new thread that its C signal handler starts. aihc has no
   dispatch from a C signal handler to Haskell code. If the runtime accepted the
   code, the signal would stop the default action and no handler would run.
   With the error code, the signal keeps its current action.

   The previous action code comes from the operating system, not from a table.
   Thus an action that the process inherits, for example an ignored SIGHUP
   under nohup, gives AIHC_SIGNAL_IGNORE. A C handler that other code installed
   gives AIHC_SIGNAL_DEFAULT, as an unknown signal does in GHC.

   The mask is the set of signals that GHC blocks while its handler runs. The
   default and ignore actions run no handler, so the runtime does not read the
   mask. */
int stg_sig_install(int signal_number, int action, void *mask) {
  (void)mask;
  struct sigaction requested;
  struct sigaction previous;
  memset(&requested, 0, sizeof requested);
  sigemptyset(&requested.sa_mask);
  switch (action) {
  case AIHC_SIGNAL_DEFAULT:
    requested.sa_handler = SIG_DFL;
    break;
  case AIHC_SIGNAL_IGNORE:
    requested.sa_handler = SIG_IGN;
    break;
  default:
    return AIHC_SIGNAL_ERROR;
  }
  if (signal_number == SIGCHLD && nocldstop != 0) {
    requested.sa_flags |= SA_NOCLDSTOP;
  }
  if (signal_number <= 0 ||
      sigaction(signal_number, &requested, &previous) != 0) {
    return AIHC_SIGNAL_ERROR;
  }
  return previous.sa_handler == SIG_IGN ? AIHC_SIGNAL_IGNORE
                                        : AIHC_SIGNAL_DEFAULT;
}
