/* The time profiler of a program built with --profile-time. See "Time
   profile" in docs/native-runtime-objects.md.

   Compiled code gives the capability the code and the continuation of each
   function at its entry. A SIGPROF signal interrupts each thread of the
   program at a fixed interval of the CPU time of that thread. The signal
   handler reads the two fields of the
   capability that the interrupted OS thread runs, walks the continuation
   frames from that continuation through field zero of each frame, and
   appends the codes to a sample buffer. At exit, the profiler sorts the
   samples, counts the equal ones, and writes one line for each stack in
   the folded format: the names from the bottom frame to the running
   function, separated by semicolons, then the count. */

/* sigaction and setitimer need a POSIX feature level under -std=c11 on
   glibc, and SA_RESTART needs the native set as well. See
   aihc_host_posix.c. */
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _POSIX_C_SOURCE 200809L
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _DEFAULT_SOURCE
// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _DARWIN_C_SOURCE

#include "aihc_runtime.h"
#include "aihc_runtime_internal.h"

#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <pthread.h>
#include <time.h>
#endif

/* The update frame that aihc_lir_eval pushes. It is the only frame that the
   runtime units push, so it is not in the info tables of the program. */
extern const AihcInfo aihc_lir_cps_update_info;

/* The codes that do not belong to a function. A function code is never
   one of these small values. */
enum {
  AIHC_PROFILE_NO_CODE = 0,
  AIHC_PROFILE_COLLECTION = 1,
  AIHC_PROFILE_TRUNCATED = 2,
  AIHC_PROFILE_SPECIAL_CODES = 3,
};

/* The frames a sample keeps. A deeper stack loses its bottom frames, and
   the sample then starts with a truncation frame. */
#define AIHC_PROFILE_MAX_FRAMES 1024
/* The words of the sample buffer: 256 MiB of address space. The host gives
   pages only when a sample writes them. */
#define AIHC_PROFILE_BUFFER_WORDS ((size_t)32 << 20)
#define AIHC_PROFILE_DEFAULT_INTERVAL_US 1000

typedef struct {
  uintptr_t code;
  const char *name;
} AihcProfileName;

static AihcProfileName *aihc_profile_names;
static uint64_t aihc_profile_name_count;
static uintptr_t *aihc_profile_infos;
static uint64_t aihc_profile_info_count;

/* Each sample is its length in words, then its codes from the running
   function to the bottom frame. */
static uintptr_t *aihc_profile_buffer;
static size_t aihc_profile_used;
static uint64_t aihc_profile_dropped;
static uint64_t aihc_profile_unbound;

static _Thread_local AihcCapability *aihc_profile_capability;

/* The timer that sends SIGPROF. */

#ifdef __APPLE__
/* macOS examines the ITIMER_PROF timer of a thread only when the thread
   goes into the kernel. Compiled code that only calculates seldom goes into
   the kernel, but a collection does at each page fault. Thus the kernel
   kept an expired timer until the next collection. The samples in
   collections were more than twenty times the collection time.

   Thus a sampler thread sends the signals. At each interval of wall-clock
   time, it reads the CPU time of each thread of the program. It sends
   SIGPROF to each thread that used an interval of CPU time since its last
   signal. The kernel interrupts a running thread at once for a signal from
   another thread. A thread gets a maximum of one signal at each wake of the
   sampler. */

typedef struct {
  pthread_t thread;
  thread_act_t port;
  /* The CPU time of the thread at the last wake, in microseconds. */
  uint64_t used_us;
  /* The CPU time since the last signal, in microseconds. */
  uint64_t credit_us;
} AihcProfileThread;

static pthread_mutex_t aihc_profile_threads_lock = PTHREAD_MUTEX_INITIALIZER;
static AihcProfileThread *aihc_profile_threads;
static size_t aihc_profile_thread_count;
static size_t aihc_profile_thread_capacity;
static int aihc_profile_active;
static int aihc_profile_stopping;
static uint64_t aihc_profile_interval_us;
static pthread_t aihc_profile_sampler;

/* The user and system CPU time of a thread, in microseconds. */
static int aihc_profile_thread_time(thread_act_t port, uint64_t *used_us) {
  thread_basic_info_data_t info;
  mach_msg_type_number_t count = THREAD_BASIC_INFO_COUNT;
  if (thread_info(port, THREAD_BASIC_INFO, (thread_info_t)&info, &count) !=
      KERN_SUCCESS) {
    return 0;
  }
  *used_us =
      ((uint64_t)info.user_time.seconds + (uint64_t)info.system_time.seconds) *
          UINT64_C(1000000) +
      (uint64_t)info.user_time.microseconds +
      (uint64_t)info.system_time.microseconds;
  return 1;
}

/* Add the calling thread to the threads that the sampler interrupts. */
static void aihc_profile_timer_add_thread(void) {
  if (!aihc_profile_active) {
    return;
  }
  thread_act_t port = pthread_mach_thread_np(pthread_self());
  uint64_t used_us = 0;
  (void)aihc_profile_thread_time(port, &used_us);
  pthread_mutex_lock(&aihc_profile_threads_lock);
  if (aihc_profile_thread_count == aihc_profile_thread_capacity) {
    size_t capacity = aihc_profile_thread_capacity == 0
                          ? 8
                          : 2 * aihc_profile_thread_capacity;
    AihcProfileThread *threads =
        realloc(aihc_profile_threads, capacity * sizeof(AihcProfileThread));
    if (threads == NULL) {
      aihc_fail("out of memory");
    }
    aihc_profile_threads = threads;
    aihc_profile_thread_capacity = capacity;
  }
  aihc_profile_threads[aihc_profile_thread_count++] =
      (AihcProfileThread){pthread_self(), port, used_us, 0};
  pthread_mutex_unlock(&aihc_profile_threads_lock);
}

static void *aihc_profile_sampler_main(void *argument) {
  (void)argument;
  uint64_t interval = aihc_profile_interval_us;
  struct timespec pause = {.tv_sec = (time_t)(interval / 1000000),
                           .tv_nsec = (long)(interval % 1000000) * 1000};
  while (!__atomic_load_n(&aihc_profile_stopping, __ATOMIC_ACQUIRE)) {
    nanosleep(&pause, NULL);
    pthread_mutex_lock(&aihc_profile_threads_lock);
    for (size_t index = 0; index < aihc_profile_thread_count; ++index) {
      AihcProfileThread *each = &aihc_profile_threads[index];
      uint64_t used_us;
      if (!aihc_profile_thread_time(each->port, &used_us)) {
        continue;
      }
      each->credit_us += used_us - each->used_us;
      each->used_us = used_us;
      if (each->credit_us >= interval) {
        each->credit_us %= interval;
        pthread_kill(each->thread, SIGPROF);
      }
    }
    pthread_mutex_unlock(&aihc_profile_threads_lock);
  }
  return NULL;
}

static void aihc_profile_timer_start(long interval) {
  aihc_profile_interval_us = (uint64_t)interval;
  aihc_profile_active = 1;
  aihc_profile_timer_add_thread();
  if (pthread_create(&aihc_profile_sampler, NULL, aihc_profile_sampler_main,
                     NULL) != 0) {
    aihc_fail("cannot start the time profile timer");
  }
}

static void aihc_profile_timer_stop(void) {
  __atomic_store_n(&aihc_profile_stopping, 1, __ATOMIC_RELEASE);
  pthread_join(aihc_profile_sampler, NULL);
}
#else
/* Elsewhere, the kernel examines the ITIMER_PROF timer at each clock tick,
   and sends SIGPROF to a thread that runs. */

static void aihc_profile_timer_add_thread(void) {}

static void aihc_profile_timer_start(long interval) {
  struct timeval period = {.tv_sec = (time_t)(interval / 1000000),
                           .tv_usec = (suseconds_t)(interval % 1000000)};
  struct itimerval timer = {.it_interval = period, .it_value = period};
  if (setitimer(ITIMER_PROF, &timer, NULL) != 0) {
    aihc_fail("cannot start the time profile timer");
  }
}

static void aihc_profile_timer_stop(void) {
  struct itimerval stop = {{0, 0}, {0, 0}};
  setitimer(ITIMER_PROF, &stop, NULL);
}
#endif

void aihc_time_profile_thread(AihcCapability *capability) {
  aihc_profile_capability = capability;
  aihc_profile_timer_add_thread();
}

static int aihc_profile_compare_words(const void *left, const void *right) {
  uintptr_t a = *(const uintptr_t *)left;
  uintptr_t b = *(const uintptr_t *)right;
  return a < b ? -1 : a > b ? 1 : 0;
}

static int aihc_profile_compare_names(const void *left, const void *right) {
  return aihc_profile_compare_words(&((const AihcProfileName *)left)->code,
                                    &((const AihcProfileName *)right)->code);
}

/* Whether a header word names an info table of a function of the program.
   The handler reads a frame only through such a table. */
static int aihc_profile_known_info(uintptr_t info) {
  uint64_t low = 0;
  uint64_t high = aihc_profile_info_count;
  while (low < high) {
    uint64_t middle = low + (high - low) / 2;
    if (aihc_profile_infos[middle] < info) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  return low < aihc_profile_info_count && aihc_profile_infos[low] == info;
}

/* The code that a frame returns to, or AIHC_PROFILE_NO_CODE for a frame
   without one. An update frame gives the code of its thunk. The walk stops
   at a frame that it cannot read. */
static int aihc_profile_frame_code(const AihcValue *frame, int collecting,
                                   uintptr_t *code) {
  if (((uintptr_t)frame & (sizeof(AihcSlot) - 1)) != 0 ||
      aihc_region_kind(frame) != AIHC_REGION_STACK) {
    return 0;
  }
  uintptr_t info = (uintptr_t)frame->header & ~(uintptr_t)AIHC_HEADER_TAG_MASK;
  if (info == (uintptr_t)&aihc_lir_cps_update_info) {
    *code = AIHC_PROFILE_NO_CODE;
    /* A collection can move the thunk while the handler reads it. */
    if (!collecting) {
      const AihcValue *thunk = (const AihcValue *)(uintptr_t)frame->fields[1];
      uintptr_t thunk_info =
          (uintptr_t)thunk->header & ~(uintptr_t)AIHC_HEADER_TAG_MASK;
      if (aihc_profile_known_info(thunk_info)) {
        *code = ((const AihcInfo *)thunk_info)->identity;
      }
    }
    return 1;
  }
  if (!aihc_profile_known_info(info)) {
    return 0;
  }
  *code = ((const AihcInfo *)info)->identity;
  return 1;
}

static void aihc_profile_signal(int signal_number, siginfo_t *signal_info,
                                void *context) {
  (void)signal_number;
  (void)signal_info;
  (void)context;
  int saved_errno = errno;
  AihcCapability *capability = aihc_profile_capability;
  if (capability == NULL) {
    __atomic_add_fetch(&aihc_profile_unbound, 1, __ATOMIC_RELAXED);
    errno = saved_errno;
    return;
  }
  uintptr_t sample[AIHC_PROFILE_MAX_FRAMES + 4];
  size_t length = 1;
  int collecting = capability->profile_collecting != 0;
  if (collecting) {
    sample[length++] = AIHC_PROFILE_COLLECTION;
  }
  sample[length++] = (uintptr_t)capability->profile_code;
  const AihcValue *frame = capability->profile_frame;
  size_t frames = 0;
  while (frame != NULL) {
    uintptr_t code;
    if (!aihc_profile_frame_code(frame, collecting, &code)) {
      break;
    }
    if (frames == AIHC_PROFILE_MAX_FRAMES) {
      sample[length++] = AIHC_PROFILE_TRUNCATED;
      break;
    }
    if (code != AIHC_PROFILE_NO_CODE) {
      sample[length++] = code;
      ++frames;
    }
    frame = (const AihcValue *)(uintptr_t)frame->fields[0];
  }
  sample[0] = length;
  size_t start =
      __atomic_fetch_add(&aihc_profile_used, length, __ATOMIC_RELAXED);
  if (start + length > AIHC_PROFILE_BUFFER_WORDS) {
    __atomic_add_fetch(&aihc_profile_dropped, 1, __ATOMIC_RELAXED);
  } else {
    memcpy(aihc_profile_buffer + start, sample, length * sizeof(uintptr_t));
  }
  errno = saved_errno;
}

static const char *aihc_profile_name(uintptr_t code, char *scratch,
                                     size_t size) {
  switch (code) {
  case AIHC_PROFILE_NO_CODE:
    return "[runtime]";
  case AIHC_PROFILE_COLLECTION:
    return "[collection]";
  case AIHC_PROFILE_TRUNCATED:
    return "[truncated]";
  default:
    break;
  }
  AihcProfileName key = {code, NULL};
  const AihcProfileName *found =
      bsearch(&key, aihc_profile_names, aihc_profile_name_count,
              sizeof(AihcProfileName), aihc_profile_compare_names);
  if (found != NULL) {
    return found->name;
  }
  snprintf(scratch, size, "0x%llx", (unsigned long long)code);
  return scratch;
}

/* The sample order: the length first, then the codes. Equal samples are
   adjacent after the sort. */
static int aihc_profile_compare_samples(const void *left, const void *right) {
  const uintptr_t *a = aihc_profile_buffer + *(const size_t *)left;
  const uintptr_t *b = aihc_profile_buffer + *(const size_t *)right;
  if (a[0] != b[0]) {
    return a[0] < b[0] ? -1 : 1;
  }
  for (size_t index = 1; index < a[0]; ++index) {
    if (a[index] != b[index]) {
      return a[index] < b[index] ? -1 : 1;
    }
  }
  return 0;
}

static void aihc_profile_write_sample(FILE *file, const uintptr_t *sample,
                                      uint64_t count) {
  char scratch[32];
  for (size_t index = sample[0] - 1; index >= 1; --index) {
    fputs(aihc_profile_name(sample[index], scratch, sizeof(scratch)), file);
    fputc(index == 1 ? ' ' : ';', file);
  }
  fprintf(file, "%llu\n", (unsigned long long)count);
}

/* The AIHC_PROFILE file, or the name of the program with .folded. */
static const char *aihc_profile_path(char *scratch, size_t size) {
  const char *path = getenv("AIHC_PROFILE");
  if (path != NULL && *path != 0) {
    return path;
  }
  const char *program = aihc_program_initial_name();
  if (program == NULL) {
    program = "aihc";
  }
  const char *base = strrchr(program, '/');
  snprintf(scratch, size, "%s.folded", base == NULL ? program : base + 1);
  return scratch;
}

static void aihc_profile_finish(void) {
  aihc_profile_timer_stop();
  signal(SIGPROF, SIG_IGN);
  size_t used = __atomic_load_n(&aihc_profile_used, __ATOMIC_RELAXED);
  if (used > AIHC_PROFILE_BUFFER_WORDS) {
    used = AIHC_PROFILE_BUFFER_WORDS;
  }
  size_t count = 0;
  for (size_t offset = 0; offset < used && aihc_profile_buffer[offset] != 0;
       offset += aihc_profile_buffer[offset]) {
    ++count;
  }
  size_t *order = malloc((count == 0 ? 1 : count) * sizeof(size_t));
  if (order == NULL) {
    aihc_fail("out of memory");
  }
  size_t index = 0;
  for (size_t offset = 0; index < count;
       offset += aihc_profile_buffer[offset]) {
    order[index++] = offset;
  }
  qsort(order, count, sizeof(size_t), aihc_profile_compare_samples);
  char scratch[4096];
  const char *path = aihc_profile_path(scratch, sizeof(scratch));
  FILE *file = fopen(path, "w");
  if (file == NULL) {
    fprintf(stderr, "aihc runtime: cannot write the time profile %s: %s\n",
            path, strerror(errno));
    free(order);
    return;
  }
  for (size_t first = 0; first < count;) {
    size_t next = first + 1;
    while (next < count &&
           aihc_profile_compare_samples(&order[first], &order[next]) == 0) {
      ++next;
    }
    aihc_profile_write_sample(file, aihc_profile_buffer + order[first],
                              next - first);
    first = next;
  }
  fclose(file);
  free(order);
  if (aihc_profile_dropped != 0) {
    fprintf(stderr,
            "aihc runtime: the time profile buffer was full, %llu samples "
            "are not in %s\n",
            (unsigned long long)aihc_profile_dropped, path);
  }
}

void aihc_time_profile_register(AihcCapability *capability,
                                const void *const *codes,
                                const char *const *names,
                                const uint64_t *code_count,
                                const AihcInfo *const *infos,
                                const uint64_t *info_count) {
  aihc_profile_name_count = *code_count;
  aihc_profile_names =
      malloc((aihc_profile_name_count + 1) * sizeof(AihcProfileName));
  aihc_profile_info_count = *info_count + 1;
  aihc_profile_infos = malloc(aihc_profile_info_count * sizeof(uintptr_t));
  /* The buffer must read as zero where no sample was written, so the
     report finds the end of a buffer that a dropped sample left short. */
  aihc_profile_buffer = calloc(AIHC_PROFILE_BUFFER_WORDS, sizeof(uintptr_t));
  if (aihc_profile_names == NULL || aihc_profile_infos == NULL ||
      aihc_profile_buffer == NULL) {
    aihc_fail("out of memory");
  }
  for (uint64_t index = 0; index < aihc_profile_name_count; ++index) {
    aihc_profile_names[index] =
        (AihcProfileName){(uintptr_t)codes[index], names[index]};
  }
  qsort(aihc_profile_names, aihc_profile_name_count, sizeof(AihcProfileName),
        aihc_profile_compare_names);
  for (uint64_t index = 0; index < *info_count; ++index) {
    aihc_profile_infos[index] = (uintptr_t)infos[index];
  }
  aihc_profile_infos[*info_count] = (uintptr_t)&aihc_lir_cps_update_info;
  qsort(aihc_profile_infos, aihc_profile_info_count, sizeof(uintptr_t),
        aihc_profile_compare_words);
  aihc_profile_capability = capability;
  aihc_time_profile_finish = aihc_profile_finish;
  struct sigaction action;
  memset(&action, 0, sizeof(action));
  action.sa_sigaction = aihc_profile_signal;
  action.sa_flags = SA_SIGINFO | SA_RESTART;
  sigemptyset(&action.sa_mask);
  if (sigaction(SIGPROF, &action, NULL) != 0) {
    aihc_fail("cannot install the time profile signal handler");
  }
  long interval = AIHC_PROFILE_DEFAULT_INTERVAL_US;
  const char *requested = getenv("AIHC_PROFILE_INTERVAL_US");
  if (requested != NULL && *requested != 0) {
    char *end = NULL;
    interval = strtol(requested, &end, 10);
    if (*end != 0 || interval <= 0) {
      aihc_fail("AIHC_PROFILE_INTERVAL_US is not a positive number");
    }
  }
  aihc_profile_timer_start(interval);
}
