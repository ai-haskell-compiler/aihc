#include "aihc_runtime_internal.h"
#include <assert.h>
#include <pthread.h>
#include <stdlib.h>

/* The fault removes mutual exclusion from two actual RTS calls.
   The normal RTS permits only one OS thread. */

static AihcMachine machine;
static void *mvar;
static pthread_mutex_t lock;
static uint64_t results[2];

static void *worker(void *argument) {
  size_t index = (size_t)argument;
#ifndef INJECT_RACE
  pthread_mutex_lock(&lock);
#endif
  results[index] = aihc_mvar_try_put(&machine, mvar, index + 1);
#ifndef INJECT_RACE
  pthread_mutex_unlock(&lock);
#endif
  return NULL;
}

int main(void) {
  pthread_t workers[2];
  machine.heap_start = malloc(256);
  assert(machine.heap_start != NULL);
  machine.heap_next = machine.heap_start;
  machine.heap_limit = machine.heap_start + 256;
  machine.nursery_bytes = 256;
  mvar = aihc_mvar_new(&machine);
  pthread_mutex_init(&lock, NULL);
  pthread_create(&workers[0], NULL, worker, (void *)0);
  pthread_create(&workers[1], NULL, worker, (void *)1);
  pthread_join(workers[0], NULL);
  pthread_join(workers[1], NULL);
  assert(results[0] + results[1] == 1);
  assert(aihc_mvar_try_take(&machine, mvar) == 1);
  assert(aihc_mvar_try_take(&machine, mvar) == 0);
  pthread_mutex_destroy(&lock);
  free(machine.heap_start);
  return 0;
}
