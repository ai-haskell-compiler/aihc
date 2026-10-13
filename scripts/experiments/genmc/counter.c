#include "aihc_runtime_internal.h"
#include <assert.h>
#include <pthread.h>

/* The fault permits concurrent access to the actual stable-name counter. */

static AihcMachine machine;
static pthread_mutex_t lock;
static uint64_t hashes[2];

static void *worker(void *argument) {
  size_t index = (size_t)argument;
#ifndef INJECT_RACE
  pthread_mutex_lock(&lock);
#endif
  hashes[index] = aihc_stable_name_take_hash(&machine);
#ifndef INJECT_RACE
  pthread_mutex_unlock(&lock);
#endif
  return NULL;
}

int main(void) {
  pthread_t workers[2];
  machine.next_stable_name = 1;
  pthread_mutex_init(&lock, NULL);
  pthread_create(&workers[0], NULL, worker, (void *)0);
  pthread_create(&workers[1], NULL, worker, (void *)1);
  pthread_join(workers[0], NULL);
  pthread_join(workers[1], NULL);
  assert(hashes[0] != hashes[1]);
  pthread_mutex_destroy(&lock);
  return 0;
}
