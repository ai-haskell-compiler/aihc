#include "aihc_runtime_internal.h"
#include <assert.h>
#include <stdlib.h>

static AihcMachine machine;

int main(void) {
  machine.heap_start = malloc(256);
  assert(machine.heap_start != NULL);
  machine.heap_next = machine.heap_start;
  machine.heap_limit = machine.heap_start + 256;
  machine.nursery_bytes = 256;
  return aihc_gc_allocate_pinned(&machine, 1) == NULL;
}
