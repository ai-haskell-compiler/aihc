#include "aihc_runtime_internal.h"

static AihcMachine machine;
static AihcSlot nursery[32];

int main(void) {
  machine.heap_start = (uint8_t *)nursery;
  machine.heap_next = machine.heap_start;
  machine.heap_limit = machine.heap_start + sizeof(nursery);
  machine.nursery_bytes = sizeof(nursery);
  return aihc_gc_allocate_pinned(&machine, 1) == NULL;
}
