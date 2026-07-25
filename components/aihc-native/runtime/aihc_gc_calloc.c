#include "aihc_runtime_internal.h"

#include <stdint.h>

void aihc_gc_init(AihcMachine *machine) {
  machine->heap_next = NULL;
  machine->heap_limit = (uint8_t *)UINTPTR_MAX;
}

void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots) {
  (void)machine;
  (void)words;
  (void)root_count;
  (void)roots;
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  (void)machine;
  return aihc_allocate_zeroed(sizeof(AihcSlot) * words);
}
