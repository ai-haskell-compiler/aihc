#include "aihc_runtime_internal.h"

#include <stdint.h>

static size_t aihc_heap_bytes(uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap allocation is too large");
  }
  return sizeof(AihcSlot) * (size_t)words;
}

static void aihc_check_heap_limit(const AihcMachine *machine, size_t bytes) {
  if (machine->heap_limit_enabled &&
      (machine->heap_allocated_bytes > machine->heap_max_bytes ||
       bytes > machine->heap_max_bytes - machine->heap_allocated_bytes)) {
    aihc_fail("heap limit exceeded");
  }
}

void aihc_gc_init(AihcMachine *machine) {
  machine->heap_next = NULL;
  machine->heap_limit = (uint8_t *)UINTPTR_MAX;
}

void aihc_gc_note_update(AihcValue *object) { (void)object; }

void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots) {
  (void)root_count;
  (void)roots;
  aihc_check_heap_limit(machine, aihc_heap_bytes(words));
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  size_t bytes = aihc_heap_bytes(words);
  aihc_check_heap_limit(machine, bytes);
  AihcValue *value = aihc_allocate_zeroed(bytes);
  if (bytes > UINT64_MAX - machine->heap_allocated_bytes) {
    aihc_fail("heap allocation counter overflow");
  }
  machine->heap_allocated_bytes += bytes;
  return value;
}
