#include "aihc_runtime_internal.h"
#include <assert.h>

int main(void) {
  AihcMachine *machine = aihc_machine_initialize();
  assert(machine == &aihc_machine);
  assert(machine->heap_start != NULL);
  assert(machine->heap_next == machine->heap_start);
  assert(machine->nursery_bytes == AIHC_NURSERY_BYTES);
#ifdef COLLECT
  aihc_gc_collect_generation(machine, 0, 0, NULL, NULL);
  assert(machine->gc_count == 1);
#endif
  AihcValue *reference = aihc_mutvar_new(machine, 73);
  assert(((AihcSlot *)reference)[2] == 73);
  return 0;
}
