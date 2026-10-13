#include "aihc_runtime_internal.h"
#include "libc.h"
#include <assert.h>

#define machine aihc_machine

int main(void) {
  machine.heap_start = malloc(4096);
  assert(machine.heap_start != NULL);
  machine.heap_next = machine.heap_start;
  machine.heap_alloc_base = machine.heap_start;
  machine.heap_limit = machine.heap_start + 4096;
  machine.nursery_bytes = 4096;
  assert(aihc_rts_nursery_bytes() == 0);
  AihcValue *reference = aihc_mutvar_new(&machine, 41);
  assert(((AihcSlot *)reference)[2] == 41);
  void *array = aihc_byte_array_new(&machine, 8);
  assert(aihc_byte_array_get_size(array) == 8);
  aihc_byte_array_set(array, 0, 8, 0x2a);
  unsigned char bytes[8];
  aihc_byte_array_copy_to_addr(array, 0, bytes, 8);
  for (size_t index = 0; index < sizeof(bytes); ++index) {
    assert(bytes[index] == 0x2a);
  }
  free(machine.heap_start);
  return 0;
}
