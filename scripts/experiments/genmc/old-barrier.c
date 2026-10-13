#include "aihc_runtime_internal.h"
static AihcMachine machine;
static AihcMVar object;
static const AihcInfo info = {.object_kind = AIHC_OBJECT_MVAR};
int main(void) {
  object.header = (AihcSlot)(uintptr_t)&info;
  aihc_write_barrier(&machine, (AihcValue *)&object);
  return 0;
}
