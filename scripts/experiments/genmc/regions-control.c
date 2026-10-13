#include "aihc_runtime_internal.h"
#include <assert.h>

int main(void) {
  uint8_t *region = aihc_host_map_regions(1);
  assert(region != NULL);
  assert((uintptr_t)region % AIHC_REGION_BYTES == 0);
  region[0] = 19;
  region[AIHC_REGION_BYTES - 1] = 73;
  assert(region[0] == 19 && region[AIHC_REGION_BYTES - 1] == 73);
  assert(aihc_host_unmap_regions(region, 1) == 0);
  return 0;
}
