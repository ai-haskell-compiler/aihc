#include "aihc_runtime_internal.h"
#include "libc.h"
#include <stdint.h>

/* A functional region allocator over malloc. The region contract permits
   unspecified contents. The native control executes this same allocator. */
uint8_t *aihc_host_map_regions(size_t count) {
  if (count == 0 || count > (SIZE_MAX >> AIHC_REGION_SHIFT) - 1) {
    return NULL;
  }
  size_t bytes = count << AIHC_REGION_SHIFT;
  void *allocation = malloc(bytes + AIHC_REGION_BYTES);
  if (allocation == NULL) {
    return NULL;
  }
  uintptr_t aligned = ((uintptr_t)allocation + AIHC_REGION_BYTES) &
                      ~(uintptr_t)(AIHC_REGION_BYTES - 1);
  ((void **)aligned)[-1] = allocation;
  return (uint8_t *)aligned;
}

int aihc_host_unmap_regions(void *base, size_t count) {
  (void)count;
  free(((void **)base)[-1]);
  return 0;
}
