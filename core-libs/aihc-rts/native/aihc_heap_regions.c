#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The region table. kinds has one byte for each region of the range. runs
   holds the length of an acquired run at the index of its first region, so a
   release needs only the address of the run. committed is the number of
   regions from the start of the range that the host has given the runtime,
   or that other allocators use. A free run is only searched below it. */
static uint8_t *aihc_region_base;
static size_t aihc_region_count;
static size_t aihc_region_committed;
static uint8_t *aihc_region_kinds;
static uint32_t *aihc_region_runs;
static size_t aihc_region_free_count;
/* No region below this index is free. */
static size_t aihc_region_search_start;

void aihc_regions_init(void) {
  if (aihc_region_kinds != NULL) {
    return;
  }
  size_t used = 0;
  size_t committed = 0;
  aihc_host_reserve_regions(&aihc_region_base, &aihc_region_count, &used,
                            &committed);
  if (aihc_region_count == 0 || used > committed ||
      committed > aihc_region_count) {
    aihc_fail("cannot reserve the heap");
  }
  aihc_region_kinds = calloc(aihc_region_count, sizeof(*aihc_region_kinds));
  aihc_region_runs = calloc(aihc_region_count, sizeof(*aihc_region_runs));
  if (aihc_region_kinds == NULL || aihc_region_runs == NULL) {
    aihc_fail("out of memory for the region table");
  }
  memset(aihc_region_kinds + used, AIHC_REGION_FREE, committed - used);
  aihc_region_free_count = committed - used;
  aihc_region_committed = committed;
  aihc_region_search_start = used;
}

size_t aihc_regions_for_bytes(size_t bytes) {
  if (bytes > SIZE_MAX - (AIHC_REGION_BYTES - 1)) {
    aihc_fail("heap allocation is too large");
  }
  return (bytes + AIHC_REGION_BYTES - 1) >> AIHC_REGION_SHIFT;
}

static void *aihc_region_address(size_t index) {
  return aihc_region_base + ((size_t)index << AIHC_REGION_SHIFT);
}

static void *aihc_regions_take(size_t first, size_t count,
                               AihcRegionKind kind) {
  if (count > UINT32_MAX) {
    aihc_fail("heap allocation is too large");
  }
  memset(aihc_region_kinds + first, kind, count);
  aihc_region_runs[first] = (uint32_t)count;
  return aihc_region_address(first);
}

/* First fit over the committed part of the range. */
static int aihc_regions_find_free(size_t count, size_t *first) {
  if (aihc_region_free_count < count) {
    return 0;
  }
  size_t run = 0;
  int seen_free = 0;
  for (size_t index = aihc_region_search_start; index < aihc_region_committed;
       ++index) {
    if (aihc_region_kinds[index] != AIHC_REGION_FREE) {
      if (!seen_free) {
        /* Nothing free below here either. */
        aihc_region_search_start = index + 1;
      }
      run = 0;
      continue;
    }
    seen_free = 1;
    ++run;
    if (run == count) {
      *first = index + 1 - count;
      return 1;
    }
  }
  return 0;
}

void *aihc_regions_acquire(size_t count, AihcRegionKind kind) {
  if (count == 0 || kind == AIHC_REGION_OUTSIDE || kind == AIHC_REGION_FREE) {
    aihc_fail("invalid region request");
  }
  if (aihc_region_kinds == NULL) {
    aihc_regions_init();
  }
  size_t first = 0;
  if (aihc_regions_find_free(count, &first)) {
    aihc_region_free_count -= count;
    return aihc_regions_take(first, count, kind);
  }
  if (count > aihc_region_count - aihc_region_committed ||
      aihc_host_grow_regions(count, &first) != 0) {
    aihc_fail("out of memory");
  }
  /* The host can give regions above the committed part. The regions between
     belong to another allocator and keep the kind AIHC_REGION_OUTSIDE. */
  if (first < aihc_region_committed || count > aihc_region_count - first) {
    aihc_fail("the host grew the heap outside the reserved range");
  }
  aihc_region_committed = first + count;
  return aihc_regions_take(first, count, kind);
}

static size_t aihc_region_index(const void *address) {
  return (size_t)((const uint8_t *)address - aihc_region_base) >>
         AIHC_REGION_SHIFT;
}

/* The index compare avoids the byte count of the range, which does not fit
   a 32-bit word when the range is the whole wasm32 memory. */
static int aihc_in_region_range(const void *address) {
  uintptr_t value = (uintptr_t)address;
  uintptr_t base = (uintptr_t)aihc_region_base;
  return aihc_region_kinds != NULL && value >= base &&
         ((value - base) >> AIHC_REGION_SHIFT) < aihc_region_count;
}

void aihc_regions_release(void *base) {
  if (!aihc_in_region_range(base) ||
      base != aihc_region_address(aihc_region_index(base))) {
    aihc_fail("released memory is not a region run");
  }
  size_t first = aihc_region_index(base);
  size_t count = aihc_region_runs[first];
  if (count == 0 || aihc_region_kinds[first] == AIHC_REGION_FREE ||
      aihc_region_kinds[first] == AIHC_REGION_OUTSIDE) {
    aihc_fail("released memory is not an acquired run");
  }
  aihc_region_runs[first] = 0;
  memset(aihc_region_kinds + first, AIHC_REGION_FREE, count);
  aihc_region_free_count += count;
  if (first < aihc_region_search_start) {
    aihc_region_search_start = first;
  }
  aihc_host_release_regions(base, count << AIHC_REGION_SHIFT);
}

AihcRegionKind aihc_region_kind(const void *address) {
  if (!aihc_in_region_range(address)) {
    return AIHC_REGION_OUTSIDE;
  }
  return (AihcRegionKind)aihc_region_kinds[aihc_region_index(address)];
}
