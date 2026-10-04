#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The region table is a two-level map from an address to the state of its
   region. The top level is indexed by the address bits above the leaf, and a
   leaf covers 4 GiB: one entry for each 64 KiB region. A process touches a
   few leaves, and the pages of a leaf cost memory only when they are
   written, so the table needs no reservation of address space.

   The runtime takes memory from the host one mapping at a time, each the
   exact size of the run that is wanted. Regions of a released run wait in a
   free list for the next run that fits. When every region of a mapping is
   free and the free list is above its cache bound, the mapping goes back to
   the host. */

#define AIHC_REGION_LEAF_BITS 16
#define AIHC_REGION_LEAF_COUNT ((size_t)1 << AIHC_REGION_LEAF_BITS)
#define AIHC_REGION_LEAF_MASK (AIHC_REGION_LEAF_COUNT - 1)
/* The free regions the table keeps for reuse before it unmaps a free
   mapping: 1024 regions are 64 MiB. */
#define AIHC_REGION_FREE_CACHE ((size_t)1024)

typedef struct AihcRegionMapping {
  uint8_t *base;
  size_t count;
  size_t free_count;
} AihcRegionMapping;

typedef struct {
  /* The mapping that holds the region, or null outside every mapping. */
  AihcRegionMapping *mapping;
  /* The length of the run that starts here, or zero elsewhere. */
  uint32_t run;
  /* The position of the region in its run. */
  uint32_t offset;
  uint8_t kind;
} AihcRegionEntry;

typedef struct {
  AihcRegionEntry entries[AIHC_REGION_LEAF_COUNT];
} AihcRegionLeaf;

/* A free run. The list is in address order, and two neighbours in the same
   mapping are always merged. */
typedef struct AihcRegionFreeRun {
  uint8_t *base;
  size_t count;
  struct AihcRegionFreeRun *next;
} AihcRegionFreeRun;

static AihcRegionLeaf **aihc_region_top;
static size_t aihc_region_top_count;
static AihcRegionFreeRun *aihc_region_free_runs;
static size_t aihc_region_free_count;

/* The two shifts of sixteen keep the top index defined on a 32-bit host,
   where one shift of thirty-two would be undefined. */
static size_t aihc_region_top_index(const void *address) {
  return (size_t)(((uintptr_t)address >> AIHC_REGION_SHIFT) >>
                  AIHC_REGION_LEAF_BITS);
}

static size_t aihc_region_leaf_index(const void *address) {
  return (size_t)(((uintptr_t)address >> AIHC_REGION_SHIFT) &
                  AIHC_REGION_LEAF_MASK);
}

/* The entry of the region that holds an address. With create, the top level
   grows and the leaf is made when they are missing. Without it, a missing
   leaf gives null. */
static AihcRegionEntry *aihc_region_entry(const void *address, int create) {
  size_t top = aihc_region_top_index(address);
  if (top >= aihc_region_top_count) {
    if (!create) {
      return NULL;
    }
    size_t grown = aihc_region_top_count == 0 ? 1 : aihc_region_top_count;
    while (grown <= top) {
      if (grown > SIZE_MAX / 2 / sizeof(*aihc_region_top)) {
        aihc_fail("region table is too large");
      }
      grown *= 2;
    }
    AihcRegionLeaf **table = realloc(aihc_region_top, grown * sizeof(*table));
    if (table == NULL) {
      aihc_fail("out of memory for the region table");
    }
    memset(table + aihc_region_top_count, 0,
           (grown - aihc_region_top_count) * sizeof(*table));
    aihc_region_top = table;
    aihc_region_top_count = grown;
  }
  AihcRegionLeaf *leaf = aihc_region_top[top];
  if (leaf == NULL) {
    if (!create) {
      return NULL;
    }
    leaf = calloc(1, sizeof(*leaf));
    if (leaf == NULL) {
      aihc_fail("out of memory for the region table");
    }
    aihc_region_top[top] = leaf;
  }
  return &leaf->entries[aihc_region_leaf_index(address)];
}

size_t aihc_regions_for_bytes(size_t bytes) {
  if (bytes > SIZE_MAX - (AIHC_REGION_BYTES - 1)) {
    aihc_fail("heap allocation is too large");
  }
  return (bytes + AIHC_REGION_BYTES - 1) >> AIHC_REGION_SHIFT;
}

static uint8_t *aihc_region_at(uint8_t *base, size_t index) {
  return base + (index << AIHC_REGION_SHIFT);
}

/* Set the kind of each region of a run, and the run length at its start. */
static void aihc_regions_mark(uint8_t *base, size_t count,
                              AihcRegionMapping *mapping, AihcRegionKind kind,
                              uint32_t run) {
  for (size_t index = 0; index < count; ++index) {
    AihcRegionEntry *entry = aihc_region_entry(aihc_region_at(base, index), 1);
    entry->mapping = mapping;
    entry->kind = (uint8_t)kind;
    entry->run = index == 0 ? run : 0;
    entry->offset = (uint32_t)index;
  }
}

/* Take the first count regions of a free run out of the free list. */
static void aihc_free_run_take(AihcRegionFreeRun **link, size_t count) {
  AihcRegionFreeRun *run = *link;
  run->base = aihc_region_at(run->base, count);
  run->count -= count;
  if (run->count == 0) {
    *link = run->next;
    free(run);
  }
  aihc_region_free_count -= count;
}

/* Put a run in the free list. A neighbour in the same mapping that touches
   the run is merged with it. */
static void aihc_free_run_insert(uint8_t *base, size_t count,
                                 AihcRegionMapping *mapping) {
  AihcRegionFreeRun *previous = NULL;
  AihcRegionFreeRun *next = aihc_region_free_runs;
  while (next != NULL && next->base < base) {
    previous = next;
    next = next->next;
  }
  AihcRegionFreeRun **link =
      previous == NULL ? &aihc_region_free_runs : &previous->next;
  aihc_region_free_count += count;
  if (previous != NULL &&
      aihc_region_at(previous->base, previous->count) == base &&
      aihc_region_entry(previous->base, 0)->mapping == mapping) {
    previous->count += count;
    if (next != NULL &&
        aihc_region_at(previous->base, previous->count) == next->base &&
        aihc_region_entry(next->base, 0)->mapping == mapping) {
      previous->count += next->count;
      previous->next = next->next;
      free(next);
    }
    return;
  }
  if (next != NULL && aihc_region_at(base, count) == next->base &&
      aihc_region_entry(next->base, 0)->mapping == mapping) {
    next->base = base;
    next->count += count;
    return;
  }
  AihcRegionFreeRun *run = malloc(sizeof(*run));
  if (run == NULL) {
    aihc_fail("out of memory for the region table");
  }
  run->base = base;
  run->count = count;
  run->next = next;
  *link = run;
}

void *aihc_regions_acquire(size_t count, AihcRegionKind kind) {
  if (count == 0 || count > UINT32_MAX || kind == AIHC_REGION_OUTSIDE ||
      kind == AIHC_REGION_FREE) {
    aihc_fail("invalid region request");
  }
  for (AihcRegionFreeRun **link = &aihc_region_free_runs; *link != NULL;
       link = &(*link)->next) {
    if ((*link)->count < count) {
      continue;
    }
    uint8_t *base = (*link)->base;
    AihcRegionMapping *mapping = aihc_region_entry(base, 0)->mapping;
    aihc_free_run_take(link, count);
    mapping->free_count -= count;
    aihc_regions_mark(base, count, mapping, kind, (uint32_t)count);
    return base;
  }
  uint8_t *base = aihc_host_map_regions(count);
  if (base == NULL) {
    aihc_fail("out of memory");
  }
  AihcRegionMapping *mapping = malloc(sizeof(*mapping));
  if (mapping == NULL) {
    aihc_fail("out of memory for the region table");
  }
  mapping->base = base;
  mapping->count = count;
  mapping->free_count = 0;
  aihc_regions_mark(base, count, mapping, kind, (uint32_t)count);
  return base;
}

/* Give a mapping whose regions are all free back to the host. The free list
   holds the mapping as one run, because the regions of one mapping merge. */
static void aihc_mapping_unmap(AihcRegionMapping *mapping) {
  if (aihc_host_unmap_regions(mapping->base, mapping->count) != 0) {
    return;
  }
  AihcRegionFreeRun **link = &aihc_region_free_runs;
  while (*link != NULL && (*link)->base != mapping->base) {
    link = &(*link)->next;
  }
  if (*link == NULL || (*link)->count != mapping->count) {
    aihc_fail("region table lost a free mapping");
  }
  aihc_free_run_take(link, mapping->count);
  aihc_regions_mark(mapping->base, mapping->count, NULL, AIHC_REGION_OUTSIDE,
                    0);
  free(mapping);
}

void aihc_regions_release(void *base) {
  AihcRegionEntry *entry = aihc_region_entry(base, 0);
  if (entry == NULL || entry->mapping == NULL || entry->run == 0 ||
      entry->kind == AIHC_REGION_FREE ||
      ((uintptr_t)base & (AIHC_REGION_BYTES - 1)) != 0) {
    aihc_fail("released memory is not an acquired run");
  }
  size_t count = entry->run;
  AihcRegionMapping *mapping = entry->mapping;
  aihc_regions_mark(base, count, mapping, AIHC_REGION_FREE, 0);
  mapping->free_count += count;
  aihc_free_run_insert(base, count, mapping);
  if (mapping->free_count == mapping->count &&
      aihc_region_free_count > AIHC_REGION_FREE_CACHE) {
    aihc_mapping_unmap(mapping);
  }
}

void aihc_regions_set_kind(void *base, AihcRegionKind kind) {
  AihcRegionEntry *entry = aihc_region_entry(base, 0);
  if (entry == NULL || entry->mapping == NULL || entry->run == 0 ||
      kind == AIHC_REGION_OUTSIDE || kind == AIHC_REGION_FREE) {
    aihc_fail("relabeled memory is not an acquired run");
  }
  aihc_regions_mark(base, entry->run, entry->mapping, kind, entry->run);
}

AihcRegionKind aihc_region_kind(const void *address) {
  const AihcRegionEntry *entry = aihc_region_entry(address, 0);
  return entry == NULL ? AIHC_REGION_OUTSIDE : (AihcRegionKind)entry->kind;
}

void *aihc_region_run_base(const void *address) {
  const AihcRegionEntry *entry = aihc_region_entry(address, 0);
  if (entry == NULL || entry->mapping == NULL) {
    return NULL;
  }
  uint8_t *region =
      (uint8_t *)((uintptr_t)address & ~(uintptr_t)(AIHC_REGION_BYTES - 1));
  return region - ((size_t)entry->offset << AIHC_REGION_SHIFT);
}
