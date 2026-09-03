#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The collector copies live objects between two spaces. The current space is
   described by heap_start, heap_next, and heap_limit. The other space and its
   capacity wait in other_space and other_space_bytes. semispace_bytes is the
   capacity that the next collection gives the destination space. It doubles
   until it holds twice the live data, so the program does not collect on every
   allocation when its live data grows. The -M limit caps that capacity. */

typedef struct {
  AihcMachine *machine;
  uint8_t *from_start;
  size_t from_bytes;
} AihcForwardingContext;

static int aihc_in_space(const uint8_t *start, size_t bytes,
                         const AihcValue *value) {
  uintptr_t address = (uintptr_t)value;
  uintptr_t first = (uintptr_t)start;
  return address >= first && address - first < bytes;
}

static size_t aihc_semispace_capacity(const AihcMachine *machine) {
  return (size_t)(machine->heap_limit - machine->heap_start);
}

static uint8_t *aihc_semispace_new(size_t bytes) {
  uint8_t *space = malloc(bytes == 0 ? 1 : bytes);
  if (space == NULL) {
    aihc_fail("out of memory");
  }
  return space;
}

static _Noreturn void aihc_semispace_exhausted(const AihcMachine *machine) {
  if (machine->heap_limit_enabled) {
    aihc_fail("heap limit exceeded");
  }
  aihc_fail("live data exceeds semispace");
}

static AihcValue *aihc_forward(AihcForwardingContext *context,
                               AihcValue *value) {
  AihcMachine *machine = context->machine;
  if (value == NULL ||
      !aihc_in_space(context->from_start, context->from_bytes, value)) {
    return value;
  }
  AihcValue *forwarded = (AihcValue *)(uintptr_t)value->header;
  if (aihc_in_space(machine->heap_start, aihc_semispace_capacity(machine),
                    forwarded)) {
    return forwarded;
  }

  uint64_t words = aihc_value_words(value);
  size_t bytes = sizeof(AihcSlot) * words;
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_semispace_exhausted(machine);
  }
  AihcValue *copy = (AihcValue *)machine->heap_next;
  memcpy(copy, value, bytes);
  machine->heap_next += bytes;
  value->header = (AihcSlot)(uintptr_t)copy;
  return copy;
}

static AihcSlot aihc_forward_root(AihcSlot root, void *opaque_context) {
  return (AihcSlot)(uintptr_t)aihc_forward(opaque_context,
                                           (AihcValue *)(uintptr_t)root);
}

static void aihc_scan_to_space(AihcForwardingContext *context) {
  AihcMachine *machine = context->machine;
  uint8_t *scan = machine->heap_start;
  while (scan < machine->heap_next) {
    AihcValue *object = (AihcValue *)scan;
    const AihcInfo *info = aihc_value_info_table(object);
    AihcObjectKind kind = info->object_kind;
    uint64_t count = info->field_count;
    if (kind == AIHC_OBJECT_INDIRECTION) {
      object->fields[0] = aihc_forward_root(object->fields[0], context);
    } else if (kind == AIHC_OBJECT_ARRAY) {
      uint64_t length = aihc_array_length(object);
      AihcSlot *elements = aihc_array_elements(object);
      for (uint64_t index = 0; index < length; ++index) {
        elements[index] = aihc_forward_root(elements[index], context);
      }
    } else if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
               kind == AIHC_OBJECT_THUNK ||
               kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR ||
               kind == AIHC_OBJECT_BLACKHOLE) {
      for (uint64_t index = 0; index < count; ++index) {
        if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
          object->fields[index] =
              aihc_forward_root(object->fields[index], context);
        }
      }
    } else {
      aihc_fail("collector encountered an invalid object kind");
    }
    scan += sizeof(AihcSlot) * aihc_value_words(object);
  }
}

/* Select the capacity of the destination space. Live data never exceeds the
   used part of the source space, so that size plus the pending reservation
   always fits unless the -M limit forbids it. */
static size_t aihc_destination_bytes(const AihcMachine *machine,
                                     size_t required_bytes) {
  size_t used = (size_t)(machine->heap_next - machine->heap_start);
  if (required_bytes > SIZE_MAX - used) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = used + required_bytes;
  if (bytes < machine->semispace_bytes) {
    bytes = machine->semispace_bytes;
  }
  if (machine->heap_limit_enabled && bytes > machine->heap_max_bytes) {
    bytes = (size_t)machine->heap_max_bytes;
  }
  return bytes;
}

/* Double the target capacity until it holds twice the live data. */
static void aihc_grow_target(AihcMachine *machine, size_t occupied_bytes) {
  size_t target = machine->semispace_bytes;
  if (target == 0) {
    target = 1;
  }
  while (target < occupied_bytes || target - occupied_bytes < occupied_bytes) {
    if (target > SIZE_MAX / 2) {
      target = SIZE_MAX;
      break;
    }
    target *= 2;
  }
  if (machine->heap_limit_enabled && target > machine->heap_max_bytes) {
    target = (size_t)machine->heap_max_bytes;
  }
  machine->semispace_bytes = target;
}

static void aihc_collect(AihcMachine *machine, size_t required_bytes,
                         uint64_t root_count, AihcSlot *roots) {
  uint8_t *from_start = machine->heap_start;
  size_t from_bytes = aihc_semispace_capacity(machine);
  size_t to_bytes = aihc_destination_bytes(machine, required_bytes);
  if (machine->other_space == NULL || machine->other_space_bytes < to_bytes) {
    free(machine->other_space);
    machine->other_space = aihc_semispace_new(to_bytes);
    machine->other_space_bytes = to_bytes;
  }
  uint8_t *to_start = machine->other_space;
  machine->heap_start = to_start;
  machine->heap_next = to_start;
  machine->heap_limit = to_start + machine->other_space_bytes;

  AihcForwardingContext context = {machine, from_start, from_bytes};
  aihc_visit_roots(machine, root_count, roots, aihc_forward_root, &context);
  aihc_scan_to_space(&context);

  machine->other_space = from_start;
  machine->other_space_bytes = from_bytes;
  size_t live_bytes = (size_t)(machine->heap_next - machine->heap_start);
  if (required_bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_semispace_exhausted(machine);
  }
  aihc_grow_target(machine, live_bytes + required_bytes);
}

void aihc_gc_init(AihcMachine *machine) {
  machine->semispace_bytes = AIHC_SEMISPACE_BYTES;
  if (machine->heap_limit_enabled &&
      machine->semispace_bytes > machine->heap_max_bytes) {
    machine->semispace_bytes = machine->heap_max_bytes;
  }
  machine->heap_start = aihc_semispace_new(machine->semispace_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->semispace_bytes;
  machine->other_space = NULL;
  machine->other_space_bytes = 0;
}

void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_limit_enabled && bytes > machine->heap_max_bytes) {
    aihc_fail("heap limit exceeded");
  }
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_collect(machine, bytes, root_count, roots);
  }
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap allocation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  AihcValue *value = (AihcValue *)machine->heap_next;
  machine->heap_next += bytes;
  memset(value, 0, bytes);
  return value;
}
