#include "aihc_runtime_internal.h"

#include <string.h>

typedef struct {
  AihcMachine *machine;
  uint8_t *from_start;
} AihcForwardingContext;

static int aihc_in_space(const uint8_t *start, uint64_t bytes,
                         const AihcValue *value) {
  uintptr_t address = (uintptr_t)value;
  uintptr_t first = (uintptr_t)start;
  return address >= first && address - first < bytes;
}

static AihcValue *aihc_forward(AihcMachine *machine, uint8_t *from_start,
                               AihcValue *value) {
  if (value == NULL ||
      !aihc_in_space(from_start, machine->semispace_bytes, value)) {
    return value;
  }
  AihcValue *forwarded = (AihcValue *)(uintptr_t)value->header;
  if (aihc_in_space(machine->heap_start, machine->semispace_bytes, forwarded)) {
    return forwarded;
  }

  const AihcInfo *info = aihc_value_info_table(value);
  uint64_t words = aihc_object_words(info);
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_fail("live data exceeds semispace");
  }
  AihcValue *copy = (AihcValue *)machine->heap_next;
  memcpy(copy, value, bytes);
  machine->heap_next += bytes;
  value->header = (AihcSlot)(uintptr_t)copy;
  return copy;
}

static AihcSlot aihc_forward_root(AihcSlot root, void *opaque_context) {
  AihcForwardingContext *context = opaque_context;
  return (AihcSlot)(uintptr_t)aihc_forward(
      context->machine, context->from_start, (AihcValue *)(uintptr_t)root);
}

static void aihc_collect(AihcMachine *machine, uint64_t required_words,
                         uint64_t root_count, AihcSlot *roots) {
  uint8_t *from_start = machine->heap_start;
  uint8_t *to_start = machine->other_space;
  machine->heap_start = to_start;
  machine->heap_next = to_start;
  machine->heap_limit = to_start + machine->semispace_bytes;

  AihcForwardingContext context = {machine, from_start};
  aihc_visit_roots(machine, root_count, roots, aihc_forward_root, &context);

  uint8_t *scan = to_start;
  while (scan < machine->heap_next) {
    AihcValue *object = (AihcValue *)scan;
    const AihcInfo *info = aihc_value_info_table(object);
    AihcObjectKind kind = info->object_kind;
    uint64_t count = info->field_count;
    if (kind == AIHC_OBJECT_INDIRECTION) {
      object->fields[0] = aihc_forward_root(object->fields[0], &context);
    } else if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
               kind == AIHC_OBJECT_THUNK ||
               kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR ||
               kind == AIHC_OBJECT_BLACKHOLE) {
      for (uint64_t index = 0; index < count; ++index) {
        if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
          object->fields[index] =
              aihc_forward_root(object->fields[index], &context);
        }
      }
    } else {
      aihc_fail("collector encountered an invalid object kind");
    }
    scan += sizeof(AihcSlot) * aihc_object_words(info);
  }

  machine->other_space = from_start;
  size_t required_bytes = sizeof(AihcSlot) * required_words;
  if (machine->heap_next + required_bytes > machine->heap_limit) {
    aihc_fail("insufficient heap after collection");
  }
}

void aihc_gc_init(AihcMachine *machine) {
  machine->semispace_bytes = AIHC_SEMISPACE_BYTES;
  machine->heap_start =
      aihc_allocate_auxiliary(machine, machine->semispace_bytes);
  machine->other_space =
      aihc_allocate_auxiliary(machine, machine->semispace_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->semispace_bytes;
}

void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots) {
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_collect(machine, words, root_count, roots);
  }
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_next + bytes > machine->heap_limit) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  AihcValue *value = (AihcValue *)machine->heap_next;
  machine->heap_next += bytes;
  memset(value, 0, bytes);
  return value;
}
