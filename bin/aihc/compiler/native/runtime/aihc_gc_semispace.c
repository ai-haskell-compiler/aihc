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

/* Static objects never move, so the collector does not copy them. It has to
   decide instead which of them are live. An evaluated CAF is an indirection
   into the managed heap, so treating every static object as a root would keep
   everything any CAF has ever produced. A static object is live when a static
   reference table names it, when a table reachable from a live object or from
   the running function names it, or when a live object points at it.

   The static-root section lists exactly the objects worth marking, so the
   collector hashes those addresses once into an open-addressed table and keeps
   one mark byte per slot. */
typedef struct {
  AihcValue **slots;
  uint8_t *marked;
  size_t capacity;
  size_t count;
  uintptr_t lowest;
  uintptr_t highest;
  int initialized;
} AihcStaticObjects;

typedef struct {
  AihcValue **items;
  size_t count;
  size_t capacity;
} AihcStaticWorklist;

typedef struct {
  const AihcSrt **items;
  size_t count;
  size_t capacity;
} AihcSrtWorklist;

static AihcStaticObjects aihc_static_objects;
static AihcStaticWorklist aihc_static_worklist;
static AihcSrtWorklist aihc_srt_worklist;
/* Terminates the list of tables this collection has walked. Tables form a
   cyclic graph across recursive functions, so each one is stamped once and the
   whole list is cleared when the collection ends. */
static AihcSrt aihc_srt_list_end;
static AihcSrt *aihc_srt_stamped;

static size_t aihc_static_slot_of(uintptr_t address, size_t capacity) {
  /* Object addresses are word-aligned, so the low bits carry no information. */
  uintptr_t mixed = address >> 3;
  mixed ^= mixed >> 17;
  mixed *= (uintptr_t)0x27d4eb2dU;
  mixed ^= mixed >> 15;
  return (size_t)mixed & (capacity - 1);
}

static void aihc_static_objects_insert(AihcValue *object) {
  size_t slot =
      aihc_static_slot_of((uintptr_t)object, aihc_static_objects.capacity);
  while (aihc_static_objects.slots[slot] != NULL) {
    if (aihc_static_objects.slots[slot] == object) {
      return;
    }
    slot = (slot + 1) & (aihc_static_objects.capacity - 1);
  }
  aihc_static_objects.slots[slot] = object;
  ++aihc_static_objects.count;
  if ((uintptr_t)object < aihc_static_objects.lowest) {
    aihc_static_objects.lowest = (uintptr_t)object;
  }
  if ((uintptr_t)object > aihc_static_objects.highest) {
    aihc_static_objects.highest = (uintptr_t)object;
  }
}

static void aihc_static_objects_initialize(void) {
  if (aihc_static_objects.initialized) {
    return;
  }
  aihc_static_objects.initialized = 1;
  aihc_static_objects.lowest = UINTPTR_MAX;
  AihcValue **first = aihc_static_root_start();
  AihcValue **last = aihc_static_root_end();
  size_t entries = (first == NULL || last == NULL) ? 0 : (size_t)(last - first);
  size_t capacity = 8;
  while (capacity < entries * 2) {
    if (capacity > SIZE_MAX / 2) {
      aihc_fail("static object table is too large");
    }
    capacity *= 2;
  }
  aihc_static_objects.capacity = capacity;
  aihc_static_objects.slots =
      aihc_allocate_zeroed(sizeof(*aihc_static_objects.slots) * capacity);
  aihc_static_objects.marked =
      aihc_allocate_zeroed(sizeof(*aihc_static_objects.marked) * capacity);
  if (first != NULL && last != NULL) {
    for (AihcValue **entry = first; entry < last; ++entry) {
      if (*entry != NULL) {
        aihc_static_objects_insert(*entry);
      }
    }
  }
  if (aihc_static_objects.count == 0) {
    aihc_static_objects.lowest = 1;
    aihc_static_objects.highest = 0;
  }
}

static void *aihc_worklist_grow(void *items, size_t *capacity,
                                size_t item_bytes) {
  size_t next = *capacity == 0 ? 16 : *capacity * 2;
  if (next > SIZE_MAX / item_bytes) {
    aihc_fail("collector worklist is too large");
  }
  void *grown = realloc(items, next * item_bytes);
  if (grown == NULL) {
    aihc_fail("out of memory");
  }
  *capacity = next;
  return grown;
}

/* Mark one static object and queue it for scanning. Returns whether the
   address belongs to a static object at all, so callers can tell a static
   object from an ordinary to-space pointer. */
static int aihc_mark_static(AihcValue *object) {
  uintptr_t address = (uintptr_t)object;
  if (address < aihc_static_objects.lowest ||
      address > aihc_static_objects.highest) {
    return 0;
  }
  size_t slot = aihc_static_slot_of(address, aihc_static_objects.capacity);
  while (aihc_static_objects.slots[slot] != NULL) {
    if (aihc_static_objects.slots[slot] == object) {
      if (aihc_static_objects.marked[slot]) {
        return 1;
      }
      aihc_static_objects.marked[slot] = 1;
      if (aihc_static_worklist.count == aihc_static_worklist.capacity) {
        aihc_static_worklist.items = aihc_worklist_grow(
            aihc_static_worklist.items, &aihc_static_worklist.capacity,
            sizeof(*aihc_static_worklist.items));
      }
      aihc_static_worklist.items[aihc_static_worklist.count++] = object;
      return 1;
    }
    slot = (slot + 1) & (aihc_static_objects.capacity - 1);
  }
  return 0;
}

static void aihc_walk_srt(const AihcSrt *srt) {
  if (srt == NULL || srt->walked != NULL) {
    return;
  }
  AihcSrt *stamped = (AihcSrt *)(uintptr_t)srt;
  stamped->walked =
      aihc_srt_stamped == NULL ? &aihc_srt_list_end : aihc_srt_stamped;
  aihc_srt_stamped = stamped;
  if (aihc_srt_worklist.count == aihc_srt_worklist.capacity) {
    aihc_srt_worklist.items =
        aihc_worklist_grow(aihc_srt_worklist.items, &aihc_srt_worklist.capacity,
                           sizeof(*aihc_srt_worklist.items));
  }
  aihc_srt_worklist.items[aihc_srt_worklist.count++] = srt;
}

static void aihc_clear_srt_stamps(void) {
  AihcSrt *stamped = aihc_srt_stamped;
  while (stamped != NULL) {
    AihcSrt *next =
        stamped->walked == &aihc_srt_list_end ? NULL : stamped->walked;
    stamped->walked = NULL;
    stamped = next;
  }
  aihc_srt_stamped = NULL;
}

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
  if (value == NULL) {
    return value;
  }
  if (!aihc_in_space(context->from_start, context->from_bytes, value)) {
    /* Anything outside from-space is either already copied or a static
       object. Static objects stay where they are, but a live one still has to
       be scanned, and an evaluated CAF still holds a heap pointer. */
    aihc_mark_static(value);
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

/* Scan one object wherever it lives. The object's info table also names the
   static objects its code reaches, so tracing an object walks that table. */
static void aihc_scan_object(AihcForwardingContext *context,
                             AihcValue *object) {
  const AihcInfo *info = aihc_value_info_table(object);
  AihcObjectKind kind = info->object_kind;
  uint64_t count = info->field_count;
  aihc_walk_srt(info->srt);
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
}

/* Copying, static marking, and table walking all feed one another: a table
   names static objects, a static object holds heap pointers, and a copied
   object names further tables. Run all three to quiescence. */
static void aihc_trace(AihcForwardingContext *context) {
  AihcMachine *machine = context->machine;
  uint8_t *scan = machine->heap_start;
  for (;;) {
    if (aihc_srt_worklist.count != 0) {
      const AihcSrt *srt = aihc_srt_worklist.items[--aihc_srt_worklist.count];
      for (uintptr_t index = 0; index < srt->object_count; ++index) {
        aihc_mark_static((AihcValue *)srt->entries[index]);
      }
      for (uintptr_t index = 0; index < srt->child_count; ++index) {
        aihc_walk_srt((const AihcSrt *)srt->entries[srt->object_count + index]);
      }
      continue;
    }
    if (aihc_static_worklist.count != 0) {
      aihc_scan_object(
          context, aihc_static_worklist.items[--aihc_static_worklist.count]);
      continue;
    }
    if (scan < machine->heap_next) {
      AihcValue *object = (AihcValue *)scan;
      aihc_scan_object(context, object);
      scan += sizeof(AihcSlot) * aihc_value_words(object);
      continue;
    }
    return;
  }
}

/* Overwrite the indirection target of every static object this collection did
   not mark. A table that is missing an entry then fails on the next use of
   that CAF instead of quietly reading collected memory. */
static void aihc_poison_dead_cafs(void) {
  for (size_t slot = 0; slot < aihc_static_objects.capacity; ++slot) {
    AihcValue *object = aihc_static_objects.slots[slot];
    if (object == NULL || aihc_static_objects.marked[slot]) {
      continue;
    }
    if (aihc_value_kind(object) == AIHC_OBJECT_INDIRECTION) {
      object->fields[0] = (AihcSlot)UINT64_C(0xDEAD0000CAF00000);
    }
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
  memset(aihc_static_objects.marked, 0, aihc_static_objects.capacity);
  aihc_static_worklist.count = 0;
  aihc_srt_worklist.count = 0;
  if (aihc_rts_config()->static_reference_roots) {
    /* The running function has no heap object of its own to carry its table,
       so it publishes one on entry. Suspended code is a continuation closure
       and reaches its table through its info table like any other object. */
    aihc_walk_srt(aihc_current_srt);
  } else {
    /* Every static object stays alive. The tables do not yet name everything
       a running program reaches, so this remains the default. */
    for (size_t slot = 0; slot < aihc_static_objects.capacity; ++slot) {
      if (aihc_static_objects.slots[slot] != NULL) {
        aihc_mark_static(aihc_static_objects.slots[slot]);
      }
    }
  }
  aihc_visit_roots(machine, root_count, roots, aihc_forward_root, &context);
  aihc_trace(&context);
  if (aihc_rts_config()->poison_dead_cafs) {
    aihc_poison_dead_cafs();
  }
  aihc_clear_srt_stamps();

  machine->other_space = from_start;
  machine->other_space_bytes = from_bytes;
  size_t live_bytes = (size_t)(machine->heap_next - machine->heap_start);
  if (required_bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_semispace_exhausted(machine);
  }
  aihc_grow_target(machine, live_bytes + required_bytes);
}

void aihc_gc_init(AihcMachine *machine) {
  aihc_static_objects_initialize();
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
