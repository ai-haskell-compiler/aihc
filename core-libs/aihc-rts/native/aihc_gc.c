#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The generational copying collector. See docs/gc-design.md.

   The heap has three generations. The nursery, generation zero, is one run
   of regions that compiled code fills with a bump pointer between heap_start
   and heap_limit. Gen1 and gen2 are lists of blocks that only the collector
   fills. A collection of the generations up to g copies every live object
   of those generations one generation up, except that an object a scanned
   object of generation k refers to goes to generation k at least. Thus an
   old object points only at old objects after one scan, and the barrier
   entry for it can be dropped.

   The blocks a collection copies away are relabeled FROM1 and FROM2 in the
   region table at its start, so one lookup tells whether a pointer names an
   object that moves. A copied object keeps its new address in its old
   header with the low two bits set to two: an info table pointer never has
   that pattern, because the second bit is set only on a blackhole, whose
   first bit is set as well.

   Objects that never move carry their age elsewhere. A large object and a
   pinned block keep it in their pinned block header, with the mark of the
   running collection. A continuation frame takes the age of its stack
   chunk. A static object is older than every generation: a minor collection
   reaches it only through the remembered set, and only a full collection
   traces the static reference tables. */

uint8_t *aihc_nursery_start;
uint64_t aihc_nursery_bytes;

/* The capacity of a block of an old generation. Every object below the large
   object bound fits in one. */
#define AIHC_HEAP_BLOCK_REGIONS ((size_t)4)
#define AIHC_HEAP_BLOCK_BYTES (AIHC_HEAP_BLOCK_REGIONS * AIHC_REGION_BYTES)
/* The generation that is older than every heap generation. */
#define AIHC_GENERATION_STATIC 3U

typedef struct {
  AihcMachine *machine;
  /* The oldest generation this collection copies. */
  unsigned collected;
  /* Objects the next collection must scan again: an object that points at
     a younger object after its scan. */
  AihcValue **kept;
  size_t kept_count;
  size_t kept_capacity;
  /* The stack chunks with a frame this collection scanned. */
  AihcStackChunk **touched;
  size_t touched_count;
  size_t touched_capacity;
} AihcGcContext;

/* The packed state of a stack chunk. */
#define AIHC_CHUNK_GENERATION_MASK UINT64_C(0xFF)
#define AIHC_CHUNK_YOUNGEST_SHIFT 8
#define AIHC_CHUNK_YOUNGEST_MASK (UINT64_C(0xFF) << AIHC_CHUNK_YOUNGEST_SHIFT)
#define AIHC_CHUNK_STAMP_SHIFT 16

static unsigned aihc_chunk_generation(const AihcStackChunk *chunk) {
  return (unsigned)(chunk->state & AIHC_CHUNK_GENERATION_MASK);
}

static void aihc_chunk_set_generation(AihcStackChunk *chunk,
                                      unsigned generation) {
  chunk->state = (chunk->state & ~AIHC_CHUNK_GENERATION_MASK) | generation;
}

/* The visitor state of one object scan. target is the lowest generation a
   referent may end in, and youngest is the youngest generation any referent
   ended in. */
typedef struct {
  AihcGcContext *gc;
  unsigned target;
  unsigned youngest;
} AihcScanContext;

/* Static objects and continuation frames never move, so the collector marks
   each one it reaches in an open-addressed hash set and scans it once from a
   worklist. */
typedef struct {
  AihcValue **slots;
  size_t capacity;
  size_t count;
} AihcAddressSet;

typedef struct {
  AihcValue **items;
  size_t count;
  size_t capacity;
} AihcValueWorklist;

typedef struct {
  const AihcSrt **items;
  size_t count;
  size_t capacity;
} AihcSrtWorklist;

struct AihcHostBuffer {
  AihcByteArray array;
  AihcHostBuffer *next;
  _Alignas(max_align_t) uint8_t contents[];
};

_Static_assert(offsetof(AihcPinnedBlock, object) % _Alignof(AihcHostBuffer) ==
                   0,
               "pinned metadata must preserve host buffer alignment");

static const AihcInfo aihc_buffer_info = {
    .object_kind = AIHC_OBJECT_BYTE_ARRAY,
};

static AihcAddressSet aihc_marked_statics;
static AihcValueWorklist aihc_static_worklist;
static AihcValueWorklist aihc_pinned_worklist;
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

static void aihc_address_set_grow(AihcAddressSet *set) {
  size_t capacity = set->capacity == 0 ? 64 : set->capacity * 2;
  if (capacity > SIZE_MAX / sizeof(*set->slots) / 2) {
    aihc_fail("static object set is too large");
  }
  AihcValue **slots = calloc(capacity, sizeof(*slots));
  if (slots == NULL) {
    aihc_fail("out of memory");
  }
  for (size_t index = 0; index < set->capacity; ++index) {
    AihcValue *object = set->slots[index];
    if (object == NULL) {
      continue;
    }
    size_t slot = aihc_static_slot_of((uintptr_t)object, capacity);
    while (slots[slot] != NULL) {
      slot = (slot + 1) & (capacity - 1);
    }
    slots[slot] = object;
  }
  free(set->slots);
  set->slots = slots;
  set->capacity = capacity;
}

/* Add one address to a set. Returns whether the address was new. */
static int aihc_address_set_insert(AihcAddressSet *set, AihcValue *object) {
  if (set->count * 2 >= set->capacity) {
    aihc_address_set_grow(set);
  }
  size_t slot = aihc_static_slot_of((uintptr_t)object, set->capacity);
  while (set->slots[slot] != NULL) {
    if (set->slots[slot] == object) {
      return 0;
    }
    slot = (slot + 1) & (set->capacity - 1);
  }
  set->slots[slot] = object;
  ++set->count;
  return 1;
}

/* Test membership without allocation or a new mark. */
static int aihc_address_set_contains(const AihcAddressSet *set,
                                     AihcValue *object) {
  if (set->capacity == 0) {
    return 0;
  }
  size_t slot = aihc_static_slot_of((uintptr_t)object, set->capacity);
  while (set->slots[slot] != NULL) {
    if (set->slots[slot] == object) {
      return 1;
    }
    slot = (slot + 1) & (set->capacity - 1);
  }
  return 0;
}

static void aihc_address_set_clear(AihcAddressSet *set) {
  if (set->count != 0) {
    memset(set->slots, 0, sizeof(*set->slots) * set->capacity);
  }
  set->count = 0;
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

static void aihc_value_worklist_push(AihcValueWorklist *list,
                                     AihcValue *object) {
  if (list->count == list->capacity) {
    list->items =
        aihc_worklist_grow(list->items, &list->capacity, sizeof(*list->items));
  }
  list->items[list->count++] = object;
}

/* Mark one object that never moves and queue it for scanning. */
static void aihc_mark_static(AihcValue *object) {
  if (object == NULL ||
      !aihc_address_set_insert(&aihc_marked_statics, object)) {
    return;
  }
  aihc_value_worklist_push(&aihc_static_worklist, object);
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

static int aihc_in_nursery(const AihcMachine *machine, const void *value) {
  return (uintptr_t)value - (uintptr_t)machine->heap_start <
         machine->nursery_bytes;
}

/* Whether an object outside every region is a block of the pinned list and
   not a static object. The only kinds the runtime allocates pinned are byte
   arrays and IO requests, and no static object has either kind. */
static int aihc_outside_is_pinned(const AihcValue *object) {
  AihcObjectKind kind = aihc_value_kind(object);
  if (kind == AIHC_OBJECT_IO_REQUEST) {
    return 1;
  }
  return kind == AIHC_OBJECT_BYTE_ARRAY &&
         ((const AihcByteArray *)object)->pinned != 0;
}

/* The number of the generation that holds an object after this collection.
   The nursery and the from-spaces do not qualify: the caller forwards first.
   A frame, a static object, and a null pointer are older than every
   generation. */
static unsigned aihc_generation_of(const AihcMachine *machine,
                                   const AihcValue *value) {
  if (value == NULL) {
    return AIHC_GENERATION_STATIC;
  }
  switch (aihc_region_kind(value)) {
  case AIHC_REGION_GEN1:
    return 1;
  case AIHC_REGION_GEN2:
    return 2;
  case AIHC_REGION_LARGE:
  case AIHC_REGION_PINNED:
    return aihc_pinned_generation(aihc_pinned_block_of(value));
  case AIHC_REGION_OUTSIDE:
    if (aihc_outside_is_pinned(value)) {
      return aihc_pinned_generation(aihc_pinned_block_of(value));
    }
    return AIHC_GENERATION_STATIC;
  case AIHC_REGION_STACK:
    return AIHC_GENERATION_STATIC;
  case AIHC_REGION_NURSERY:
  case AIHC_REGION_FROM1:
  case AIHC_REGION_FROM2:
    if (aihc_in_nursery(machine, value)) {
      return 0;
    }
    aihc_fail("collector asked the generation of an object that moves");
  default:
    aihc_fail("collector asked the generation of an invalid address");
  }
}

/* The blocks of the old generations. */

static AihcGeneration *aihc_generation(AihcMachine *machine,
                                       unsigned generation) {
  return &machine->generations[generation - 1];
}

static AihcRegionKind aihc_generation_kind(unsigned generation) {
  return generation == 1 ? AIHC_REGION_GEN1 : AIHC_REGION_GEN2;
}

static uint64_t aihc_generation_used(const AihcGeneration *generation) {
  uint64_t used = 0;
  for (const AihcHeapBlock *block = generation->first; block != NULL;
       block = block->link) {
    used += (uint64_t)(block->next - block->start);
  }
  return used;
}

/* Allocate bytes for a copied object in an old generation. */
static uint8_t *aihc_generation_allocate(AihcMachine *machine,
                                         unsigned generation, size_t bytes) {
  AihcGeneration *target = aihc_generation(machine, generation);
  AihcHeapBlock *block = target->last;
  if (block == NULL || bytes > (size_t)(block->limit - block->next)) {
    if (bytes > AIHC_HEAP_BLOCK_BYTES) {
      aihc_fail("object exceeds a heap block");
    }
    block = malloc(sizeof(*block));
    if (block == NULL) {
      aihc_fail("out of memory");
    }
    block->start = aihc_regions_acquire(AIHC_HEAP_BLOCK_REGIONS,
                                        aihc_generation_kind(generation));
    block->next = block->start;
    block->limit = block->start + AIHC_HEAP_BLOCK_BYTES;
    block->link = NULL;
    if (target->last == NULL) {
      target->first = block;
    } else {
      target->last->link = block;
    }
    target->last = block;
    target->bytes += AIHC_HEAP_BLOCK_BYTES;
  }
  uint8_t *object = block->next;
  block->next += bytes;
  return object;
}

static void aihc_blocks_set_kind(AihcHeapBlock *blocks, AihcRegionKind kind) {
  for (AihcHeapBlock *block = blocks; block != NULL; block = block->link) {
    aihc_regions_set_kind(block->start, kind);
  }
}

static void aihc_blocks_release(AihcHeapBlock *blocks) {
  while (blocks != NULL) {
    AihcHeapBlock *link = blocks->link;
    aihc_regions_release(blocks->start);
    free(blocks);
    blocks = link;
  }
}

/* The bytes the objects of the old generations and the pinned list take.
   The -M limit bounds this count, not the capacity of the blocks that hold
   it and not the nursery: a block is a whole run of regions, and a program
   with a limit far below one run still has to run. */
static uint64_t aihc_old_bytes(const AihcMachine *machine) {
  return aihc_generation_used(&machine->generations[0]) +
         aihc_generation_used(&machine->generations[1]) + machine->fixed_bytes;
}

static uint64_t aihc_occupied_bytes(const AihcMachine *machine) {
  return (uint64_t)(machine->heap_next - machine->heap_start) +
         aihc_old_bytes(machine);
}

/* Thread stacks. */

static AihcStackChunk *aihc_stack_chunk_of(const void *address) {
  return (AihcStackChunk *)((uintptr_t)address &
                            ~(uintptr_t)(AIHC_STACK_CHUNK_BYTES - 1));
}

static uint8_t *aihc_stack_chunk_frames(AihcStackChunk *chunk) {
  return (uint8_t *)chunk + AIHC_STACK_CHUNK_HEADER_BYTES;
}

_Static_assert(AIHC_REGION_BYTES % AIHC_STACK_CHUNK_BYTES == 0,
               "a stack region holds whole chunks");

static AihcStackChunk *aihc_stack_chunk_new(AihcMachine *machine,
                                            AihcStack *stack) {
  if (machine->spare_chunks == NULL) {
    /* Chunks come from stack regions. The chunks of a region stay with the
       machine: a released chunk goes to the spare list and is used again. */
    uint8_t *region = aihc_regions_acquire(1, AIHC_REGION_STACK);
    for (size_t offset = AIHC_REGION_BYTES; offset != 0;) {
      offset -= AIHC_STACK_CHUNK_BYTES;
      AihcStackChunk *spare = (AihcStackChunk *)(region + offset);
      spare->stack = NULL;
      spare->below = NULL;
      spare->above = machine->spare_chunks;
      machine->spare_chunks = spare;
      ++machine->spare_chunk_count;
    }
  }
  AihcStackChunk *chunk = machine->spare_chunks;
  machine->spare_chunks = chunk->above;
  --machine->spare_chunk_count;
  chunk->stack = stack;
  chunk->below = NULL;
  chunk->above = NULL;
  chunk->state = 0;
  return chunk;
}

static void aihc_stack_chunk_free(AihcMachine *machine, AihcStackChunk *chunk) {
  chunk->stack = NULL;
  chunk->below = NULL;
  chunk->above = machine->spare_chunks;
  machine->spare_chunks = chunk;
  ++machine->spare_chunk_count;
}

AihcStack *aihc_stack_new(AihcMachine *machine, AihcThread *thread) {
  AihcStack *stack = malloc(sizeof(*stack));
  if (stack == NULL) {
    aihc_fail("out of memory for the thread stack");
  }
  stack->thread = thread;
  stack->base = aihc_stack_chunk_new(machine, stack);
  stack->next = machine->stacks;
  machine->stacks = stack;
  return stack;
}

uint8_t *aihc_stack_base(const AihcStack *stack) {
  return aihc_stack_chunk_frames(stack->base);
}

AihcStack *aihc_stack_of(const void *frame) {
  return aihc_stack_chunk_of(frame)->stack;
}

void aihc_stack_release(AihcMachine *machine, AihcStack *stack) {
  AihcStack **link = &machine->stacks;
  while (*link != stack) {
    if (*link == NULL) {
      aihc_fail("released stack is not registered");
    }
    link = &(*link)->next;
  }
  *link = stack->next;
  AihcStackChunk *chunk = stack->base;
  while (chunk != NULL) {
    AihcStackChunk *above = chunk->above;
    aihc_stack_chunk_free(machine, chunk);
    chunk = above;
  }
  free(stack);
}

AihcValue *aihc_stack_grow(AihcMachine *machine, uint8_t *stack_next,
                           uint64_t words) {
  if (words > (AIHC_STACK_CHUNK_BYTES - AIHC_STACK_CHUNK_HEADER_BYTES) /
                  sizeof(AihcSlot)) {
    aihc_fail("continuation frame exceeds a stack chunk");
  }
  AihcStackChunk *current = aihc_stack_chunk_of(stack_next - 1);
  AihcStackChunk *next = current->above;
  if (next == NULL) {
    next = aihc_stack_chunk_new(machine, current->stack);
    next->below = current;
    current->above = next;
  }
  aihc_chunk_set_generation(next, 0);
  return (AihcValue *)aihc_stack_chunk_frames(next);
}

AihcValue *aihc_stack_push(AihcMachine *machine, uint64_t words) {
  uint8_t *frame = machine->stack_next;
  if (frame == NULL) {
    aihc_fail("stack push before the machine started");
  }
  uintptr_t last = (uintptr_t)frame + words * sizeof(AihcSlot) - 1;
  if ((((uintptr_t)frame - 1) ^ last) >= AIHC_STACK_CHUNK_BYTES) {
    frame = (uint8_t *)aihc_stack_grow(machine, frame, words);
  }
  machine->stack_next = frame + words * sizeof(AihcSlot);
  return (AihcValue *)frame;
}

void aihc_stack_resume_after(AihcMachine *machine, const AihcValue *frame) {
  machine->stack_next =
      (uint8_t *)frame + aihc_value_words(frame) * sizeof(AihcSlot);
  aihc_stack_enter_chunk(machine, frame);
}

void aihc_stack_enter_chunk(AihcMachine *machine, const AihcValue *frame) {
  (void)machine;
  aihc_chunk_set_generation(aihc_stack_chunk_of(frame), 0);
}

/* The remembered set. */

static int aihc_compare_addresses(const void *left, const void *right) {
  uintptr_t first = (uintptr_t)*(AihcValue *const *)left;
  uintptr_t second = (uintptr_t)*(AihcValue *const *)right;
  return first < second ? -1 : first > second;
}

/* Keep one entry for each object. A hot mutable object enters the set at
   every store, so the list is compacted when it is full, and it grows only
   when distinct objects fill it. */
static void aihc_remembered_compact(AihcMachine *machine) {
  if (machine->remembered_count == 0) {
    /* qsort takes no null list, and an empty list has nothing to sort. */
    return;
  }
  qsort(machine->remembered, machine->remembered_count,
        sizeof(*machine->remembered), aihc_compare_addresses);
  uint64_t kept = 0;
  for (uint64_t index = 0; index < machine->remembered_count; ++index) {
    if (kept == 0 ||
        machine->remembered[kept - 1] != machine->remembered[index]) {
      machine->remembered[kept++] = machine->remembered[index];
    }
  }
  machine->remembered_count = kept;
}

void aihc_remember(AihcMachine *machine, AihcValue *object) {
  if (machine->remembered_count == machine->remembered_capacity) {
    if (machine->remembered_capacity != 0) {
      aihc_remembered_compact(machine);
    }
    if (machine->remembered_count * 2 >= machine->remembered_capacity) {
      uint64_t capacity = machine->remembered_capacity == 0
                              ? 256
                              : machine->remembered_capacity * 2;
      if (capacity > SIZE_MAX / sizeof(*machine->remembered)) {
        aihc_fail("remembered set is too large");
      }
      AihcValue **grown =
          realloc(machine->remembered, capacity * sizeof(*machine->remembered));
      if (grown == NULL) {
        aihc_fail("out of memory");
      }
      machine->remembered = grown;
      machine->remembered_capacity = capacity;
    }
  }
  machine->remembered[machine->remembered_count++] = object;
}

void aihc_write_barrier(AihcMachine *machine, AihcValue *object) {
  if (object == NULL || aihc_in_nursery(machine, object)) {
    return;
  }
  aihc_remember(machine, object);
}

static void aihc_gc_keep(AihcGcContext *context, AihcValue *object) {
  if (context->kept_count == context->kept_capacity) {
    context->kept = aihc_worklist_grow(context->kept, &context->kept_capacity,
                                       sizeof(*context->kept));
  }
  context->kept[context->kept_count++] = object;
}

/* Copying. */

static int aihc_header_is_forward(AihcSlot header) {
  return (header & AIHC_HEADER_TAG_MASK) == AIHC_HEADER_WAITERS;
}

static AihcValue *aihc_header_forward(AihcSlot header) {
  return (AihcValue *)(uintptr_t)(header & ~(AihcSlot)AIHC_HEADER_TAG_MASK);
}

static AihcValue *aihc_copy(AihcGcContext *context, AihcValue *value,
                            unsigned generation) {
  uint64_t words = aihc_value_words(value);
  size_t bytes = sizeof(AihcSlot) * words;
  AihcValue *copy = (AihcValue *)aihc_generation_allocate(context->machine,
                                                          generation, bytes);
  memcpy(copy, value, bytes);
  value->header = (AihcSlot)(uintptr_t)copy | AIHC_HEADER_WAITERS;
  return copy;
}

static void aihc_mark_pinned(AihcGcContext *context, AihcValue *object,
                             unsigned target) {
  AihcPinnedBlock *block = aihc_pinned_block_of(object);
  unsigned generation = aihc_pinned_generation(block);
  if (generation > context->collected ||
      (block->bytes & AIHC_PINNED_MARK) != 0) {
    return;
  }
  unsigned promoted = generation + 1;
  if (promoted < target) {
    promoted = target;
  }
  if (promoted > 2) {
    promoted = 2;
  }
  block->bytes |= AIHC_PINNED_MARK;
  aihc_pinned_set_generation(block, promoted);
  aihc_value_worklist_push(&aihc_pinned_worklist, object);
}

/* Copy or mark one object and return where it lives now. target is the
   lowest generation a copied object may land in, or zero when the referrer
   makes no demand. Heap indirections in a from-space are followed and not
   copied, so no indirection chain grows across collections. */
static AihcValue *aihc_evacuate(AihcGcContext *context, AihcValue *value,
                                unsigned target) {
  AihcMachine *machine = context->machine;
  for (;;) {
    if (value == NULL) {
      return NULL;
    }
    unsigned source;
    if (aihc_in_nursery(machine, value)) {
      source = 0;
    } else {
      switch (aihc_region_kind(value)) {
      case AIHC_REGION_FROM1:
        source = 1;
        break;
      case AIHC_REGION_FROM2:
        source = 2;
        break;
      case AIHC_REGION_GEN1:
      case AIHC_REGION_GEN2:
        return value;
      case AIHC_REGION_LARGE:
      case AIHC_REGION_PINNED:
        aihc_mark_pinned(context, value, target);
        return value;
      case AIHC_REGION_STACK:
        /* A frame of a young chunk is scanned from the worklist. A frame of
           an older chunk points only at objects of that generation or
           above, which this collection does not move. */
        if (aihc_chunk_generation(aihc_stack_chunk_of(value)) <=
            context->collected) {
          aihc_mark_static(value);
        }
        return value;
      case AIHC_REGION_OUTSIDE:
        if (aihc_outside_is_pinned(value)) {
          aihc_mark_pinned(context, value, target);
        } else if (context->collected == 2) {
          aihc_mark_static(value);
        }
        return value;
      default:
        aihc_fail("collector reached an invalid address");
      }
    }
    AihcSlot header = value->header;
    if (aihc_header_is_forward(header)) {
      return aihc_header_forward(header);
    }
    if (aihc_value_kind(value) == AIHC_OBJECT_INDIRECTION) {
      value = (AihcValue *)(uintptr_t)value->fields[0];
      continue;
    }
    unsigned generation = source + 1;
    if (generation < target) {
      generation = target;
    }
    if (generation > 2) {
      generation = 2;
    }
    return aihc_copy(context, value, generation);
  }
}

static AihcSlot aihc_evacuate_root(AihcSlot root, void *opaque_context) {
  return (AihcSlot)(uintptr_t)aihc_evacuate(opaque_context,
                                            (AihcValue *)(uintptr_t)root, 0);
}

static AihcSlot aihc_scan_slot(AihcSlot slot, void *opaque_context) {
  AihcScanContext *scan = opaque_context;
  AihcValue *value =
      aihc_evacuate(scan->gc, (AihcValue *)(uintptr_t)slot, scan->target);
  unsigned generation = aihc_generation_of(scan->gc->machine, value);
  if (generation < scan->youngest) {
    scan->youngest = generation;
  }
  return (AihcSlot)(uintptr_t)value;
}

/* Record a scanned frame on its chunk. The first frame of a collection
   starts the youngest-referent record of the chunk, and every frame lowers
   it. The chunk takes the result when the collection ends. */
static void aihc_chunk_note_frame(AihcGcContext *context, AihcStackChunk *chunk,
                                  unsigned youngest) {
  uint64_t stamp = context->machine->gc_count;
  if ((chunk->state >> AIHC_CHUNK_STAMP_SHIFT) != stamp) {
    chunk->state =
        (stamp << AIHC_CHUNK_STAMP_SHIFT) |
        ((uint64_t)AIHC_GENERATION_STATIC << AIHC_CHUNK_YOUNGEST_SHIFT) |
        aihc_chunk_generation(chunk);
    if (context->touched_count == context->touched_capacity) {
      context->touched =
          aihc_worklist_grow(context->touched, &context->touched_capacity,
                             sizeof(*context->touched));
    }
    context->touched[context->touched_count++] = chunk;
  }
  unsigned recorded = (unsigned)((chunk->state & AIHC_CHUNK_YOUNGEST_MASK) >>
                                 AIHC_CHUNK_YOUNGEST_SHIFT);
  if (youngest < recorded) {
    chunk->state = (chunk->state & ~AIHC_CHUNK_YOUNGEST_MASK) |
                   ((uint64_t)youngest << AIHC_CHUNK_YOUNGEST_SHIFT);
  }
}

/* Give each chunk with a scanned frame its new generation: one above the
   collected generation, or the youngest generation its frames refer to when
   that is lower. The chunk of the running stack pointer stays young, because
   compiled code pushes into it without a runtime call. */
static void aihc_age_chunks(AihcGcContext *context) {
  AihcMachine *machine = context->machine;
  AihcStackChunk *running = machine->stack_next == NULL
                                ? NULL
                                : aihc_stack_chunk_of(machine->stack_next - 1);
  for (size_t index = 0; index < context->touched_count; ++index) {
    AihcStackChunk *chunk = context->touched[index];
    unsigned youngest = (unsigned)((chunk->state & AIHC_CHUNK_YOUNGEST_MASK) >>
                                   AIHC_CHUNK_YOUNGEST_SHIFT);
    unsigned generation = context->collected + 1;
    if (youngest < generation) {
      generation = youngest;
    }
    if (generation > 2) {
      generation = 2;
    }
    if (chunk == running) {
      generation = 0;
    }
    aihc_chunk_set_generation(chunk, generation);
  }
  free(context->touched);
}

/* Scan one object wherever it lives. generation is where the object lives
   now, or AIHC_GENERATION_STATIC for a frame or a static object. A referent
   lands in that generation at least. When a referent still ends younger,
   because an earlier reference copied it there, the object goes back to the
   remembered set. In a full collection the info table of the object also
   names the static objects its code reaches. */
static void aihc_scan_object(AihcGcContext *context, AihcValue *object,
                             unsigned generation) {
  AihcScanContext scan = {
      .gc = context,
      .target = generation == AIHC_GENERATION_STATIC ? 2 : generation,
      .youngest = AIHC_GENERATION_STATIC,
  };
  const AihcInfo *info = aihc_value_info_table(object);
  AihcObjectKind kind = info->object_kind;
  uint64_t count = info->field_count;
  if (!aihc_visit_runtime_object(object, aihc_scan_slot, &scan)) {
    if (context->collected == 2) {
      aihc_walk_srt(info->srt);
    }
    if (kind == AIHC_OBJECT_INDIRECTION) {
      /* Only a static object reaches this branch: an evaluated CAF keeps its
         indirection because it cannot move. */
      object->fields[0] = aihc_scan_slot(object->fields[0], &scan);
    } else if (kind == AIHC_OBJECT_ARRAY) {
      uint64_t length = aihc_array_length(object);
      AihcSlot *elements = aihc_array_elements(object);
      for (uint64_t index = 0; index < length; ++index) {
        elements[index] = aihc_scan_slot(elements[index], &scan);
      }
    } else if (kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
      /* Field zero holds the applied count, and the slots filled so far are
         a prefix of the saturated constructor's, so the shared bitmap
         answers for them. */
      uint64_t applied = aihc_partial_applied(object);
      AihcSlot *fields = aihc_partial_fields(object);
      for (uint64_t index = 0; index < applied; ++index) {
        if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
          fields[index] = aihc_scan_slot(fields[index], &scan);
        }
      }
    } else if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
               kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_BLACKHOLE) {
      for (uint64_t index = 0; index < count; ++index) {
        if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
          object->fields[index] = aihc_scan_slot(object->fields[index], &scan);
        }
      }
    } else {
      aihc_fail("collector encountered an invalid object kind");
    }
  }
  /* A static object is traced at every full collection, so it needs an
     entry only for a referent in gen1, which a full collection does not
     reach through it: the entry keeps the referent alive and updated until
     then. A frame needs no entry: its chunk takes the youngest generation
     its frames refer to, and a collection of that generation scans it. */
  unsigned demand = generation == AIHC_GENERATION_STATIC ? 2 : generation;
  if (aihc_region_kind(object) == AIHC_REGION_STACK) {
    aihc_chunk_note_frame(context, aihc_stack_chunk_of(object), scan.youngest);
  } else if (scan.youngest < demand) {
    aihc_gc_keep(context, object);
  }
}

/* Whether a generation has copied objects its cursor has not scanned. */
static int aihc_generation_scan_pending(AihcGeneration *generation) {
  if (generation->scan_block == NULL) {
    if (generation->first == NULL) {
      return 0;
    }
    generation->scan_block = generation->first;
    generation->scan = generation->first->start;
  }
  while (generation->scan == generation->scan_block->next) {
    if (generation->scan_block->link == NULL) {
      return 0;
    }
    generation->scan_block = generation->scan_block->link;
    generation->scan = generation->scan_block->start;
  }
  return 1;
}

static void aihc_generation_scan_one(AihcGcContext *context,
                                     unsigned generation) {
  AihcGeneration *target = aihc_generation(context->machine, generation);
  AihcValue *object = (AihcValue *)target->scan;
  aihc_scan_object(context, object, generation);
  target->scan += sizeof(AihcSlot) * aihc_value_words(object);
}

/* Copying, marking, and table walking all feed one another, so run every
   worklist to quiescence. */
static void aihc_trace(AihcGcContext *context) {
  AihcMachine *machine = context->machine;
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
      AihcValue *object =
          aihc_static_worklist.items[--aihc_static_worklist.count];
      aihc_scan_object(context, object, AIHC_GENERATION_STATIC);
      continue;
    }
    if (aihc_pinned_worklist.count != 0) {
      AihcValue *object =
          aihc_pinned_worklist.items[--aihc_pinned_worklist.count];
      aihc_scan_object(context, object,
                       aihc_pinned_generation(aihc_pinned_block_of(object)));
      continue;
    }
    if (aihc_generation_scan_pending(aihc_generation(machine, 2))) {
      aihc_generation_scan_one(context, 2);
      continue;
    }
    if (aihc_generation_scan_pending(aihc_generation(machine, 1))) {
      aihc_generation_scan_one(context, 1);
      continue;
    }
    return;
  }
}

/* Scan the remembered set. An entry in a from-space is dropped: the object
   is copied and scanned if it is live. Another entry is scanned with its own
   generation as the floor of its referents. */
static void aihc_scan_remembered(AihcGcContext *context) {
  AihcMachine *machine = context->machine;
  /* A hot object has one entry for each store since the last compaction,
     so the set is compacted before each entry costs a scan. */
  aihc_remembered_compact(machine);
  AihcValue **entries = machine->remembered;
  uint64_t count = machine->remembered_count;
  machine->remembered = NULL;
  machine->remembered_count = 0;
  machine->remembered_capacity = 0;
  for (uint64_t index = 0; index < count; ++index) {
    AihcValue *object = entries[index];
    if (aihc_in_nursery(machine, object)) {
      continue;
    }
    unsigned generation;
    switch (aihc_region_kind(object)) {
    case AIHC_REGION_FROM1:
    case AIHC_REGION_FROM2:
      continue;
    case AIHC_REGION_GEN1:
      generation = 1;
      break;
    case AIHC_REGION_GEN2:
      generation = 2;
      break;
    case AIHC_REGION_LARGE:
    case AIHC_REGION_PINNED:
      generation = aihc_pinned_generation(aihc_pinned_block_of(object));
      break;
    case AIHC_REGION_OUTSIDE:
      generation = aihc_outside_is_pinned(object)
                       ? aihc_pinned_generation(aihc_pinned_block_of(object))
                       : AIHC_GENERATION_STATIC;
      break;
    default:
      aihc_fail("remembered set holds an invalid address");
    }
    if (generation <= context->collected) {
      /* A young pinned block is marked and scanned when something reaches
         it. */
      continue;
    }
    if (generation == AIHC_GENERATION_STATIC && context->collected == 2) {
      /* A full collection traces the static objects through the static
         reference tables alone, so an evaluated CAF that no live code
         reaches gives its value up. */
      continue;
    }
    aihc_scan_object(context, object, generation);
  }
  free(entries);
}

/* Return the relocated address only if tracing retained the object. */
static AihcValue *aihc_live_value(AihcGcContext *context, AihcValue *value) {
  AihcMachine *machine = context->machine;
  for (;;) {
    if (value == NULL) {
      return NULL;
    }
    if (!aihc_in_nursery(machine, value)) {
      switch (aihc_region_kind(value)) {
      case AIHC_REGION_FROM1:
      case AIHC_REGION_FROM2:
        /* A from-space object: forwarded, followed, or dead, as below. */
        break;
      case AIHC_REGION_GEN1:
      case AIHC_REGION_GEN2:
      case AIHC_REGION_STACK:
        return value;
      case AIHC_REGION_LARGE:
      case AIHC_REGION_PINNED: {
        const AihcPinnedBlock *block = aihc_pinned_block_of(value);
        return aihc_pinned_generation(block) > context->collected ||
                       (block->bytes & AIHC_PINNED_MARK) != 0
                   ? value
                   : NULL;
      }
      case AIHC_REGION_OUTSIDE:
        if (aihc_outside_is_pinned(value)) {
          const AihcPinnedBlock *block = aihc_pinned_block_of(value);
          return aihc_pinned_generation(block) > context->collected ||
                         (block->bytes & AIHC_PINNED_MARK) != 0
                     ? value
                     : NULL;
        }
        return context->collected < 2 ||
                       aihc_address_set_contains(&aihc_marked_statics, value)
                   ? value
                   : NULL;
      default:
        aihc_fail("collector asked the liveness of an invalid address");
      }
    }
    AihcSlot header = value->header;
    if (aihc_header_is_forward(header)) {
      return aihc_header_forward(header);
    }
    if (aihc_value_kind(value) != AIHC_OBJECT_INDIRECTION) {
      return NULL;
    }
    value = (AihcValue *)(uintptr_t)value->fields[0];
  }
}

/* Release the stacks of the threads this collection did not retain, and the
   chunks above the one after the running chunk. Frames above the stack
   pointer are dead, and one spare chunk stops a loop at a chunk boundary
   from allocating a chunk on every push. */
static void aihc_sweep_stacks(AihcGcContext *context) {
  AihcMachine *machine = context->machine;
  AihcStack *stack = machine->stacks;
  while (stack != NULL) {
    AihcStack *next = stack->next;
    AihcThread *thread =
        (AihcThread *)aihc_live_value(context, (AihcValue *)stack->thread);
    if (thread == NULL) {
      aihc_stack_release(machine, stack);
    } else {
      stack->thread = thread;
    }
    stack = next;
  }
  if (machine->stack_next == NULL) {
    return;
  }
  AihcStackChunk *spare = aihc_stack_chunk_of(machine->stack_next - 1)->above;
  if (spare == NULL) {
    return;
  }
  AihcStackChunk *chunk = spare->above;
  spare->above = NULL;
  while (chunk != NULL) {
    AihcStackChunk *above = chunk->above;
    aihc_stack_chunk_free(machine, chunk);
    chunk = above;
  }
}

/* The target of a chain of heap indirections. An indirection of an older
   generation outlives its thunk until a collection copies that generation,
   so a weak referent follows it here, as the copy follows the indirections
   of the generations it copies. A name made on a thunk thus names the value
   after the next collection, as it did under the semispace collector. */
static AihcValue *aihc_follow_heap_indirections(AihcValue *value) {
  while (value != NULL && aihc_value_kind(value) == AIHC_OBJECT_INDIRECTION &&
         aihc_region_kind(value) != AIHC_REGION_OUTSIDE) {
    value = (AihcValue *)(uintptr_t)value->fields[0];
  }
  return value;
}

/* Rebuild the weak lookup list after strong tracing. Do not retain names
   through this list or retain referents through their names. */
static void aihc_update_stable_names(AihcGcContext *context) {
  AihcStableName *old = context->machine->stable_names;
  AihcStableName **tail = &context->machine->stable_names;
  *tail = NULL;
  while (old != NULL) {
    AihcStableName *next = old->next;
    AihcStableName *live =
        (AihcStableName *)aihc_live_value(context, (AihcValue *)old);
    if (live != NULL) {
      live->value =
          aihc_follow_heap_indirections(aihc_live_value(context, old->value));
      live->next = NULL;
      if (live->value != NULL) {
        *tail = live;
        tail = &live->next;
      }
    }
    old = next;
  }
}

void aihc_pinned_block_release(AihcPinnedBlock *block) {
  if (aihc_region_kind(block) == AIHC_REGION_OUTSIDE) {
    free(block);
  } else {
    aihc_regions_release(block);
  }
}

/* Free the pinned blocks of the collected generations that nothing reached,
   and clear the marks of the others. */
static void aihc_sweep_pinned(AihcGcContext *context) {
  AihcMachine *machine = context->machine;
  AihcPinnedBlock **link = &machine->pinned_blocks;
  while (*link != NULL) {
    AihcPinnedBlock *block = *link;
    if ((block->bytes & AIHC_PINNED_MARK) != 0) {
      block->bytes &= ~AIHC_PINNED_MARK;
      link = &block->next;
    } else if (aihc_pinned_generation(block) <= context->collected) {
      *link = block->next;
      machine->fixed_bytes -= aihc_pinned_bytes(block);
      aihc_pinned_block_release(block);
    } else {
      link = &block->next;
    }
  }
}

void aihc_heap_account(AihcMachine *machine) {
  size_t taken = (size_t)(machine->heap_next - machine->heap_alloc_base);
  if ((uint64_t)taken > UINT64_MAX - machine->heap_allocated_bytes) {
    aihc_fail("allocated byte counter overflow");
  }
  machine->heap_allocated_bytes += (uint64_t)taken;
  machine->heap_alloc_base = machine->heap_next;
}

void aihc_gc_record_peak(AihcMachine *machine) {
  uint64_t used = aihc_occupied_bytes(machine);
  if (used > machine->heap_peak_bytes) {
    machine->heap_peak_bytes = used;
  }
}

/* The oldest generation the next collection copies. Gen1 goes when it is
   above its maximum, gen2 when it has grown past its limit, and everything
   when the -M limit is near. */
static unsigned aihc_choose_generation(const AihcMachine *machine,
                                       uint64_t required_bytes) {
  unsigned generation = 0;
  if (machine->generations[0].bytes >= machine->gen1_max_bytes) {
    generation = 1;
  }
  if (machine->generations[1].bytes >= machine->gen2_limit_bytes) {
    generation = 2;
  }
  if (machine->heap_limit_enabled &&
      aihc_old_bytes(machine) + required_bytes > machine->heap_max_bytes) {
    generation = 2;
  }
  return generation;
}

static void aihc_collect(AihcMachine *machine, unsigned collected,
                         uint64_t root_count, AihcSlot *roots,
                         const AihcSrt *srt) {
  uint64_t started_ns = aihc_host_monotonic_ns();
  aihc_gc_record_peak(machine);
  /* Everything the mutator took from the nursery it is about to leave. */
  aihc_heap_account(machine);
  if (machine->gc_count == UINT64_MAX) {
    aihc_fail("collection counter overflow");
  }
  ++machine->gc_count;
  if (collected == 0) {
    ++machine->gc_minor_count;
  } else if (collected == 1) {
    ++machine->gc_gen1_count;
  } else {
    ++machine->gc_full_count;
  }
  AihcGcContext context = {.machine = machine, .collected = collected};

  /* Detach the blocks this collection copies away, so the generations fill
     fresh blocks. */
  AihcHeapBlock *from1 = NULL;
  AihcHeapBlock *from2 = NULL;
  for (unsigned generation = 1; generation <= 2; ++generation) {
    AihcGeneration *target = aihc_generation(machine, generation);
    if (generation <= collected) {
      aihc_blocks_set_kind(target->first, generation == 1 ? AIHC_REGION_FROM1
                                                          : AIHC_REGION_FROM2);
      if (generation == 1) {
        from1 = target->first;
      } else {
        from2 = target->first;
      }
      target->first = NULL;
      target->last = NULL;
      target->bytes = 0;
    }
    target->scan_block = target->last;
    target->scan = target->last == NULL ? NULL : target->last->next;
  }

  aihc_address_set_clear(&aihc_marked_statics);
  aihc_static_worklist.count = 0;
  aihc_pinned_worklist.count = 0;
  aihc_srt_worklist.count = 0;
  if (collected == 2) {
    /* The table of the code that requested the collection, or NULL when
       that code reaches no static object of its own. */
    aihc_walk_srt(srt);
    for (AihcForeignFrame *frame = machine->foreign_frames; frame != NULL;
         frame = frame->previous) {
      aihc_walk_srt(frame->srt);
    }
  }
  aihc_visit_roots(machine, root_count, roots, aihc_evacuate_root, &context);
  aihc_scan_remembered(&context);
  aihc_trace(&context);
  aihc_update_stable_names(&context);
  aihc_sweep_stacks(&context);
  aihc_sweep_pinned(&context);
  aihc_age_chunks(&context);
  aihc_clear_srt_stamps();

  aihc_blocks_release(from1);
  aihc_blocks_release(from2);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->nursery_bytes;
  machine->heap_alloc_base = machine->heap_next;
  machine->fixed_since_gc = 0;
  for (size_t index = 0; index < context.kept_count; ++index) {
    aihc_remember(machine, context.kept[index]);
  }
  free(context.kept);
  if (collected == 2) {
    uint64_t limit = machine->generations[1].bytes;
    if (limit > UINT64_MAX / machine->gen2_factor) {
      limit = UINT64_MAX;
    } else {
      limit *= machine->gen2_factor;
    }
    machine->gen2_limit_bytes =
        limit < AIHC_GEN2_MINIMUM_BYTES ? AIHC_GEN2_MINIMUM_BYTES : limit;
  }
  machine->heap_live_bytes = aihc_occupied_bytes(machine);
  uint64_t pause_ns = aihc_host_monotonic_ns() - started_ns;
  machine->gc_time_ns += pause_ns;
  if (pause_ns > machine->gc_max_pause_ns) {
    machine->gc_max_pause_ns = pause_ns;
  }
}

static _Noreturn void aihc_heap_exhausted(void) {
  aihc_fail("heap limit exceeded");
}

/* Collect for a reservation, escalating to a full collection when the -M
   limit demands it, and stop the program when even that does not fit. */
static void aihc_collect_for(AihcMachine *machine, size_t required_bytes,
                             uint64_t root_count, AihcSlot *roots,
                             const AihcSrt *srt) {
  unsigned collected = aihc_choose_generation(machine, required_bytes);
  aihc_collect(machine, collected, root_count, roots, srt);
  if (machine->heap_limit_enabled &&
      aihc_old_bytes(machine) + required_bytes > machine->heap_max_bytes) {
    if (collected < 2) {
      aihc_collect(machine, 2, root_count, roots, srt);
    }
    if (aihc_old_bytes(machine) + required_bytes > machine->heap_max_bytes) {
      aihc_heap_exhausted();
    }
  }
}

void aihc_gc_collect_generation(AihcMachine *machine, unsigned generation,
                                uint64_t root_count, AihcSlot *roots,
                                const AihcSrt *srt) {
  aihc_collect(machine, generation > 2 ? 2 : generation, root_count, roots,
               srt);
}

static void aihc_nursery_acquire(AihcMachine *machine, size_t bytes) {
  machine->nursery_bytes = bytes;
  machine->heap_start =
      aihc_regions_acquire(aihc_regions_for_bytes(bytes), AIHC_REGION_NURSERY);
  machine->heap_next = machine->heap_start;
  machine->heap_alloc_base = machine->heap_start;
  machine->heap_limit = machine->heap_start + bytes;
  aihc_nursery_start = machine->heap_start;
  aihc_nursery_bytes = bytes;
}

void aihc_nursery_replace(AihcMachine *machine, size_t bytes) {
  if (bytes == 0) {
    aihc_fail("the nursery needs at least one byte");
  }
  if (machine->heap_start != NULL) {
    aihc_regions_release(machine->heap_start);
  }
  aihc_nursery_acquire(machine, bytes);
}

void aihc_heap_reset(AihcMachine *machine, size_t nursery_bytes) {
  for (unsigned generation = 1; generation <= 2; ++generation) {
    AihcGeneration *target = aihc_generation(machine, generation);
    aihc_blocks_release(target->first);
    target->first = NULL;
    target->last = NULL;
    target->bytes = 0;
    target->scan_block = NULL;
    target->scan = NULL;
  }
  while (machine->pinned_blocks != NULL) {
    AihcPinnedBlock *block = machine->pinned_blocks;
    machine->pinned_blocks = block->next;
    aihc_pinned_block_release(block);
  }
  machine->fixed_bytes = 0;
  machine->fixed_since_gc = 0;
  free(machine->remembered);
  machine->remembered = NULL;
  machine->remembered_count = 0;
  machine->remembered_capacity = 0;
  aihc_nursery_replace(machine, nursery_bytes);
  machine->gen2_limit_bytes = AIHC_GEN2_MINIMUM_BYTES;
}

void aihc_gc_init(AihcMachine *machine) {
  uint64_t nursery = aihc_rts_nursery_bytes();
  aihc_nursery_acquire(machine, nursery == 0 ? AIHC_NURSERY_BYTES : nursery);
  machine->gen1_max_bytes = AIHC_GEN1_MAX_BYTES;
  machine->gen2_limit_bytes = AIHC_GEN2_MINIMUM_BYTES;
  machine->gen2_factor = AIHC_GEN2_FACTOR;
}

void aihc_gc_apply_options(AihcMachine *machine) {
  uint64_t nursery = aihc_rts_nursery_bytes();
  if (nursery != 0 && nursery != machine->nursery_bytes &&
      machine->heap_next == machine->heap_start) {
    aihc_nursery_replace(machine, nursery);
  }
  uint64_t gen1_max = aihc_rts_gen1_max_bytes();
  if (gen1_max != 0) {
    machine->gen1_max_bytes = gen1_max;
  }
  uint64_t factor = aihc_rts_gen2_factor();
  if (factor != 0) {
    machine->gen2_factor = factor;
  }
}

void aihc_gc_apply_limit(AihcMachine *machine) {
  if (machine->heap_limit_enabled &&
      machine->nursery_bytes > machine->heap_max_bytes &&
      machine->heap_next == machine->heap_start) {
    aihc_nursery_replace(machine, machine->heap_max_bytes == 0
                                      ? 1
                                      : (size_t)machine->heap_max_bytes);
  }
}

/* The bytes of a reservation, which a request the address space or the heap
   limit cannot hold does not return from. */
static size_t aihc_reservation_bytes(const AihcMachine *machine,
                                     uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_limit_enabled && bytes > machine->heap_max_bytes) {
    aihc_heap_exhausted();
  }
  return bytes;
}

/* Whether a reservation names an object that gets regions of its own and
   therefore takes no room in the nursery. */
static int aihc_reservation_is_large(size_t bytes) {
  return bytes >= AIHC_LARGE_OBJECT_BYTES - sizeof(AihcPinnedBlock);
}

/* Give a large request its collection when the fixed allocations since the
   last collection have reached the size of a nursery, or when the limit is
   near. */
static void aihc_ensure_large(AihcMachine *machine, size_t bytes,
                              uint64_t root_count, AihcSlot *roots,
                              const AihcSrt *srt) {
  int limit_near = machine->heap_limit_enabled &&
                   aihc_old_bytes(machine) + bytes > machine->heap_max_bytes;
  if (machine->fixed_since_gc + bytes > machine->nursery_bytes || limit_near) {
    aihc_collect_for(machine, bytes, root_count, roots, srt);
  }
}

/* Collect for a caller that has already found the words do not fit. Compiled
   code compares the bump pointer against the end of the space itself and only
   calls the runtime on the slow path. A request of a large object reaches
   this function whenever the nursery is smaller than the object. */
void aihc_gc_collect(AihcMachine *machine, uint64_t words, uint64_t root_count,
                     AihcSlot *roots, const AihcSrt *srt) {
  size_t bytes = aihc_reservation_bytes(machine, words);
  if (aihc_reservation_is_large(bytes)) {
    aihc_ensure_large(machine, bytes, root_count, roots, srt);
    return;
  }
  aihc_collect_for(machine, bytes, root_count, roots, srt);
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    /* The nursery is empty now, so a reservation it cannot hold grows it.
       Only a nursery far below the default, as the tests use, reaches this
       branch: a reservation below the large object bound fits the default. */
    aihc_nursery_replace(machine, bytes);
  }
}

/* Reserve for a caller that has not compared anything: the machine start-up
   path, the runtime units, and the C programs of the tests. */
void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots, const AihcSrt *srt) {
  size_t bytes = aihc_reservation_bytes(machine, words);
  if (aihc_reservation_is_large(bytes)) {
    aihc_ensure_large(machine, bytes, root_count, roots, srt);
    return;
  }
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_gc_collect(machine, words, root_count, roots, srt);
  }
}

/* Put a block on the pinned list and charge it to the budget. The charge
   also shortens the nursery, so fixed allocation brings the next collection
   closer as nursery allocation does. */
static AihcValue *aihc_pinned_block_adopt(AihcMachine *machine,
                                          AihcPinnedBlock *block,
                                          size_t charge_bytes) {
  /* The cast keeps the compare meaningful on a 32-bit host. */
  if ((uint64_t)charge_bytes > AIHC_PINNED_BYTES_MASK) {
    aihc_fail("pinned allocation is too large");
  }
  aihc_heap_account(machine);
  if (charge_bytes > UINT64_MAX - machine->heap_allocated_bytes) {
    aihc_fail("allocated byte counter overflow");
  }
  block->bytes = charge_bytes;
  block->next = machine->pinned_blocks;
  machine->pinned_blocks = block;
  machine->fixed_bytes += charge_bytes;
  machine->fixed_since_gc += charge_bytes;
  size_t room = (size_t)(machine->heap_limit - machine->heap_next);
  machine->heap_limit -= charge_bytes < room ? charge_bytes : room;
  machine->heap_allocated_bytes += charge_bytes;
  return (AihcValue *)block->object;
}

/* A large object or a large pinned block gets regions of its own. Its
   pinned block header puts it on the pinned list, so the sweep, the owner
   lookup of the IO layer, and the budget treat it like a pinned block. */
static AihcPinnedBlock *aihc_fixed_block_new(size_t bytes,
                                             AihcRegionKind kind) {
  AihcPinnedBlock *block =
      aihc_regions_acquire(aihc_regions_for_bytes(bytes), kind);
  /* The content of a run is unspecified, so the header is set here. */
  block->next = NULL;
  block->bytes = 0;
  return block;
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap allocation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (aihc_reservation_is_large(bytes)) {
    AihcPinnedBlock *block = aihc_fixed_block_new(
        sizeof(AihcPinnedBlock) + bytes, AIHC_REGION_LARGE);
#ifdef DEBUG
    memset(block->object, 0, bytes);
#endif
    return aihc_pinned_block_adopt(machine, block, bytes);
  }
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  AihcValue *value = (AihcValue *)machine->heap_next;
  machine->heap_next += bytes;
#ifdef DEBUG
  memset(value, 0, bytes);
#endif
  return value;
}

/* Consume a prior reservation. The allocation list is not a root. */
AihcValue *aihc_gc_allocate_pinned(AihcMachine *machine, uint64_t words) {
  size_t bytes = sizeof(AihcPinnedBlock) + words * sizeof(AihcSlot);
  if (words > (SIZE_MAX - sizeof(AihcPinnedBlock)) / sizeof(AihcSlot) ||
      bytes < sizeof(AihcPinnedBlock)) {
    aihc_fail("pinned allocation is too large");
  }
  AihcPinnedBlock *block;
  if (bytes >= AIHC_LARGE_OBJECT_BYTES) {
    block = aihc_fixed_block_new(bytes, AIHC_REGION_PINNED);
    /* A pinned block reads as zero, as the C allocation below does. */
    memset(block->object, 0, bytes - sizeof(AihcPinnedBlock));
  } else {
    if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
      aihc_fail("unchecked pinned allocation exceeded reserved heap");
    }
    block = calloc(1, bytes);
    if (block == NULL) {
      aihc_fail("out of memory");
    }
  }
  return aihc_pinned_block_adopt(machine, block, bytes);
}

void aihc_roots_enter(AihcMachine *machine, AihcRootFrame *frame,
                      uint64_t count, AihcSlot *roots) {
  *frame = (AihcRootFrame){
      .next = machine->root_frames, .roots = roots, .count = count};
  machine->root_frames = frame;
}

void aihc_roots_leave(AihcMachine *machine, AihcRootFrame *frame) {
  AihcRootFrame **link = &machine->root_frames;
  while (*link != frame) {
    if (*link == NULL) {
      aihc_fail("host scope is not active");
    }
    link = &(*link)->next;
  }
  *link = frame->next;
}

void aihc_visit_host_roots(AihcMachine *machine, AihcRootVisitor visitor,
                           void *context) {
  for (AihcRootFrame *frame = machine->root_frames; frame;
       frame = frame->next) {
    for (uint64_t index = 0; index < frame->count; ++index) {
      frame->roots[index] = visitor(frame->roots[index], context);
    }
    for (AihcHostBuffer *buffer = frame->buffers; buffer;
         buffer = buffer->next) {
      (void)visitor((AihcSlot)(uintptr_t)buffer, context);
    }
  }
}

void *aihc_host_byte_array(AihcMachine *machine, AihcRootFrame *frame,
                           uint64_t bytes) {
  AihcRootFrame *active = machine->root_frames;
  while (active != NULL && active != frame) {
    active = active->next;
  }
  if (active == NULL) {
    aihc_fail("host allocation requires an active root scope");
  }
  size_t overhead = sizeof(AihcHostBuffer) + sizeof(AihcPinnedBlock);
  if (bytes > SIZE_MAX - overhead - sizeof(AihcSlot)) {
    aihc_fail("host buffer is too large");
  }
  uint64_t occupied = bytes == 0 ? 1 : bytes;
  uint64_t words = (sizeof(AihcHostBuffer) + occupied + sizeof(AihcSlot) - 1) /
                   sizeof(AihcSlot);
  aihc_gc_ensure(machine, words + sizeof(AihcPinnedBlock) / sizeof(AihcSlot), 0,
                 NULL, NULL);
  AihcHostBuffer *buffer =
      (AihcHostBuffer *)aihc_gc_allocate_pinned(machine, words);
  buffer->array.header = (AihcSlot)(uintptr_t)&aihc_buffer_info;
  buffer->array.size = bytes;
  buffer->array.contents = buffer->contents;
  buffer->array.pinned = 1;
  buffer->array.alignment = _Alignof(max_align_t);
  buffer->array.words = words;
  buffer->next = frame->buffers;
  frame->buffers = buffer;
  return buffer;
}
