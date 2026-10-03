#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The generational collector. See docs/gc-design.md.

   The heap has three generations. The nursery, generation zero, is one run
   of regions that compiled code fills with a bump pointer between heap_start
   and heap_limit. Gen1 is a list of blocks that only the collector fills.
   Gen2 is a set of segments that never move an object. A collection of the
   generations up to g copies every live object of the nursery and gen1 that
   it covers one generation up, except that an object a scanned object of
   generation k refers to goes to generation k at least. Thus an old object
   points only at old objects after one scan, and the barrier entry for it
   can be dropped. A full collection marks the live gen2 objects in place.

   The gen1 blocks a collection copies away are relabeled FROM1 in the
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
/* The largest -F factor, and the bits it takes. */
#define AIHC_GEN2_FACTOR_BITS 10
#define AIHC_GEN2_FACTOR_MAX (UINT64_C(1) << AIHC_GEN2_FACTOR_BITS)

/* Gen2 segments. A segment is one block of regions that holds slots of one
   size class, with one bit for each slot. A set bit is an occupied slot. A
   full collection takes the bits of a segment as cleared until it marks an
   object in the segment, and sets the bit of each object it reaches. An
   object copied into gen2 sets its bit when it is allocated. Thus the
   bitmap is the free map of the allocator between full collections.

   The size classes are the word counts two to eight, and then four classes
   in each doubling: a class holds at most a quarter more than the object
   needs. The largest class holds every object below the large object
   bound. */
#define AIHC_SEGMENT_BYTES AIHC_HEAP_BLOCK_BYTES
#define AIHC_SEGMENT_REGIONS AIHC_HEAP_BLOCK_REGIONS
#define AIHC_SLOT_MIN_WORDS 2
#define AIHC_SLOT_MAX_WORDS 4096
#define AIHC_SEGMENT_MAX_SLOTS                                                 \
  (AIHC_SEGMENT_BYTES / (AIHC_SLOT_MIN_WORDS * sizeof(AihcSlot)))
#define AIHC_SEGMENT_BITMAP_WORDS (AIHC_SEGMENT_MAX_SLOTS / 64)
#define AIHC_SIZE_CLASS_COUNT 43U
/* The unswept segments one collection sweeps when it ends. */
#define AIHC_SWEEP_SLICE 64U

/* A large boxed array keeps one card for each run of this many elements
   after its elements. The card is set when a store touches the run. */
#define AIHC_CARD_SHIFT 7
#define AIHC_CARD_ELEMENTS (UINT64_C(1) << AIHC_CARD_SHIFT)

typedef struct AihcSegment {
  struct AihcSegment *link;
  /* The full collection whose marks the bitmap holds. A segment with an
     older epoch holds no object outside a full collection. */
  uint64_t epoch;
  uint32_t size_class;
  uint32_t slot_words;
  uint32_t slot_count;
  /* The slot below which every slot is occupied. */
  uint32_t cursor;
  uint64_t bitmap[AIHC_SEGMENT_BITMAP_WORDS];
} AihcSegment;

_Static_assert(AIHC_SLOT_MAX_WORDS * sizeof(AihcSlot) >=
                   AIHC_LARGE_OBJECT_BYTES,
               "every object below the large object bound fits a size class");
_Static_assert(sizeof(AihcSegment) % sizeof(AihcSlot) == 0,
               "segment slots start at a word boundary");

/* The segments of one size class. The allocator fills current. It takes
   available segments, which a sweep found room in, before it sweeps an
   unswept segment or acquires a new one. A filled segment waits for the
   next full collection. */
typedef struct {
  AihcSegment *current;
  AihcSegment *available;
  AihcSegment *filled;
  AihcSegment *unswept;
} AihcSizeClass;

static AihcSizeClass aihc_size_classes[AIHC_SIZE_CLASS_COUNT];
/* Set while a full collection traces: the allocator then takes new
   segments only, because the marks of an old segment are not final. */
static int aihc_gen2_marking;
/* The size class the sweep slice looks at first. */
static unsigned aihc_sweep_class;

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
/* The gen2 objects a collection has marked or copied and not yet scanned.
   Gen2 has no Cheney cursor, because its slots are not in allocation
   order. */
static AihcValueWorklist aihc_gen2_worklist;
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

/* The room between the allocation pointer and the end of the nursery. A
   caller of the runtime allocator has reserved its words against the heap
   limit, and a large object adopted since then may have moved the limit
   below the reserved words, so the check of a reservation is against the
   end of the nursery and not against the limit. The allocation pointer can
   thus pass the limit, and every compare against the limit allows for
   that. */
static size_t aihc_nursery_room(const AihcMachine *machine) {
  return (size_t)(machine->heap_start + machine->nursery_bytes -
                  machine->heap_next);
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
    if (aihc_in_nursery(machine, value)) {
      return 0;
    }
    aihc_fail("collector asked the generation of an object that moves");
  default:
    aihc_fail("collector asked the generation of an invalid address");
  }
}

/* The blocks of gen1. */

static AihcGeneration *aihc_generation(AihcMachine *machine,
                                       unsigned generation) {
  return &machine->generations[generation - 1];
}

static uint64_t aihc_generation_used(const AihcGeneration *generation) {
  uint64_t used = 0;
  for (const AihcHeapBlock *block = generation->first; block != NULL;
       block = block->link) {
    used += (uint64_t)(block->next - block->start);
  }
  return used;
}

/* Allocate bytes for a copied object in gen1. */
static uint8_t *aihc_gen1_allocate(AihcMachine *machine, size_t bytes) {
  AihcGeneration *target = aihc_generation(machine, 1);
  AihcHeapBlock *block = target->last;
  if (block == NULL || bytes > (size_t)(block->limit - block->next)) {
    if (bytes > AIHC_HEAP_BLOCK_BYTES) {
      aihc_fail("object exceeds a heap block");
    }
    block = malloc(sizeof(*block));
    if (block == NULL) {
      aihc_fail("out of memory");
    }
    block->start =
        aihc_regions_acquire(AIHC_HEAP_BLOCK_REGIONS, AIHC_REGION_GEN1);
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

/* The segments of gen2. */

/* The class of the smallest slot that holds the given words. */
static unsigned aihc_size_class_of(uint64_t words) {
  if (words <= 8) {
    return words < AIHC_SLOT_MIN_WORDS ? 0 : (unsigned)words - 2;
  }
  if (words > AIHC_SLOT_MAX_WORDS) {
    aihc_fail("object exceeds the largest gen2 size class");
  }
  /* words is in the doubling above two to the power, and the quarter of
     that doubling that holds it names the class. */
  unsigned power = 63U - (unsigned)__builtin_clzll(words - 1);
  uint64_t quarter =
      (words - (UINT64_C(1) << power) + (UINT64_C(1) << (power - 2)) - 1) >>
      (power - 2);
  return 7U + (power - 3U) * 4U + (unsigned)quarter - 1U;
}

static uint32_t aihc_size_class_words(unsigned size_class) {
  if (size_class < 7U) {
    return size_class + 2U;
  }
  unsigned power = 3U + (size_class - 7U) / 4U;
  unsigned quarter = (size_class - 7U) % 4U + 1U;
  return (1U << power) + quarter * (1U << (power - 2U));
}

static uint8_t *aihc_segment_slots(AihcSegment *segment) {
  return (uint8_t *)segment + sizeof(AihcSegment);
}

static size_t aihc_segment_slot_bytes(const AihcSegment *segment) {
  return (size_t)segment->slot_words * sizeof(AihcSlot);
}

static AihcSegment *aihc_segment_of(const void *object) {
  AihcSegment *segment = aihc_region_run_base(object);
  if (segment == NULL) {
    aihc_fail("gen2 object is outside every segment");
  }
  return segment;
}

static uint32_t aihc_segment_slot_of(AihcSegment *segment, const void *object) {
  return (uint32_t)(((const uint8_t *)object - aihc_segment_slots(segment)) /
                    aihc_segment_slot_bytes(segment));
}

static int aihc_segment_test(const AihcSegment *segment, uint32_t slot) {
  return (int)((segment->bitmap[slot >> 6] >> (slot & 63U)) & 1U);
}

static void aihc_segment_set(AihcSegment *segment, uint32_t slot) {
  segment->bitmap[slot >> 6] |= UINT64_C(1) << (slot & 63U);
}

static AihcSegment *aihc_segment_new(AihcMachine *machine,
                                     unsigned size_class) {
  AihcSegment *segment =
      aihc_regions_acquire(AIHC_SEGMENT_REGIONS, AIHC_REGION_GEN2);
  segment->link = NULL;
  segment->epoch = machine->gc_full_count;
  segment->size_class = size_class;
  segment->slot_words = aihc_size_class_words(size_class);
  segment->slot_count = (uint32_t)((AIHC_SEGMENT_BYTES - sizeof(AihcSegment)) /
                                   aihc_segment_slot_bytes(segment));
  segment->cursor = 0;
  memset(segment->bitmap, 0, sizeof(segment->bitmap));
  return segment;
}

/* Take the first free slot at or above the cursor, or UINT32_MAX when the
   segment is full. Every slot below the cursor is occupied. */
static uint32_t aihc_segment_take(AihcSegment *segment) {
  uint32_t word = segment->cursor >> 6;
  uint32_t words = (segment->slot_count + 63U) >> 6;
  while (word < words) {
    uint64_t free = ~segment->bitmap[word];
    if (free != 0) {
      uint32_t slot = word * 64U + (uint32_t)__builtin_ctzll(free);
      if (slot >= segment->slot_count) {
        break;
      }
      segment->bitmap[word] |= UINT64_C(1) << (slot & 63U);
      segment->cursor = slot + 1U;
      return slot;
    }
    ++word;
  }
  segment->cursor = segment->slot_count;
  return UINT32_MAX;
}

/* Sweep one segment after a full collection: release it when no object was
   marked in it, and put it where the allocator finds it otherwise. A
   segment the collection did not touch kept an older epoch, and nothing is
   allocated in an unswept segment, so such a segment is empty. */
static void aihc_segment_sweep(AihcMachine *machine, AihcSizeClass *class,
                               AihcSegment *segment) {
  uint32_t live = 0;
  if (segment->epoch == machine->gc_full_count) {
    for (size_t index = 0; index < AIHC_SEGMENT_BITMAP_WORDS; ++index) {
      live += (uint32_t)__builtin_popcountll(segment->bitmap[index]);
    }
  }
  if (live == 0) {
    aihc_regions_release(segment);
    return;
  }
  segment->cursor = 0;
  if (live == segment->slot_count) {
    segment->link = class->filled;
    class->filled = segment;
  } else {
    segment->link = class->available;
    class->available = segment;
  }
}

/* Sweep a bounded number of unswept segments. Each collection runs one
   slice, so the sweep is paced by collections and no pause depends on the
   size of gen2. */
static void aihc_sweep_slice(AihcMachine *machine) {
  unsigned budget = AIHC_SWEEP_SLICE;
  unsigned idle = 0;
  while (budget != 0 && idle < AIHC_SIZE_CLASS_COUNT) {
    AihcSizeClass *class = &aihc_size_classes[aihc_sweep_class];
    AihcSegment *segment = class->unswept;
    if (segment == NULL) {
      ++idle;
      aihc_sweep_class = (aihc_sweep_class + 1U) % AIHC_SIZE_CLASS_COUNT;
      continue;
    }
    idle = 0;
    class->unswept = segment->link;
    aihc_segment_sweep(machine, class, segment);
    --budget;
  }
}

/* The segment the allocator fills next for a size class. */
static AihcSegment *aihc_class_next_segment(AihcMachine *machine,
                                            unsigned size_class) {
  AihcSizeClass *class = &aihc_size_classes[size_class];
  for (;;) {
    AihcSegment *segment = class->available;
    if (segment != NULL) {
      class->available = segment->link;
      segment->link = NULL;
      return segment;
    }
    if (aihc_gen2_marking || class->unswept == NULL) {
      return aihc_segment_new(machine, size_class);
    }
    segment = class->unswept;
    class->unswept = segment->link;
    aihc_segment_sweep(machine, class, segment);
  }
}

/* Allocate a slot for a copied object in gen2. The slot is occupied from
   now on: in a full collection the object is live by construction. */
static uint8_t *aihc_gen2_allocate(AihcMachine *machine, size_t bytes) {
  unsigned size_class = aihc_size_class_of(bytes / sizeof(AihcSlot));
  AihcSizeClass *class = &aihc_size_classes[size_class];
  for (;;) {
    AihcSegment *segment = class->current;
    if (segment == NULL) {
      segment = aihc_class_next_segment(machine, size_class);
      class->current = segment;
    }
    uint32_t slot = aihc_segment_take(segment);
    if (slot != UINT32_MAX) {
      machine->generations[1].bytes += aihc_segment_slot_bytes(segment);
      return aihc_segment_slots(segment) +
             (size_t)slot * aihc_segment_slot_bytes(segment);
    }
    segment->link = class->filled;
    class->filled = segment;
    class->current = NULL;
  }
}

/* Mark one gen2 object in a full collection and queue it for scanning.
   Returns whether the object was unmarked. */
static int aihc_mark_gen2(AihcMachine *machine, AihcValue *object) {
  AihcSegment *segment = aihc_segment_of(object);
  if (segment->epoch != machine->gc_full_count) {
    memset(segment->bitmap, 0, sizeof(segment->bitmap));
    segment->epoch = machine->gc_full_count;
  }
  uint32_t slot = aihc_segment_slot_of(segment, object);
  if (aihc_segment_test(segment, slot)) {
    return 0;
  }
  aihc_segment_set(segment, slot);
  machine->generations[1].bytes += aihc_segment_slot_bytes(segment);
  aihc_value_worklist_push(&aihc_gen2_worklist, object);
  return 1;
}

/* Whether a gen2 object is marked in the running full collection, or
   occupied outside one. */
static int aihc_gen2_marked(const AihcMachine *machine,
                            const AihcValue *object) {
  AihcSegment *segment = aihc_segment_of(object);
  return segment->epoch == machine->gc_full_count &&
         aihc_segment_test(segment, aihc_segment_slot_of(segment, object));
}

/* Move every segment to the unswept list at the start of a full
   collection. The allocator takes new segments while the collection
   traces. */
static void aihc_gen2_begin_marking(AihcMachine *machine) {
  for (unsigned index = 0; index < AIHC_SIZE_CLASS_COUNT; ++index) {
    AihcSizeClass *class = &aihc_size_classes[index];
    AihcSegment **lists[] = {&class->current, &class->available,
                             &class->filled};
    for (size_t list = 0; list < 3; ++list) {
      while (*lists[list] != NULL) {
        AihcSegment *segment = *lists[list];
        *lists[list] = segment->link;
        segment->link = class->unswept;
        class->unswept = segment;
      }
    }
  }
  machine->generations[1].bytes = 0;
  aihc_gen2_marking = 1;
}

static void aihc_segments_release(AihcSegment *segments) {
  while (segments != NULL) {
    AihcSegment *link = segments->link;
    aihc_regions_release(segments);
    segments = link;
  }
}

static void aihc_gen2_release(AihcMachine *machine) {
  for (unsigned index = 0; index < AIHC_SIZE_CLASS_COUNT; ++index) {
    AihcSizeClass *class = &aihc_size_classes[index];
    aihc_segments_release(class->current);
    aihc_segments_release(class->available);
    aihc_segments_release(class->filled);
    aihc_segments_release(class->unswept);
    *class = (AihcSizeClass){0};
  }
  machine->generations[1].bytes = 0;
}

static uint8_t *aihc_generation_allocate(AihcMachine *machine,
                                         unsigned generation, size_t bytes) {
  return generation == 1 ? aihc_gen1_allocate(machine, bytes)
                         : aihc_gen2_allocate(machine, bytes);
}

/* The bytes the objects of the old generations and the pinned list take.
   The -M limit bounds this count, not the capacity of the blocks that hold
   it and not the nursery: a block is a whole run of regions, and a program
   with a limit far below one run still has to run. */
static uint64_t aihc_old_bytes(const AihcMachine *machine) {
  return aihc_generation_used(&machine->generations[0]) +
         machine->generations[1].bytes + machine->fixed_bytes;
}

static uint64_t aihc_occupied_bytes(const AihcMachine *machine) {
  return (uint64_t)(machine->heap_next - machine->heap_start) +
         aihc_old_bytes(machine);
}

/* Cards of large boxed arrays. */

/* The bytes of the card table a large object of the given bytes needs
   when it is a boxed array. The table follows the object in its run. */
static size_t aihc_card_table_bytes(size_t object_bytes) {
  return (object_bytes / sizeof(AihcSlot) + AIHC_CARD_ELEMENTS - 1) >>
         AIHC_CARD_SHIFT;
}

/* Whether an object has a card table: a boxed array in a large region. */
static int aihc_has_cards(const AihcValue *object) {
  return aihc_region_kind(object) == AIHC_REGION_LARGE &&
         aihc_value_kind(object) == AIHC_OBJECT_ARRAY;
}

static uint8_t *aihc_array_cards(AihcValue *array) {
  AihcPinnedBlock *block = aihc_pinned_block_of(array);
  return (uint8_t *)block->object + aihc_pinned_bytes(block);
}

static uint64_t aihc_array_card_count(const AihcValue *array) {
  return (aihc_array_length(array) + AIHC_CARD_ELEMENTS - 1) >> AIHC_CARD_SHIFT;
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
  if (machine->remembered_count != 0 &&
      machine->remembered[machine->remembered_count - 1] == object) {
    /* A loop that stores into one object enters it once. */
    return;
  }
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
  if (aihc_has_cards(object)) {
    memset(aihc_array_cards(object), 1, aihc_array_card_count(object));
  }
  aihc_remember(machine, object);
}

void aihc_write_barrier_at(AihcMachine *machine, AihcValue *object,
                           uint64_t index) {
  if (object == NULL || aihc_in_nursery(machine, object)) {
    return;
  }
  if (aihc_has_cards(object)) {
    uint64_t card = index >> AIHC_CARD_SHIFT;
    if (card < aihc_array_card_count(object)) {
      aihc_array_cards(object)[card] = 1;
    }
  }
  aihc_remember(machine, object);
}

void aihc_write_barrier_range(AihcMachine *machine, AihcValue *object,
                              uint64_t offset, uint64_t count) {
  if (object == NULL || count == 0 || aihc_in_nursery(machine, object)) {
    return;
  }
  if (aihc_has_cards(object)) {
    uint8_t *cards = aihc_array_cards(object);
    uint64_t last = (offset + count - 1) >> AIHC_CARD_SHIFT;
    uint64_t card_count = aihc_array_card_count(object);
    if (last >= card_count) {
      last = card_count - 1;
    }
    for (uint64_t card = offset >> AIHC_CARD_SHIFT; card <= last; ++card) {
      cards[card] = 1;
    }
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
  if (generation == 2) {
    aihc_value_worklist_push(&aihc_gen2_worklist, copy);
  }
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
      case AIHC_REGION_GEN1:
        return value;
      case AIHC_REGION_GEN2:
        if (context->collected < 2) {
          return value;
        }
        /* A full collection follows a gen2 indirection as the copy follows
           one in a from-space: the indirection stays unmarked, and its
           slot is free after the collection. */
        if (aihc_value_kind(value) == AIHC_OBJECT_INDIRECTION) {
          value = (AihcValue *)(uintptr_t)value->fields[0];
          continue;
        }
        aihc_mark_gen2(machine, value);
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

/* Scan a large boxed array card by card. With dirty_only, only the cards
   that stores touched are scanned: the array is in the remembered set, and
   its clean cards point at objects of its generation or above. A card stays
   dirty when a referent of it still ends younger than the array, and the
   array then goes back to the remembered set. generation is where the
   array lives, which is one at least: a young array is scanned after its
   promotion. */
static void aihc_scan_large_array(AihcGcContext *context, AihcValue *array,
                                  unsigned generation, int dirty_only) {
  AihcScanContext scan = {.gc = context, .target = generation};
  uint8_t *cards = aihc_array_cards(array);
  uint64_t length = aihc_array_length(array);
  AihcSlot *elements = aihc_array_elements(array);
  uint64_t card_count = aihc_array_card_count(array);
  int kept = 0;
  for (uint64_t card = 0; card < card_count; ++card) {
    if (dirty_only && cards[card] == 0) {
      continue;
    }
    uint64_t end = (card + 1) << AIHC_CARD_SHIFT;
    if (end > length) {
      end = length;
    }
    scan.youngest = AIHC_GENERATION_STATIC;
    for (uint64_t index = card << AIHC_CARD_SHIFT; index < end; ++index) {
      elements[index] = aihc_scan_slot(elements[index], &scan);
    }
    cards[card] = scan.youngest < generation;
    kept |= cards[card];
  }
  if (kept) {
    aihc_gc_keep(context, array);
  }
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
      if (aihc_has_cards(object)) {
        aihc_scan_large_array(context, object, generation, 0);
        return;
      }
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
    if (aihc_gen2_worklist.count != 0) {
      AihcValue *object = aihc_gen2_worklist.items[--aihc_gen2_worklist.count];
      aihc_scan_object(context, object, 2);
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
    AihcRegionKind kind = aihc_region_kind(object);
    switch (kind) {
    case AIHC_REGION_FROM1:
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
    if (kind == AIHC_REGION_LARGE &&
        aihc_value_kind(object) == AIHC_OBJECT_ARRAY) {
      aihc_scan_large_array(context, object, generation, 1);
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
        /* A from-space object: forwarded, followed, or dead, as below. */
        break;
      case AIHC_REGION_GEN2:
        if (context->collected < 2 || aihc_gen2_marked(machine, value)) {
          return value;
        }
        if (aihc_value_kind(value) != AIHC_OBJECT_INDIRECTION) {
          return NULL;
        }
        value = (AihcValue *)(uintptr_t)value->fields[0];
        continue;
      case AIHC_REGION_GEN1:
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

  /* Detach the gen1 blocks this collection copies away, so gen1 fills fresh
     blocks. A full collection marks gen2 in place. */
  AihcHeapBlock *from1 = NULL;
  AihcGeneration *gen1 = aihc_generation(machine, 1);
  if (collected >= 1) {
    aihc_blocks_set_kind(gen1->first, AIHC_REGION_FROM1);
    from1 = gen1->first;
    gen1->first = NULL;
    gen1->last = NULL;
    gen1->bytes = 0;
  }
  gen1->scan_block = gen1->last;
  gen1->scan = gen1->last == NULL ? NULL : gen1->last->next;
  if (collected == 2) {
    aihc_gen2_begin_marking(machine);
  }

  aihc_address_set_clear(&aihc_marked_statics);
  aihc_static_worklist.count = 0;
  aihc_pinned_worklist.count = 0;
  aihc_gen2_worklist.count = 0;
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
  aihc_gen2_marking = 0;
  aihc_update_stable_names(&context);
  aihc_sweep_stacks(&context);
  aihc_sweep_pinned(&context);
  aihc_age_chunks(&context);
  aihc_clear_srt_stamps();
  aihc_sweep_slice(machine);

  aihc_blocks_release(from1);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->nursery_bytes;
  machine->heap_alloc_base = machine->heap_next;
  machine->fixed_since_gc = 0;
  for (size_t index = 0; index < context.kept_count; ++index) {
    aihc_remember(machine, context.kept[index]);
  }
  free(context.kept);
  if (collected == 2) {
    /* The factor is at most AIHC_GEN2_FACTOR_MAX, so the product fits when
       the bytes leave the top bits clear. A check against a quotient would
       become a wide multiply that the wasm32 link does not provide. */
    uint64_t limit = machine->generations[1].bytes;
    if (limit > (UINT64_MAX >> AIHC_GEN2_FACTOR_BITS)) {
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
  AihcGeneration *gen1 = aihc_generation(machine, 1);
  aihc_blocks_release(gen1->first);
  gen1->first = NULL;
  gen1->last = NULL;
  gen1->bytes = 0;
  gen1->scan_block = NULL;
  gen1->scan = NULL;
  aihc_gen2_release(machine);
  aihc_gen2_worklist.count = 0;
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
    machine->gen2_factor =
        factor > AIHC_GEN2_FACTOR_MAX ? AIHC_GEN2_FACTOR_MAX : factor;
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
  if (bytes > aihc_nursery_room(machine)) {
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
  if (machine->heap_next > machine->heap_limit ||
      bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_gc_collect(machine, words, root_count, roots, srt);
  }
}

/* Put a block on the pinned list and charge it to the budget. The charge
   also shortens the nursery, so fixed allocation brings the next collection
   closer as nursery allocation does. Compiled code reserves a large object
   with one compare against the heap limit and allocates it through the
   runtime, so this is the only pressure a large object below the nursery
   size puts on the collector. */
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
    /* The card table of a boxed array follows the object. The runtime does
       not know the kind of the object yet, so every large object gets the
       room, which is below a thousandth of its size. */
    size_t card_bytes = aihc_card_table_bytes(bytes);
    if (bytes > SIZE_MAX - sizeof(AihcPinnedBlock) - card_bytes) {
      aihc_fail("heap allocation is too large");
    }
    AihcPinnedBlock *block = aihc_fixed_block_new(
        sizeof(AihcPinnedBlock) + bytes + card_bytes, AIHC_REGION_LARGE);
#ifdef DEBUG
    memset(block->object, 0, bytes);
#endif
    memset((uint8_t *)block->object + bytes, 0, card_bytes);
    return aihc_pinned_block_adopt(machine, block, bytes);
  }
  if (bytes > aihc_nursery_room(machine)) {
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
    if (bytes > aihc_nursery_room(machine)) {
      aihc_fail("unchecked pinned allocation exceeded reserved heap");
    }
    block = calloc(1, bytes);
    if (block == NULL) {
      aihc_fail("out of memory");
    }
  }
  return aihc_pinned_block_adopt(machine, block, bytes);
}

/* The heap walk of the test drivers. */

static void aihc_walk_range(uint8_t *start, uint8_t *end,
                            AihcObjectVisitor visitor, void *context) {
  uint8_t *cursor = start;
  while (cursor < end) {
    AihcValue *object = (AihcValue *)cursor;
    if (object->header == 0) {
      /* The slop behind a thunk that an update turned into an indirection. */
      cursor += sizeof(AihcSlot);
      continue;
    }
    visitor(object, context);
    cursor += sizeof(AihcSlot) * aihc_value_words(object);
  }
  if (cursor != end) {
    aihc_fail("object sizes do not end at the allocation pointer");
  }
}

static void aihc_walk_segments(const AihcMachine *machine,
                               AihcSegment *segments, AihcObjectVisitor visitor,
                               void *context) {
  for (AihcSegment *segment = segments; segment != NULL;
       segment = segment->link) {
    if (segment->epoch != machine->gc_full_count) {
      continue;
    }
    for (uint32_t slot = 0; slot < segment->slot_count; ++slot) {
      if (aihc_segment_test(segment, slot)) {
        visitor((AihcValue *)(aihc_segment_slots(segment) +
                              (size_t)slot * aihc_segment_slot_bytes(segment)),
                context);
      }
    }
  }
}

void aihc_gc_walk_objects(AihcMachine *machine, AihcObjectVisitor visitor,
                          void *context) {
  aihc_walk_range(machine->heap_start, machine->heap_next, visitor, context);
  for (const AihcHeapBlock *block = machine->generations[0].first;
       block != NULL; block = block->link) {
    aihc_walk_range(block->start, block->next, visitor, context);
  }
  for (unsigned index = 0; index < AIHC_SIZE_CLASS_COUNT; ++index) {
    const AihcSizeClass *class = &aihc_size_classes[index];
    aihc_walk_segments(machine, class->current, visitor, context);
    aihc_walk_segments(machine, class->available, visitor, context);
    aihc_walk_segments(machine, class->filled, visitor, context);
    aihc_walk_segments(machine, class->unswept, visitor, context);
  }
  for (AihcPinnedBlock *block = machine->pinned_blocks; block != NULL;
       block = block->next) {
    if (aihc_region_kind(block) == AIHC_REGION_LARGE) {
      visitor((AihcValue *)block->object, context);
    }
  }
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
