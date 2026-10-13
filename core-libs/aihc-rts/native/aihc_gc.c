#include "aihc_runtime_internal.h"

#include <inttypes.h>
#include <stdio.h>
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

   Gen2 is collected by an incremental snapshot-at-the-beginning marking:
   a cycle starts at the end of a collection that emptied gen1, the write
   barrier shades the old values of stores into old objects, and slices at
   the end of later collections mark until the mark stack and the stacks
   are done. A full collection runs a whole cycle in one pause.

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
  /* The gen2 cycle whose marks the mark bitmap holds. A segment with an
     older epoch has no mark in the running cycle. */
  uint64_t epoch;
  uint32_t size_class;
  uint32_t slot_words;
  uint32_t slot_count;
  /* The slot below which every slot is occupied. */
  uint32_t cursor;
  /* The marks of the cycle of the epoch. */
  uint64_t bitmap[AIHC_SEGMENT_BITMAP_WORDS];
  /* The occupied slots: the live slots after the last sweep and the slots
     allocated since. The heap walk of the test drivers reads this map. */
  uint64_t occupied[AIHC_SEGMENT_BITMAP_WORDS];
} AihcSegment;

/* The elements of a boxed array one mark step scans before it puts the
   rest back on the mark stack. */
#define AIHC_MARK_ARRAY_PIECE ((uint64_t)4096)

/* One item of mark work: an object, and for a boxed array the first
   element the step scans. */
typedef struct {
  AihcValue *object;
  uint64_t start;
} AihcMarkEntry;

typedef struct {
  AihcMarkEntry *items;
  size_t count;
  size_t capacity;
} AihcMarkStack;

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
/* The size class the sweep slice looks at first. */
static unsigned aihc_sweep_class;
/* The work of the active gen2 cycle: the old objects that are marked and
   not yet scanned. */
static AihcMarkStack aihc_mark_stack;

static void aihc_mark_old(AihcCapability *capability, AihcValue *value);
static void aihc_shade_fields(AihcCapability *capability, AihcValue *object);
static void aihc_shade_elements(AihcCapability *capability, AihcValue *array,
                                uint64_t first, uint64_t end);
static void aihc_mark_frames(AihcCapability *capability, AihcValue *frame);
static int aihc_frame_above(const AihcValue *first, const AihcValue *second);
static int aihc_frame_is_live(const AihcCapability *capability,
                              const AihcValue *frame);
#ifdef AIHC_GC_VERIFY
static void aihc_gc_verify(AihcCapability *capability, uint64_t root_count,
                           AihcSlot *roots, const AihcSrt *srt);
#endif

typedef struct {
  AihcCapability *capability;
  /* The oldest generation this collection copies. */
  unsigned collected;
  /* Set at the end of a gen2 cycle: the young generations were swept
     already, so a young object is live, and gen2 is judged by the marks of
     the cycle. */
  int finishing;
  /* Objects the next collection must scan again: an object that points at
     a younger object after its scan. */
  AihcValue **kept;
  size_t kept_count;
  size_t kept_capacity;
  /* The stack chunks with a frame this collection scanned. */
  AihcStackChunk **touched;
  size_t touched_count;
  size_t touched_capacity;
  /* The bytes this collection copied. The mark slice at its end may do as
     much work. */
  uint64_t copied_bytes;
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
  /* Whether the scanned object is a frame. The parent link of a frame does
     not age its chunk, but a record that names a frame must stay in the
     remembered set while the chunk of the frame can be scanned again. */
  int from_frame;
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

/* The static objects and frames one collection has reached. */
static AihcAddressSet aihc_marked_statics;
/* The static objects the active gen2 cycle has marked. */
static AihcAddressSet aihc_cycle_statics;
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

/* Mark one object that never moves and queue it for scanning. A static
   object is also marked for the gen2 cycle, which decides the liveness of
   static objects at its end. */
static void aihc_mark_static(AihcValue *object) {
  if (object == NULL ||
      !aihc_address_set_insert(&aihc_marked_statics, object)) {
    return;
  }
  if (aihc_region_kind(object) == AIHC_REGION_OUTSIDE) {
    (void)aihc_address_set_insert(&aihc_cycle_statics, object);
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

static int aihc_in_nursery(const AihcCapability *capability,
                           const void *value) {
  return (uintptr_t)value - (uintptr_t)capability->heap_start <
         capability->nursery_bytes;
}

/* The room between the allocation pointer and the end of the nursery. A
   caller of the runtime allocator has reserved its words against the heap
   limit, and a large object adopted since then may have moved the limit
   below the reserved words, so the check of a reservation is against the
   end of the nursery and not against the limit. The allocation pointer can
   thus pass the limit, and every compare against the limit allows for
   that. */
static size_t aihc_nursery_room(const AihcCapability *capability) {
  return (size_t)(capability->heap_start + capability->nursery_bytes -
                  capability->heap_next);
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

/* The blocks of gen1. */

static AihcGeneration *aihc_generation(AihcCapability *capability,
                                       unsigned generation) {
  return &capability->machine->generations[generation - 1];
}

/* The bytes the gen1 and gen2 tests read: the blocks of the generation and
   the fixed blocks of the same generation. A fixed block is a large object
   or a pinned block. It has no space in a generation. Without its charge
   here, a dead old fixed block waits for the -M limit. */
static uint64_t aihc_gen1_bytes(const AihcCapability *capability) {
  return capability->machine->generations[0].bytes +
         capability->machine->fixed_generation_bytes[1];
}

static uint64_t aihc_gen2_bytes(const AihcCapability *capability) {
  return capability->machine->generations[1].bytes +
         capability->machine->fixed_generation_bytes[2];
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
static uint8_t *aihc_gen1_allocate(AihcCapability *capability, size_t bytes) {
  AihcGeneration *target = aihc_generation(capability, 1);
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

static int aihc_segment_occupied(const AihcSegment *segment, uint32_t slot) {
  return (int)((segment->occupied[slot >> 6] >> (slot & 63U)) & 1U);
}

static AihcSegment *aihc_segment_new(AihcCapability *capability,
                                     unsigned size_class) {
  AihcSegment *segment =
      aihc_regions_acquire(AIHC_SEGMENT_REGIONS, AIHC_REGION_GEN2);
  segment->link = NULL;
  segment->epoch = capability->machine->gen2_epoch;
  segment->size_class = size_class;
  segment->slot_words = aihc_size_class_words(size_class);
  segment->slot_count = (uint32_t)((AIHC_SEGMENT_BYTES - sizeof(AihcSegment)) /
                                   aihc_segment_slot_bytes(segment));
  segment->cursor = 0;
  memset(segment->bitmap, 0, sizeof(segment->bitmap));
  memset(segment->occupied, 0, sizeof(segment->occupied));
  return segment;
}

/* Take the first free slot at or above the cursor, or UINT32_MAX when the
   segment is full. Every slot below the cursor is occupied. While a cycle
   marks, the slot is marked as well: the object is live by construction. */
static uint32_t aihc_segment_take(AihcSegment *segment, int marking) {
  uint32_t word = segment->cursor >> 6;
  uint32_t words = (segment->slot_count + 63U) >> 6;
  while (word < words) {
    uint64_t free = ~segment->occupied[word];
    if (free != 0) {
      uint32_t slot = word * 64U + (uint32_t)__builtin_ctzll(free);
      if (slot >= segment->slot_count) {
        break;
      }
      segment->occupied[word] |= UINT64_C(1) << (slot & 63U);
      if (marking) {
        segment->bitmap[word] |= UINT64_C(1) << (slot & 63U);
      }
      segment->cursor = slot + 1U;
      return slot;
    }
    ++word;
  }
  segment->cursor = segment->slot_count;
  return UINT32_MAX;
}

/* Sweep one segment after a gen2 cycle: the marks become the occupied
   slots. The segment is released when no object was marked in it, and goes
   where the allocator finds it otherwise. A segment the cycle did not touch
   kept an older epoch, and nothing is allocated in an unswept segment, so
   such a segment is empty. */
static void aihc_segment_sweep(AihcCapability *capability, AihcSizeClass *class,
                               AihcSegment *segment) {
  uint32_t live = 0;
  if (segment->epoch == capability->machine->gen2_epoch) {
    for (size_t index = 0; index < AIHC_SEGMENT_BITMAP_WORDS; ++index) {
      live += (uint32_t)__builtin_popcountll(segment->bitmap[index]);
    }
  }
  if (live == 0) {
    aihc_regions_release(segment);
    return;
  }
  memcpy(segment->occupied, segment->bitmap, sizeof(segment->occupied));
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
static void aihc_sweep_slice(AihcCapability *capability) {
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
    aihc_segment_sweep(capability, class, segment);
    --budget;
  }
}

/* The segment the allocator fills next for a size class. */
static AihcSegment *aihc_class_next_segment(AihcCapability *capability,
                                            unsigned size_class) {
  AihcSizeClass *class = &aihc_size_classes[size_class];
  for (;;) {
    AihcSegment *segment = class->available;
    if (segment != NULL) {
      class->available = segment->link;
      segment->link = NULL;
      return segment;
    }
    if (capability->machine->gen2_cycle_active || class->unswept == NULL) {
      return aihc_segment_new(capability, size_class);
    }
    segment = class->unswept;
    class->unswept = segment->link;
    aihc_segment_sweep(capability, class, segment);
  }
}

/* Allocate a slot for a copied object in gen2. The slot is occupied from
   now on: in a full collection the object is live by construction. */
static uint8_t *aihc_gen2_allocate(AihcCapability *capability, size_t bytes) {
  unsigned size_class = aihc_size_class_of(bytes / sizeof(AihcSlot));
  AihcSizeClass *class = &aihc_size_classes[size_class];
  for (;;) {
    AihcSegment *segment = class->current;
    if (segment == NULL) {
      segment = aihc_class_next_segment(capability, size_class);
      class->current = segment;
    }
    uint32_t slot =
        aihc_segment_take(segment, capability->machine->gen2_cycle_active != 0);
    if (slot != UINT32_MAX) {
      size_t slot_bytes = aihc_segment_slot_bytes(segment);
      capability->machine->generations[1].bytes += slot_bytes;
      if (capability->machine->gen2_cycle_active) {
        /* The object is black: the cycle counts it and owes mark work for
           it. */
        capability->machine->gen2_marked_bytes += slot_bytes;
        uint64_t owed = (uint64_t)slot_bytes * capability->machine->mark_factor;
        capability->machine->mark_debt =
            owed > UINT64_MAX - capability->machine->mark_debt
                ? UINT64_MAX
                : capability->machine->mark_debt + owed;
      }
      return aihc_segment_slots(segment) + (size_t)slot * slot_bytes;
    }
    segment->link = class->filled;
    class->filled = segment;
    class->current = NULL;
  }
}

/* Set the mark of a gen2 object in the active cycle. Returns whether the
   object was unmarked. */
static int aihc_gen2_set_mark(AihcCapability *capability, AihcValue *object,
                              AihcSegment *segment) {
  if (segment->epoch != capability->machine->gen2_epoch) {
    memset(segment->bitmap, 0, sizeof(segment->bitmap));
    segment->epoch = capability->machine->gen2_epoch;
  }
  uint32_t slot = aihc_segment_slot_of(segment, object);
  if (aihc_segment_test(segment, slot)) {
    return 0;
  }
  aihc_segment_set(segment, slot);
  capability->machine->gen2_marked_bytes += aihc_segment_slot_bytes(segment);
  return 1;
}

/* Mark one gen2 object a full collection reaches and queue it for the
   scan of the copy phase, which copies its young referents. */
static void aihc_mark_gen2(AihcCapability *capability, AihcValue *object) {
  if (aihc_gen2_set_mark(capability, object, aihc_segment_of(object))) {
    aihc_value_worklist_push(&aihc_gen2_worklist, object);
  }
}

/* Whether a gen2 object is marked in the active cycle. */
static int aihc_gen2_marked(const AihcCapability *capability,
                            const AihcValue *object) {
  AihcSegment *segment = aihc_segment_of(object);
  return segment->epoch == capability->machine->gen2_epoch &&
         aihc_segment_test(segment, aihc_segment_slot_of(segment, object));
}

static void aihc_mark_push(AihcValue *object, uint64_t start) {
  if (aihc_mark_stack.count == aihc_mark_stack.capacity) {
    aihc_mark_stack.items =
        aihc_worklist_grow(aihc_mark_stack.items, &aihc_mark_stack.capacity,
                           sizeof(*aihc_mark_stack.items));
  }
  aihc_mark_stack.items[aihc_mark_stack.count++] =
      (AihcMarkEntry){.object = object, .start = start};
}

/* Start a gen2 cycle: every segment goes to the unswept list, the marks of
   the last cycle are void through the new epoch, and the allocator takes
   new segments until the cycle ends. A cycle that was active is given up:
   the new cycle marks everything again. */
static void aihc_gen2_begin_marking(AihcCapability *capability) {
  if (capability->machine->gen2_epoch == UINT64_MAX) {
    aihc_fail("gen2 cycle counter overflow");
  }
  if (capability->machine->gen2_cycle_active) {
    /* The cycle that is given up marked pinned blocks. The segments carry
       the epoch, but a block carries one bit, so the marks are cleared
       here. This is the degradation path, so the walk is acceptable. */
    for (AihcPinnedBlock *block = capability->machine->pinned_blocks;
         block != NULL; block = block->next) {
      block->bytes &= ~AIHC_PINNED_CYCLE_MARK;
    }
  }
  ++capability->machine->gen2_epoch;
  capability->machine->gen2_cycle_active = 1;
  capability->machine->gen2_marked_bytes = 0;
  capability->machine->gen2_cycle_start_bytes = aihc_gen2_bytes(capability);
  capability->machine->mark_debt = 0;
  aihc_mark_stack.count = 0;
  aihc_address_set_clear(&aihc_cycle_statics);
  aihc_clear_srt_stamps();
  aihc_srt_worklist.count = 0;
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    stack->pending_count = 0;
  }
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
}

static void aihc_segments_release(AihcSegment *segments) {
  while (segments != NULL) {
    AihcSegment *link = segments->link;
    aihc_regions_release(segments);
    segments = link;
  }
}

static void aihc_gen2_release(AihcCapability *capability) {
  for (unsigned index = 0; index < AIHC_SIZE_CLASS_COUNT; ++index) {
    AihcSizeClass *class = &aihc_size_classes[index];
    aihc_segments_release(class->current);
    aihc_segments_release(class->available);
    aihc_segments_release(class->filled);
    aihc_segments_release(class->unswept);
    *class = (AihcSizeClass){0};
  }
  capability->machine->generations[1].bytes = 0;
}

static uint8_t *aihc_generation_allocate(AihcCapability *capability,
                                         unsigned generation, size_t bytes) {
  return generation == 1 ? aihc_gen1_allocate(capability, bytes)
                         : aihc_gen2_allocate(capability, bytes);
}

/* The bytes the objects of the old generations and the pinned list take.
   The -M limit bounds this count, not the capacity of the blocks that hold
   it and not the nursery: a block is a whole run of regions, and a program
   with a limit far below one run still has to run. */
static uint64_t aihc_old_bytes(const AihcCapability *capability) {
  return aihc_generation_used(&capability->machine->generations[0]) +
         capability->machine->generations[1].bytes +
         capability->machine->fixed_bytes;
}

static uint64_t aihc_occupied_bytes(const AihcCapability *capability) {
  return (uint64_t)(capability->heap_next - capability->heap_start) +
         aihc_old_bytes(capability);
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

/* Charge memory outside the nursery to the collection trigger, as if the
   mutator took it from the nursery: count it in the fixed bytes since the
   last collection, and lower the heap limit by it. When the charge uses
   the rest of the nursery, the limit goes below the nursery start. Then
   every reservation fails its compare and collects, a reservation of zero
   words too: compiled code makes one before each push of a frame. The
   first charge after a collection only empties the room: a collection at
   once would not free its memory, and a stack growth that collects again
   and again would not progress. Compiled code can keep a heap pointer
   above the capability copy, so its limit can also go below its heap pointer
   before that. */
static void aihc_charge_fixed(AihcCapability *capability, size_t bytes) {
  size_t room = capability->heap_limit > capability->heap_next
                    ? (size_t)(capability->heap_limit - capability->heap_next)
                    : 0;
  if (bytes < room) {
    capability->heap_limit -= bytes;
  } else if (capability->fixed_since_gc == 0) {
    capability->heap_limit = capability->heap_next;
  } else {
    capability->heap_limit = (uint8_t *)((uintptr_t)capability->heap_start - 1);
  }
  capability->fixed_since_gc += bytes;
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

static AihcStackChunk *aihc_stack_chunk_new(AihcCapability *capability,
                                            AihcStack *stack) {
  if (capability->spare_chunks == NULL) {
    /* Chunks come from stack regions. The chunks of a region stay with the
       capability: a released chunk goes to the spare list and is used again. */
    uint8_t *region = aihc_regions_acquire(1, AIHC_REGION_STACK);
    for (size_t offset = AIHC_REGION_BYTES; offset != 0;) {
      offset -= AIHC_STACK_CHUNK_BYTES;
      AihcStackChunk *spare = (AihcStackChunk *)(region + offset);
      spare->stack = NULL;
      spare->below = NULL;
      spare->above = capability->spare_chunks;
      capability->spare_chunks = spare;
      ++capability->spare_chunk_count;
    }
  }
  AihcStackChunk *chunk = capability->spare_chunks;
  capability->spare_chunks = chunk->above;
  --capability->spare_chunk_count;
  chunk->stack = stack;
  chunk->below = NULL;
  chunk->above = NULL;
  chunk->state = 0;
  chunk->scanned_from = NULL;
  chunk->scanned_cycle = 0;
  chunk->depth = 0;
  chunk->charged_gc = (uint32_t)capability->machine->gc_count;
  chunk->capability = capability;
  return chunk;
}

static void aihc_stack_chunk_free(AihcCapability *capability,
                                  AihcStackChunk *chunk) {
  chunk->stack = NULL;
  chunk->below = NULL;
  chunk->above = capability->spare_chunks;
  capability->spare_chunks = chunk;
  ++capability->spare_chunk_count;
}

AihcStack *aihc_stack_new(AihcCapability *capability, AihcThread *thread) {
  AihcStack *stack = malloc(sizeof(*stack));
  if (stack == NULL) {
    aihc_fail("out of memory for the thread stack");
  }
  stack->thread = thread;
  stack->base = aihc_stack_chunk_new(capability, stack);
  stack->next = capability->machine->stacks;
  stack->top = NULL;
  stack->pending = NULL;
  stack->pending_count = 0;
  stack->pending_capacity = 0;
  capability->machine->stacks = stack;
  return stack;
}

uint8_t *aihc_stack_base(const AihcStack *stack) {
  return aihc_stack_chunk_frames(stack->base);
}

AihcStack *aihc_stack_of(const void *frame) {
  return aihc_stack_chunk_of(frame)->stack;
}

void aihc_stack_release(AihcCapability *capability, AihcStack *stack) {
  AihcStack **link = &capability->machine->stacks;
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
    aihc_stack_chunk_free(capability, chunk);
    chunk = above;
  }
  free(stack->pending);
  free(stack);
}

AihcValue *aihc_stack_grow(AihcCapability *capability, uint8_t *stack_next,
                           uint64_t words) {
  if (words > (AIHC_STACK_CHUNK_BYTES - AIHC_STACK_CHUNK_HEADER_BYTES) /
                  sizeof(AihcSlot)) {
    aihc_fail("continuation frame exceeds a stack chunk");
  }
  AihcStackChunk *current = aihc_stack_chunk_of(stack_next - 1);
  AihcStackChunk *next = current->above;
  if (next == NULL) {
    next = aihc_stack_chunk_new(capability, current->stack);
    next->below = current;
    current->above = next;
  }
  if (current->depth == UINT32_MAX) {
    aihc_fail("thread stack exceeds the chunk limit");
  }
  next->depth = current->depth + 1;
  next->capability = capability;
  aihc_chunk_set_generation(next, 0);
  /* The next minor collection scans each young chunk, so a young chunk
     costs that pause as much as a nursery object of its size. Charge the
     chunk once for each collection: a push and pop across one boundary
     then does not bring the collection closer each time. */
  if (next->charged_gc != (uint32_t)(capability->machine->gc_count + 1)) {
    next->charged_gc = (uint32_t)(capability->machine->gc_count + 1);
    aihc_charge_fixed(capability, AIHC_STACK_CHUNK_BYTES);
  }
  return (AihcValue *)aihc_stack_chunk_frames(next);
}

AihcValue *aihc_stack_push(AihcCapability *capability, uint64_t words) {
  uint8_t *frame = capability->stack_next;
  if (frame == NULL) {
    aihc_fail("stack push before the capability started");
  }
  uintptr_t last = (uintptr_t)frame + words * sizeof(AihcSlot) - 1;
  if ((((uintptr_t)frame - 1) ^ last) >= AIHC_STACK_CHUNK_BYTES) {
    frame = (uint8_t *)aihc_stack_grow(capability, frame, words);
  }
  capability->stack_next = frame + words * sizeof(AihcSlot);
  return (AihcValue *)frame;
}

void aihc_stack_resume_after(AihcCapability *capability,
                             const AihcValue *frame) {
  capability->stack_next =
      (uint8_t *)frame + aihc_value_words(frame) * sizeof(AihcSlot);
  aihc_stack_enter_chunk(capability, frame);
}

/* Remove the deferred frames at or above a frame that the stack pointer
   enters. The entry pops the frames above it, and a pop can pass a frame
   without a read: a forward frame gives its values to its parent. New
   frames then take their place, so a slice must not scan them. The entered
   frame itself is scanned at once by the caller, so its entry goes too:
   the code of the frame pops it next, and the list holds live frames
   only. */
static void aihc_stack_drop_pending_above(AihcStack *stack,
                                          const AihcValue *frame) {
  size_t kept = 0;
  for (size_t index = 0; index < stack->pending_count; ++index) {
    AihcValue *pending = stack->pending[index];
    if (aihc_frame_above(frame, pending)) {
      stack->pending[kept++] = pending;
    }
  }
  stack->pending_count = kept;
}

void aihc_stack_enter_chunk(AihcCapability *capability,
                            const AihcValue *frame) {
  AihcStackChunk *chunk = aihc_stack_chunk_of(frame);
  chunk->capability = capability;
  aihc_chunk_set_generation(chunk, 0);
  if (capability->machine->gen2_cycle_active) {
    aihc_stack_drop_pending_above(chunk->stack, frame);
    /* The frames above the entered one are gone. The entered frame and
       the frames below it in the chunk are read from now on, so the cycle
       scans them before the mutator overwrites any of them. */
    aihc_mark_frames(capability, (AihcValue *)(uintptr_t)frame);
  }
}

void aihc_gc_frame_read(AihcCapability *capability, AihcValue *frame) {
  if (capability->machine->gen2_cycle_active) {
    aihc_mark_frames(capability, frame);
  }
}

void aihc_stack_note_top(AihcValue *continuation) {
  if (continuation != NULL &&
      aihc_region_kind(continuation) == AIHC_REGION_STACK) {
    aihc_stack_chunk_of(continuation)->stack->top = continuation;
  }
}

/* The remembered set. */

static int aihc_compare_addresses(const void *left, const void *right) {
  uintptr_t first = (uintptr_t)*(AihcValue *const *)left;
  uintptr_t second = (uintptr_t)*(AihcValue *const *)right;
  return first < second ? -1 : first > second;
}

static void aihc_remembered_grow(AihcCapability *capability) {
  uint64_t capacity = capability->remembered_capacity == 0
                          ? 256
                          : capability->remembered_capacity * 2;
  AihcValue **grown = realloc(capability->remembered,
                              capacity * sizeof(*capability->remembered));
  if (grown == NULL) {
    aihc_fail("out of memory");
  }
  capability->remembered = grown;
  capability->remembered_capacity = capacity;
}

/* Keep one entry for each object. A hot mutable object enters the set at
   every store, so the list is compacted when it is full of such entries.
   Without a repeatable entry since the last compaction, the list has no
   duplicate and the sort is skipped. */
static void aihc_remembered_compact(AihcCapability *capability) {
  if (capability->remembered_repeatable == 0) {
    return;
  }
  capability->remembered_repeatable = 0;
  if (capability->remembered_count == 0) {
    /* qsort takes no null list, and an empty list has nothing to sort. */
    return;
  }
  qsort(capability->remembered, capability->remembered_count,
        sizeof(*capability->remembered), aihc_compare_addresses);
  uint64_t kept = 0;
  for (uint64_t index = 0; index < capability->remembered_count; ++index) {
    if (kept == 0 ||
        capability->remembered[kept - 1] != capability->remembered[index]) {
      capability->remembered[kept++] = capability->remembered[index];
    }
  }
  capability->remembered_count = kept;
}

void aihc_remember(AihcCapability *capability, AihcValue *object) {
  if (capability->remembered_count != 0 &&
      capability->remembered[capability->remembered_count - 1] == object) {
    /* A loop that stores into one object enters it once. */
    return;
  }
  ++capability->remembered_repeatable;
  if (capability->remembered_count == capability->remembered_capacity) {
    /* Compact only when repeatable entries are half the list or more.
       Otherwise the sort would mostly reorder unique entries. */
    if (capability->remembered_capacity != 0 &&
        capability->remembered_repeatable * 2 >= capability->remembered_count) {
      aihc_remembered_compact(capability);
    }
    if (capability->remembered_count * 2 >= capability->remembered_capacity) {
      uint64_t capacity = capability->remembered_capacity == 0
                              ? 256
                              : capability->remembered_capacity * 2;
      if (capacity > SIZE_MAX / sizeof(*capability->remembered)) {
        aihc_fail("remembered set is too large");
      }
      AihcValue **grown = realloc(capability->remembered,
                                  capacity * sizeof(*capability->remembered));
      if (grown == NULL) {
        aihc_fail("out of memory");
      }
      capability->remembered = grown;
      capability->remembered_capacity = capacity;
    }
  }
  capability->remembered[capability->remembered_count++] = object;
}

/* Add an object that the set cannot hold since the last collection: no
   compaction is necessary. */
static void aihc_remember_unique(AihcCapability *capability,
                                 AihcValue *object) {
  if (capability->remembered_count == capability->remembered_capacity) {
    aihc_remembered_grow(capability);
  }
  capability->remembered[capability->remembered_count++] = object;
}

/* The barrier of a blackhole update. The update writes the result over the
   payload, so the old referents are shaded as aihc_write_barrier does. A
   blackhole is updated once, so its entry is unique. */
void aihc_write_barrier_update(AihcCapability *capability, AihcValue *object) {
  if (object == NULL || aihc_in_nursery(capability, object)) {
    return;
  }
  if (capability->machine->gen2_cycle_active) {
    aihc_shade_fields(capability, object);
  }
  aihc_remember_unique(capability, object);
}

void aihc_write_barrier(AihcCapability *capability, AihcValue *object) {
  if (object == NULL || aihc_in_nursery(capability, object)) {
    return;
  }
  if (capability->machine->gen2_cycle_active) {
    aihc_shade_fields(capability, object);
  }
  if (aihc_has_cards(object)) {
    memset(aihc_array_cards(object), 1, aihc_array_card_count(object));
  }
  aihc_remember(capability, object);
}

void aihc_write_barrier_at(AihcCapability *capability, AihcValue *object,
                           uint64_t index) {
  if (object == NULL || aihc_in_nursery(capability, object)) {
    return;
  }
  if (capability->machine->gen2_cycle_active) {
    aihc_shade_elements(capability, object, index, index + 1);
  }
  if (aihc_has_cards(object)) {
    uint64_t card = index >> AIHC_CARD_SHIFT;
    if (card < aihc_array_card_count(object)) {
      aihc_array_cards(object)[card] = 1;
    }
  }
  aihc_remember(capability, object);
}

void aihc_write_barrier_range(AihcCapability *capability, AihcValue *object,
                              uint64_t offset, uint64_t count) {
  if (object == NULL || count == 0 || aihc_in_nursery(capability, object)) {
    return;
  }
  if (capability->machine->gen2_cycle_active) {
    aihc_shade_elements(capability, object, offset, offset + count);
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
  aihc_remember(capability, object);
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

/* The words of an object. The kinds that compiled code allocates are read
   from the info table here, and the runtime records go to
   aihc_value_words. The collector asks once for each copy and once for
   each scanned gen1 object. */
static inline uint64_t aihc_gc_words(const AihcValue *value) {
  AihcObjectKind kind = aihc_value_kind(value);
  if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
      kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_INDIRECTION ||
      kind == AIHC_OBJECT_BLACKHOLE) {
    uint64_t fields = aihc_value_info_table(value)->field_count;
    if (fields == 0 && kind != AIHC_OBJECT_NODE &&
        kind != AIHC_OBJECT_CLOSURE) {
      fields = 1;
    }
    return 1 + fields;
  }
  return aihc_value_words(value);
}

static AihcValue *aihc_copy(AihcGcContext *context, AihcValue *value,
                            unsigned generation) {
  uint64_t words = aihc_gc_words(value);
  size_t bytes = sizeof(AihcSlot) * words;
  AihcValue *copy =
      (AihcValue *)(generation == 1
                        ? aihc_gen1_allocate(context->capability, bytes)
                        : aihc_generation_allocate(context->capability,
                                                   generation, bytes));
  context->copied_bytes += bytes;
  /* Most objects have a few words, and a call of memcpy costs more than
     the copy. */
  if (words <= 8) {
    AihcSlot *to = (AihcSlot *)copy;
    const AihcSlot *from = (const AihcSlot *)value;
    for (uint64_t index = 0; index < words; ++index) {
      to[index] = from[index];
    }
  } else {
    memcpy(copy, value, bytes);
  }
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
  /* A collection collects gen2 at most, so the first test also keeps the
     generation an index of the three fixed charges. It is explicit for the
     static analyzer, which does not know the bound of collected. */
  if (generation > 2 || generation > context->collected ||
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
  if (promoted == 2 && context->capability->machine->gen2_cycle_active) {
    /* The block enters gen2 black: the cycle keeps it. */
    block->bytes |= AIHC_PINNED_CYCLE_MARK;
  }
  /* The charge of the fixed block moves with it to its new generation. */
  uint64_t *fixed = context->capability->machine->fixed_generation_bytes;
  fixed[generation] -= aihc_pinned_bytes(block);
  fixed[promoted] += aihc_pinned_bytes(block);
  aihc_pinned_set_generation(block, promoted);
  aihc_value_worklist_push(&aihc_pinned_worklist, object);
}

/* The where of an evacuated frame. A frame counts as static for its
   referrer, unless the referrer is a thread record or a waiter, which reads
   the age of the chunk. */
#define AIHC_EVACUATED_FRAME (AIHC_GENERATION_STATIC + 1)

/* Copy or mark one object and return where it lives now. target is the
   lowest generation a copied object may land in, or zero when the referrer
   makes no demand. Heap indirections in a from-space are followed and not
   copied, so no indirection chain grows across collections. where receives
   the generation that holds the object after this collection: a frame, a
   static object, and a null pointer are older than every generation. The
   evacuation knows it from the region it looked up or from the copy, so
   the scan of the referrer does not look it up again. */
static AihcValue *aihc_evacuate_at(AihcGcContext *context, AihcValue *value,
                                   unsigned target, unsigned *where) {
  AihcCapability *capability = context->capability;
  for (;;) {
    if (value == NULL) {
      *where = AIHC_GENERATION_STATIC;
      return NULL;
    }
    unsigned source;
    if (aihc_in_nursery(capability, value)) {
      source = 0;
    } else {
      switch (aihc_region_kind(value)) {
      case AIHC_REGION_FROM1:
        source = 1;
        break;
      case AIHC_REGION_GEN1:
        *where = 1;
        return value;
      case AIHC_REGION_GEN2:
        *where = 2;
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
        aihc_mark_gen2(capability, value);
        return value;
      case AIHC_REGION_LARGE:
      case AIHC_REGION_PINNED:
        aihc_mark_pinned(context, value, target);
        *where = aihc_pinned_generation(aihc_pinned_block_of(value));
        return value;
      case AIHC_REGION_STACK:
        *where = AIHC_EVACUATED_FRAME;
        /* A frame of a young chunk is scanned from the worklist. A frame of
           an older chunk points only at objects of that generation or
           above, which this collection does not move. A dead object in the
           remembered set can name a frame that is popped, and new frames
           can be at its address, so only a live frame is scanned. */
        if (aihc_chunk_generation(aihc_stack_chunk_of(value)) <=
                context->collected &&
            aihc_frame_is_live(capability, value)) {
          aihc_mark_static(value);
        }
        return value;
      case AIHC_REGION_OUTSIDE:
        if (aihc_outside_is_pinned(value)) {
          aihc_mark_pinned(context, value, target);
          *where = aihc_pinned_generation(aihc_pinned_block_of(value));
        } else {
          *where = AIHC_GENERATION_STATIC;
          if (context->collected == 2) {
            aihc_mark_static(value);
          }
        }
        return value;
      default:
        aihc_fail("collector reached an invalid address");
      }
    }
    AihcSlot header = value->header;
    if (aihc_header_is_forward(header)) {
      /* An earlier reference copied the object into gen1 or gen2. */
      AihcValue *forward = aihc_header_forward(header);
      *where = aihc_region_kind(forward) == AIHC_REGION_GEN1 ? 1 : 2;
      return forward;
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
    *where = generation;
    return aihc_copy(context, value, generation);
  }
}

static AihcSlot aihc_evacuate_root(AihcSlot root, void *opaque_context) {
  unsigned where;
  return (AihcSlot)(uintptr_t)aihc_evacuate_at(
      opaque_context, (AihcValue *)(uintptr_t)root, 0, &where);
}

static AihcSlot aihc_scan_slot(AihcSlot slot, void *opaque_context) {
  AihcScanContext *scan = opaque_context;
  unsigned generation;
  AihcValue *value = aihc_evacuate_at(scan->gc, (AihcValue *)(uintptr_t)slot,
                                      scan->target, &generation);
  if (generation == AIHC_EVACUATED_FRAME) {
    generation = AIHC_GENERATION_STATIC;
  }
  if (value != NULL && !scan->from_frame &&
      generation == AIHC_GENERATION_STATIC &&
      aihc_region_kind(value) == AIHC_REGION_STACK) {
    /* A thread record or a waiter names a frame. The chunk of the frame is
       reached only through the record, so the record stays in the
       remembered set until the chunk is older than every collection that
       could scan it again: a chunk this collection scans is young, and
       another chunk has its own age. Without this rule, a record in gen2
       drops out of the set after one scan, and a later gen1 collection
       never reaches the frames of its thread. */
    unsigned age = aihc_chunk_generation(aihc_stack_chunk_of(value));
    generation = age <= scan->gc->collected ? 0 : age;
  }
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
  uint64_t stamp = context->capability->machine->gc_count;
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
  AihcCapability *capability = context->capability;
  AihcStackChunk *running =
      capability->stack_next == NULL
          ? NULL
          : aihc_stack_chunk_of(capability->stack_next - 1);
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
  /* The walk of a collection stops at the first chunk older than the
     collected generations, so no chunk may be younger than a chunk above
     it. A chunk whose frames refer to a young object takes a low age, and
     the chunks above it take that age as well: the walk then reaches it
     again. Without this rule a gen1 collection stopped at an older chunk
     above a chunk with a gen1 referent, and the referent moved away from
     under the frame. */
  for (size_t index = 0; index < context->touched_count; ++index) {
    AihcStackChunk *chunk = context->touched[index];
    unsigned generation = aihc_chunk_generation(chunk);
    for (AihcStackChunk *above = chunk->above; above != NULL;
         above = above->above) {
      if (aihc_chunk_generation(above) > generation) {
        aihc_chunk_set_generation(above, generation);
      }
    }
  }
  free(context->touched);
}

/* Visit the pointer fields of an object that is not a runtime record. */
static void aihc_visit_plain_fields(AihcValue *object, AihcRootVisitor visitor,
                                    void *context) {
  const AihcInfo *info = aihc_value_info_table(object);
  AihcObjectKind kind = info->object_kind;
  if (kind == AIHC_OBJECT_INDIRECTION) {
    object->fields[0] = visitor(object->fields[0], context);
  } else if (kind == AIHC_OBJECT_ARRAY) {
    uint64_t length = aihc_array_length(object);
    AihcSlot *elements = aihc_array_elements(object);
    for (uint64_t index = 0; index < length; ++index) {
      elements[index] = visitor(elements[index], context);
    }
  } else if (kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    /* Field zero holds the applied count, and the slots filled so far are
       a prefix of the saturated constructor's, so the shared bitmap
       answers for them. */
    uint64_t applied = aihc_partial_applied(object);
    AihcSlot *fields = aihc_partial_fields(object);
    for (uint64_t index = 0; index < applied; ++index) {
      if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
        fields[index] = visitor(fields[index], context);
      }
    }
  } else if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
             kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_BLACKHOLE) {
    for (uint64_t index = 0; index < info->field_count; ++index) {
      if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
        object->fields[index] = visitor(object->fields[index], context);
      }
    }
  } else {
    aihc_fail("collector encountered an invalid object kind");
  }
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
  /* Only a frame or a static object is scanned as static, so only those
     need the region lookup. */
  int is_frame = generation == AIHC_GENERATION_STATIC &&
                 aihc_region_kind(object) == AIHC_REGION_STACK;
  AihcScanContext scan = {
      .gc = context,
      .target = generation == AIHC_GENERATION_STATIC ? 2 : generation,
      .youngest = AIHC_GENERATION_STATIC,
      .from_frame = is_frame,
  };
  const AihcInfo *info = aihc_value_info_table(object);
  AihcObjectKind kind = aihc_value_kind(object);
  /* The kinds that compiled code allocates are the most frequent. Their
     pointer fields are scanned here, with no indirect call for each field.
     The other kinds go through the general visitors. */
  if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
      kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_BLACKHOLE) {
    if (context->collected == 2) {
      aihc_walk_srt(info->srt);
    }
    const uint8_t *pointers = info->field_is_pointer;
    if (pointers != NULL) {
      for (uint64_t index = 0; index < info->field_count; ++index) {
        if (pointers[index]) {
          object->fields[index] = aihc_scan_slot(object->fields[index], &scan);
        }
      }
    }
  } else if (!aihc_visit_runtime_object(object, aihc_scan_slot, &scan)) {
    if (context->collected == 2) {
      aihc_walk_srt(info->srt);
    }
    if (info->object_kind == AIHC_OBJECT_ARRAY && aihc_has_cards(object)) {
      aihc_scan_large_array(context, object, generation, 0);
      return;
    }
    aihc_visit_plain_fields(object, aihc_scan_slot, &scan);
  }
  /* A static object is traced at every full collection, so it needs an
     entry only for a referent in gen1, which a full collection does not
     reach through it: the entry keeps the referent alive and updated until
     then. A frame needs no entry: its chunk takes the youngest generation
     its frames refer to, and a collection of that generation scans it. */
  unsigned demand = generation == AIHC_GENERATION_STATIC ? 2 : generation;
  if (is_frame) {
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
  AihcGeneration *target = aihc_generation(context->capability, generation);
  AihcValue *object = (AihcValue *)target->scan;
  aihc_scan_object(context, object, generation);
  target->scan += sizeof(AihcSlot) * aihc_gc_words(object);
}

/* Copying, marking, and table walking all feed one another, so run every
   worklist to quiescence. */
static void aihc_trace(AihcGcContext *context) {
  AihcCapability *capability = context->capability;
  for (;;) {
    /* Only a full collection walks the static reference tables in its copy
       phase. The tables a younger collection finds on the list are mark
       work of the gen2 cycle, and a slice does it. */
    if (context->collected == 2 && aihc_srt_worklist.count != 0) {
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
    if (aihc_generation_scan_pending(aihc_generation(capability, 1))) {
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
  AihcCapability *capability = context->capability;
  /* A hot object has one entry for each store since the last compaction,
     so the set is compacted before each entry costs a scan. */
  aihc_remembered_compact(capability);
  AihcValue **entries = capability->remembered;
  uint64_t count = capability->remembered_count;
  capability->remembered = NULL;
  capability->remembered_count = 0;
  capability->remembered_capacity = 0;
  for (uint64_t index = 0; index < count; ++index) {
    AihcValue *object = entries[index];
    if (aihc_in_nursery(capability, object)) {
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

/* Whether a pinned block survives: it is older than the collected
   generations, or marked. A block of gen2 is governed by the mark of the
   gen2 cycle, a younger one by the mark of the running collection. */
static int aihc_pinned_live(const AihcGcContext *context,
                            const AihcPinnedBlock *block) {
  unsigned generation = aihc_pinned_generation(block);
  if (generation > context->collected ||
      (context->finishing && generation < 2)) {
    return 1;
  }
  uint64_t mark = generation == 2 && context->collected == 2
                      ? AIHC_PINNED_CYCLE_MARK
                      : AIHC_PINNED_MARK;
  return (block->bytes & mark) != 0;
}

/* Return the relocated address only if tracing retained the object. */
static AihcValue *aihc_live_value(AihcGcContext *context, AihcValue *value) {
  AihcCapability *capability = context->capability;
  for (;;) {
    if (value == NULL) {
      return NULL;
    }
    if (!aihc_in_nursery(capability, value)) {
      switch (aihc_region_kind(value)) {
      case AIHC_REGION_FROM1:
        /* A from-space object: forwarded, followed, or dead, as below. */
        break;
      case AIHC_REGION_GEN2:
        if (context->collected < 2 || aihc_gen2_marked(capability, value)) {
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
      case AIHC_REGION_PINNED:
        return aihc_pinned_live(context, aihc_pinned_block_of(value)) ? value
                                                                      : NULL;
      case AIHC_REGION_OUTSIDE:
        if (aihc_outside_is_pinned(value)) {
          return aihc_pinned_live(context, aihc_pinned_block_of(value)) ? value
                                                                        : NULL;
        }
        return context->collected < 2 ||
                       aihc_address_set_contains(&aihc_cycle_statics, value)
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

/* Scan the suspended frames of every stack that the collection keeps. A
   thread record of an older generation survives without a trace, and a
   traced record does not name its stack. The frames of a suspended thread
   are reached only through its resumption or its waiter. When that waiter
   dies, for example with the MVar the thread blocked on, the stack stays
   while its frames hold pointers into the space that the collection frees.
   Thus the walk starts again at the top of each kept stack until no more
   threads become live. The running stack is a root. */
static void aihc_trace_kept_stacks(AihcGcContext *context) {
  AihcCapability *capability = context->capability;
  AihcStack *running =
      capability->stack_next == NULL
          ? NULL
          : aihc_stack_chunk_of(capability->stack_next - 1)->stack;
  int scanned;
  do {
    scanned = 0;
    for (AihcStack *stack = capability->machine->stacks; stack != NULL;
         stack = stack->next) {
      if (stack == running || stack->top == NULL ||
          aihc_chunk_generation(aihc_stack_chunk_of(stack->top)) >
              context->collected ||
          aihc_address_set_contains(&aihc_marked_statics, stack->top) ||
          aihc_live_value(context, (AihcValue *)stack->thread) == NULL) {
        continue;
      }
      aihc_mark_static(stack->top);
      scanned = 1;
    }
    if (scanned) {
      aihc_trace(context);
    }
  } while (scanned);
}

/* Release the stacks of the threads this collection did not retain, and the
   chunks above the one after the running chunk. Frames above the stack
   pointer are dead, and one spare chunk stops a loop at a chunk boundary
   from allocating a chunk on every push. */
static void aihc_sweep_stacks(AihcGcContext *context) {
  AihcCapability *capability = context->capability;
  AihcStack *stack = capability->machine->stacks;
  while (stack != NULL) {
    AihcStack *next = stack->next;
    AihcThread *thread =
        (AihcThread *)aihc_live_value(context, (AihcValue *)stack->thread);
    if (thread == NULL) {
      aihc_stack_release(capability, stack);
    } else {
      stack->thread = thread;
    }
    stack = next;
  }
  if (capability->stack_next == NULL) {
    return;
  }
  AihcStackChunk *spare =
      aihc_stack_chunk_of(capability->stack_next - 1)->above;
  if (spare == NULL) {
    return;
  }
  AihcStackChunk *chunk = spare->above;
  spare->above = NULL;
  while (chunk != NULL) {
    AihcStackChunk *above = chunk->above;
    aihc_stack_chunk_free(capability, chunk);
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
  AihcStableName *old = context->capability->machine->stable_names;
  AihcStableName **tail = &context->capability->machine->stable_names;
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

/* Remove the charge of a fixed block that the sweep frees from the fixed
   byte counts. */
static void aihc_fixed_uncharge(AihcCapability *capability,
                                const AihcPinnedBlock *block) {
  uint64_t bytes = aihc_pinned_bytes(block);
  capability->machine->fixed_bytes -= bytes;
  capability->machine->fixed_generation_bytes[aihc_pinned_generation(block)] -=
      bytes;
}

/* Free the pinned blocks of the collected generations that nothing reached,
   and clear the marks of the others. */
static void aihc_sweep_pinned(AihcGcContext *context) {
  AihcCapability *capability = context->capability;
  AihcPinnedBlock **link = &capability->machine->pinned_blocks;
  while (*link != NULL) {
    AihcPinnedBlock *block = *link;
    if ((block->bytes & AIHC_PINNED_MARK) != 0) {
      block->bytes &= ~AIHC_PINNED_MARK;
      link = &block->next;
    } else if (aihc_pinned_generation(block) <= context->collected) {
      *link = block->next;
      aihc_fixed_uncharge(capability, block);
      aihc_pinned_block_release(block);
    } else {
      link = &block->next;
    }
  }
}

/* Free the gen2 blocks of the pinned list the cycle did not mark, and
   clear the cycle marks of the others. */
static void aihc_sweep_pinned_cycle(AihcCapability *capability) {
  AihcPinnedBlock **link = &capability->machine->pinned_blocks;
  while (*link != NULL) {
    AihcPinnedBlock *block = *link;
    if (aihc_pinned_generation(block) != 2) {
      link = &block->next;
    } else if ((block->bytes & AIHC_PINNED_CYCLE_MARK) != 0) {
      block->bytes &= ~AIHC_PINNED_CYCLE_MARK;
      link = &block->next;
    } else {
      *link = block->next;
      aihc_fixed_uncharge(capability, block);
      aihc_pinned_block_release(block);
    }
  }
}

/* The incremental marking of gen2. See docs/gc-design.md. */

/* Whether an object is one the cycle marks: in gen2, a pinned block of
   gen2, or static. Young objects are newer than the snapshot or are kept
   by the young collections. */
static int aihc_marks_object(const AihcValue *object) {
  switch (aihc_region_kind(object)) {
  case AIHC_REGION_GEN2:
    return 1;
  case AIHC_REGION_LARGE:
  case AIHC_REGION_PINNED:
    return aihc_pinned_generation(aihc_pinned_block_of(object)) == 2;
  case AIHC_REGION_OUTSIDE:
    return !aihc_outside_is_pinned(object) ||
           aihc_pinned_generation(aihc_pinned_block_of(object)) == 2;
  default:
    return 0;
  }
}

/* Whether frame first is above frame second in the same stack. */
static int aihc_frame_above(const AihcValue *first, const AihcValue *second) {
  const AihcStackChunk *first_chunk = aihc_stack_chunk_of(first);
  const AihcStackChunk *second_chunk = aihc_stack_chunk_of(second);
  if (first_chunk != second_chunk) {
    return first_chunk->depth > second_chunk->depth;
  }
  return first > second;
}

/* Whether a frame is live: at or below the top of its stack. The running
   stack ends at the stack pointer, and a suspended one at the frame its
   thread suspended with. */
static int aihc_frame_is_live(const AihcCapability *capability,
                              const AihcValue *frame) {
  AihcStackChunk *chunk = aihc_stack_chunk_of(frame);
  AihcStack *stack = chunk->stack;
  if (stack == NULL) {
    return 0;
  }
  if (capability->stack_next != NULL) {
    AihcStackChunk *top = aihc_stack_chunk_of(capability->stack_next - 1);
    if (top->stack == stack) {
      return chunk->depth < top->depth ||
             (chunk == top && (const uint8_t *)frame < capability->stack_next);
    }
  }
  return stack->top != NULL && !aihc_frame_above(frame, stack->top);
}

/* Mark one old object and queue it for a scan. A gen2 indirection is
   marked like any object: the marker does not rewrite the fields that
   name it, so it must stay until a copy follows it. A frame is scanned at
   once with the frames below it in its chunk, when it is live and not
   scanned yet. */
static void aihc_mark_old(AihcCapability *capability, AihcValue *value) {
  if (value == NULL || aihc_in_nursery(capability, value)) {
    return;
  }
  AihcRegionLocation location = aihc_region_location(value);
  switch (location.kind) {
  case AIHC_REGION_GEN2:
    if (aihc_gen2_set_mark(capability, value, location.base)) {
      aihc_mark_push(value, 0);
    }
    return;
  case AIHC_REGION_LARGE:
  case AIHC_REGION_PINNED: {
    AihcPinnedBlock *block = aihc_pinned_block_of(value);
    if (aihc_pinned_generation(block) == 2 &&
        (block->bytes & AIHC_PINNED_CYCLE_MARK) == 0) {
      block->bytes |= AIHC_PINNED_CYCLE_MARK;
      aihc_mark_push(value, 0);
    }
    return;
  }
  case AIHC_REGION_OUTSIDE:
    if (aihc_outside_is_pinned(value)) {
      AihcPinnedBlock *block = aihc_pinned_block_of(value);
      if (aihc_pinned_generation(block) == 2 &&
          (block->bytes & AIHC_PINNED_CYCLE_MARK) == 0) {
        block->bytes |= AIHC_PINNED_CYCLE_MARK;
        aihc_mark_push(value, 0);
      }
    } else if (aihc_address_set_insert(&aihc_cycle_statics, value)) {
      aihc_mark_push(value, 0);
    }
    return;
  case AIHC_REGION_STACK:
    if (aihc_frame_is_live(capability, value)) {
      aihc_mark_frames(capability, value);
    }
    return;
  default:
    return;
  }
}

static AihcSlot aihc_mark_slot(AihcSlot slot, void *opaque_machine) {
  aihc_mark_old(opaque_machine, (AihcValue *)(uintptr_t)slot);
  return slot;
}

/* Mark ordinary fields directly. Keep the static references and pointer
   map checks that the general visitors use. */
static int aihc_mark_plain_object(AihcCapability *capability,
                                  AihcValue *object) {
  AihcObjectKind kind = aihc_value_kind(object);
  if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
      kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_BLACKHOLE) {
    const AihcInfo *info = aihc_value_info_table(object);
    aihc_walk_srt(info->srt);
    const uint8_t *pointers = info->field_is_pointer;
    if (pointers != NULL) {
      for (uint64_t index = 0; index < info->field_count; ++index) {
        if (pointers[index]) {
          aihc_mark_old(capability,
                        (AihcValue *)(uintptr_t)object->fields[index]);
        }
      }
    }
    return 1;
  }
  return 0;
}

/* Scan one object from the mark stack: mark its old referents. A boxed
   array is scanned in pieces, so no step depends on its size. Returns the
   bytes the step scanned. */
static uint64_t aihc_mark_scan(AihcCapability *capability, AihcValue *object,
                               uint64_t start) {
  if (aihc_mark_plain_object(capability, object)) {
    return sizeof(AihcSlot) * aihc_gc_words(object);
  }
  if (aihc_visit_runtime_object(object, aihc_mark_slot, capability)) {
    return sizeof(AihcSlot) * aihc_value_words(object);
  }
  const AihcInfo *info = aihc_value_info_table(object);
  aihc_walk_srt(info->srt);
  if (info->object_kind == AIHC_OBJECT_ARRAY) {
    uint64_t length = aihc_array_length(object);
    uint64_t end = start + AIHC_MARK_ARRAY_PIECE;
    if (end > length) {
      end = length;
    }
    AihcSlot *elements = aihc_array_elements(object);
    for (uint64_t index = start; index < end; ++index) {
      aihc_mark_old(capability, (AihcValue *)(uintptr_t)elements[index]);
    }
    if (end < length) {
      aihc_mark_push(object, end);
    }
    return sizeof(AihcSlot) * (end - start + (start == 0 ? 2 : 0));
  }
  aihc_visit_plain_fields(object, aihc_mark_slot, capability);
  return sizeof(AihcSlot) * aihc_value_words(object);
}

/* The scan of one frame: the parent is the highest frame below it in the
   same chunk, and below is the frame the chain continues with in a lower
   chunk. */
typedef struct {
  AihcCapability *capability;
  AihcStackChunk *chunk;
  AihcValue *frame;
  AihcValue *parent;
  AihcValue *below;
} AihcFrameScan;

static AihcSlot aihc_mark_frame_slot(AihcSlot slot, void *opaque_scan) {
  AihcFrameScan *scan = opaque_scan;
  AihcValue *value = (AihcValue *)(uintptr_t)slot;
  if (value == NULL || aihc_region_kind(value) != AIHC_REGION_STACK) {
    aihc_mark_old(scan->capability, value);
    return slot;
  }
  AihcStackChunk *chunk = aihc_stack_chunk_of(value);
  if (chunk == scan->chunk) {
    if (value < scan->frame && (scan->parent == NULL || value > scan->parent)) {
      scan->parent = value;
    }
  } else if (chunk->stack == scan->chunk->stack &&
             chunk->depth < scan->chunk->depth &&
             (scan->below == NULL || aihc_frame_above(value, scan->below))) {
    scan->below = value;
  }
  return slot;
}

/* Record a frame that a slice must scan in a stack. The frame is the
   highest frame of a chunk below a chunk that the cycle scanned. A frame
   that an earlier scan of this cycle covered needs nothing. Every other
   frame stays on the list. One deferred frame cannot stand for another: a
   scan that starts high in the stack stops at the first chunk the cycle
   scanned, and the chunks below that chunk can still wait for a scan. */
static void aihc_stack_defer(AihcCapability *capability, AihcStack *stack,
                             AihcValue *frame) {
  AihcStackChunk *chunk = aihc_stack_chunk_of(frame);
  if (chunk->scanned_cycle == capability->machine->gen2_epoch &&
      (uint8_t *)frame <= chunk->scanned_from) {
    return;
  }
  if (stack->pending_count != 0 &&
      stack->pending[stack->pending_count - 1] == frame) {
    return;
  }
  if (stack->pending_count == stack->pending_capacity) {
    stack->pending = aihc_worklist_grow(
        stack->pending, &stack->pending_capacity, sizeof(*stack->pending));
  }
  stack->pending[stack->pending_count++] = frame;
}

/* Scan a frame and the frames below it in its chunk, down to the frames an
   earlier scan of this cycle covered. The chunk records the highest frame
   it was scanned from. When the chain leaves the chunk, the frame below is
   deferred to a slice, or to the pop that enters it. Returns the bytes
   scanned. */
static uint64_t aihc_mark_frames_counted(AihcCapability *capability,
                                         AihcValue *frame) {
  AihcStackChunk *chunk = aihc_stack_chunk_of(frame);
  uint8_t *limit = NULL;
  if (chunk->scanned_cycle == capability->machine->gen2_epoch) {
    if ((uint8_t *)frame <= chunk->scanned_from) {
      return 0;
    }
    limit = chunk->scanned_from;
  }
  chunk->scanned_cycle = capability->machine->gen2_epoch;
  chunk->scanned_from = (uint8_t *)frame;
  uint64_t scanned = 0;
  AihcFrameScan scan = {.capability = capability, .chunk = chunk};
  while (frame != NULL && (limit == NULL || (uint8_t *)frame > limit)) {
    scan.frame = frame;
    scan.parent = NULL;
    scan.below = NULL;
    scanned += sizeof(AihcSlot) * aihc_value_words(frame);
    /* The code of the frame reaches static objects through its table. */
    aihc_walk_srt(aihc_value_info_table(frame)->srt);
    aihc_visit_plain_fields(frame, aihc_mark_frame_slot, &scan);
    frame = scan.parent;
  }
  if (frame == NULL && scan.below != NULL) {
    aihc_stack_defer(capability, chunk->stack, scan.below);
  }
  return scanned;
}

static void aihc_mark_frames(AihcCapability *capability, AihcValue *frame) {
  (void)aihc_mark_frames_counted(capability, frame);
}

/* The deletion barrier: before a store into an old object, the old values
   of its pointer fields are marked, so the snapshot of the cycle keeps
   what the store deletes. A thunk update deletes every field at once. */
static void aihc_shade_fields(AihcCapability *capability, AihcValue *object) {
  if (!aihc_marks_object(object)) {
    return;
  }
  if (aihc_mark_plain_object(capability, object) ||
      aihc_visit_runtime_object(object, aihc_mark_slot, capability)) {
    return;
  }
  /* A thunk update deletes the code of the thunk as well. The code reaches
     static objects through its table, and the objects that the code made
     after the snapshot can name them, so the table is shaded too. After
     the update, the marker finds an indirection, which has no table. */
  aihc_walk_srt(aihc_value_info_table(object)->srt);
  aihc_visit_plain_fields(object, aihc_mark_slot, capability);
}

static void aihc_shade_elements(AihcCapability *capability, AihcValue *array,
                                uint64_t first, uint64_t end) {
  if (!aihc_marks_object(array) ||
      aihc_value_kind(array) != AIHC_OBJECT_ARRAY) {
    return;
  }
  uint64_t length = aihc_array_length(array);
  if (end > length) {
    end = length;
  }
  AihcSlot *elements = aihc_array_elements(array);
  for (uint64_t index = first; index < end; ++index) {
    aihc_mark_old(capability, (AihcValue *)(uintptr_t)elements[index]);
  }
}

static AihcSlot aihc_snapshot_root(AihcSlot slot, void *opaque_machine) {
  AihcCapability *capability = opaque_machine;
  AihcValue *value = (AihcValue *)(uintptr_t)slot;
  if (value != NULL && aihc_region_kind(value) == AIHC_REGION_STACK) {
    /* A root names a live frame: the top frame of the running stack, or
       a frame below it. */
    aihc_mark_frames(capability, value);
  } else {
    aihc_mark_old(capability, value);
  }
  return slot;
}

/* Scan one young object at the snapshot: its old referents and the static
   objects its code reaches. */
static void aihc_mark_young_object(AihcCapability *capability,
                                   AihcValue *object) {
  if (aihc_mark_plain_object(capability, object) ||
      aihc_visit_runtime_object(object, aihc_mark_slot, capability)) {
    return;
  }
  aihc_walk_srt(aihc_value_info_table(object)->srt);
  aihc_visit_plain_fields(object, aihc_mark_slot, capability);
}

/* Scan the objects of one object range at the snapshot. */
static void aihc_mark_range(AihcCapability *capability, uint8_t *start,
                            uint8_t *end) {
  uint8_t *cursor = start;
  while (cursor < end) {
    AihcValue *object = (AihcValue *)cursor;
    if (object->header == 0) {
      cursor += sizeof(AihcSlot);
      continue;
    }
    aihc_mark_young_object(capability, object);
    cursor += sizeof(AihcSlot) * aihc_value_words(object);
  }
}

/* Take the snapshot of a cycle at the end of a collection that copied gen1
   into gen2. The roots and the top chunk of each stack are scanned at
   once, which is bounded. Lower chunks are deferred. The young objects
   that exist at the snapshot are scanned as roots too: the survivors of
   the nursery, which the collection put in gen1, and the pinned blocks
   that are still young. They are older than the snapshot, but the barrier
   does not shade young objects, so they are scanned now. The work is
   bounded by the nursery size. */
static void aihc_cycle_start(AihcCapability *capability, uint64_t root_count,
                             AihcSlot *roots, const AihcSrt *srt) {
  aihc_gen2_begin_marking(capability);
  for (const AihcHeapBlock *block = capability->machine->generations[0].first;
       block != NULL; block = block->link) {
    aihc_mark_range(capability, block->start, block->next);
  }
  aihc_visit_roots(capability, root_count, roots, aihc_snapshot_root,
                   capability);
  aihc_walk_srt(srt);
  for (AihcForeignFrame *frame = capability->foreign_frames; frame != NULL;
       frame = frame->previous) {
    aihc_walk_srt(frame->srt);
  }
  AihcStack *running =
      capability->stack_next == NULL
          ? NULL
          : aihc_stack_chunk_of(capability->stack_next - 1)->stack;
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    if (stack != running && stack->top != NULL) {
      aihc_mark_frames(capability, stack->top);
    }
  }
  for (AihcPinnedBlock *block = capability->machine->pinned_blocks;
       block != NULL; block = block->next) {
    if (aihc_pinned_generation(block) < 2) {
      aihc_mark_young_object(capability, (AihcValue *)block->object);
    }
  }
}

/* Drop the remembered set entries of heap objects the cycle did not mark.
   A dead old object stays in the set while its referents are younger than
   it is, and the cycle frees it, so the next collection must not scan it.
   A static object keeps its entry: it is never freed, and its entry keeps a
   young value that an update after the snapshot gave it alive and in place
   until the value is old. */
static void aihc_remembered_purge(AihcCapability *capability) {
  AihcGcContext context = {
      .capability = capability, .collected = 2, .finishing = 1};
  uint64_t kept = 0;
  for (uint64_t index = 0; index < capability->remembered_count; ++index) {
    AihcValue *object = capability->remembered[index];
    int live;
    switch (aihc_region_kind(object)) {
    case AIHC_REGION_GEN2:
      live = aihc_gen2_marked(capability, object);
      break;
    case AIHC_REGION_LARGE:
    case AIHC_REGION_PINNED:
      live = aihc_pinned_live(&context, aihc_pinned_block_of(object));
      break;
    case AIHC_REGION_OUTSIDE:
      live = !aihc_outside_is_pinned(object) ||
             aihc_pinned_live(&context, aihc_pinned_block_of(object));
      break;
    default:
      live = 1;
      break;
    }
    if (live) {
      capability->remembered[kept++] = object;
    }
  }
  capability->remembered_count = kept;
}

/* End the cycle: drop the weak references to unmarked objects, free the
   unmarked gen2 blocks, and hand the segments to the lazy sweep. */
static void aihc_cycle_finish(AihcCapability *capability) {
  AihcGcContext context = {
      .capability = capability, .collected = 2, .finishing = 1};
  aihc_remembered_purge(capability);
  aihc_update_stable_names(&context);
  aihc_sweep_stacks(&context);
  aihc_sweep_pinned_cycle(capability);
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    stack->pending_count = 0;
  }
  aihc_clear_srt_stamps();
  capability->machine->gen2_cycle_active = 0;
  capability->machine->generations[1].bytes =
      capability->machine->gen2_marked_bytes;
  /* The factor is at most AIHC_GEN2_FACTOR_MAX, so the product fits when
     the bytes leave the top bits clear. A check against a quotient would
     become a wide multiply that the wasm32 link does not provide. The
     limit grows from the live gen2 fixed blocks too, because the gen2 test
     counts them. */
  uint64_t limit = aihc_gen2_bytes(capability);
  if (limit > (UINT64_MAX >> AIHC_GEN2_FACTOR_BITS)) {
    limit = UINT64_MAX;
  } else {
    limit *= capability->machine->gen2_factor;
  }
  capability->machine->gen2_limit_bytes =
      limit < AIHC_GEN2_MINIMUM_BYTES ? AIHC_GEN2_MINIMUM_BYTES : limit;
  if (capability->machine->gc_full_count == UINT64_MAX) {
    aihc_fail("collection counter overflow");
  }
  ++capability->machine->gc_full_count;
}

/* Do mark work up to a budget of scanned bytes, and end the cycle when
   nothing is left: the mark stack, the static reference tables, and the
   deferred frames of every stack. */
static void aihc_mark_slice(AihcCapability *capability, uint64_t budget) {
  uint64_t done = 0;
  while (done < budget) {
    if (aihc_srt_worklist.count != 0) {
      const AihcSrt *srt = aihc_srt_worklist.items[--aihc_srt_worklist.count];
      for (uintptr_t index = 0; index < srt->object_count; ++index) {
        aihc_mark_old(capability, (AihcValue *)srt->entries[index]);
      }
      for (uintptr_t index = 0; index < srt->child_count; ++index) {
        aihc_walk_srt((const AihcSrt *)srt->entries[srt->object_count + index]);
      }
      done += sizeof(AihcSlot) * (srt->object_count + srt->child_count + 1);
      continue;
    }
    if (aihc_mark_stack.count != 0) {
      AihcMarkEntry entry = aihc_mark_stack.items[--aihc_mark_stack.count];
      done += aihc_mark_scan(capability, entry.object, entry.start);
      continue;
    }
    AihcStack *stack = capability->machine->stacks;
    while (stack != NULL && stack->pending_count == 0) {
      stack = stack->next;
    }
    if (stack != NULL) {
      AihcValue *frame = stack->pending[--stack->pending_count];
      done += aihc_mark_frames_counted(capability, frame);
      continue;
    }
    aihc_cycle_finish(capability);
    return;
  }
}

/* The budget of the slice at the end of a collection: the work the
   promotions since the last slice owe, within the floor and the cap. A
   collection that copied more than the cap may mark as much as it copied:
   the slice then adds at most the time of the copy to the pause, and a
   gen1 collection that copied gen1 into gen2 marks the data it added. */
static uint64_t aihc_slice_budget(AihcCapability *capability, uint64_t copied) {
  uint64_t budget = capability->machine->mark_debt;
  uint64_t cap = capability->machine->mark_slice_cap;
  if (copied > cap) {
    cap = copied;
  }
  if (budget < capability->machine->mark_slice_floor) {
    budget = capability->machine->mark_slice_floor;
  }
  if (budget > cap) {
    budget = cap;
  }
  capability->machine->mark_debt = capability->machine->mark_debt > budget
                                       ? capability->machine->mark_debt - budget
                                       : 0;
  return budget;
}

void aihc_heap_account(AihcCapability *capability) {
  size_t taken = (size_t)(capability->heap_next - capability->heap_alloc_base);
  if ((uint64_t)taken > UINT64_MAX - capability->heap_allocated_bytes) {
    aihc_fail("allocated byte counter overflow");
  }
  capability->heap_allocated_bytes += (uint64_t)taken;
  capability->heap_alloc_base = capability->heap_next;
}

void aihc_gc_record_peak(AihcCapability *capability) {
  uint64_t used = aihc_occupied_bytes(capability);
  if (used > capability->machine->heap_peak_bytes) {
    capability->machine->heap_peak_bytes = used;
  }
}

/* The oldest generation the next collection copies. Gen1 goes when it is
   above its maximum, gen2 when it has grown past its limit, and everything
   when the -M limit is near. The size of gen1 and gen2 includes their fixed
   blocks. */
static unsigned aihc_choose_generation(const AihcCapability *capability,
                                       uint64_t required_bytes) {
  unsigned generation = 0;
  if (aihc_gen1_bytes(capability) >= capability->machine->gen1_max_bytes) {
    generation = 1;
  }
  uint64_t limit = capability->machine->gen2_limit_bytes;
  if (aihc_gen2_bytes(capability) >= limit &&
      !capability->machine->gen2_cycle_active) {
    /* Gen2 is above its limit. A gen1 collection copies gen1 into gen2,
       and a cycle starts at its end. */
    generation = 1;
  }
  if (capability->machine->gen2_cycle_active) {
    /* A cycle that has not finished when gen2 has doubled since its
       snapshot, and is above twice its limit, is finished in one pause:
       this is the degradation mode. The second bound matters for a cycle
       that a test started below the limit. */
    uint64_t start = capability->machine->gen2_cycle_start_bytes;
    if (start < limit) {
      start = limit;
    }
    uint64_t doubled = start > UINT64_MAX / 2 ? UINT64_MAX : 2 * start;
    if (aihc_gen2_bytes(capability) >= doubled) {
      generation = 2;
    }
  }
  if (capability->machine->heap_limit_enabled &&
      aihc_old_bytes(capability) + required_bytes >
          capability->machine->heap_max_bytes) {
    generation = 2;
  }
  return generation;
}

/* Collect the generations up to collected. A full collection also runs a
   whole gen2 cycle in this pause. With start_cycle, or when gen2 is above
   its limit, a gen1 collection starts an incremental cycle at its end. */
static void aihc_collect(AihcCapability *capability, unsigned collected,
                         int start_cycle, uint64_t root_count, AihcSlot *roots,
                         const AihcSrt *srt) {
  uint64_t started_ns = aihc_host_monotonic_ns();
  aihc_gc_record_peak(capability);
  /* Everything the mutator took from the nursery it is about to leave. */
  aihc_heap_account(capability);
  if (capability->machine->gc_count == UINT64_MAX) {
    aihc_fail("collection counter overflow");
  }
  ++capability->machine->gc_count;
  /* Clear old charge stamps before the 32-bit counter repeats. */
  if ((uint32_t)capability->machine->gc_count == UINT32_MAX) {
    for (AihcStack *stack = capability->machine->stacks; stack != NULL;
         stack = stack->next) {
      for (AihcStackChunk *chunk = stack->base; chunk != NULL;
           chunk = chunk->above) {
        chunk->charged_gc = UINT32_MAX;
      }
    }
  }
  capability->machine->gc_last_generation = collected;
  if (collected == 0) {
    ++capability->machine->gc_minor_count;
  } else if (collected == 1) {
    ++capability->machine->gc_gen1_count;
  }
  AihcGcContext context = {.capability = capability, .collected = collected};

  /* Detach the gen1 blocks this collection copies away, so gen1 fills fresh
     blocks. A full collection marks gen2 in place. */
  AihcHeapBlock *from1 = NULL;
  AihcGeneration *gen1 = aihc_generation(capability, 1);
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
    aihc_gen2_begin_marking(capability);
  }

  /* The static reference tables waiting for a walk belong to the gen2
     cycle, which keeps them across collections. A full collection started
     a new cycle above and walks them in its copy phase. */
  aihc_address_set_clear(&aihc_marked_statics);
  aihc_static_worklist.count = 0;
  aihc_pinned_worklist.count = 0;
  aihc_gen2_worklist.count = 0;
  if (collected == 2) {
    /* The table of the code that requested the collection, or NULL when
       that code reaches no static object of its own. */
    aihc_walk_srt(srt);
    for (AihcForeignFrame *frame = capability->foreign_frames; frame != NULL;
         frame = frame->previous) {
      aihc_walk_srt(frame->srt);
    }
  }
  aihc_visit_roots(capability, root_count, roots, aihc_evacuate_root, &context);
  aihc_scan_remembered(&context);
  aihc_trace(&context);
  aihc_trace_kept_stacks(&context);
  aihc_update_stable_names(&context);
  aihc_sweep_stacks(&context);
  aihc_sweep_pinned(&context);
  aihc_age_chunks(&context);

  aihc_blocks_release(from1);
  capability->heap_next = capability->heap_start;
  capability->nursery_taken =
      capability->nursery_block_bytes < capability->nursery_bytes
          ? capability->nursery_block_bytes
          : capability->nursery_bytes;
  capability->heap_limit = capability->heap_start + capability->nursery_taken;
  capability->heap_alloc_base = capability->heap_next;
  capability->fixed_since_gc = 0;
  for (size_t index = 0; index < context.kept_count; ++index) {
    aihc_remember_unique(capability, context.kept[index]);
  }
  free(context.kept);
  if (collected == 1 && !capability->machine->gen2_cycle_active &&
      (start_cycle ||
       aihc_gen2_bytes(capability) >= capability->machine->gen2_limit_bytes)) {
    aihc_cycle_start(capability, root_count, roots, srt);
  }
  if (capability->machine->gen2_cycle_active) {
    /* A full collection marked everything it reached: the slice runs the
       cycle to its end. */
    aihc_mark_slice(capability,
                    collected == 2
                        ? UINT64_MAX
                        : aihc_slice_budget(capability, context.copied_bytes));
  }
  if (!capability->machine->gen2_cycle_active) {
    aihc_sweep_slice(capability);
  }
  capability->machine->heap_live_bytes = aihc_occupied_bytes(capability);
#ifdef AIHC_GC_VERIFY
  aihc_gc_verify(capability, root_count, roots, srt);
#endif
  uint64_t pause_ns = aihc_host_monotonic_ns() - started_ns;
  capability->machine->gc_time_ns += pause_ns;
  if (pause_ns > capability->machine->gc_max_pause_ns) {
    capability->machine->gc_max_pause_ns = pause_ns;
  }
}

static _Noreturn void aihc_heap_exhausted(void) {
  aihc_fail("heap limit exceeded");
}

/* Collect for a reservation, escalating to a full collection when the -M
   limit demands it, and stop the program when even that does not fit. */
static void aihc_collect_for(AihcCapability *capability, size_t required_bytes,
                             uint64_t root_count, AihcSlot *roots,
                             const AihcSrt *srt) {
  unsigned collected = aihc_choose_generation(capability, required_bytes);
  aihc_collect(capability, collected, 0, root_count, roots, srt);
  if (capability->machine->heap_limit_enabled &&
      aihc_old_bytes(capability) + required_bytes >
          capability->machine->heap_max_bytes) {
    if (collected < 2) {
      aihc_collect(capability, 2, 0, root_count, roots, srt);
    }
    if (aihc_old_bytes(capability) + required_bytes >
        capability->machine->heap_max_bytes) {
      aihc_heap_exhausted();
    }
  }
}

void aihc_gc_collect_generation(AihcCapability *capability, unsigned generation,
                                uint64_t root_count, AihcSlot *roots,
                                const AihcSrt *srt) {
  aihc_collect(capability, generation > 2 ? 2 : generation, 0, root_count,
               roots, srt);
}

void aihc_gc_start_cycle(AihcCapability *capability, uint64_t root_count,
                         AihcSlot *roots, const AihcSrt *srt) {
  aihc_collect(capability, 1, 1, root_count, roots, srt);
}

static void aihc_nursery_acquire(AihcCapability *capability, size_t bytes) {
  capability->nursery_bytes = bytes;
  capability->heap_start =
      aihc_regions_acquire(aihc_regions_for_bytes(bytes), AIHC_REGION_NURSERY);
  capability->heap_next = capability->heap_start;
  capability->heap_alloc_base = capability->heap_start;
  capability->nursery_taken = capability->nursery_block_bytes < bytes
                                  ? capability->nursery_block_bytes
                                  : bytes;
  capability->heap_limit = capability->heap_start + capability->nursery_taken;
  aihc_nursery_start = capability->heap_start;
  aihc_nursery_bytes = bytes;
}

void aihc_nursery_replace(AihcCapability *capability, size_t bytes) {
  if (bytes == 0) {
    aihc_fail("the nursery needs at least one byte");
  }
  if (capability->heap_start != NULL) {
    aihc_regions_release(capability->heap_start);
  }
  aihc_nursery_acquire(capability, bytes);
}

void aihc_heap_reset(AihcCapability *capability, size_t nursery_bytes) {
  AihcGeneration *gen1 = aihc_generation(capability, 1);
  aihc_blocks_release(gen1->first);
  gen1->first = NULL;
  gen1->last = NULL;
  gen1->bytes = 0;
  gen1->scan_block = NULL;
  gen1->scan = NULL;
  aihc_gen2_release(capability);
  aihc_gen2_worklist.count = 0;
  aihc_mark_stack.count = 0;
  aihc_address_set_clear(&aihc_cycle_statics);
  aihc_clear_srt_stamps();
  aihc_srt_worklist.count = 0;
  capability->machine->gen2_cycle_active = 0;
  capability->machine->gen2_marked_bytes = 0;
  capability->machine->mark_debt = 0;
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    stack->pending_count = 0;
  }
  while (capability->machine->pinned_blocks != NULL) {
    AihcPinnedBlock *block = capability->machine->pinned_blocks;
    capability->machine->pinned_blocks = block->next;
    aihc_pinned_block_release(block);
  }
  capability->machine->fixed_bytes = 0;
  memset(capability->machine->fixed_generation_bytes, 0,
         sizeof(capability->machine->fixed_generation_bytes));
  capability->fixed_since_gc = 0;
  free(capability->remembered);
  capability->remembered = NULL;
  capability->remembered_count = 0;
  capability->remembered_capacity = 0;
  capability->remembered_repeatable = 0;
  aihc_nursery_replace(capability, nursery_bytes);
  capability->machine->gen2_limit_bytes = AIHC_GEN2_MINIMUM_BYTES;
}

/* Keep the block size within the address space of the target. A block does
   not need alignment because each refill extends one contiguous range. */
static void aihc_apply_block_option(AihcCapability *capability) {
  uint64_t bytes = aihc_rts_nursery_block_bytes();
  if (bytes == 0) {
    bytes = AIHC_NURSERY_BLOCK_BYTES;
  }
  if (bytes > SIZE_MAX) {
    aihc_fail("nursery block size is too large");
  }
  capability->nursery_block_bytes = bytes;
}

void aihc_gc_init(AihcCapability *capability) {
  aihc_apply_block_option(capability);
  uint64_t nursery = aihc_rts_nursery_bytes();
  aihc_nursery_acquire(capability, nursery == 0 ? AIHC_NURSERY_BYTES : nursery);
  capability->machine->gen1_max_bytes = AIHC_GEN1_MAX_BYTES;
  capability->machine->gen2_limit_bytes = AIHC_GEN2_MINIMUM_BYTES;
  capability->machine->gen2_factor = AIHC_GEN2_FACTOR;
  capability->machine->mark_factor = AIHC_MARK_FACTOR;
  capability->machine->mark_slice_floor = AIHC_MARK_SLICE_FLOOR;
  capability->machine->mark_slice_cap = AIHC_MARK_SLICE_CAP;
}

void aihc_gc_apply_options(AihcCapability *capability) {
  aihc_apply_block_option(capability);
  uint64_t nursery = aihc_rts_nursery_bytes();
  if (nursery != 0 && nursery != capability->nursery_bytes &&
      capability->heap_next == capability->heap_start) {
    aihc_nursery_replace(capability, nursery);
  }
  uint64_t gen1_max = aihc_rts_gen1_max_bytes();
  if (gen1_max != 0) {
    capability->machine->gen1_max_bytes = gen1_max;
  }
  uint64_t factor = aihc_rts_gen2_factor();
  if (factor != 0) {
    capability->machine->gen2_factor =
        factor > AIHC_GEN2_FACTOR_MAX ? AIHC_GEN2_FACTOR_MAX : factor;
  }
  uint64_t mark_factor = aihc_rts_mark_factor();
  if (mark_factor != 0) {
    capability->machine->mark_factor =
        mark_factor > AIHC_GEN2_FACTOR_MAX ? AIHC_GEN2_FACTOR_MAX : mark_factor;
  }
}

void aihc_gc_apply_limit(AihcCapability *capability) {
  if (capability->machine->heap_limit_enabled &&
      capability->nursery_bytes > capability->machine->heap_max_bytes &&
      capability->heap_next == capability->heap_start) {
    aihc_nursery_replace(capability,
                         capability->machine->heap_max_bytes == 0
                             ? 1
                             : (size_t)capability->machine->heap_max_bytes);
  }
}

/* The bytes of a reservation, which a request the address space or the heap
   limit cannot hold does not return from. */
static size_t aihc_reservation_bytes(const AihcCapability *capability,
                                     uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (capability->machine->heap_limit_enabled &&
      bytes > capability->machine->heap_max_bytes) {
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
static void aihc_ensure_large(AihcCapability *capability, size_t bytes,
                              uint64_t root_count, AihcSlot *roots,
                              const AihcSrt *srt) {
  int limit_near =
      capability->machine->heap_limit_enabled &&
      aihc_old_bytes(capability) + bytes > capability->machine->heap_max_bytes;
  if (capability->fixed_since_gc + bytes > capability->nursery_bytes ||
      limit_near) {
    aihc_collect_for(capability, bytes, root_count, roots, srt);
  }
}

/* Extend the block limit without a gap or a collection. Fixed allocation
   and stack charges use the same nursery budget as movable objects. */
static int aihc_nursery_refill(AihcCapability *capability, size_t bytes) {
  size_t used = (size_t)(capability->heap_next - capability->heap_start);
  uint64_t room = capability->nursery_bytes - used;
  if (capability->fixed_since_gc != 0 && capability->fixed_since_gc >= room) {
    return 0;
  }
  room -= capability->fixed_since_gc;
  if (bytes > room) {
    return 0;
  }
  uint64_t required = used + capability->fixed_since_gc + bytes;
  uint64_t taken = capability->nursery_taken;
  if (required > taken) {
    uint64_t block = capability->nursery_block_bytes;
    uint64_t extra = required - taken;
    uint64_t count = extra / block + (extra % block != 0);
    uint64_t left = capability->nursery_bytes - taken;
    taken += count > left / block ? left : count * block;
    capability->nursery_taken = taken;
  }
  capability->heap_limit =
      capability->heap_start + (taken - capability->fixed_since_gc);
  return 1;
}

/* Collect and reserve the requested space. Startup also uses this function
   to collect temporary imports before the heap limit applies. */
void aihc_gc_collect(AihcCapability *capability, uint64_t words,
                     uint64_t root_count, AihcSlot *roots, const AihcSrt *srt) {
  size_t bytes = aihc_reservation_bytes(capability, words);
  if (aihc_reservation_is_large(bytes)) {
    aihc_ensure_large(capability, bytes, root_count, roots, srt);
    return;
  }
  aihc_collect_for(capability, bytes, root_count, roots, srt);
  if (bytes > aihc_nursery_room(capability)) {
    /* The nursery is empty now, so a reservation it cannot hold grows it.
       Only a nursery far below the default, as the tests use, reaches this
       branch: a reservation below the large object bound fits the default. */
    aihc_nursery_replace(capability, bytes);
  }
  aihc_nursery_refill(capability, bytes);
}

/* Refill before a collection. Large objects retain their fixed budget. */
void aihc_gc_refill(AihcCapability *capability, uint64_t words,
                    uint64_t root_count, AihcSlot *roots, const AihcSrt *srt) {
  size_t bytes = aihc_reservation_bytes(capability, words);
  if (aihc_reservation_is_large(bytes)) {
    aihc_ensure_large(capability, bytes, root_count, roots, srt);
  } else if (!aihc_nursery_refill(capability, bytes)) {
    aihc_gc_collect(capability, words, root_count, roots, srt);
  }
}

/* Reserve for a caller that has not compared anything: the capability start-up
   path, the runtime units, and the C programs of the tests. */
void aihc_gc_ensure(AihcCapability *capability, uint64_t words,
                    uint64_t root_count, AihcSlot *roots, const AihcSrt *srt) {
  size_t bytes = aihc_reservation_bytes(capability, words);
  if (aihc_reservation_is_large(bytes)) {
    aihc_ensure_large(capability, bytes, root_count, roots, srt);
    return;
  }
  if (capability->heap_next > capability->heap_limit ||
      bytes > (size_t)(capability->heap_limit - capability->heap_next)) {
    aihc_gc_refill(capability, words, root_count, roots, srt);
  }
}

/* Put a block on the pinned list and charge it to the budget. The charge
   also shortens the nursery, so fixed allocation brings the next collection
   closer as nursery allocation does. Compiled code reserves a large object
   with one compare against the heap limit and allocates it through the
   runtime, so this is the only pressure a large object below the nursery
   size puts on the collector. */
static AihcValue *aihc_pinned_block_adopt(AihcCapability *capability,
                                          AihcPinnedBlock *block,
                                          size_t charge_bytes) {
  /* The cast keeps the compare meaningful on a 32-bit host. */
  if ((uint64_t)charge_bytes > AIHC_PINNED_BYTES_MASK) {
    aihc_fail("pinned allocation is too large");
  }
  aihc_heap_account(capability);
  if (charge_bytes > UINT64_MAX - capability->heap_allocated_bytes) {
    aihc_fail("allocated byte counter overflow");
  }
  block->bytes = charge_bytes;
  block->next = capability->machine->pinned_blocks;
  capability->machine->pinned_blocks = block;
  capability->machine->fixed_bytes += charge_bytes;
  /* A new fixed block is young, as a new object in the nursery is. */
  capability->machine->fixed_generation_bytes[0] += charge_bytes;
  aihc_charge_fixed(capability, charge_bytes);
  capability->heap_allocated_bytes += charge_bytes;
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

AihcValue *aihc_gc_allocate(AihcCapability *capability, uint64_t words) {
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
    return aihc_pinned_block_adopt(capability, block, bytes);
  }
  if (bytes > aihc_nursery_room(capability)) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  AihcValue *value = (AihcValue *)capability->heap_next;
  capability->heap_next += bytes;
#ifdef DEBUG
  memset(value, 0, bytes);
#endif
  return value;
}

/* Consume a prior reservation. The allocation list is not a root. */
AihcValue *aihc_gc_allocate_pinned(AihcCapability *capability, uint64_t words) {
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
    if (bytes > aihc_nursery_room(capability)) {
      aihc_fail("unchecked pinned allocation exceeded reserved heap");
    }
    block = calloc(1, bytes);
    if (block == NULL) {
      aihc_fail("out of memory");
    }
  }
  return aihc_pinned_block_adopt(capability, block, bytes);
}

/* The verifier. Under AIHC_GC_VERIFY, every collection ends with a walk of
   the objects the roots reach, and each step of the walk checks the
   invariants the collector relies on: a reachable object is in an occupied
   slot of a live block, its header names an info table, a reachable frame
   is live in a registered chunk, the remembered set and the blackhole
   table hold valid objects, the pending frames of a cycle are live, and the
   chunks of a stack grow older from its top. A violation stops the
   program at the collection that caused it. The test runtimes enable the
   flag; a release runtime does not pay for the walk. */

#ifdef AIHC_GC_VERIFY

static AihcAddressSet aihc_verify_seen;
static AihcAddressSet aihc_verify_pinned;
static AihcAddressSet aihc_verify_srts;
static AihcValueWorklist aihc_verify_work;
static AihcSrtWorklist aihc_verify_srt_work;
static const AihcCapability *aihc_verify_machine;
/* The object whose fields the walk scans, for the report of a violation. */
static const AihcValue *aihc_verify_parent;

static _Noreturn void aihc_verify_fail(const char *message,
                                       const AihcValue *value) {
  const AihcInfo *info = aihc_verify_parent == NULL
                             ? NULL
                             : aihc_value_info_table(aihc_verify_parent);
  fprintf(stderr,
          "aihc verifier: %s: value %p (region kind %d) reached from %p (kind "
          "%d, frame kind %d, identity %" PRIuPTR ")\n",
          message, (const void *)value, (int)aihc_region_kind(value),
          (const void *)aihc_verify_parent,
          info == NULL ? -1 : (int)info->object_kind,
          info == NULL ? -1 : (int)info->frame_kind,
          info == NULL ? (uintptr_t)0 : info->identity);
  aihc_fail(message);
}

static void aihc_verify_srt(const AihcSrt *srt) {
  if (srt == NULL || !aihc_address_set_insert(&aihc_verify_srts,
                                              (AihcValue *)(uintptr_t)srt)) {
    return;
  }
  if (aihc_verify_srt_work.count == aihc_verify_srt_work.capacity) {
    aihc_verify_srt_work.items = aihc_worklist_grow(
        aihc_verify_srt_work.items, &aihc_verify_srt_work.capacity,
        sizeof(*aihc_verify_srt_work.items));
  }
  aihc_verify_srt_work.items[aihc_verify_srt_work.count++] = srt;
}

/* Check one reachable value and queue it for a scan when it is new. */
static void aihc_verify_value(AihcValue *value) {
  const AihcCapability *capability = aihc_verify_machine;
  if (value == NULL) {
    return;
  }
  switch (aihc_region_kind(value)) {
  case AIHC_REGION_NURSERY:
    if (aihc_in_nursery(capability, value)) {
      aihc_fail("verifier: a reachable object is in the empty nursery");
    }
    aihc_fail("verifier: a reachable object is in a released nursery");
  case AIHC_REGION_FROM1:
    aihc_verify_fail("verifier: a reachable object is in a released gen1 block",
                     value);
  case AIHC_REGION_FREE:
    aihc_verify_fail("verifier: a reachable object is in a free region", value);
  case AIHC_REGION_GEN1: {
    int inside = 0;
    for (const AihcHeapBlock *block = capability->machine->generations[0].first;
         block != NULL; block = block->link) {
      if ((const uint8_t *)value >= block->start &&
          (const uint8_t *)value < block->next) {
        inside = 1;
        break;
      }
    }
    if (!inside) {
      aihc_fail("verifier: a reachable gen1 address is outside every block");
    }
    break;
  }
  case AIHC_REGION_GEN2: {
    AihcSegment *segment = aihc_segment_of(value);
    size_t offset =
        (size_t)((const uint8_t *)value - aihc_segment_slots(segment));
    if (offset % aihc_segment_slot_bytes(segment) != 0) {
      aihc_fail("verifier: a reachable gen2 address is not a slot start");
    }
    uint32_t slot = aihc_segment_slot_of(segment, value);
    if (slot >= segment->slot_count) {
      aihc_fail("verifier: a reachable gen2 address is past its segment");
    }
    if (!aihc_segment_occupied(segment, slot)) {
      aihc_fail("verifier: a reachable gen2 object is in a free slot");
    }
    break;
  }
  case AIHC_REGION_LARGE:
  case AIHC_REGION_PINNED: {
    AihcPinnedBlock *block = aihc_pinned_block_of(value);
    if (!aihc_address_set_contains(&aihc_verify_pinned, (AihcValue *)block)) {
      aihc_fail("verifier: a reachable fixed object is not on the pinned list");
    }
    if (aihc_pinned_generation(block) > 2) {
      aihc_fail("verifier: a reachable fixed object has an invalid generation");
    }
    break;
  }
  case AIHC_REGION_OUTSIDE:
    if (aihc_outside_is_pinned(value)) {
      AihcPinnedBlock *block = aihc_pinned_block_of(value);
      if (!aihc_address_set_contains(&aihc_verify_pinned, (AihcValue *)block)) {
        aihc_fail(
            "verifier: a reachable pinned object is not on the pinned list");
      }
    }
    break;
  case AIHC_REGION_STACK: {
    AihcStackChunk *chunk = aihc_stack_chunk_of(value);
    if (chunk->stack == NULL) {
      aihc_fail("verifier: a reachable frame is in a released chunk");
    }
    if ((const uint8_t *)value < aihc_stack_chunk_frames(chunk)) {
      aihc_fail("verifier: a reachable frame starts in a chunk header");
    }
    if (!aihc_frame_is_live(capability, value)) {
      aihc_fail("verifier: a reachable frame is popped");
    }
    break;
  }
  default:
    aihc_fail("verifier: a reachable address has an invalid region kind");
  }
  if (aihc_header_is_forward(value->header)) {
    aihc_fail("verifier: a reachable object has a forwarding header");
  }
  const AihcInfo *info = aihc_value_info_table(value);
  AihcObjectKind kind = info->object_kind;
  if (kind > AIHC_OBJECT_IO_HANDLE || kind == AIHC_OBJECT_RUNTIME ||
      kind == AIHC_OBJECT_BLACKHOLE_RECORD) {
    aihc_fail("verifier: a reachable object has an invalid kind");
  }
  if (aihc_region_kind(value) == AIHC_REGION_STACK &&
      (kind != AIHC_OBJECT_CLOSURE || info->frame_kind == AIHC_FRAME_NONE)) {
    aihc_fail("verifier: a reachable frame is not a continuation");
  }
  if (aihc_address_set_insert(&aihc_verify_seen, value)) {
    aihc_value_worklist_push(&aihc_verify_work, value);
  }
}

static AihcSlot aihc_verify_slot(AihcSlot slot, void *context) {
  (void)context;
  aihc_verify_value((AihcValue *)(uintptr_t)slot);
  return slot;
}

/* Scan one reachable object: its pointer fields, the static objects its
   code reaches, and for a frame the parent link. */
static void aihc_verify_scan(AihcValue *object) {
  aihc_verify_parent = object;
  if (aihc_visit_runtime_object(object, aihc_verify_slot, NULL)) {
    return;
  }
  const AihcInfo *info = aihc_value_info_table(object);
  aihc_verify_srt(info->srt);
  aihc_visit_plain_fields(object, aihc_verify_slot, NULL);
  if (aihc_region_kind(object) == AIHC_REGION_STACK && info->field_count != 0) {
    AihcValue *parent = (AihcValue *)(uintptr_t)object->fields[0];
    if (parent != NULL) {
      if (aihc_region_kind(parent) != AIHC_REGION_STACK ||
          aihc_stack_chunk_of(parent)->stack !=
              aihc_stack_chunk_of(object)->stack ||
          !aihc_frame_above(object, parent)) {
        aihc_fail("verifier: a frame's parent is not below it in its stack");
      }
    }
  }
}

/* Check one entry of the remembered set: an allocated old object. */
static void aihc_verify_remembered(const AihcValue *object) {
  switch (aihc_region_kind(object)) {
  case AIHC_REGION_GEN1:
  case AIHC_REGION_LARGE:
  case AIHC_REGION_PINNED:
  case AIHC_REGION_OUTSIDE:
    break;
  case AIHC_REGION_GEN2: {
    AihcSegment *segment = aihc_segment_of(object);
    if (!aihc_segment_occupied(segment,
                               aihc_segment_slot_of(segment, object))) {
      aihc_fail("verifier: the remembered set holds a freed gen2 object");
    }
    break;
  }
  default:
    aihc_fail("verifier: the remembered set holds an invalid address");
  }
  if (aihc_header_is_forward(object->header)) {
    aihc_fail("verifier: the remembered set holds a forwarded object");
  }
  if (aihc_value_info_table(object)->object_kind > AIHC_OBJECT_IO_HANDLE) {
    aihc_fail("verifier: the remembered set holds an invalid object kind");
  }
}

/* Check the stacks: every pending frame is live in its stack, and the
   chunks of a stack grow older from its top, so the walk of a collection
   that stops at the first older chunk misses nothing. */
static void aihc_verify_stacks(const AihcCapability *capability) {
  AihcStackChunk *running =
      capability->stack_next == NULL
          ? NULL
          : aihc_stack_chunk_of(capability->stack_next - 1);
  if (running != NULL && running->stack == NULL) {
    aihc_fail("verifier: the stack pointer is in a released chunk");
  }
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    for (size_t index = 0; index < stack->pending_count; ++index) {
      const AihcValue *frame = stack->pending[index];
      if (aihc_region_kind(frame) != AIHC_REGION_STACK ||
          aihc_stack_chunk_of(frame)->stack != stack) {
        aihc_fail("verifier: a pending frame is not in its stack");
      }
      if (!aihc_frame_is_live(capability, frame)) {
        aihc_fail("verifier: a pending frame is popped");
      }
    }
    AihcStackChunk *top = NULL;
    if (running != NULL && running->stack == stack) {
      top = running;
    } else if (stack->top != NULL) {
      top = aihc_stack_chunk_of(stack->top);
    }
    unsigned generation = 0;
    for (AihcStackChunk *chunk = top; chunk != NULL; chunk = chunk->below) {
      if (chunk->stack != stack) {
        aihc_fail("verifier: a chunk of a stack names another stack");
      }
      if (aihc_chunk_generation(chunk) < generation) {
        aihc_fail("verifier: a chunk is younger than the chunk above it");
      }
      generation = aihc_chunk_generation(chunk);
    }
  }
}

static void aihc_gc_verify(AihcCapability *capability, uint64_t root_count,
                           AihcSlot *roots, const AihcSrt *srt) {
  aihc_verify_machine = capability;
  aihc_verify_parent = NULL;
  aihc_address_set_clear(&aihc_verify_seen);
  aihc_address_set_clear(&aihc_verify_pinned);
  aihc_address_set_clear(&aihc_verify_srts);
  aihc_verify_work.count = 0;
  aihc_verify_srt_work.count = 0;
  uint64_t fixed[3] = {0, 0, 0};
  for (AihcPinnedBlock *block = capability->machine->pinned_blocks;
       block != NULL; block = block->next) {
    (void)aihc_address_set_insert(&aihc_verify_pinned, (AihcValue *)block);
    if (aihc_pinned_generation(block) > 2) {
      aihc_fail("verifier: a fixed block has an invalid generation");
    }
    fixed[aihc_pinned_generation(block)] += aihc_pinned_bytes(block);
  }
  /* The counts of each generation must agree with the pinned list. */
  for (unsigned generation = 0; generation < 3; ++generation) {
    if (fixed[generation] !=
        capability->machine->fixed_generation_bytes[generation]) {
      aihc_fail("verifier: the fixed bytes of a generation are wrong");
    }
  }
  if (fixed[0] + fixed[1] + fixed[2] != capability->machine->fixed_bytes) {
    aihc_fail("verifier: the fixed bytes are wrong");
  }
  aihc_verify_stacks(capability);
  for (uint64_t index = 0; index < capability->remembered_count; ++index) {
    aihc_verify_remembered(capability->remembered[index]);
  }
  aihc_visit_roots(capability, root_count, roots, aihc_verify_slot, NULL);
  aihc_verify_srt(srt);
  for (const AihcForeignFrame *frame = capability->foreign_frames;
       frame != NULL; frame = frame->previous) {
    aihc_verify_srt(frame->srt);
  }
  for (AihcStableName *name = capability->machine->stable_names; name != NULL;
       name = name->next) {
    aihc_verify_value((AihcValue *)name);
    aihc_verify_value(name->value);
  }
  const AihcBlackholeTable *table = capability->machine->blackholes;
  if (table != NULL) {
    for (size_t index = 0; index < table->capacity; ++index) {
      AihcValue *object = table->entries[index].object;
      if (object == NULL) {
        continue;
      }
      if (aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE ||
          (object->header & AIHC_HEADER_WAITERS) == 0) {
        aihc_fail("verifier: a blackhole table key is not a contended thunk");
      }
    }
  }
  for (;;) {
    if (aihc_verify_srt_work.count != 0) {
      const AihcSrt *table_entry =
          aihc_verify_srt_work.items[--aihc_verify_srt_work.count];
      for (uintptr_t index = 0; index < table_entry->object_count; ++index) {
        aihc_verify_value((AihcValue *)table_entry->entries[index]);
      }
      for (uintptr_t index = 0; index < table_entry->child_count; ++index) {
        aihc_verify_srt((const AihcSrt *)table_entry
                            ->entries[table_entry->object_count + index]);
      }
      continue;
    }
    if (aihc_verify_work.count != 0) {
      aihc_verify_scan(aihc_verify_work.items[--aihc_verify_work.count]);
      continue;
    }
    break;
  }
}

#endif

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

static void aihc_walk_segments(const AihcCapability *capability,
                               AihcSegment *segments, AihcObjectVisitor visitor,
                               void *context) {
  (void)capability;
  for (AihcSegment *segment = segments; segment != NULL;
       segment = segment->link) {
    for (uint32_t slot = 0; slot < segment->slot_count; ++slot) {
      if (aihc_segment_occupied(segment, slot)) {
        visitor((AihcValue *)(aihc_segment_slots(segment) +
                              (size_t)slot * aihc_segment_slot_bytes(segment)),
                context);
      }
    }
  }
}

void aihc_gc_walk_objects(AihcCapability *capability, AihcObjectVisitor visitor,
                          void *context) {
  aihc_walk_range(capability->heap_start, capability->heap_next, visitor,
                  context);
  for (const AihcHeapBlock *block = capability->machine->generations[0].first;
       block != NULL; block = block->link) {
    aihc_walk_range(block->start, block->next, visitor, context);
  }
  for (unsigned index = 0; index < AIHC_SIZE_CLASS_COUNT; ++index) {
    const AihcSizeClass *class = &aihc_size_classes[index];
    aihc_walk_segments(capability, class->current, visitor, context);
    aihc_walk_segments(capability, class->available, visitor, context);
    aihc_walk_segments(capability, class->filled, visitor, context);
    aihc_walk_segments(capability, class->unswept, visitor, context);
  }
  for (AihcPinnedBlock *block = capability->machine->pinned_blocks;
       block != NULL; block = block->next) {
    if (aihc_region_kind(block) == AIHC_REGION_LARGE) {
      visitor((AihcValue *)block->object, context);
    }
  }
}

void aihc_roots_enter(AihcCapability *capability, AihcRootFrame *frame,
                      uint64_t count, AihcSlot *roots) {
  *frame = (AihcRootFrame){
      .next = capability->root_frames, .roots = roots, .count = count};
  capability->root_frames = frame;
}

void aihc_roots_leave(AihcCapability *capability, AihcRootFrame *frame) {
  AihcRootFrame **link = &capability->root_frames;
  while (*link != frame) {
    if (*link == NULL) {
      aihc_fail("host scope is not active");
    }
    link = &(*link)->next;
  }
  *link = frame->next;
}

void aihc_visit_host_roots(AihcCapability *capability, AihcRootVisitor visitor,
                           void *context) {
  for (AihcRootFrame *frame = capability->root_frames; frame;
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

void *aihc_host_byte_array(AihcCapability *capability, AihcRootFrame *frame,
                           uint64_t bytes) {
  AihcRootFrame *active = capability->root_frames;
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
  aihc_gc_ensure(capability, words + sizeof(AihcPinnedBlock) / sizeof(AihcSlot),
                 0, NULL, NULL);
  AihcHostBuffer *buffer =
      (AihcHostBuffer *)aihc_gc_allocate_pinned(capability, words);
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
