/* Fuzz driver for the generational collector.

   The driver reads scripts from standard input. A script builds a heap and
   the stacks of several threads, changes them, and forces collections.
   After each collection the driver walks the heap and the stacks and prints
   every object, frame, thread, root, and static object. The test compares
   that report with a model of the same script.

   The driver plays the part of compiled code. It keeps the stack pointer of
   the running thread in the capability, pushes and enters frames as the
   continue helpers of the Lir runtime do, and gives each resumption the
   scheduler returns to the thread it names. Every blocking operation goes
   through the C runtime, so waiters, the run queue, and the blackhole table
   are the real ones.

   The driver keeps a table from object identity to the current address of
   the object. Every object gets its own info table, and the identity field
   of that table is the object identity. After a collection the driver
   rebuilds the address table from the heap, so a script can name an object
   after the object has moved. Frames never move, so a frame keeps its
   address until a pop releases it.

   Script commands, one for each line:

     machine G R S [L]    new machine with G globals, R root slots, a
                          nursery of S bytes, and a mark slice of L bytes
     capabilities C       give the machine C capabilities
     gen2limit B          start a gen2 cycle at the next collection of
                          gen1 when gen2 has B bytes or more
     heap K               allocate the objects of later commands in the
                          nursery of capability K
     srt I O C e...       reference table I names O static objects and C
                          child tables
     current_srt I|-1     pass table I as the running function's table to
                          every later collection
     ssrt K I|-1          give static object K the table I
     fill K               allocate garbage until K words remain
     reserve W            reserve W words; this can collect
     new ID KIND BITS S   new object with kind node, closure, thunk, or
                          partial, pointer bitmap BITS, and table S or -1
     array ID N S         new array with N elements and table S or -1
     mvar ID              new empty MVar
     set ID I V           write value V to field or element I
     supdate K V          turn static thunk K into an indirection to V
     sset K I V           write value V to field I of static node K
     global I V           write value V to global I
     root I V             write value V to root slot I
     stable ID            make a stable name for object ID
     promote ID G         promote object ID and store the result in global G;
                          this can collect
     push F normal BITS S v...   push a frame with pointer bitmap BITS for
                          the fields after the parent, table S, and values
     push F forward BITS S v...  the same for a frame without an entry
     push F catch S H     push a catch frame with handler H
     push F prompt S T    push a prompt frame with tag T
     push F update T      enter thunk T: push its update frame and mark it
     push F stop          push the frame at the bottom of a stack
     enter F [V]          continue into frame F; an update frame takes the
                          result V
     raise E              raise exception E on the running thread
     yield                give the processor to the next runnable thread
     fork T F A           new thread T whose stack starts with stop frame F,
                          and that applies A when it runs
     block T              block the running thread on blackholed thunk T
     take M, read M       take or read MVar M on the running thread
     put M V              put V into MVar M on the running thread
     collect [G]          collect the generations up to G, or the nursery
     cycle                collect the nursery and gen1 and start a gen2 cycle
     end                  finish the script

   Values: n (null), hID (heap object), fID (frame), sK (static slot),
   wHEX (raw word), and aID (the address of a heap object as a raw word).
   Static slots 0-7 are thunks, 8-11 are nodes with two pointer fields, and
   12-15 are nullary constructors. The initial thread has identity 1.

   The driver prints one block for each collection:

     collection C G       command C ran a collection of the generations up to G
     cycle start          a gen2 cycle took its snapshot at this collection
     finish               a gen2 cycle ended at this collection
     running hT
     queue hT...
     obj ID KIND N V...
     obj ID mvar full V|empty readers N (hT fF)... takers N (hT fF)...
                          putters N (hT fF V)...
     obj ID thread KIND FN CONT [VALUE]
     age ID G             object ID lives in generation G
     stack hT top fF|n    a live stack, followed by its frames from the top
     frame F KIND N V...  the parent is the first value
     global I V
     root I V
     stable V
     blackhole ID N (hT fF)...
     static K thunk | static K ind V | static K node V V
     violation TEXT
     endcollection

   and prints done after the end command. A fatal script error prints fail
   TEXT and stops the process. */

#include "aihc_runtime_internal.h"

/* The info table of an indirection, defined by aihc_helpers.lir. */
extern const AihcInfo aihc_indirection_info;

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
  STATIC_THUNKS = 8,
  STATIC_NODES = 4,
  STATIC_NULLARY = 4,
  STATIC_NODE_FIELDS = 2,
  STATIC_COUNT = STATIC_THUNKS + STATIC_NODES + STATIC_NULLARY,
  LINE_CAPACITY = 1 << 16,
  MAX_TOKENS = 4096,
  INITIAL_THREAD = 1,
};

typedef struct {
  AihcSlot header;
  AihcSlot target;
} StaticThunk;

typedef struct {
  AihcSlot header;
  AihcSlot fields[STATIC_NODE_FIELDS];
} StaticNode;

typedef struct {
  AihcSlot header;
} StaticNullary;

/* One heap object the script named. */
typedef struct {
  int defined;
  /* Set when an update turned the object into an indirection. The entry
     keeps the address while the indirection is in the heap: a value may
     still name it, and the collector follows it. */
  int indirection;
  int is_thread;
  int is_mvar;
  AihcValue *address;
  AihcInfo *info;
  uint8_t *pointers;
  uint64_t *shadow;
  uint8_t *has_shadow;
  uint64_t field_count;
  /* For a thread: the identity of its topmost live frame, or zero. */
  uint64_t top;
} Entry;

/* One continuation frame the script pushed. */
typedef struct {
  int defined;
  /* Null once the frame is popped or its stack is released. */
  AihcValue *address;
  AihcInfo *info;
  uint8_t *pointers;
  uint64_t *shadow;
  uint8_t *has_shadow;
  uint64_t field_count;
  uint64_t parent;
  uint64_t thread;
} Frame;

static AihcInfo static_thunk_info[STATIC_THUNKS];
static AihcInfo static_node_info[STATIC_NODES];
static AihcInfo static_nullary_info[STATIC_NULLARY];
static const uint8_t static_node_pointers[STATIC_NODE_FIELDS] = {1, 1};
static StaticThunk static_thunks[STATIC_THUNKS];
static StaticNode static_nodes[STATIC_NODES];
static StaticNullary static_nullary[STATIC_NULLARY];

/* The capability that runs the threads. */
static AihcCapability *capability;
/* The capability whose nursery gets the objects of the script, and its
   index. Runtime operations allocate in the nursery of the running
   capability. */
static AihcCapability *heap;
static uint64_t heap_index;
static Entry *entries;
static size_t entry_capacity;
static Frame *frames;
static size_t frame_capacity;
static AihcSrt **srts;
static size_t srt_count;
/* The table the script passes to each collection as the running function's
   table. */
static const AihcSrt *current_srt;
static AihcSlot *root_slots;
static uint64_t root_count;
static AihcStableName **stable_names;
static size_t stable_count;
/* The reserved words of each capability. */
#define MAX_CAPABILITIES 4
static uint64_t reserved[MAX_CAPABILITIES];
#define reserved_words reserved[heap_index]
/* Whether the script promoted an object. Only a promotion makes a second
   copy of an object. */
static int promoted_ever;
static size_t command_index;
static AihcValue **object_starts;
static size_t object_start_count;
static size_t object_start_capacity;
/* The identity of the running thread. */
static uint64_t running;
/* The collection counters at the last report, so the next report can say
   which generation a collection copied. */
static uint64_t reported_full_count;
/* Whether the collections of the current command include a bridge
   collection that a local collection does not show. */
static int bridge;
static uint64_t reported_cycle_active;
static int srts_linked;

static _Noreturn void fail(const char *message) {
  printf("fail %s\n", message);
  fflush(stdout);
  exit(1);
}

/* Violations are collected while a report line is in progress and printed
   before the report ends, so every report line stays whole. */
static const char **violations;
static size_t violation_count;
static size_t violation_capacity;

static void violation(const char *message) {
  if (violation_count == violation_capacity) {
    violation_capacity = violation_capacity == 0 ? 16 : violation_capacity * 2;
    const char **grown =
        realloc(violations, violation_capacity * sizeof(*violations));
    if (grown == NULL) {
      fail("out of memory");
    }
    violations = grown;
  }
  violations[violation_count++] = message;
}

static void print_violations(void) {
  for (size_t index = 0; index < violation_count; ++index) {
    printf("violation %s\n", violations[index]);
  }
  violation_count = 0;
}

static void *checked_calloc(size_t count, size_t size) {
  void *memory = calloc(count == 0 ? 1 : count, size);
  if (memory == NULL) {
    fail("out of memory");
  }
  return memory;
}

static uint64_t parse_unsigned(const char *token) {
  char *end = NULL;
  if (*token < '0' || *token > '9') {
    fail("expected a number");
  }
  uint64_t value = strtoull(token, &end, 10);
  if (*end != 0) {
    fail("invalid number");
  }
  return value;
}

static int64_t parse_signed(const char *token) {
  if (token[0] == '-') {
    return -(int64_t)parse_unsigned(token + 1);
  }
  return (int64_t)parse_unsigned(token);
}

static uint64_t parse_hex(const char *token) {
  char *end = NULL;
  uint64_t value = strtoull(token, &end, 16);
  if (*token == 0 || *end != 0) {
    fail("invalid hex word");
  }
  return value;
}

/* Static slot numbers cover the three pools in order. */
static AihcValue *static_slot_address(uint64_t slot) {
  if (slot < STATIC_THUNKS) {
    return (AihcValue *)&static_thunks[slot];
  }
  if (slot < STATIC_THUNKS + STATIC_NODES) {
    return (AihcValue *)&static_nodes[slot - STATIC_THUNKS];
  }
  if (slot < STATIC_COUNT) {
    return (AihcValue *)&static_nullary[slot - STATIC_THUNKS - STATIC_NODES];
  }
  fail("static slot out of range");
}

static int static_slot_of(const void *address, uint64_t *slot) {
  for (uint64_t index = 0; index < STATIC_COUNT; ++index) {
    if (static_slot_address(index) == address) {
      *slot = index;
      return 1;
    }
  }
  return 0;
}

static void initialize_statics(void) {
  for (int index = 0; index < STATIC_THUNKS; ++index) {
    static_thunk_info[index].object_kind = AIHC_OBJECT_THUNK;
    static_thunk_info[index].needs_eval = AIHC_NEEDS_EVAL_ENTER;
    static_thunk_info[index].frame_kind = AIHC_FRAME_NONE;
    static_thunk_info[index].identity = 0;
    static_thunks[index].header =
        (AihcSlot)(uintptr_t)&static_thunk_info[index];
  }
  for (int index = 0; index < STATIC_NODES; ++index) {
    static_node_info[index].object_kind = AIHC_OBJECT_NODE;
    static_node_info[index].frame_kind = AIHC_FRAME_NONE;
    static_node_info[index].field_count = STATIC_NODE_FIELDS;
    static_node_info[index].field_is_pointer = static_node_pointers;
    static_nodes[index].header = (AihcSlot)(uintptr_t)&static_node_info[index];
  }
  for (int index = 0; index < STATIC_NULLARY; ++index) {
    static_nullary_info[index].object_kind = AIHC_OBJECT_NODE;
    static_nullary_info[index].frame_kind = AIHC_FRAME_NONE;
    static_nullary[index].header =
        (AihcSlot)(uintptr_t)&static_nullary_info[index];
  }
}

/* Put every static object back into its initial state for a new script. */
static void reset_statics(void) {
  for (int index = 0; index < STATIC_THUNKS; ++index) {
    static_thunk_info[index].srt = NULL;
    static_thunks[index].header =
        (AihcSlot)(uintptr_t)&static_thunk_info[index];
    static_thunks[index].target = 0;
  }
  for (int index = 0; index < STATIC_NODES; ++index) {
    static_node_info[index].srt = NULL;
    for (int field = 0; field < STATIC_NODE_FIELDS; ++field) {
      static_nodes[index].fields[field] = 0;
    }
  }
}

static Entry *entry_of(uint64_t identity) {
  if (identity == 0) {
    fail("identity zero is reserved");
  }
  if (identity >= entry_capacity) {
    size_t capacity = entry_capacity == 0 ? 64 : entry_capacity;
    while (capacity <= identity) {
      capacity *= 2;
    }
    Entry *grown = realloc(entries, capacity * sizeof(*entries));
    if (grown == NULL) {
      fail("out of memory");
    }
    memset(grown + entry_capacity, 0,
           (capacity - entry_capacity) * sizeof(*entries));
    entries = grown;
    entry_capacity = capacity;
  }
  return &entries[identity];
}

static Entry *live_entry(uint64_t identity) {
  Entry *entry = entry_of(identity);
  if (!entry->defined) {
    fail("object is not defined");
  }
  if (entry->address == NULL) {
    fail("object is dead");
  }
  return entry;
}

static Frame *frame_of(uint64_t identity) {
  if (identity == 0) {
    fail("frame identity zero is reserved");
  }
  if (identity >= frame_capacity) {
    size_t capacity = frame_capacity == 0 ? 64 : frame_capacity;
    while (capacity <= identity) {
      capacity *= 2;
    }
    Frame *grown = realloc(frames, capacity * sizeof(*frames));
    if (grown == NULL) {
      fail("out of memory");
    }
    memset(grown + frame_capacity, 0,
           (capacity - frame_capacity) * sizeof(*frames));
    frames = grown;
    frame_capacity = capacity;
  }
  return &frames[identity];
}

static Frame *live_frame(uint64_t identity) {
  Frame *frame = frame_of(identity);
  if (!frame->defined) {
    fail("frame is not defined");
  }
  if (frame->address == NULL) {
    fail("frame is popped");
  }
  return frame;
}

/* The frame at an address, when the driver pushed it and it is live. */
static Frame *frame_at(const AihcValue *address) {
  if (address == NULL || aihc_region_kind(address) != AIHC_REGION_STACK) {
    return NULL;
  }
  /* A dead heap address can now name stack storage. It need not hold a
     frame header. Check the known frame addresses before a header access. */
  for (size_t identity = 1; identity < frame_capacity; ++identity) {
    Frame *frame = &frames[identity];
    if (frame->defined && frame->address == address &&
        aihc_value_info_table(address) == frame->info) {
      return frame;
    }
  }
  return NULL;
}

static void free_entries(void) {
  for (size_t index = 0; index < entry_capacity; ++index) {
    free(entries[index].info);
    free(entries[index].pointers);
    free(entries[index].shadow);
    free(entries[index].has_shadow);
  }
  free(entries);
  entries = NULL;
  entry_capacity = 0;
  for (size_t index = 0; index < frame_capacity; ++index) {
    free(frames[index].info);
    free(frames[index].pointers);
    free(frames[index].shadow);
    free(frames[index].has_shadow);
  }
  free(frames);
  frames = NULL;
  frame_capacity = 0;
}

static void free_srts(void) {
  for (size_t index = 0; index < srt_count; ++index) {
    free(srts[index]);
  }
  free(srts);
  srts = NULL;
  srt_count = 0;
}

static const AihcSrt *srt_of(int64_t index) {
  if (index < 0) {
    return NULL;
  }
  if ((uint64_t)index >= srt_count || srts[index] == NULL) {
    fail("reference table is not defined");
  }
  return srts[index];
}

/* Parse a value token. A raw word is a non-pointer payload. */
static AihcSlot parse_value(const char *token, int *is_word) {
  *is_word = 0;
  switch (token[0]) {
  case 'n':
    return 0;
  case 'h':
    return (AihcSlot)(uintptr_t)live_entry(parse_unsigned(token + 1))->address;
  case 'f':
    return (AihcSlot)(uintptr_t)live_frame(parse_unsigned(token + 1))->address;
  case 's':
    return (AihcSlot)(uintptr_t)static_slot_address(parse_unsigned(token + 1));
  case 'w':
    *is_word = 1;
    return parse_hex(token + 1);
  case 'a':
    *is_word = 1;
    return (AihcSlot)(uintptr_t)live_entry(parse_unsigned(token + 1))->address;
  default:
    fail("invalid value");
  }
}

static AihcSlot parse_pointer(const char *token) {
  int is_word = 0;
  AihcSlot value = parse_value(token, &is_word);
  if (is_word) {
    fail("raw word in a pointer slot");
  }
  return value;
}

static AihcValue *parse_object(const char *token) {
  AihcSlot value = parse_pointer(token);
  if (value == 0) {
    fail("null where an object is required");
  }
  return (AihcValue *)(uintptr_t)value;
}

/* The words below the heap limit. A large object adopted after a
   reservation lowers the limit, and a runtime allocation from that
   reservation can then pass it, so the limit can be below the allocation
   pointer. */
static size_t remaining_words(void) {
  if (heap->heap_next >= heap->heap_limit) {
    return 0;
  }
  return (size_t)(heap->heap_limit - heap->heap_next) / sizeof(AihcSlot);
}

/* Take the words a runtime call on the running capability allocated from
   the reservation. */
static void consume_reservation(const uint8_t *before) {
  uint64_t words =
      (uint64_t)(capability->heap_next - before) / sizeof(AihcSlot);
  if (words > reserved[0]) {
    fail("runtime allocation exceeds its reservation");
  }
  reserved[0] -= words;
}

static int in_range(const void *address, const uint8_t *start, size_t bytes) {
  uintptr_t value = (uintptr_t)address;
  uintptr_t first = (uintptr_t)start;
  return start != NULL && value >= first && value - first < bytes;
}

static int is_object_start(const AihcValue *object) {
  size_t low = 0;
  size_t high = object_start_count;
  while (low < high) {
    size_t middle = low + (high - low) / 2;
    if (object_starts[middle] == object) {
      return 1;
    }
    if ((uintptr_t)object_starts[middle] < (uintptr_t)object) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  return 0;
}

/* Whether a thunk with bit zero is evaluated: field zero is its value, not
   its owner thread or the blackhole queue of its owner. */
static int thunk_is_evaluated(const AihcValue *object) {
  const AihcValue *word = (const AihcValue *)(uintptr_t)object->fields[0];
  AihcObjectKind kind = aihc_value_info_table(word)->object_kind;
  return kind != AIHC_OBJECT_THREAD && kind != AIHC_OBJECT_BLACKHOLE_QUEUE;
}

/* Whether an object stands for the object that field zero names: an
   indirection, or an evaluated thunk. */
static int is_indirection(const AihcValue *object) {
  if (aihc_value_kind(object) == AIHC_OBJECT_BLACKHOLE) {
    return thunk_is_evaluated(object);
  }
  return aihc_value_kind(object) == AIHC_OBJECT_INDIRECTION;
}

/* A pointer to a heap indirection names its target: the mutator follows
   indirections and evaluated thunks, and the collector removes only the
   ones it copies away. */
static const AihcValue *follow_indirections(const AihcValue *object) {
  for (int fuel = 0; fuel < 1000; ++fuel) {
    if (!is_object_start(object) || !is_indirection(object)) {
      return object;
    }
    object = (const AihcValue *)(uintptr_t)object->fields[0];
  }
  violation("indirection chain is too long");
  return object;
}

/* Print one slot as the model names it: n for null, h and the identity of a
   heap object, f and the identity of a live frame, s and the slot of a
   static object, or o for an address that no object starts at. The last one
   is a stale pointer: the memory it names left the heap. An object that
   nothing reaches may hold one, so it is not a violation here, and the model
   compares the fields of every live object. */
static void print_pointer(AihcSlot slot) {
  const void *address = (const void *)(uintptr_t)slot;
  uint64_t static_slot = 0;
  if (slot == 0) {
    printf(" n");
    return;
  }
  if (aihc_region_kind(address) == AIHC_REGION_STACK) {
    Frame *frame = frame_at(address);
    if (frame == NULL) {
      printf(" o");
    } else {
      printf(" f%" PRIuPTR, aihc_value_info_table(frame->address)->identity);
    }
    return;
  }
  const AihcValue *object = follow_indirections(address);
  if (is_object_start(object)) {
    printf(" h%" PRIuPTR, aihc_value_info_table(object)->identity);
    return;
  }
  if (static_slot_of(object, &static_slot)) {
    printf(" s%" PRIu64, static_slot);
    return;
  }
  printf(" o");
}

static const char *kind_name(AihcObjectKind kind) {
  switch (kind) {
  case AIHC_OBJECT_NODE:
    return "node";
  case AIHC_OBJECT_CLOSURE:
    return "closure";
  case AIHC_OBJECT_THUNK:
    return "thunk";
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR:
    return "partial";
  case AIHC_OBJECT_INDIRECTION:
    return "indirection";
  case AIHC_OBJECT_BLACKHOLE:
    return "blackhole";
  case AIHC_OBJECT_ARRAY:
    return "array";
  case AIHC_OBJECT_THREAD:
    return "thread";
  case AIHC_OBJECT_MVAR:
    return "mvar";
  default:
    return "invalid";
  }
}

static const char *frame_kind_name(AihcFrameKind kind) {
  switch (kind) {
  case AIHC_FRAME_NORMAL:
    return "normal";
  case AIHC_FRAME_CATCH:
    return "catch";
  case AIHC_FRAME_UPDATE:
    return "update";
  case AIHC_FRAME_STOP:
    return "stop";
  case AIHC_FRAME_PROMPT:
    return "prompt";
  case AIHC_FRAME_FORWARD:
    return "forward";
  default:
    return "invalid";
  }
}

static const char *resume_kind_name(AihcResumeKind kind) {
  switch (kind) {
  case AIHC_RESUME_NONE:
    return "none";
  case AIHC_RESUME_APPLY:
    return "apply";
  case AIHC_RESUME_CONTINUE:
    return "continue";
  case AIHC_RESUME_RAISE:
    return "raise";
  default:
    return "invalid";
  }
}

/* The payload of one object. A partial constructor spends field zero on the
   count of the slots it has filled, and a thunk spends it on its owner word,
   so their fields start one slot later. */
static AihcSlot *entry_fields(Entry *entry) {
  if (entry->info->object_kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return aihc_partial_fields(entry->address);
  }
  if (entry->info->object_kind == AIHC_OBJECT_THUNK) {
    return entry->address->fields + 1;
  }
  return entry->address->fields;
}

static void print_waiters(const AihcMVarWaiter *waiter, int with_value) {
  size_t count = 0;
  for (const AihcMVarWaiter *cursor = waiter; cursor != NULL;
       cursor = cursor->next) {
    ++count;
  }
  printf(" %zu", count);
  for (const AihcMVarWaiter *cursor = waiter; cursor != NULL;
       cursor = cursor->next) {
    print_pointer((AihcSlot)(uintptr_t)cursor->thread);
    print_pointer((AihcSlot)(uintptr_t)cursor->continuation);
    if (with_value) {
      print_pointer(cursor->value);
    }
  }
}

/* The identity of a filler piece. No entry has it. */
#define FILLER_IDENTITY UINTPTR_MAX

/* Whether an object is in the allocated part of a nursery. A local
   collection resets only its own nursery, so a nursery of another
   capability keeps its garbage. */
static int in_some_nursery(const AihcValue *object) {
  const AihcMachine *machine = capability->machine;
  for (uint64_t index = 0; index < machine->capability_count; ++index) {
    const AihcCapability *each = machine->capabilities[index];
    if ((const uint8_t *)object >= each->heap_start &&
        (const uint8_t *)object < each->heap_next) {
      return 1;
    }
  }
  return 0;
}

static void print_object(AihcValue *object) {
  AihcObjectKind kind = aihc_value_kind(object);
  if (kind == AIHC_OBJECT_MVAR_WAITER || kind == AIHC_OBJECT_BLACKHOLE_WAITER ||
      kind == AIHC_OBJECT_STABLE_NAME || kind == AIHC_OBJECT_INDIRECTION ||
      kind == AIHC_OBJECT_BYTE_ARRAY || kind == AIHC_OBJECT_BLACKHOLE_QUEUE ||
      is_indirection(object)) {
    /* Runtime records without an identity of their own. The owners of the
       waiters report them, and an indirection reports only its age. */
    return;
  }
  const AihcInfo *info = aihc_value_info_table(object);
  uintptr_t identity = info->identity;
  if (identity == FILLER_IDENTITY && in_some_nursery(object)) {
    /* A filler piece of a nursery that this collection did not copy. */
    return;
  }
  Entry *entry =
      identity == 0 || identity >= entry_capacity ? NULL : &entries[identity];
  if (entry == NULL || !entry->defined) {
    violation("object with an unknown identity survived");
    printf("obj %" PRIuPTR " %s 0\n", identity, kind_name(kind));
    return;
  }
  if (entry->address != object) {
    /* Another object with this identity represents it. */
    return;
  }
  if (info != entry->info) {
    violation("object header does not name its own info table");
  }
  if (kind == AIHC_OBJECT_MVAR) {
    const AihcMVar *mvar = (const AihcMVar *)object;
    printf("obj %" PRIuPTR " mvar", identity);
    if (mvar->full) {
      printf(" full");
      print_pointer(mvar->value);
    } else {
      printf(" empty");
    }
    printf(" readers");
    print_waiters(mvar->readers_head, 0);
    printf(" takers");
    print_waiters(mvar->takers_head, 0);
    printf(" putters");
    print_waiters(mvar->putters_head, 1);
    printf("\n");
    return;
  }
  if (kind == AIHC_OBJECT_THREAD) {
    const AihcThread *thread = (const AihcThread *)object;
    printf("obj %" PRIuPTR " thread %s", identity,
           resume_kind_name(thread->resume_kind));
    print_pointer((AihcSlot)(uintptr_t)thread->resume_function);
    print_pointer((AihcSlot)(uintptr_t)thread->resume_continuation);
    if ((thread->resume_kind == AIHC_RESUME_CONTINUE ||
         thread->resume_kind == AIHC_RESUME_APPLY) &&
        thread->resume_count == 1) {
      print_pointer(thread->resume_value);
    }
    printf("\n");
    return;
  }
  if (info->object_kind == AIHC_OBJECT_ARRAY) {
    uint64_t length = aihc_array_length(object);
    AihcSlot *elements = aihc_array_elements(object);
    printf("obj %" PRIuPTR " array %" PRIu64, identity, length);
    for (uint64_t index = 0; index < length; ++index) {
      print_pointer(elements[index]);
    }
    printf("\n");
    return;
  }
  const AihcSlot *fields = entry_fields(entry);
  printf("obj %" PRIuPTR " %s %" PRIuPTR, identity, kind_name(kind),
         (uintptr_t)entry->field_count);
  for (uint64_t index = 0; index < entry->field_count; ++index) {
    if (entry->pointers[index]) {
      print_pointer(fields[index]);
    } else {
      if (entry->has_shadow[index] && entry->shadow[index] != fields[index]) {
        violation("non-pointer field changed");
      }
      printf(" w%" PRIx64, fields[index]);
    }
  }
  printf("\n");
}

/* Print one live frame and return its parent. */
static const AihcValue *print_frame(Frame *frame) {
  const AihcValue *object = frame->address;
  const AihcInfo *info = aihc_value_info_table(object);
  if (info != frame->info) {
    violation("frame header does not name its own info table");
  }
  printf("frame %" PRIuPTR " %s %" PRIuPTR, info->identity,
         frame_kind_name(info->frame_kind), (uintptr_t)info->field_count);
  const AihcSlot *fields = aihc_value_fields_const(object);
  for (uint64_t index = 0; index < info->field_count; ++index) {
    if (frame->pointers[index]) {
      print_pointer(fields[index]);
    } else {
      if (frame->has_shadow[index] && frame->shadow[index] != fields[index]) {
        violation("non-pointer frame field changed");
      }
      printf(" w%" PRIx64, fields[index]);
    }
  }
  printf("\n");
  if (info->field_count == 0) {
    return NULL;
  }
  return (const AihcValue *)(uintptr_t)fields[0];
}

static void print_static_thunk(uint64_t slot) {
  const StaticThunk *thunk = &static_thunks[slot];
  const AihcValue *object = (const AihcValue *)thunk;
  if (aihc_value_kind(object) == AIHC_OBJECT_THUNK) {
    printf("static %" PRIu64 " thunk\n", slot);
  } else if (aihc_value_kind(object) != AIHC_OBJECT_INDIRECTION) {
    violation("static thunk has an invalid kind");
    printf("static %" PRIu64 " invalid\n", slot);
  } else {
    printf("static %" PRIu64 " ind", slot);
    print_pointer(thunk->target);
    printf("\n");
  }
}

static int compare_object_addresses(const void *left, const void *right) {
  uintptr_t first = (uintptr_t)*(AihcValue *const *)left;
  uintptr_t second = (uintptr_t)*(AihcValue *const *)right;
  return first < second ? -1 : first > second;
}

/* Record where one object of the heap starts. */
static void record_object(AihcValue *object, void *context) {
  (void)context;
  /* A forwarding header has bit one alone. */
  if ((object->header & AIHC_HEADER_TAG_MASK) == AIHC_HEADER_CLAIMING) {
    violation("forwarding header in a live block");
    return;
  }
  if (object_start_count == object_start_capacity) {
    object_start_capacity =
        object_start_capacity == 0 ? 256 : object_start_capacity * 2;
    AihcValue **grown =
        realloc(object_starts, object_start_capacity * sizeof(*object_starts));
    if (grown == NULL) {
      fail("out of memory");
    }
    object_starts = grown;
  }
  object_starts[object_start_count++] = object;
}

static unsigned generation_of_address(const void *address) {
  switch (aihc_region_kind(address)) {
  case AIHC_REGION_GEN1:
    return 1;
  case AIHC_REGION_GEN2:
    return 2;
  case AIHC_REGION_LARGE:
    return aihc_pinned_generation(aihc_pinned_block_of(address));
  default:
    return 0;
  }
}

/* The identity of a thread record. */
static uint64_t thread_identity(const AihcThread *thread) {
  return aihc_value_info_table((const AihcValue *)thread)->identity;
}

/* Release every frame of a thread whose stack is gone. */
static void drop_frames_of(uint64_t thread) {
  for (size_t index = 1; index < frame_capacity; ++index) {
    if (frames[index].defined && frames[index].thread == thread) {
      frames[index].address = NULL;
    }
  }
  if (thread < entry_capacity) {
    entries[thread].top = 0;
  }
}

static void report_stack(AihcStack *stack) {
  AihcThread *thread = stack->thread;
  uint64_t identity = thread_identity(thread);
  const AihcValue *top = NULL;
  if (thread == capability->current_thread) {
    Entry *entry = entry_of(identity);
    if (entry->top != 0) {
      top = frame_of(entry->top)->address;
    }
    if (top == NULL && capability->stack_next != aihc_stack_base(stack)) {
      violation("the running stack holds no frame but is not empty");
    }
  } else {
    top = stack->top;
  }
  printf("stack");
  print_pointer((AihcSlot)(uintptr_t)thread);
  printf(" top");
  print_pointer((AihcSlot)(uintptr_t)top);
  printf("\n");
  const AihcValue *cursor = top;
  for (int fuel = 0; cursor != NULL; ++fuel) {
    if (fuel > 100000) {
      violation("frame chain is too long");
      return;
    }
    Frame *frame = frame_at(cursor);
    if (frame == NULL) {
      violation("stack chain reached a frame the driver did not push");
      return;
    }
    if (frame->thread != identity) {
      violation("stack chain reached a frame of another thread");
      return;
    }
    const AihcValue *parent = print_frame(frame);
    if ((frame->parent == 0) != (parent == NULL) ||
        (parent != NULL && frame_at(parent) != frame_of(frame->parent))) {
      violation("frame parent differs from the pushed parent");
      return;
    }
    cursor = parent;
  }
}

/* Whether an object can have a second copy: a promotion copies an
   immutable object and leaves the original as it is. */
static int is_duplicable(const AihcValue *object) {
  switch (aihc_value_kind(object)) {
  case AIHC_OBJECT_NODE:
  case AIHC_OBJECT_CLOSURE:
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR:
  case AIHC_OBJECT_THUNK:
    return 1;
  default:
    return 0;
  }
}

/* Two copies of an object hold equal fields when each pointer field names
   the same identity, frame, or static object, and each word is equal. */
static int same_slot(AihcSlot left, AihcSlot right) {
  if (left == right) {
    return 1;
  }
  const AihcValue *first = follow_indirections((const void *)(uintptr_t)left);
  const AihcValue *second = follow_indirections((const void *)(uintptr_t)right);
  if (first == second) {
    return 1;
  }
  return first != NULL && second != NULL && is_object_start(first) &&
         is_object_start(second) &&
         aihc_value_info_table(first)->identity ==
             aihc_value_info_table(second)->identity;
}

static int same_fields(const Entry *entry, AihcValue *left, AihcValue *right) {
  uint64_t offset =
      entry->info->object_kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR ||
              entry->info->object_kind == AIHC_OBJECT_THUNK
          ? 1
          : 0;
  for (uint64_t index = 0; index < entry->field_count; ++index) {
    AihcSlot first = left->fields[index + offset];
    AihcSlot second = right->fields[index + offset];
    if (entry->pointers[index] ? !same_slot(first, second) : first != second) {
      return 0;
    }
  }
  return 1;
}

/* Choose the object that represents each identity. After a promotion, a
   duplicable object can have two copies with equal fields. The copy in the
   oldest generation represents it. */
static void choose_representatives(void) {
  for (size_t index = 0; index < object_start_count; ++index) {
    AihcValue *object = object_starts[index];
    if (is_indirection(object)) {
      continue;
    }
    uintptr_t identity = aihc_value_info_table(object)->identity;
    if (identity == 0 || identity >= entry_capacity ||
        !entries[identity].defined || entries[identity].indirection) {
      continue;
    }
    Entry *entry = &entries[identity];
    if (entry->address == NULL) {
      entry->address = object;
      continue;
    }
    if (!promoted_ever || !is_duplicable(object) ||
        !is_duplicable(entry->address) ||
        !same_fields(entry, entry->address, object)) {
      violation("two objects share one identity");
      continue;
    }
    if (generation_of_address(object) > generation_of_address(entry->address)) {
      entry->address = object;
    }
  }
}

static void report_collection(void) {
  unsigned collected = (unsigned)capability->machine->gc_last_generation;
  int finished = capability->machine->gc_full_count != reported_full_count;
  int started =
      capability->machine->gen2_cycle_active != 0 && reported_cycle_active == 0;
  reported_full_count = capability->machine->gc_full_count;
  reported_cycle_active = capability->machine->gen2_cycle_active;
  printf("collection %zu %u\n", command_index, collected);
  /* A gen2 cycle that started in this command took its snapshot at the end
     of the collection. A cycle that ended freed the gen2 objects that were
     dead at its snapshot. */
  if (started) {
    printf("cycle start\n");
  }
  if (finished) {
    printf("finish\n");
  }
  if (bridge) {
    printf("bridge\n");
  }

  object_start_count = 0;
  aihc_gc_walk_objects(capability, record_object, NULL);
  if (object_start_count != 0) {
    qsort(object_starts, object_start_count, sizeof(*object_starts),
          compare_object_addresses);
  }

  for (size_t index = 0; index < entry_capacity; ++index) {
    Entry *entry = &entries[index];
    if (entry->indirection && entry->address != NULL &&
        is_object_start(entry->address) && is_indirection(entry->address)) {
      /* An indirection of an old generation stays until a collection of
         that generation, and the remembered set keeps its target alive
         while it is there. The model needs its age for that, so the driver
         reports the age of the indirection without its object. The end of
         a gen2 cycle can free the slot of the indirection, and a new
         object can take the slot. That object is not an indirection, so
         the entry goes. */
      printf("age %zu %u\n", index, generation_of_address(entry->address));
      continue;
    }
    entry->address = NULL;
  }
  choose_representatives();
  for (size_t index = 0; index < object_start_count; ++index) {
    AihcValue *object = object_starts[index];
    print_object(object);
    const AihcInfo *info = aihc_value_info_table(object);
    if (info->identity != 0 && info->identity < entry_capacity &&
        entries[info->identity].address == object) {
      printf("age %" PRIuPTR " %u\n", info->identity,
             generation_of_address(object));
    }
  }
  /* A thread the collection did not keep lost its stack. */
  for (size_t index = 1; index < entry_capacity; ++index) {
    if (entries[index].defined && entries[index].is_thread &&
        entries[index].address == NULL) {
      drop_frames_of(index);
    }
  }
  if (capability->current_thread != NULL) {
    running = thread_identity(capability->current_thread);
  }
  printf("running");
  print_pointer((AihcSlot)(uintptr_t)capability->current_thread);
  printf("\nqueue");
  for (const AihcThread *thread = capability->run_queue_head; thread != NULL;
       thread = thread->next) {
    print_pointer((AihcSlot)(uintptr_t)thread);
  }
  printf("\n");
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    report_stack(stack);
  }

  for (uint64_t index = 0; index < capability->machine->global_count; ++index) {
    printf("global %" PRIu64, index);
    print_pointer(capability->machine->globals[index]);
    printf("\n");
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    printf("root %" PRIu64, index);
    print_pointer(root_slots[index]);
    printf("\n");
  }
  for (size_t index = 0; index < stable_count; ++index) {
    const AihcStableName *name = stable_names[index];
    if (!is_object_start((const AihcValue *)name) ||
        aihc_value_kind((const AihcValue *)name) != AIHC_OBJECT_STABLE_NAME) {
      violation("stable name is not a managed object");
    }
    printf("stable");
    print_pointer((AihcSlot)(uintptr_t)name->value);
    printf("\n");
  }
  /* A claimed thunk with waiters names its blackhole queue in field zero. */
  for (size_t index = 0; index < entry_capacity; ++index) {
    const Entry *thunk_entry = &entries[index];
    const AihcValue *object = thunk_entry->address;
    if (!thunk_entry->defined || object == NULL ||
        thunk_entry->info->object_kind != AIHC_OBJECT_THUNK ||
        aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE ||
        aihc_value_info_table((const AihcValue *)(uintptr_t)object->fields[0])
                ->object_kind != AIHC_OBJECT_BLACKHOLE_QUEUE) {
      continue;
    }
    const AihcBlackholeQueue *queue =
        (const AihcBlackholeQueue *)(uintptr_t)object->fields[0];
    if (queue->thunk != object) {
      violation("blackhole queue names another thunk");
    }
    {
      printf("blackhole %zu", index);
      size_t count = 0;
      for (const AihcBlackholeWaiter *waiter = queue->head; waiter != NULL;
           waiter = waiter->next) {
        ++count;
      }
      printf(" %zu", count);
      for (const AihcBlackholeWaiter *waiter = queue->head; waiter != NULL;
           waiter = waiter->next) {
        print_pointer((AihcSlot)(uintptr_t)waiter->thread);
        print_pointer((AihcSlot)(uintptr_t)waiter->continuation);
      }
      printf("\n");
    }
  }
  for (uint64_t slot = 0; slot < STATIC_THUNKS; ++slot) {
    print_static_thunk(slot);
  }
  for (uint64_t slot = 0; slot < STATIC_NODES; ++slot) {
    printf("static %" PRIu64 " node", STATIC_THUNKS + slot);
    for (int field = 0; field < STATIC_NODE_FIELDS; ++field) {
      print_pointer(static_nodes[slot].fields[field]);
    }
    printf("\n");
  }
  print_violations();
  printf("endcollection\n");
}

/* The roots the driver retains explicitly: the root slots, the stable
   names, and the continuation of the running code, which compiled code
   keeps live across every collection. The model knows all three. */
static AihcSlot *gather_roots(size_t *total) {
  if (root_count > SIZE_MAX / sizeof(AihcSlot) ||
      stable_count > SIZE_MAX / sizeof(AihcSlot) - root_count - 1) {
    fail("too many roots");
  }
  *total = (size_t)root_count + stable_count + 1;
  AihcSlot *roots = checked_calloc(*total, sizeof(*roots));
  for (size_t index = 0; index < *total; ++index) {
    if (index < root_count) {
      roots[index] = root_slots[index];
    } else if (index < root_count + stable_count) {
      roots[index] = (AihcSlot)(uintptr_t)stable_names[index - root_count];
    } else {
      Entry *thread = entry_of(running);
      roots[index] = thread->top == 0
                         ? 0
                         : (AihcSlot)(uintptr_t)frame_of(thread->top)->address;
    }
  }
  return roots;
}

static void scatter_roots(AihcSlot *roots, size_t total) {
  for (size_t index = 0; index < total; ++index) {
    if (index < root_count) {
      root_slots[index] = roots[index];
    } else if (index < root_count + stable_count) {
      stable_names[index - root_count] =
          (AihcStableName *)(uintptr_t)roots[index];
    }
  }
  free(roots);
}

/* Reserve words through the collector's own entry point and report a
   collection when one ran. */
static void ensure(uint64_t words) {
  uint64_t before = capability->machine->gc_count;
  uint64_t nursery_bytes = heap->nursery_bytes;
  /* The local collection of the reservation does not change the other
     nurseries. */
  int others_used = 0;
  for (uint64_t index = 0; index < capability->machine->capability_count;
       ++index) {
    AihcCapability *other = capability->machine->capabilities[index];
    if (other != heap && other->heap_next != other->heap_start) {
      others_used = 1;
    }
  }
  size_t total = 0;
  AihcSlot *roots = gather_roots(&total);
  aihc_ensure_heap(heap, words, total, roots, current_srt);
  scatter_roots(roots, total);
  /* A larger nursery comes from a new arena, which replaces all
     nurseries. When another nursery holds objects, a bridge collection
     empties it first. Otherwise only the local collection ran. */
  bridge = heap->nursery_bytes != nursery_bytes && others_used;
  if (capability->machine->gc_count != before) {
    report_collection();
  }
  bridge = 0;
}

/* Collect the generations up to the given one and report. */
static void collect_generation(unsigned generation) {
  size_t total = 0;
  AihcSlot *roots = gather_roots(&total);
  aihc_gc_collect_generation(capability, generation, total, roots, current_srt);
  scatter_roots(roots, total);
  report_collection();
}

/* Collect the nursery and gen1, start a gen2 cycle, and report. */
static void start_cycle(void) {
  size_t total = 0;
  AihcSlot *roots = gather_roots(&total);
  aihc_gc_start_cycle(capability, total, roots, current_srt);
  scatter_roots(roots, total);
  report_collection();
}

/* Give a thread record an info table of its own, so the report names it. */
static void adopt_thread(AihcThread *thread, uint64_t identity) {
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("thread identity is already defined");
  }
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->object_kind = AIHC_OBJECT_THREAD;
  info->frame_kind = AIHC_FRAME_NONE;
  thread->header = (AihcSlot)(uintptr_t)info;
  entry->defined = 1;
  entry->is_thread = 1;
  entry->address = (AihcValue *)thread;
  entry->info = info;
  entry->pointers = checked_calloc(1, 1);
  entry->shadow = checked_calloc(1, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(1, 1);
  entry->field_count = 0;
  entry->top = 0;
}

static void command_machine(char **tokens, size_t count) {
  if (count != 4 && count != 5) {
    fail("machine expects three or four arguments");
  }
  uint64_t global_count = parse_unsigned(tokens[1]);
  uint64_t slot_count = parse_unsigned(tokens[2]);
  uint64_t space_bytes = parse_unsigned(tokens[3]);
  /* The bytes one mark slice scans. A small slice spreads a gen2 cycle
     over many collections. */
  uint64_t slice_bytes = count == 5 ? parse_unsigned(tokens[4]) : 0;
  if (space_bytes == 0 || space_bytes % sizeof(AihcSlot) != 0) {
    fail("initial space must be a positive number of words");
  }
  if (capability != NULL) {
    /* The next script starts with one capability. Its objects are garbage,
       so the nurseries are empty for the change. */
    for (uint64_t index = 0; index < capability->machine->capability_count;
         ++index) {
      AihcCapability *each = capability->machine->capabilities[index];
      each->heap_next = each->heap_start;
    }
    aihc_machine_set_capabilities(capability, 1);
    free(capability->machine->globals);
    for (uint64_t index = 0; index < 3; ++index) {
      aihc_rts_set_root(index, NULL);
    }
    /* Give the heap and every stack back. A capability without a nursery
       starts from zero when it is initialized again, so the next script
       gets a fresh one. */
    while (capability->machine->stacks != NULL) {
      aihc_stack_release(capability, capability->machine->stacks);
    }
    aihc_heap_reset(capability, 1);
    aihc_regions_release(capability->heap_start);
    capability->heap_start = NULL;
  }
  free_entries();
  free_srts();
  srts_linked = 0;
  free(root_slots);
  free(stable_names);
  stable_names = NULL;
  stable_count = 0;
  reset_statics();
  current_srt = NULL;
  memset(reserved, 0, sizeof(reserved));
  promoted_ever = 0;
  /* The model supplies external roots and excludes process startup objects. */
  for (uint64_t index = 0; index < 3; ++index) {
    aihc_rts_set_root(index, NULL);
  }
  capability = aihc_machine_new(0);
  heap = capability;
  heap_index = 0;
  capability->machine->global_count = global_count;
  capability->machine->globals = checked_calloc(global_count, sizeof(AihcSlot));
  /* Keep the initial thread when the script replaces the default space. */
  AihcThread initial_thread = *capability->current_thread;
  if (space_bytes > SIZE_MAX - sizeof(initial_thread)) {
    fail("initial space is too large");
  }
  space_bytes += sizeof(initial_thread);
  /* The script drives every collection above the nursery itself, so the
     policy never chooses one. */
  aihc_heap_reset(capability, space_bytes);
  if (slice_bytes != 0) {
    capability->machine->mark_slice_floor = slice_bytes;
    capability->machine->mark_slice_cap = slice_bytes;
  }
  reported_cycle_active = 0;
  capability->machine->gen1_collections = UINT64_MAX;
  capability->machine->gen2_limit_bytes = UINT64_MAX;
  memset(capability->heap_start, 0, space_bytes);
  /* The reset released gen2, which held the thread record. The new record
     charges the nursery for the space the script added for it. */
  capability->current_thread = (AihcThread *)aihc_gc_allocate_global(
      capability, aihc_value_words((const AihcValue *)&initial_thread));
  *capability->current_thread = initial_thread;
  /* The stack of the initial thread names the thread by its new address. */
  for (AihcStack *stack = capability->machine->stacks; stack != NULL;
       stack = stack->next) {
    stack->thread = capability->current_thread;
  }
  adopt_thread(capability->current_thread, INITIAL_THREAD);
  running = INITIAL_THREAD;
  reported_full_count = capability->machine->gc_full_count;
  root_count = slot_count;
  root_slots = checked_calloc(slot_count, sizeof(*root_slots));
}

static void command_srt(char **tokens, size_t count) {
  if (count < 4) {
    fail("srt expects an index and two counts");
  }
  uint64_t index = parse_unsigned(tokens[1]);
  uint64_t objects = parse_unsigned(tokens[2]);
  uint64_t children = parse_unsigned(tokens[3]);
  if (count != 4 + objects + children) {
    fail("srt entry count does not match");
  }
  if (srts_linked) {
    fail("srt must come before any object or collection");
  }
  if (index >= srt_count) {
    size_t grown_count = (size_t)index + 1;
    AihcSrt **grown = realloc(srts, grown_count * sizeof(*srts));
    if (grown == NULL) {
      fail("out of memory");
    }
    memset(grown + srt_count, 0, (grown_count - srt_count) * sizeof(*srts));
    srts = grown;
    srt_count = grown_count;
  }
  if (srts[index] != NULL) {
    fail("reference table is already defined");
  }
  AihcSrt *srt = checked_calloc(1, sizeof(*srt) + (size_t)(objects + children) *
                                                      sizeof(uintptr_t));
  srt->object_count = objects;
  srt->child_count = children;
  srts[index] = srt;
  for (uint64_t entry = 0; entry < objects; ++entry) {
    const char *token = tokens[4 + entry];
    if (token[0] != 's') {
      fail("srt objects must be static slots");
    }
    srt->entries[entry] =
        (uintptr_t)static_slot_address(parse_unsigned(token + 1));
  }
  /* Children can name tables that a later line defines, so store the index
     and patch it when the script publishes or attaches a table. */
  for (uint64_t entry = 0; entry < children; ++entry) {
    srt->entries[objects + entry] =
        (uintptr_t)parse_unsigned(tokens[4 + objects + entry]);
  }
}

/* Replace child indices with table addresses once every table exists. */
static void link_srts(void) {
  if (srts_linked) {
    return;
  }
  for (size_t index = 0; index < srt_count; ++index) {
    AihcSrt *srt = srts[index];
    if (srt == NULL) {
      continue;
    }
    for (uintptr_t entry = 0; entry < srt->child_count; ++entry) {
      uintptr_t child = srt->entries[srt->object_count + entry];
      srt->entries[srt->object_count + entry] =
          (uintptr_t)srt_of((int64_t)child);
    }
  }
  srts_linked = 1;
}

/* Parse a pointer bitmap into a fresh array. */
static uint8_t *parse_bitmap(const char *bits, uint64_t *field_count) {
  *field_count = strcmp(bits, "-") == 0 ? 0 : strlen(bits);
  uint8_t *pointers = checked_calloc(*field_count, sizeof(*pointers));
  for (uint64_t index = 0; index < *field_count; ++index) {
    if (bits[index] != '0' && bits[index] != '1') {
      fail("invalid pointer bitmap");
    }
    pointers[index] = bits[index] == '1';
  }
  return pointers;
}

static void command_new(char **tokens, size_t count) {
  if (count != 5) {
    fail("new expects four arguments");
  }
  uint64_t identity = parse_unsigned(tokens[1]);
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("object is already defined");
  }
  AihcObjectKind kind = 0;
  if (strcmp(tokens[2], "node") == 0) {
    kind = AIHC_OBJECT_NODE;
  } else if (strcmp(tokens[2], "closure") == 0) {
    kind = AIHC_OBJECT_CLOSURE;
  } else if (strcmp(tokens[2], "thunk") == 0) {
    kind = AIHC_OBJECT_THUNK;
  } else if (strcmp(tokens[2], "partial") == 0) {
    kind = AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  } else {
    fail("invalid object kind");
  }
  uint64_t field_count = 0;
  uint8_t *pointers = parse_bitmap(tokens[3], &field_count);
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->field_count = field_count;
  info->field_is_pointer = pointers;
  if (kind == AIHC_OBJECT_THUNK) {
    /* Field zero of a thunk is its owner word, which is not a pointer
       until a claim. The script names the free variables after it. */
    uint8_t *owned = checked_calloc(field_count + 1, 1);
    memcpy(owned + 1, pointers, field_count);
    info->field_count = field_count + 1;
    info->field_is_pointer = owned;
  }
  info->frame_kind = AIHC_FRAME_NONE;
  info->object_kind = kind;
  info->needs_eval =
      kind == AIHC_OBJECT_THUNK ? AIHC_NEEDS_EVAL_ENTER : AIHC_NEEDS_EVAL_NONE;
  info->srt = srt_of(parse_signed(tokens[4]));
  /* A partial constructor carries its applied count in field zero and shares
     one info table with the saturated form the count is measured against. */
  int partial = kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  if (partial) {
    AihcInfo *saturated = checked_calloc(1, sizeof(*saturated));
    saturated->identity = identity;
    saturated->field_count = field_count;
    saturated->field_is_pointer = pointers;
    saturated->frame_kind = AIHC_FRAME_NONE;
    saturated->object_kind = AIHC_OBJECT_NODE;
    info->next = saturated;
  }
  uint64_t words = partial ? 2 + field_count : aihc_object_words(info);
  if (words > reserved_words) {
    fail("block exceeds its reservation");
  }
  reserved_words -= words;
  AihcValue *object = aihc_gc_allocate(heap, words);
  object->header = (AihcSlot)(uintptr_t)info;
  for (uint64_t index = 0; index + 1 < words; ++index) {
    object->fields[index] = 0;
  }
  if (partial) {
    object->fields[0] = field_count;
  }
  entry->defined = 1;
  entry->address = object;
  entry->info = info;
  entry->pointers = pointers;
  entry->shadow = checked_calloc(field_count, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(field_count, sizeof(*entry->has_shadow));
  entry->field_count = field_count;
}

static void command_array(char **tokens, size_t count) {
  if (count != 4) {
    fail("array expects three arguments");
  }
  uint64_t identity = parse_unsigned(tokens[1]);
  uint64_t length = parse_unsigned(tokens[2]);
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("object is already defined");
  }
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->field_count = 1;
  info->frame_kind = AIHC_FRAME_NONE;
  info->object_kind = AIHC_OBJECT_ARRAY;
  info->srt = srt_of(parse_signed(tokens[3]));
  uint64_t words = 2 + length;
  /* A large array gets regions of its own without a collection, as a
     compiled array allocation does, so it takes nothing from the
     reservation of the block. */
  if (words * sizeof(AihcSlot) <
      AIHC_LARGE_OBJECT_BYTES - sizeof(AihcPinnedBlock)) {
    if (words > reserved_words) {
      fail("block exceeds its reservation");
    }
    reserved_words -= words;
  }
  AihcValue *object = aihc_gc_allocate(heap, words);
  object->header = (AihcSlot)(uintptr_t)info;
  object->fields[0] = length;
  for (uint64_t index = 0; index < length; ++index) {
    object->fields[index + 1] = 0;
  }
  entry->defined = 1;
  entry->address = object;
  entry->info = info;
  entry->pointers = checked_calloc(length, sizeof(*entry->pointers));
  memset(entry->pointers, 1, length);
  entry->shadow = checked_calloc(length, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(length, sizeof(*entry->has_shadow));
  entry->field_count = length;
}

static void command_mvar(char **tokens, size_t count) {
  if (count != 2) {
    fail("mvar expects one argument");
  }
  uint64_t identity = parse_unsigned(tokens[1]);
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("object is already defined");
  }
  const uint8_t *before = heap->heap_next;
  AihcMVar *mvar = aihc_mvar_new(heap);
  uint64_t words = (uint64_t)(heap->heap_next - before) / sizeof(AihcSlot);
  if (words > reserved_words) {
    fail("block exceeds its reservation");
  }
  reserved_words -= words;
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->object_kind = AIHC_OBJECT_MVAR;
  info->frame_kind = AIHC_FRAME_NONE;
  mvar->header = (AihcSlot)(uintptr_t)info;
  entry->defined = 1;
  entry->is_mvar = 1;
  entry->address = (AihcValue *)mvar;
  entry->info = info;
  entry->pointers = checked_calloc(1, 1);
  entry->shadow = checked_calloc(1, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(1, 1);
  entry->field_count = 0;
}

static AihcMVar *parse_mvar(const char *token) {
  if (token[0] != 'h') {
    fail("expected an MVar");
  }
  Entry *entry = live_entry(parse_unsigned(token + 1));
  if (!entry->is_mvar) {
    fail("object is not an MVar");
  }
  return (AihcMVar *)entry->address;
}

/* The capability that writes an object. A young object belongs to the
   local heap of one capability, and only that capability writes it. Thus
   its remembered entry goes to the set that the local collection of that
   capability scans. Any capability writes a global object, and the
   driver writes it through the capability of the current nursery. */
static AihcCapability *writer_of(const AihcValue *object) {
  AihcMachine *machine = capability->machine;
  for (uint64_t index = 0; index < machine->capability_count; ++index) {
    AihcCapability *each = machine->capabilities[index];
    if ((const uint8_t *)object >= each->heap_start &&
        (const uint8_t *)object < each->heap_next) {
      return each;
    }
    for (const AihcHeapBlock *block = each->gen1.first; block != NULL;
         block = block->link) {
      if ((const uint8_t *)object >= block->start &&
          (const uint8_t *)object < block->next) {
        return each;
      }
    }
  }
  if (aihc_region_kind(object) == AIHC_REGION_LARGE) {
    const AihcPinnedBlock *block = aihc_pinned_block_of(object);
    if (aihc_pinned_generation(block) < 2) {
      return machine->capabilities[aihc_pinned_owner(block)];
    }
  }
  return heap;
}

static void command_set(char **tokens, size_t count) {
  if (count != 4) {
    fail("set expects three arguments");
  }
  /* Parse the value first: a value lookup can grow the entry table and move
     every entry. */
  uint64_t index = parse_unsigned(tokens[2]);
  int is_word = 0;
  AihcSlot value = parse_value(tokens[3], &is_word);
  Entry *entry = live_entry(parse_unsigned(tokens[1]));
  if (entry->is_thread || entry->is_mvar) {
    fail("set expects a plain object");
  }
  if (index >= entry->field_count) {
    fail("field index out of range");
  }
  if (entry->pointers[index] == is_word) {
    fail("value kind does not match the field kind");
  }
  /* Compiled code puts the write barrier before a store into an existing
     object, so the driver does the same for its direct stores. A store into
     an array gives the index, as the compiled array stores do. */
  if (entry->info->object_kind == AIHC_OBJECT_ARRAY) {
    aihc_write_barrier_at(writer_of(entry->address), entry->address, index);
    aihc_array_elements(entry->address)[index] = value;
    return;
  }
  aihc_write_barrier(writer_of(entry->address), entry->address);
  entry_fields(entry)[index] = value;
  if (is_word) {
    entry->shadow[index] = value;
    entry->has_shadow[index] = 1;
  }
}

static void command_fill(char **tokens, size_t count) {
  if (count != 2) {
    fail("fill expects one argument");
  }
  uint64_t keep = parse_unsigned(tokens[1]);
  size_t remaining = remaining_words();
  if (remaining <= keep) {
    return;
  }
  uint64_t words = remaining - keep;
  /* The filler is garbage in the space. A field count is one byte, and an
     object at or above the large object bound would get regions of its own
     instead, so the filler is cut into pieces of at most 256 words. The info
     tables stay allocated because a later walk of the old space must not
     read freed memory. */
  const uint64_t piece_limit = 256;
  _Static_assert(256 * sizeof(AihcSlot) < AIHC_LARGE_OBJECT_BYTES,
                 "a filler piece stays in the space");
  while (words != 0) {
    uint64_t piece = words > piece_limit ? piece_limit : words;
    if (words - piece == 1) {
      /* An object has at least one word, so leave two for the last piece. */
      piece -= 1;
    }
    AihcInfo *info = checked_calloc(1, sizeof(*info));
    info->field_count = piece - 1;
    info->identity = FILLER_IDENTITY;
    info->frame_kind = AIHC_FRAME_NONE;
    info->object_kind = AIHC_OBJECT_NODE;
    AihcValue *object = aihc_gc_allocate(heap, piece);
    object->header = (AihcSlot)(uintptr_t)info;
    words -= piece;
  }
}

/* Threads and stacks. */

static Entry *running_thread(void) {
  Entry *entry = entry_of(running);
  if (!entry->defined || !entry->is_thread) {
    fail("the running thread is not defined");
  }
  if ((AihcThread *)entry->address != capability->current_thread) {
    fail("the running thread is not the current thread of the capability");
  }
  return entry;
}

/* Register one frame the driver pushed. */
static Frame *define_frame(uint64_t identity, AihcValue *address,
                           AihcFrameKind kind, uint64_t field_count,
                           uint8_t *pointers, const AihcSrt *srt,
                           uint64_t parent, uint64_t thread) {
  Frame *frame = frame_of(identity);
  if (frame->defined) {
    fail("frame is already defined");
  }
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->field_count = field_count;
  info->field_is_pointer = pointers;
  info->frame_kind = kind;
  info->object_kind = AIHC_OBJECT_CLOSURE;
  info->remaining_arity = 1;
  info->srt = srt;
  frame->defined = 1;
  frame->address = address;
  frame->info = info;
  frame->pointers = pointers;
  frame->shadow = checked_calloc(field_count, sizeof(*frame->shadow));
  frame->has_shadow = checked_calloc(field_count, sizeof(*frame->has_shadow));
  frame->field_count = field_count;
  frame->parent = parent;
  frame->thread = thread;
  return frame;
}

static void command_push(char **tokens, size_t count) {
  if (count < 3) {
    fail("push expects a frame and a kind");
  }
  Entry *thread = running_thread();
  uint64_t identity = parse_unsigned(tokens[1]);
  if (frame_of(identity)->defined) {
    fail("frame is already defined");
  }
  AihcFrameKind kind;
  uint64_t field_count = 0;
  uint8_t *pointers = NULL;
  const AihcSrt *srt = NULL;
  AihcSlot *values = NULL;
  uint8_t *words = NULL;
  const char *kind_token = tokens[2];
  if (strcmp(kind_token, "normal") == 0 || strcmp(kind_token, "forward") == 0) {
    if (count < 5) {
      fail("push normal expects a bitmap and a table");
    }
    kind = kind_token[0] == 'n' ? AIHC_FRAME_NORMAL : AIHC_FRAME_FORWARD;
    uint64_t payload = 0;
    uint8_t *payload_pointers = parse_bitmap(tokens[3], &payload);
    if (count != 5 + payload) {
      fail("push value count does not match the bitmap");
    }
    field_count = 1 + payload;
    pointers = checked_calloc(field_count, 1);
    pointers[0] = 1;
    memcpy(pointers + 1, payload_pointers, payload);
    free(payload_pointers);
    srt = srt_of(parse_signed(tokens[4]));
    values = checked_calloc(field_count, sizeof(*values));
    words = checked_calloc(field_count, 1);
    for (uint64_t index = 1; index < field_count; ++index) {
      int is_word = 0;
      values[index] = parse_value(tokens[4 + index], &is_word);
      if (pointers[index] == is_word) {
        fail("frame value kind does not match the bitmap");
      }
      words[index] = (uint8_t)is_word;
    }
  } else if (strcmp(kind_token, "catch") == 0 ||
             strcmp(kind_token, "prompt") == 0) {
    if (count != 5) {
      fail("push catch expects a table and one value");
    }
    kind = kind_token[0] == 'c' ? AIHC_FRAME_CATCH : AIHC_FRAME_PROMPT;
    field_count = 2;
    pointers = checked_calloc(field_count, 1);
    pointers[0] = 1;
    pointers[1] = 1;
    srt = srt_of(parse_signed(tokens[3]));
    values = checked_calloc(field_count, sizeof(*values));
    words = checked_calloc(field_count, 1);
    values[1] = (AihcSlot)(uintptr_t)parse_object(tokens[4]);
  } else if (strcmp(kind_token, "update") == 0) {
    if (count != 4) {
      fail("push update expects a thunk");
    }
    kind = AIHC_FRAME_UPDATE;
    field_count = 2;
    pointers = checked_calloc(field_count, 1);
    pointers[0] = 1;
    pointers[1] = 1;
    AihcValue *thunk = parse_object(tokens[3]);
    if (aihc_value_kind(thunk) != AIHC_OBJECT_THUNK ||
        aihc_region_kind(thunk) == AIHC_REGION_OUTSIDE) {
      fail("push update expects a heap thunk");
    }
    values = checked_calloc(field_count, sizeof(*values));
    words = checked_calloc(field_count, 1);
    values[1] = (AihcSlot)(uintptr_t)thunk;
    /* The entry writes nothing into the thunk, as in aihc_lir_eval. A pause
       claims it later. */
  } else if (strcmp(kind_token, "stop") == 0) {
    if (count != 3) {
      fail("push stop expects no argument");
    }
    if (thread->top != 0) {
      fail("a stop frame goes at the bottom of a stack");
    }
    kind = AIHC_FRAME_STOP;
    pointers = checked_calloc(1, 1);
    values = checked_calloc(1, sizeof(*values));
    words = checked_calloc(1, 1);
  } else {
    fail("invalid frame kind");
  }
  if (kind != AIHC_FRAME_STOP && thread->top == 0) {
    fail("a stack starts with a stop frame");
  }
  uint64_t parent = thread->top;
  AihcValue *parent_address = parent == 0 ? NULL : frame_of(parent)->address;
  AihcValue *frame = aihc_stack_push(capability, 1 + field_count);
  Frame *record = define_frame(identity, frame, kind, field_count, pointers,
                               srt, parent, running);
  frame->header = (AihcSlot)(uintptr_t)record->info;
  AihcSlot *fields = aihc_value_fields(frame);
  if (field_count != 0) {
    fields[0] = (AihcSlot)(uintptr_t)parent_address;
  }
  for (uint64_t index = 1; index < field_count; ++index) {
    fields[index] = values[index];
    if (words[index]) {
      record->shadow[index] = values[index];
      record->has_shadow[index] = 1;
    }
  }
  free(values);
  free(words);
  thread->top = identity;
}

/* Pop the frames of a thread from its top down to a frame. With inclusive,
   the frame itself is popped as well. */
static void pop_to(Entry *thread, uint64_t target, int inclusive) {
  uint64_t cursor = thread->top;
  while (cursor != target) {
    if (cursor == 0) {
      fail("popped past the bottom of the stack");
    }
    Frame *frame = frame_of(cursor);
    frame->address = NULL;
    cursor = frame->parent;
  }
  if (inclusive && target != 0) {
    Frame *frame = frame_of(target);
    frame->address = NULL;
    thread->top = frame->parent;
  } else {
    thread->top = target;
  }
}

/* The frame a continue helper enters for a continuation: the first frame
   of the chain that is not a forward frame. */
static Frame *entered_frame(Frame *frame) {
  while (frame->info->frame_kind == AIHC_FRAME_FORWARD) {
    if (frame->parent == 0) {
      fail("forward frame has no parent");
    }
    frame = live_frame(frame->parent);
  }
  return frame;
}

/* Enter a frame as the continue helpers do: the frame is the new stack
   pointer, and its chunk is made young again when the stack pointer was in
   another chunk. */
static void set_stack_pointer(Frame *target) {
  AihcValue *address = target->address;
  uint8_t *next = capability->stack_next;
  if (next == NULL ||
      (((uintptr_t)next - 1) ^ (uintptr_t)address) >= AIHC_STACK_CHUNK_BYTES) {
    aihc_stack_enter_chunk(capability, address);
  }
  capability->stack_next = (uint8_t *)address;
}

static void apply_resume(const AihcResume *resume);

typedef AihcValue *(*Update)(AihcCapability *, AihcValue *, AihcValue *);

/* Run an update as compiled code does. With several capabilities, the
   update of a global thunk promotes the value. When the promotion needs a
   collection, the update gives null, and the driver collects with its roots,
   the thunk, and the value, and runs the update again. The report of the
   collection follows the update, so it shows the state after the
   command. */
static void escaping_update(Update update, AihcValue *thunk, AihcValue *value) {
  int collected = 0;
  if (capability->machine->capability_count > 1) {
    promoted_ever = 1;
  }
  while (update(capability, thunk, value) == NULL) {
    size_t total = 0;
    AihcSlot *roots = gather_roots(&total);
    AihcSlot *extended = checked_calloc(total + 2, sizeof(*extended));
    memcpy(extended, roots, total * sizeof(*extended));
    extended[total] = (AihcSlot)(uintptr_t)thunk;
    extended[total + 1] = (AihcSlot)(uintptr_t)value;
    aihc_escape_collect(capability, total + 2, extended, current_srt);
    memcpy(roots, extended, total * sizeof(*extended));
    thunk = (AihcValue *)(uintptr_t)extended[total];
    value = (AihcValue *)(uintptr_t)extended[total + 1];
    free(extended);
    scatter_roots(roots, total);
    collected = 1;
  }
  if (collected) {
    report_collection();
  }
}

/* Continue into a frame of the running thread with an optional value, as
   the entry of the frame would: an update frame updates its thunk with the
   value and wakes the waiters of the thunk, a stop frame ends the thread,
   and every other frame runs its code, which the script spells out. */
static void continue_into(Frame *frame, AihcValue *value, int has_value) {
  Entry *thread = running_thread();
  Frame *target = entered_frame(frame);
  AihcFrameKind kind = target->info->frame_kind;
  if (kind == AIHC_FRAME_UPDATE) {
    if (!has_value) {
      fail("an update frame takes the result of its thunk");
    }
    AihcValue *thunk =
        (AihcValue *)(uintptr_t)aihc_value_fields_const(target->address)[1];
    /* The update replaces the header, so the identity is read first. */
    uintptr_t identity = aihc_value_info_table(thunk)->identity;
    set_stack_pointer(target);
    pop_to(thread, target->info->identity, 1);
    escaping_update(aihc_update_blackhole, thunk, value);
    if (identity != 0 && identity < entry_capacity) {
      entries[identity].indirection = 1;
    }
    return;
  }
  set_stack_pointer(target);
  pop_to(thread, target->info->identity, 1);
  if (kind == AIHC_FRAME_STOP) {
    uint64_t finished = running;
    const AihcResume *resume = aihc_thread_done(capability);
    drop_frames_of(finished);
    apply_resume(resume);
  }
}

/* Enter one frame of the running thread with an optional value. */
static void command_enter(char **tokens, size_t count) {
  if (count != 2 && count != 3) {
    fail("enter expects a frame and at most one value");
  }
  (void)running_thread();
  Frame *frame = live_frame(parse_unsigned(tokens[1]));
  if (frame->thread != running) {
    fail("enter expects a frame of the running thread");
  }
  AihcValue *value = count == 3 ? parse_object(tokens[2]) : NULL;
  continue_into(frame, value, count == 3);
}

/* Give a resumption to the thread it names, as aihc_lir_resume does. */
static void apply_resume(const AihcResume *resume) {
  if (resume == NULL) {
    fail("the scheduler suspended the capability");
  }
  uint64_t slots[5];
  aihc_lir_take_resume(capability, (AihcResume *)(uintptr_t)resume, slots);
  running = thread_identity(capability->current_thread);
  Entry *thread = running_thread();
  AihcResumeKind kind = slots[0];
  AihcValue *function = (AihcValue *)(uintptr_t)slots[1];
  AihcValue *continuation = (AihcValue *)(uintptr_t)slots[2];
  switch (kind) {
  case AIHC_RESUME_APPLY: {
    /* The function returns to the continuation, which is the top live
       frame: the frames above it are gone. */
    Frame *frame = frame_at(continuation);
    if (frame == NULL || frame->thread != running) {
      fail("resumption continues at an unknown frame");
    }
    pop_to(thread, frame->info->identity, 0);
    return;
  }
  case AIHC_RESUME_CONTINUE: {
    /* The continue helper enters the continuation with the values of the
       resumption. */
    Frame *frame = frame_at(function);
    if (frame == NULL || frame->thread != running) {
      fail("resumption enters an unknown frame");
    }
    continue_into(frame, (AihcValue *)(uintptr_t)slots[3], slots[4] == 1);
    return;
  }
  case AIHC_RESUME_RAISE:
    apply_resume(aihc_raise(capability, function, continuation));
    return;
  default:
    fail("invalid resumption");
  }
}

static AihcValue *running_continuation(void) {
  Entry *thread = running_thread();
  if (thread->top == 0) {
    fail("the running thread has no frame");
  }
  return live_frame(thread->top)->address;
}

static void command_fork(char **tokens, size_t count) {
  if (count != 4) {
    fail("fork expects a thread, a frame, and an action");
  }
  (void)running_thread();
  uint64_t identity = parse_unsigned(tokens[1]);
  uint64_t frame_identity = parse_unsigned(tokens[2]);
  if (entry_of(identity)->defined || frame_of(frame_identity)->defined) {
    fail("fork names a defined thread or frame");
  }
  AihcValue *action = parse_object(tokens[3]);
  /* The stop frame of the new thread gets an info table of its own. */
  uint8_t *pointers = checked_calloc(1, 1);
  Frame *record = define_frame(frame_identity, NULL, AIHC_FRAME_STOP, 0,
                               pointers, NULL, 0, identity);
  capability->machine->thread_done_info = record->info;
  const uint8_t *before = capability->heap_next;
  AihcThread *child = (AihcThread *)(uintptr_t)aihc_fork(capability, action);
  consume_reservation(before);
  record->address = (AihcValue *)aihc_stack_base(capability->machine->stacks);
  adopt_thread(child, identity);
  entry_of(identity)->top = frame_identity;
}

static void command_block(char **tokens, size_t count) {
  if (count != 2) {
    fail("block expects a thunk");
  }
  AihcValue *continuation = running_continuation();
  AihcValue *thunk = parse_object(tokens[1]);
  const uint8_t *before = capability->heap_next;
  const AihcResume *resume =
      aihc_block_on_blackhole(capability, thunk, continuation);
  consume_reservation(before);
  apply_resume(resume);
}

/* Promote an object and store the result in a global, as an escape into a
   global object does. The promotion collects when the object reaches a
   mutable object. Afterwards the driver names each promoted thunk by its
   copy, because the original is an indirection to it. */
static void command_promote(char **tokens, size_t count) {
  if (count != 3) {
    fail("promote expects two arguments");
  }
  link_srts();
  AihcValue *object = parse_object(tokens[1]);
  uint64_t index = parse_unsigned(tokens[2]);
  if (index >= capability->machine->global_count) {
    fail("global index out of range");
  }
  uint64_t before = capability->machine->gc_count;
  size_t total = 0;
  AihcSlot *roots = gather_roots(&total);
  AihcValue *promoted =
      aihc_promote(capability, object, total, roots, current_srt);
  scatter_roots(roots, total);
  capability->machine->globals[index] = (AihcSlot)(uintptr_t)promoted;
  promoted_ever = 1;
  if (capability->machine->gc_count != before) {
    report_collection();
    return;
  }
  for (size_t identity = 1; identity < entry_capacity; ++identity) {
    Entry *entry = &entries[identity];
    if (entry->defined && !entry->indirection && entry->address != NULL &&
        entry->address->header == (AihcSlot)(uintptr_t)&aihc_indirection_info) {
      entry->address = (AihcValue *)(uintptr_t)entry->address->fields[0];
    }
  }
}

static void run_command(char **tokens, size_t count) {
  const char *name = tokens[0];
  if (strcmp(name, "machine") == 0) {
    command_machine(tokens, count);
    return;
  }
  if (capability == NULL) {
    fail("machine must come first");
  }
  if (strcmp(name, "srt") == 0) {
    command_srt(tokens, count);
  } else if (strcmp(name, "current_srt") == 0) {
    if (count != 2) {
      fail("current_srt expects one argument");
    }
    link_srts();
    current_srt = srt_of(parse_signed(tokens[1]));
  } else if (strcmp(name, "gen2limit") == 0) {
    if (count != 2) {
      fail("gen2limit expects one argument");
    }
    capability->machine->gen2_limit_bytes = parse_unsigned(tokens[1]);
  } else if (strcmp(name, "capabilities") == 0) {
    if (count != 2) {
      fail("capabilities expects one argument");
    }
    uint64_t total = parse_unsigned(tokens[1]);
    if (total == 0 || total > MAX_CAPABILITIES) {
      fail("capabilities expects a count from one to four");
    }
    aihc_machine_set_capabilities(capability, total);
    heap = capability;
    heap_index = 0;
    /* With several capabilities, every store into a global object can
       promote, and a promotion can copy. */
    if (total > 1) {
      promoted_ever = 1;
    }
  } else if (strcmp(name, "heap") == 0) {
    if (count != 2) {
      fail("heap expects one argument");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= capability->machine->capability_count) {
      fail("heap expects a capability");
    }
    heap = capability->machine->capabilities[index];
    heap_index = index;
  } else if (strcmp(name, "fill") == 0) {
    command_fill(tokens, count);
  } else if (strcmp(name, "reserve") == 0) {
    if (count != 2) {
      fail("reserve expects one argument");
    }
    link_srts();
    uint64_t words = parse_unsigned(tokens[1]);
    ensure(words);
    reserved_words = words;
  } else if (strcmp(name, "new") == 0) {
    link_srts();
    command_new(tokens, count);
  } else if (strcmp(name, "array") == 0) {
    link_srts();
    command_array(tokens, count);
  } else if (strcmp(name, "mvar") == 0) {
    command_mvar(tokens, count);
  } else if (strcmp(name, "set") == 0) {
    command_set(tokens, count);
  } else if (strcmp(name, "supdate") == 0) {
    if (count != 3) {
      fail("supdate expects two arguments");
    }
    uint64_t slot = parse_unsigned(tokens[1]);
    if (slot >= STATIC_THUNKS) {
      fail("supdate expects a static thunk");
    }
    escaping_update(aihc_update, (AihcValue *)&static_thunks[slot],
                    parse_object(tokens[2]));
  } else if (strcmp(name, "sset") == 0) {
    if (count != 4) {
      fail("sset expects three arguments");
    }
    uint64_t slot = parse_unsigned(tokens[1]);
    uint64_t field = parse_unsigned(tokens[2]);
    if (slot < STATIC_THUNKS || slot >= STATIC_THUNKS + STATIC_NODES ||
        field >= STATIC_NODE_FIELDS) {
      fail("sset expects a static node field");
    }
    AihcSlot value = parse_pointer(tokens[3]);
    if (value != 0 &&
        !in_range((const void *)(uintptr_t)value,
                  (const uint8_t *)static_thunks, sizeof(static_thunks)) &&
        !in_range((const void *)(uintptr_t)value, (const uint8_t *)static_nodes,
                  sizeof(static_nodes)) &&
        !in_range((const void *)(uintptr_t)value,
                  (const uint8_t *)static_nullary, sizeof(static_nullary))) {
      fail("static node fields hold static objects only");
    }
    /* A static node is old, so the store has the barrier that compiled
       code puts before a store into an old object. */
    aihc_write_barrier(capability,
                       (AihcValue *)&static_nodes[slot - STATIC_THUNKS]);
    static_nodes[slot - STATIC_THUNKS].fields[field] = value;
  } else if (strcmp(name, "ssrt") == 0) {
    if (count != 3) {
      fail("ssrt expects two arguments");
    }
    link_srts();
    uint64_t slot = parse_unsigned(tokens[1]);
    const AihcSrt *srt = srt_of(parse_signed(tokens[2]));
    if (slot < STATIC_THUNKS) {
      static_thunk_info[slot].srt = srt;
    } else if (slot < STATIC_THUNKS + STATIC_NODES) {
      static_node_info[slot - STATIC_THUNKS].srt = srt;
    } else {
      fail("ssrt expects a static thunk or node");
    }
  } else if (strcmp(name, "global") == 0) {
    if (count != 3) {
      fail("global expects two arguments");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= capability->machine->global_count) {
      fail("global index out of range");
    }
    capability->machine->globals[index] = parse_pointer(tokens[2]);
  } else if (strcmp(name, "promote") == 0) {
    command_promote(tokens, count);
  } else if (strcmp(name, "root") == 0) {
    if (count != 3) {
      fail("root expects two arguments");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= root_count) {
      fail("root index out of range");
    }
    root_slots[index] = parse_pointer(tokens[2]);
  } else if (strcmp(name, "stable") == 0) {
    if (count != 2) {
      fail("stable expects one argument");
    }
    AihcValue *object = parse_object(tokens[1]);
    const uint8_t *before = capability->heap_next;
    AihcStableName *stable = aihc_stable_name_make(capability, object);
    int collected = 0;
    while (stable == NULL) {
      /* With several capabilities, the name of a young object asks for the
         collection of an escape first, as a store safepoint gives it. */
      size_t total = 0;
      AihcSlot *roots = gather_roots(&total);
      AihcSlot *extended = checked_calloc(total + 1, sizeof(*extended));
      memcpy(extended, roots, total * sizeof(*extended));
      extended[total] = (AihcSlot)(uintptr_t)object;
      aihc_escape_collect(capability, total + 1, extended, current_srt);
      memcpy(roots, extended, total * sizeof(*extended));
      object = (AihcValue *)(uintptr_t)extended[total];
      free(extended);
      scatter_roots(roots, total);
      before = capability->heap_next;
      stable = aihc_stable_name_make(capability, object);
      collected = 1;
    }
    consume_reservation(before);
    int known = 0;
    for (size_t index = 0; index < stable_count; ++index) {
      if (stable_names[index] == stable) {
        known = 1;
      }
    }
    if (!known) {
      if (stable_count >= SIZE_MAX / sizeof(*stable_names) - 1) {
        fail("too many stable names");
      }
      AihcStableName **grown =
          realloc(stable_names, (stable_count + 1) * sizeof(*grown));
      if (grown == NULL) {
        fail("out of memory");
      }
      stable_names = grown;
      memmove(stable_names + 1, stable_names,
              stable_count * sizeof(*stable_names));
      stable_names[0] = stable;
      ++stable_count;
    }
    /* The report shows the state after the command. */
    if (collected) {
      report_collection();
    }
  } else if (strcmp(name, "push") == 0) {
    link_srts();
    command_push(tokens, count);
  } else if (strcmp(name, "enter") == 0) {
    command_enter(tokens, count);
  } else if (strcmp(name, "raise") == 0) {
    if (count != 2) {
      fail("raise expects an exception");
    }
    AihcValue *continuation = running_continuation();
    AihcValue *exception = parse_object(tokens[1]);
    apply_resume(aihc_raise(capability, exception, continuation));
  } else if (strcmp(name, "yield") == 0) {
    if (count != 1) {
      fail("yield expects no argument");
    }
    apply_resume(aihc_yield(capability, running_continuation()));
  } else if (strcmp(name, "fork") == 0) {
    command_fork(tokens, count);
  } else if (strcmp(name, "block") == 0) {
    command_block(tokens, count);
  } else if (strcmp(name, "take") == 0 || strcmp(name, "read") == 0) {
    if (count != 2) {
      fail("take expects an MVar");
    }
    AihcValue *continuation = running_continuation();
    AihcMVar *mvar = parse_mvar(tokens[1]);
    const uint8_t *before = capability->heap_next;
    const AihcResume *resume =
        name[0] == 't' ? aihc_mvar_take(capability, mvar, continuation)
                       : aihc_mvar_read(capability, mvar, continuation);
    consume_reservation(before);
    apply_resume(resume);
  } else if (strcmp(name, "put") == 0) {
    if (count != 3) {
      fail("put expects an MVar and a value");
    }
    AihcValue *continuation = running_continuation();
    AihcMVar *mvar = parse_mvar(tokens[1]);
    AihcSlot value = parse_pointer(tokens[2]);
    /* With several capabilities, a put into a global MVar can collect for
       its promotion. The roots of the driver go into a host root frame for
       the call, so that collection updates them. */
    uint64_t collections = capability->machine->gc_count;
    size_t total = 0;
    AihcSlot *roots = gather_roots(&total);
    AihcRootFrame frame;
    aihc_roots_enter(capability, &frame, total, roots);
    const uint8_t *before = capability->heap_next;
    const AihcResume *resume =
        aihc_mvar_put(capability, mvar, value, continuation);
    aihc_roots_leave(capability, &frame);
    scatter_roots(roots, total);
    if (capability->machine->gc_count == collections) {
      consume_reservation(before);
    } else {
      /* The collection emptied the nursery, so the operation took its words
         from the start of the nursery. */
      consume_reservation(capability->heap_start);
    }
    apply_resume(resume);
    if (capability->machine->gc_count != collections) {
      report_collection();
    }
  } else if (strcmp(name, "cycle") == 0) {
    if (count != 1) {
      fail("cycle expects no argument");
    }
    link_srts();
    /* An explicit collection consumes nothing of the reservation: the
       nursery is empty afterwards, so the reserved words still fit. */
    start_cycle();
  } else if (strcmp(name, "collect") == 0) {
    /* Without an argument, the collection is a minor one. With one, the
       script names the oldest generation to copy. */
    if (count > 2) {
      fail("collect expects at most one argument");
    }
    link_srts();
    uint64_t generation = count == 1 ? 0 : parse_unsigned(tokens[1]);
    if (generation > 2) {
      fail("collect expects a generation up to two");
    }
    collect_generation((unsigned)generation);
  } else {
    fail("unknown command");
  }
}

static size_t tokenize(char *line, char **tokens) {
  size_t count = 0;
  char *cursor = line;
  while (*cursor != 0) {
    while (*cursor == ' ' || *cursor == '\t' || *cursor == '\n' ||
           *cursor == '\r') {
      ++cursor;
    }
    if (*cursor == 0) {
      break;
    }
    if (count == MAX_TOKENS) {
      fail("too many tokens");
    }
    tokens[count++] = cursor;
    while (*cursor != 0 && *cursor != ' ' && *cursor != '\t' &&
           *cursor != '\n' && *cursor != '\r') {
      ++cursor;
    }
    if (*cursor != 0) {
      *cursor = 0;
      ++cursor;
    }
  }
  return count;
}

int main(int argc, char *const argv[]) {
  aihc_program_arguments_initialize(argc, argv);
  initialize_statics();
  char *line = checked_calloc(LINE_CAPACITY, 1);
  char **tokens = checked_calloc(MAX_TOKENS, sizeof(*tokens));
  while (fgets(line, LINE_CAPACITY, stdin) != NULL) {
    size_t length = strlen(line);
    if (length + 1 == LINE_CAPACITY && line[length - 1] != '\n') {
      fail("line is too long");
    }
    size_t count = tokenize(line, tokens);
    if (count == 0) {
      continue;
    }
    if (strcmp(tokens[0], "end") == 0) {
      printf("done\n");
      fflush(stdout);
      command_index = 0;
      continue;
    }
    run_command(tokens, count);
    ++command_index;
  }
  free(line);
  free(tokens);
  return 0;
}
