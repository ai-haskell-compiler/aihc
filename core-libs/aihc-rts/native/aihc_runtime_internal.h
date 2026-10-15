#ifndef AIHC_RUNTIME_INTERNAL_H
#define AIHC_RUNTIME_INTERNAL_H

#include "aihc_runtime.h"

#include <errno.h>
#include <stddef.h>

typedef struct AihcBlackholeWaiter AihcBlackholeWaiter;
typedef struct AihcMVarWaiter AihcMVarWaiter;

typedef enum {
  AIHC_IO_READ,
  AIHC_IO_WRITE,
  AIHC_IO_OPEN,
  AIHC_IO_TIMER,
} AihcIoKind;

typedef enum {
  AIHC_IO_READABLE = 1U << 0,
  AIHC_IO_WRITABLE = 1U << 1,
} AihcIoCapability;

typedef enum {
  AIHC_IO_ERROR_IO = EIO,
  AIHC_IO_ERROR_BAD_DESCRIPTOR = EBADF,
  AIHC_IO_ERROR_INVALID_ARGUMENT = EINVAL,
  AIHC_IO_ERROR_NOT_SUPPORTED = ENOSYS,
} AihcIoError;

typedef enum {
  AIHC_IO_SUBMITTED,
  AIHC_IO_PENDING,
  AIHC_IO_COMPLETED,
  AIHC_IO_CONSUMED,
} AihcIoState;

typedef enum {
  AIHC_IO_POLL_PROGRESS,
  AIHC_IO_POLL_SUSPENDED,
} AihcIoPollOutcome;

struct AihcThread {
  AihcSlot header;
  /* The number that identifies the thread. The field is directly after the
     header, and both are eight bytes on every target. Thus compiled code can
     read it at one offset. Refer to docs/native-runtime-objects.md. */
  uint64_t id;
  AihcResumeKind resume_kind;
  AihcValue *resume_function;
  AihcValue *resume_continuation;
  AihcSlot resume_value;
  uint64_t resume_count;
  AihcThread *next;
  AihcTransaction *transaction;
  /* The blackhole queues of the thunks that this thread claimed. */
  AihcBlackholeQueue *queues;
  /* The index of the capability whose run queue holds the thread, or that
     runs it or will wake it. Only that capability changes the field, when
     it pushes the thread to another capability. */
  uint64_t capability;
};

/* Match the fork# reservation in Aihc.Grin.Primitive. */
_Static_assert(sizeof(AihcThread) <= 11 * sizeof(AihcSlot),
               "thread exceeds the GRIN reservation");

typedef struct AihcTransactionWrite AihcTransactionWrite;

struct AihcTransactionWrite {
  AihcSlot header;
  AihcValue *variable;
  AihcSlot previous;
  AihcTransactionWrite *next;
};

struct AihcTransactionTimer {
  AihcSlot header;
  AihcValue *variable;
  AihcSlot final;
  uint64_t deadline;
  AihcTransactionTimer *next;
};

struct AihcTransaction {
  AihcSlot header;
  AihcTransactionWrite *writes;
  AihcTransaction *parent;
};

/* Match the fixed allocation bounds in Aihc.Grin.Primitive.
   Heap slots have eight bytes on both 32-bit and 64-bit targets. */
_Static_assert(sizeof(AihcTransaction) <= 3 * sizeof(AihcSlot),
               "transaction exceeds the GRIN reservation");
_Static_assert(sizeof(AihcTransactionWrite) <= 4 * sizeof(AihcSlot),
               "write log exceeds the GRIN reservation");
_Static_assert(sizeof(AihcTransactionTimer) <= 5 * sizeof(AihcSlot),
               "timer exceeds the GRIN reservation");

struct AihcBlackholeWaiter {
  AihcSlot header;
  AihcThread *thread;
  AihcValue *continuation;
  AihcBlackholeWaiter *next;
};

/* A claimed thunk with waiters names a blackhole queue in field zero. The
   queue names the owner thread, the thunk, and the waiters in wake order.
   The owner keeps its queues on a list, so it can wake them when it updates
   or abandons the thunk. */
struct AihcBlackholeQueue {
  AihcSlot header;
  AihcThread *owner;
  AihcValue *thunk;
  AihcBlackholeWaiter *head;
  AihcBlackholeWaiter *tail;
  AihcBlackholeQueue *next;
};

/* Match the evaluation reservation in aihc_constants.lir. */
_Static_assert(sizeof(AihcBlackholeWaiter) <= 4 * sizeof(AihcSlot),
               "blackhole waiter exceeds the GRIN reservation");
_Static_assert(sizeof(AihcBlackholeWaiter) + sizeof(AihcBlackholeQueue) <=
                   10 * sizeof(AihcSlot),
               "blackhole queue exceeds the GRIN reservation");

struct AihcMVarWaiter {
  AihcSlot header;
  AihcThread *thread;
  AihcValue *continuation;
  AihcSlot value;
  AihcMVarWaiter *next;
};

struct AihcMVar {
  AihcSlot header;
  uint8_t full;
  AihcSlot value;
  AihcMVarWaiter *readers_head;
  AihcMVarWaiter *readers_tail;
  AihcMVarWaiter *takers_head;
  AihcMVarWaiter *takers_tail;
  AihcMVarWaiter *putters_head;
  AihcMVarWaiter *putters_tail;
};

_Static_assert(sizeof(AihcMVar) <= 9 * sizeof(AihcSlot),
               "MVar exceeds the GRIN reservation");
_Static_assert(sizeof(AihcMVarWaiter) <= 5 * sizeof(AihcSlot),
               "MVar waiter exceeds the GRIN reservation");

/* aihc_stable_name.lir allocates and links these, so every field starts one
   eight-byte slot after the last on every target. The static assertions in
   aihc_runtime.c hold the two descriptions together. */
/* The referent and lookup link are weak. Neither field retains an object. */
struct AihcStableName {
  AihcSlot header;
  AihcValue *value;
  uint64_t hash;
  AihcStableName *next;
};

struct AihcIoHandle {
  AihcSlot header;
  int64_t error;
  uintptr_t backend_token;
  uint64_t position;
  uint32_t capabilities;
  uint8_t append;
  uint8_t closed;
};

struct AihcIoRequest {
  AihcSlot header;
  AihcCapability *capability;
  AihcIoKind kind;
  AihcIoState state;
  AihcIoHandle *handle;
  uint8_t *buffer;
  AihcValue *buffer_owner;
  size_t offset;
  size_t length;
  int64_t mode;
  uint64_t deadline;
  AihcThread *thread;
  AihcValue *continuation;
  int64_t result;
  AihcIoRequest *next;
};

/* Sixteen slots include the two pinned metadata slots. */
_Static_assert(sizeof(AihcIoRequest) <= 14 * sizeof(AihcSlot),
               "IO request exceeds the GRIN reservation");

struct AihcIoBackend {
  int (*prepare)(AihcIoRequest *request);
  int (*try_request)(AihcIoRequest *request, int64_t *result);
  int64_t (*finish_request)(AihcIoRequest *request, int64_t result);
  AihcIoPollOutcome (*poll)(AihcCapability *capability, int may_block);
};

typedef AihcSlot (*AihcRootVisitor)(AihcSlot root, void *context);

int aihc_visit_runtime_object(AihcValue *object, AihcRootVisitor visitor,
                              void *context);

_Noreturn void aihc_fail(const char *message);
void aihc_record_allocation(AihcCapability *capability);
/* Six fixed slots precede the inline byte payload. */
typedef struct {
  AihcSlot header;
  uint64_t size;
  union {
    uint8_t *contents;
    AihcSlot contents_slot;
  };
  uint64_t pinned;
  uint64_t alignment;
  uint64_t words;
} AihcByteArray;

/* Two metadata slots precede each pinned object on both target word sizes.
   The second slot packs the charge of the block with its owner, its
   generation, and its marks: the low 48 bits are the bytes, bits 48 to 55
   the index of the capability that allocated the block, bits 56 to 59 the
   generation, and bit 63 the mark of the running collection. A young block
   belongs to the local heap of its owner. */
typedef struct AihcPinnedBlock {
  union {
    struct AihcPinnedBlock *next;
    AihcSlot next_slot;
  };
  uint64_t bytes;
  AihcSlot object[];
} AihcPinnedBlock;

#define AIHC_PINNED_BYTES_MASK ((UINT64_C(1) << 48) - 1)
#define AIHC_PINNED_OWNER_SHIFT 48
#define AIHC_PINNED_OWNER_MASK (UINT64_C(255) << AIHC_PINNED_OWNER_SHIFT)
#define AIHC_PINNED_GENERATION_SHIFT 56
#define AIHC_PINNED_GENERATION_MASK                                            \
  (UINT64_C(15) << AIHC_PINNED_GENERATION_SHIFT)
#define AIHC_PINNED_MARK (UINT64_C(1) << 63)
/* The mark of the gen2 cycle: a block of generation two is live at the end
   of the cycle when it is set. */
#define AIHC_PINNED_CYCLE_MARK (UINT64_C(1) << 62)

static inline uint64_t aihc_pinned_bytes(const AihcPinnedBlock *block) {
  return block->bytes & AIHC_PINNED_BYTES_MASK;
}

static inline uint64_t aihc_pinned_owner(const AihcPinnedBlock *block) {
  return (block->bytes & AIHC_PINNED_OWNER_MASK) >> AIHC_PINNED_OWNER_SHIFT;
}

static inline void aihc_pinned_set_owner(AihcPinnedBlock *block,
                                         uint64_t owner) {
  block->bytes = (block->bytes & ~AIHC_PINNED_OWNER_MASK) |
                 (owner << AIHC_PINNED_OWNER_SHIFT);
}

static inline unsigned aihc_pinned_generation(const AihcPinnedBlock *block) {
  return (unsigned)((block->bytes & AIHC_PINNED_GENERATION_MASK) >>
                    AIHC_PINNED_GENERATION_SHIFT);
}

static inline void aihc_pinned_set_generation(AihcPinnedBlock *block,
                                              unsigned generation) {
  block->bytes = (block->bytes & ~AIHC_PINNED_GENERATION_MASK) |
                 ((uint64_t)generation << AIHC_PINNED_GENERATION_SHIFT);
}

/* The block of a pinned object or a large object. */
static inline AihcPinnedBlock *aihc_pinned_block_of(const AihcValue *object) {
  return (AihcPinnedBlock *)((uint8_t *)(uintptr_t)object -
                             offsetof(AihcPinnedBlock, object));
}

_Static_assert(sizeof(AihcByteArray) == 48, "byte-array descriptor size");
_Static_assert(offsetof(AihcByteArray, contents) == 16,
               "byte-array contents offset");
_Static_assert(offsetof(AihcByteArray, words) == 40,
               "byte-array allocation size offset");
_Static_assert(offsetof(AihcPinnedBlock, object) == 16,
               "pinned allocation metadata size");

AihcValue *aihc_gc_allocate_pinned(AihcCapability *capability, uint64_t words);
/* Give a block of the pinned list back: a region run to the region table, a
   C allocation to the C allocator. */
void aihc_pinned_block_release(AihcPinnedBlock *block);

/* Heap regions. Every managed allocation lives in a run of regions of
   AIHC_REGION_BYTES that the runtime takes from the host one mapping at a
   time. A two-level table gives the kind of the region that holds an
   address. An address outside every mapping has the kind
   AIHC_REGION_OUTSIDE: it names a static object, a C allocation, or the
   memory of another allocator. */
typedef enum {
  AIHC_REGION_OUTSIDE = 0,
  AIHC_REGION_FREE,
  AIHC_REGION_NURSERY,
  AIHC_REGION_GEN1,
  AIHC_REGION_GEN2,
  /* The blocks of gen1 that the running collection copies away. */
  AIHC_REGION_FROM1,
  AIHC_REGION_LARGE,
  AIHC_REGION_PINNED,
  AIHC_REGION_STACK,
} AihcRegionKind;

/* The number of regions that hold the given bytes. */
size_t aihc_regions_for_bytes(size_t bytes);
/* Acquire a run of consecutive regions with the given kind. The run starts
   at a region boundary. The function does not return when the host has no
   room for the run. */
void *aihc_regions_acquire(size_t count, AihcRegionKind kind);
/* Acquire a run whose start is a multiple of alignment regions.
   The alignment must be a power of two. Unused regions remain available. */
void *aihc_regions_acquire_aligned(size_t count, size_t alignment,
                                   AihcRegionKind kind);
/* Give a run back. The address is the start of a run that acquire gave. */
void aihc_regions_release(void *base);
/* Change the kind of every region of an acquired run. */
void aihc_regions_set_kind(void *base, AihcRegionKind kind);
/* The kind and run base from one region table lookup. */
typedef struct {
  AihcRegionKind kind;
  void *base;
} AihcRegionLocation;
AihcRegionLocation aihc_region_location(const void *address);
/* The kind of the region that holds an address. */
AihcRegionKind aihc_region_kind(const void *address);
/* The start of the acquired run that holds an address, or null outside every
   mapping. */
void *aihc_region_run_base(const void *address);

/* Visit every object of the heap: the nursery, the blocks of gen1, the
   occupied slots of gen2, and the large objects. The visitor must not
   allocate. The test drivers use this walk. */
typedef void (*AihcObjectVisitor)(AihcValue *object, void *context);
void aihc_gc_walk_objects(AihcCapability *capability, AihcObjectVisitor visitor,
                          void *context);

/* The host side of the region table. Map count regions at a region
   boundary, or give null when the host has no room. The content of the
   mapping is unspecified. */
uint8_t *aihc_host_map_regions(size_t count);
/* Give a mapping back to the host. The result is zero when the memory is
   unmapped, or nonzero when the host keeps it, as linear memory does. */
int aihc_host_unmap_regions(void *base, size_t count);

/* Add an object to the remembered set. */
void aihc_remember(AihcCapability *capability, AihcValue *object);
/* The write barrier of an update. A blackhole is updated once, so its
   remembered-set entry cannot repeat and needs no compaction. */
void aihc_write_barrier_update(AihcCapability *capability, AihcValue *object);
/* Replace the nursery of a capability with an empty one of the given bytes.
   The collector fuzz driver uses it to force frequent collections. */
void aihc_nursery_replace(AihcCapability *capability, size_t bytes);
/* Change the number of capabilities. All nurseries must be empty. A removed
   capability must be idle. Its gen1 blocks go to the first capability. */
void aihc_machine_set_capabilities(AihcCapability *capability, uint64_t count);
/* Empty the heap of a capability: release every block, every pinned block,
   and the remembered set, and give the capability an empty nursery of the
   given bytes. The caller holds no pointer into the old heap afterwards. */
void aihc_heap_reset(AihcCapability *capability, size_t nursery_bytes);
/* Run one collection of the generations up to the given one. */
void aihc_gc_collect_generation(AihcCapability *capability, unsigned generation,
                                uint64_t root_count, AihcSlot *roots,
                                const AihcSrt *srt);

/* Thread stacks. Each thread owns a doubly linked list of chunks, and the
   continuation frames of the thread live in them. A chunk has
   AIHC_STACK_CHUNK_BYTES bytes and the same alignment, so the chunk of any
   address in it is the address with the low bits cleared. The frames of a
   chunk start after its header.

   Frames are pushed and popped in stack order. A push writes the frame at
   the stack pointer. When the frame does not fit in the rest of the chunk,
   the push continues at the start of the next chunk. Compiled code keeps
   the stack pointer and the stack limit, the end of the chunk of the stack
   pointer, in registers. capability->stack_next holds a copy of the stack
   pointer only while a runtime function can read it: compiled code stores
   the stack pointer before it calls such a function. A runtime function
   that changes the stack pointer of a thread, such as the scheduler, writes
   capability->stack_next, and compiled code loads it again. A new chunk
   comes from the C allocator and not from the managed heap, so a push never
   collects. But a growth charges the new chunk to the nursery, and a
   reservation after it collects when the charges use the nursery. See
   aihc_stack_grow. The first frame of a chunk keeps the address of its parent
   in field zero, whichever chunk the parent is in. Thus the chain of frames
   needs no link frame at a chunk boundary.

   Frames never move. The collector finds a live frame through a pointer to
   it, as it finds a static object, and scans it in place. A stack stays
   while the collector retains its thread.

   Both sizes must agree with aihc_constants.lir and with stackChunkBytes
   and stackChunkHeaderBytes in Aihc.Lir.Lower. */
#define AIHC_STACK_CHUNK_BYTES ((size_t)4096)
#define AIHC_STACK_CHUNK_HEADER_BYTES ((size_t)64)

typedef struct AihcStack AihcStack;
typedef struct AihcStackChunk AihcStackChunk;

struct AihcStackChunk {
  AihcStack *stack;
  AihcStackChunk *below;
  AihcStackChunk *above;
  /* The highest frame the marking of cycle scanned_cycle has scanned in
     this chunk. Every frame of the chunk at or below it is scanned. */
  uint8_t *scanned_from;
  /* The age of the frames in the chunk, packed with the bookkeeping of the
     running collection: bits 0 to 7 hold the generation, bits 8 to 15 the
     youngest generation a referent of a scanned frame ended in, and the
     rest the number of the collection those bits belong to. See
     docs/gc-design.md. */
  uint64_t state;
  uint64_t scanned_cycle;
  /* The number of chunks below this one in its stack. */
  uint32_t depth;
  /* The low 32 bits of one plus the collection count at the last charge.
     The collector clears old stamps before the counter repeats. */
  uint32_t charged_gc;
  AihcCapability *capability;
};

_Static_assert(offsetof(AihcStackChunk, capability) == 4 * sizeof(void *) + 24,
               "stack capability ABI");

_Static_assert(sizeof(AihcStackChunk) <= AIHC_STACK_CHUNK_HEADER_BYTES,
               "stack chunk header exceeds its reserved bytes");

struct AihcStack {
  /* The thread that owns the stack. The collector updates this pointer
     and releases the stack when the thread is not retained. */
  AihcThread *thread;
  AihcStackChunk *base;
  AihcStack *next;
  /* The top frame of the stack when its thread suspended last. The frames
     at or below it are live while the thread is not running. */
  AihcValue *top;
  /* The frames the marking of the active gen2 cycle deferred. Each one is
     the highest frame of a chunk below a chunk that the cycle scanned. A
     slice takes them. The list is empty when the cycle scanned every frame
     below the ones it scanned. */
  AihcValue **pending;
  size_t pending_count;
  size_t pending_capacity;
};

/* Make the stack of a new thread. */
AihcStack *aihc_stack_new(AihcCapability *capability, AihcThread *thread);
/* The address of the first frame of a stack. */
uint8_t *aihc_stack_base(const AihcStack *stack);
/* The stack that holds a frame. */
AihcStack *aihc_stack_of(const void *frame);
/* Record the top frame of a stack whose thread suspends with the given
   continuation. The incremental marking reads frames of a suspended thread
   only at or below this frame. */
void aihc_stack_note_top(AihcValue *continuation);
/* The pause of the running thread before another thread runs: claim the
   thunk of each update frame of its stack that no pause processed. See
   "Blackholing" in docs/multi-threaded-rts-design.md. A collection is a
   pause too, and the collector claims the thunks of the frames it scans. */
void aihc_thread_pause(AihcCapability *capability, AihcValue *continuation);
/* Claim a thunk for a thread when the thunk is in the state Thunk: field
   zero names the owner, and the header gets bit zero. The caller takes the
   write barrier first when the thunk can be old and a mutator runs. */
void aihc_thunk_claim(AihcValue *thunk, AihcThread *owner);
/* The C runtime reads the fields of a live frame it will pop: an exception
   walk or a continuation capture. The marking of the active gen2 cycle
   scans the frame and the frames below it in its chunk first. */
void aihc_gc_frame_read(AihcCapability *capability, AihcValue *frame);
/* Collect the nursery and gen1 and start an incremental gen2 cycle. The
   test drivers use this entry. */
void aihc_gc_start_cycle(AihcCapability *capability, uint64_t root_count,
                         AihcSlot *roots, const AihcSrt *srt);
/* Promote an escaping value: copy it and the local objects it reaches into
   gen2, and return the global value. A value that reaches a mutable object
   runs a collection that moves every young survivor into gen2, with the
   value as one more root, so the caller passes its roots. See "Local heaps"
   in docs/multi-threaded-rts-design.md. */
AihcValue *aihc_promote(AihcCapability *capability, AihcValue *value,
                        uint64_t root_count, AihcSlot *roots,
                        const AihcSrt *srt);
/* The value that a store of value into object stores. When the machine has
   several capabilities, the object is global, and the value is young, the
   store is an escape and the result is the global copy of the value. The
   result is AIHC_ESCAPE_COLLECT when the promotion needs a collection. */
AihcSlot aihc_escape(AihcCapability *capability, const AihcValue *object,
                     AihcSlot value);
/* Whether a value is young: in a nursery, in gen1, or a young pinned block.
   Until local collection is enforced, every young object counts as
   local. */
int aihc_value_is_young(const AihcMachine *machine, const void *value);
/* Allocate a runtime record that a runtime operation links into the given
   object. With several capabilities and a global object, the record goes
   into gen2, so no global object names a local record. The words come from
   the reservation of the caller in both cases. */
AihcValue *aihc_record_allocate(AihcCapability *capability,
                                const AihcValue *container, uint64_t words);
/* The value that a scheduler operation stores into an object, promoted when
   the store is an escape. A scheduler operation has its live values in its
   continuation, so the promotion can collect. The continuation is also the
   top frame of the pause walk. */
AihcSlot aihc_escape_in_operation(AihcCapability *capability,
                                  const AihcValue *object, AihcSlot value,
                                  AihcValue *continuation);
/* Whether makeStableName# of a value must collect first: the machine has
   several capabilities, and the value is young. */
int64_t aihc_stable_name_escapes(AihcCapability *capability,
                                 const AihcValue *value);
/* The copy of a run into a global array with several capabilities: zero when
   the copy is not an escape, one when this function copied, and two when a
   promotion needs a collection and nothing was stored. */
int64_t aihc_array_copy_escaping(AihcCapability *capability, AihcValue *source,
                                 uint64_t source_offset, AihcValue *target,
                                 uint64_t target_offset, uint64_t count);
/* The collection of an escape: it moves every young survivor into gen2. */
void aihc_escape_collect(AihcCapability *capability, uint64_t root_count,
                         AihcSlot *roots, const AihcSrt *srt);
/* Give the chunks of a stack back. No frame of the stack can be live. */
void aihc_stack_release(AihcCapability *capability, AihcStack *stack);
/* Give each chunk of the stack of a thread to another capability, which
   runs the thread from now on. */
void aihc_stack_move(AihcThread *thread, AihcCapability *capability);
/* Push one frame of the given words on the stack of the running thread.
   The caller writes the header and every field. */
AihcValue *aihc_stack_push(AihcCapability *capability, uint64_t words);
/* The slow path of a push: the frame does not fit in the chunk of the
   given stack pointer. Give back the frame at the start of the next chunk,
   AIHC_STACK_CHUNK_HEADER_BYTES after the start of the chunk. Compiled code
   computes the stack limit from this rule. The function does not change
   capability->stack_next, because compiled code keeps the stack pointer
   itself. */
AihcValue *aihc_stack_grow(AihcCapability *capability, uint8_t *stack_next,
                           uint64_t words);
/* Set the stack pointer to the first byte after a frame. The frame must be
   the topmost live frame of its stack. */
void aihc_stack_resume_after(AihcCapability *capability,
                             const AihcValue *frame);

/* A host scope publishes all live C references before a host call can collect.
   Pinned ABI buffers remain roots until the scope ends. */
typedef struct AihcHostBuffer AihcHostBuffer;
typedef struct AihcRootFrame {
  struct AihcRootFrame *next;
  AihcSlot *roots;
  uint64_t count;
  AihcHostBuffer *buffers;
} AihcRootFrame;
void aihc_roots_enter(AihcCapability *capability, AihcRootFrame *frame,
                      uint64_t count, AihcSlot *roots);
void aihc_roots_leave(AihcCapability *capability, AihcRootFrame *frame);
void aihc_visit_host_roots(AihcCapability *capability, AihcRootVisitor visitor,
                           void *context);
void *aihc_host_byte_array(AihcCapability *capability, AihcRootFrame *frame,
                           uint64_t bytes);
extern const AihcInfo aihc_io_handle_info;
AihcIoHandle *aihc_io_handle_new(AihcCapability *capability);
/* Five slots hold each handle. Open requests reserve these slots in advance. */
_Static_assert(sizeof(AihcIoHandle) <= 5 * sizeof(AihcSlot),
               "IO handle exceeds the GRIN reservation");
void *aihc_rts_root(uint64_t index);
void aihc_rts_set_root(uint64_t index, void *value);
void *aihc_wasi_allocate(uint64_t bytes);
void *aihc_wasi_reallocate(void *ptr, size_t old_size, size_t align,
                           size_t new_size);
void aihc_memory_copy(void *destination, const void *source, uint64_t length);
void aihc_memory_move(void *destination, const void *source, uint64_t length);
void aihc_memory_set(void *destination, uint64_t byte, uint64_t length);
/* These accessors reach the stable-name fields of the shared machine. */
AihcStableName **aihc_stable_names(AihcCapability *capability);
uint64_t aihc_stable_name_take_hash(AihcCapability *capability);
/* The RTS options of the process, parsed by aihc_runtime_options.lir from
   the arguments the host passed. */
uint64_t aihc_rts_heap_max_bytes(void);
uint64_t aihc_rts_heap_limit_enabled(void);
/* The -A, -B, and -F options, or zero for the default. */
uint64_t aihc_rts_nursery_bytes(void);
uint64_t aihc_rts_nursery_block_bytes(void);
uint64_t aihc_rts_gen1_collections(void);
uint64_t aihc_rts_gen1_collections_set(void);
uint64_t aihc_rts_gen2_factor(void);
/* The -R option, or zero for the default. */
uint64_t aihc_rts_barrier_budget_bytes(void);
uint64_t aihc_rts_capabilities(void);
uint64_t aihc_rts_mark_factor(void);
/* Apply the parsed collector options to a capability whose nursery is empty. */
void aihc_gc_apply_options(AihcCapability *capability);
/* Cap the nursery at the -M limit once the limit applies, so a reservation
   above the limit reaches the runtime, which refuses it. */
void aihc_gc_apply_limit(AihcCapability *capability);
/* The zero-terminated path of the statistics file, or null when the
   environment names none. */
const char *aihc_rts_stats_path(void);
/* Flatten one null-terminated list of NAME=VALUE strings for the parser. */
void aihc_environment_initialize(char *const envp[]);
uint64_t aihc_object_words(const AihcInfo *info);
uint64_t aihc_value_words(const AihcValue *value);
AihcSlot *aihc_array_elements(AihcValue *array);
uint64_t aihc_array_length(const AihcValue *array);
const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                           uint64_t stages,
                                           uint64_t supplied_count);
const AihcInfo *aihc_applied_constructor_info(const AihcInfo *info,
                                              uint64_t applied);
int64_t aihc_io_error(int error);
void aihc_resume_io_request(AihcCapability *capability, AihcIoRequest *request,
                            int64_t result);
const AihcResume *aihc_complete_io(AihcCapability *capability, int64_t result);
void aihc_visit_roots(AihcCapability *capability, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context);
/* The two parts of aihc_visit_roots: the roots of one capability, and the
   roots of the machine with the roots of the requester. */
void aihc_visit_capability_roots(AihcCapability *capability,
                                 AihcRootVisitor visitor, void *context);
void aihc_visit_machine_roots(AihcMachine *machine, uint64_t root_count,
                              AihcSlot *roots, AihcRootVisitor visitor,
                              void *context);
/* The header every runtime object outside the managed heap carries. */

void aihc_gc_init(AihcCapability *capability);
void aihc_gc_collect(AihcCapability *capability, uint64_t words,
                     uint64_t root_count, AihcSlot *roots, const AihcSrt *srt);
/* Refill the current block, or collect when the nursery budget is full. */
void aihc_gc_refill(AihcCapability *capability, uint64_t words,
                    uint64_t root_count, AihcSlot *roots, const AihcSrt *srt);
void aihc_gc_ensure(AihcCapability *capability, uint64_t words,
                    uint64_t root_count, AihcSlot *roots, const AihcSrt *srt);
/* Consume reserved memory without collection. The memory is not initialized
   unless DEBUG is defined. Initialize all fields before a call can collect. */
AihcValue *aihc_gc_allocate(AihcCapability *capability, uint64_t words);
/* Consume a prior reservation with an object in gen2. The object never moves
   and reads as zero. Its bytes charge the nursery budget, as a pinned block
   does. A thread record is the only such object. */
AihcValue *aihc_gc_allocate_global(AihcCapability *capability, uint64_t words);
/* Raise heap_peak_bytes to what the current space holds now. */
void aihc_gc_record_peak(AihcCapability *capability);
/* Fold the bytes the mutator has taken since the last account into
   heap_allocated_bytes and move the base up. Idempotent: a second call in a
   row adds nothing. */
void aihc_heap_account(AihcCapability *capability);

_Noreturn void aihc_host_fail(const char *message);
const AihcIoBackend *aihc_host_io_backend(void);
/* A monotonic clock in nanoseconds, or zero on a host without one. */
uint64_t aihc_host_monotonic_ns(void);
/* Workers. See "Workers" in docs/multi-threaded-rts-design.md. Until build
   step 6, the run lock lets only one worker at a time run Haskell code or
   the runtime. A host without threads has one capability and never starts
   workers. */
/* Start one worker on its own OS thread for each capability except the
   first, which the calling thread runs. The caller then holds the run
   lock. */
void aihc_host_workers_start(AihcCapability *capability);
void aihc_host_run_lock(void);
void aihc_host_run_unlock(void);
/* Whether another worker waits for the run lock. */
int aihc_host_run_lock_wanted(void);
/* A lock that one worker at a time holds. */
void *aihc_host_lock_new(void);
void aihc_host_lock_acquire(void *lock);
void aihc_host_lock_release(void *lock);
/* End the idle wait of a capability. */
void aihc_host_wake(AihcCapability *capability);
/* The entry of a worker: it runs the threads of its capability and does
   not return. The caller holds the run lock. */
_Noreturn void aihc_worker_run(AihcCapability *capability);
void aihc_host_sleep_ns(uint64_t duration);
/* Replace the file at path with the given bytes. The result is zero, or an
   errno value when the host cannot write the file. */
int aihc_host_write_file(const char *path, const void *bytes, size_t length);

#endif
