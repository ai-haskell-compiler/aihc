#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

/* The heap is a range of regions. Every managed allocation lives in a
   region, and a table with one byte per region gives the kind of the memory
   at an address. A region is one WebAssembly page, so the wasm32 host grows
   its memory by whole regions. See docs/native-runtime-objects.md. */
#define AIHC_REGION_SHIFT 16
#define AIHC_REGION_BYTES ((size_t)1 << AIHC_REGION_SHIFT)
/* An object of this many bytes or more gets regions of its own and never
   moves. The bound counts the object with its pinned block header. */
#define AIHC_LARGE_OBJECT_BYTES (AIHC_REGION_BYTES / 2)

/* The defaults of the generational collector. The nursery is one run of
   regions that the mutator fills with a bump pointer. Gen1 holds the
   survivors of the last collections: the gen1 count is the number of
   collections after which the next collection copies all of gen1 into gen2.
   Gen2 holds the rest, and a full collection runs when gen2 has grown by the
   factor since the last one, or at least to the minimum. RTS options -A, -B,
   and -F replace the first three. */
#ifndef AIHC_NURSERY_BYTES
#define AIHC_NURSERY_BYTES (UINT64_C(4) << 20)
#endif
#define AIHC_NURSERY_BLOCK_BYTES (UINT64_C(32) << 10)
#define AIHC_GEN1_COLLECTIONS UINT64_C(1)
#ifndef AIHC_GEN2_MINIMUM_BYTES
#define AIHC_GEN2_MINIMUM_BYTES (UINT64_C(8) << 20)
#endif
#define AIHC_GEN2_FACTOR UINT64_C(2)
/* The pacing of the incremental gen2 marking: each mark slice does the
   factor times the bytes promoted into gen2 since the last slice, at least
   the floor and at most the cap. The cap is the default nursery size: a
   slice marks at most as many bytes as a minor collection can copy. */
#define AIHC_MARK_FACTOR UINT64_C(2)
#ifndef AIHC_MARK_SLICE_FLOOR
#define AIHC_MARK_SLICE_FLOOR (UINT64_C(256) << 10)
#endif
#ifndef AIHC_MARK_SLICE_CAP
#define AIHC_MARK_SLICE_CAP (UINT64_C(4) << 20)
#endif

enum {
  AIHC_OBJECT_NODE,
  AIHC_OBJECT_CLOSURE,
  AIHC_OBJECT_THUNK,
  AIHC_OBJECT_PARTIAL_CONSTRUCTOR,
  AIHC_OBJECT_INDIRECTION,
  AIHC_OBJECT_BLACKHOLE,
  AIHC_OBJECT_ARRAY,
  AIHC_OBJECT_THREAD,
  /* Reserved legacy object kind. */
  AIHC_OBJECT_RUNTIME,
  AIHC_OBJECT_TRANSACTION,
  AIHC_OBJECT_TRANSACTION_WRITE,
  AIHC_OBJECT_TRANSACTION_TIMER,
  AIHC_OBJECT_MVAR,
  AIHC_OBJECT_MVAR_WAITER,
  AIHC_OBJECT_BLACKHOLE_WAITER,
  AIHC_OBJECT_BLACKHOLE_QUEUE,
  AIHC_OBJECT_STABLE_NAME,
  AIHC_OBJECT_BYTE_ARRAY,
  AIHC_OBJECT_IO_REQUEST,
  AIHC_OBJECT_IO_HANDLE,
};
typedef uint8_t AihcObjectKind;

/* The values of the needs_eval byte of an info table. Compiled code goes to
   its ready branch when the byte is zero. It follows an indirection itself,
   and it gives a thunk or a blackhole to aihc_lir_eval. */
enum {
  AIHC_NEEDS_EVAL_NONE = 0,
  AIHC_NEEDS_EVAL_ENTER = 1,
  AIHC_NEEDS_EVAL_FOLLOW = 2,
};

typedef struct AihcValue AihcValue;
typedef struct AihcMachine AihcMachine;
typedef struct AihcCapability AihcCapability;
typedef struct AihcTransaction AihcTransaction;
typedef struct AihcTransactionTimer AihcTransactionTimer;
typedef struct AihcInfo AihcInfo;
typedef struct AihcSrt AihcSrt;
typedef struct AihcThread AihcThread;
typedef struct AihcBlackholeQueue AihcBlackholeQueue;
typedef struct AihcIoHandle AihcIoHandle;
typedef struct AihcIoRequest AihcIoRequest;
typedef struct AihcIoBackend AihcIoBackend;
typedef struct AihcMVar AihcMVar;
typedef struct AihcStableName AihcStableName;
typedef uint64_t AihcSlot;
/* The portable entry of an info table. Reserved: Lir stores null until the
   runtime moves to Lir. The exit code of a capability has this type. */
typedef void (*AihcEntry)(void);
/* The backend entry is a Lir function with the signature
   (context, object, continuation, supplied values...). Common runtime code
   preserves it but never calls it. */
typedef void (*AihcBackendEntry)(void);

enum {
  AIHC_RESUME_NONE,
  AIHC_RESUME_APPLY,
  AIHC_RESUME_CONTINUE,
  AIHC_RESUME_RAISE,
};
typedef uint64_t AihcResumeKind;

typedef struct {
  AihcResumeKind kind;
  AihcValue *function;
  AihcValue *continuation;
  AihcSlot value;
  uint64_t count;
} AihcResume;

enum {
  AIHC_FRAME_NONE = 0,
  AIHC_FRAME_NORMAL = 1,
  AIHC_FRAME_CATCH = 2,
  AIHC_FRAME_UPDATE = 3,
  AIHC_FRAME_RESTORE_MASK = 4,
  AIHC_FRAME_STOP = 5,
  /* The delimiter that prompt# pushes: [parent, tag]. Exception unwinding
     passes through it; control0# captures the frames above it. */
  AIHC_FRAME_PROMPT = 6,
  AIHC_FRAME_FORWARD = 7,
};
typedef uint8_t AihcFrameKind;

/* A static reference table names the static objects one function reaches
   without going through a heap object. Tables are chained: a table names the
   tables of the functions its code calls directly, so the collector walks the
   graph instead of the compiler flattening it.

   Backends emit one word-uniform record per table: the walk link, the two
   counts, then object_count static object addresses followed by child_count
   table addresses. The link is mutable, so tables belong in a writable
   section even though info tables themselves are read-only. */
struct AihcSrt {
  /* Null until a collection walks this table, then the next walked table.
     Recursive functions make the table graph cyclic, so the collector links
     every table it walks and clears the whole list when the collection
     ends. */
  AihcSrt *walked;
  uintptr_t object_count;
  uintptr_t child_count;
  uintptr_t entries[];
};

/* Five word-wide fields followed by five byte-wide ones, so Lir addresses
   word field k at offset k words and byte field j at offset 5 words + j on
   every target. See the "Info tables" section of docs/lir.md. */
struct AihcInfo {
  uintptr_t identity;
  /* One byte per slot of the saturated object. A partial constructor indexes
     the same array as the finished one: the slots it has filled are a prefix
     of the slots the saturated constructor holds. */
  const uint8_t *field_is_pointer;
  /* The info table of the saturated constructor, for a partial constructor.
     Null for every other kind. */
  const AihcInfo *next;
  /* Backend-owned dynamic entry. Lir gives this word its own callable
     type. */
  AihcBackendEntry backend_entry;
  /* The static objects this object's code reaches, or null when it reaches
     none. The collector marks them whenever it traces the object. */
  const AihcSrt *srt;
  /* The slots an object of this info table holds. Both tables of a
     constructor give the full width of the saturated object. Every partial
     stage shares one info table, so a partial constructor keeps the count of
     the slots filled so far in field zero. See aihc_value_count. The lowering
     rejects an object with more than 255 slots. */
  uint8_t field_count;
  /* The lowering rejects a function that takes more than 255 arguments. */
  uint8_t remaining_arity;
  /* Continuation closures have their parent in field zero. This kind is
     backend-independent so the runtime can unwind them uniformly. */
  AihcFrameKind frame_kind;
  AihcObjectKind object_kind;
  /* Nonzero for an object that is not a value: AIHC_NEEDS_EVAL_FOLLOW for
     an indirection, and AIHC_NEEDS_EVAL_ENTER for a thunk or a blackhole.
     AIHC_NEEDS_EVAL_NONE for every other kind. The inline evaluation check
     of compiled code reads only this byte. A thunk under evaluation keeps
     its thunk table, and an update changes the header to the indirection
     table. Thus the table always gives the correct value. On 64-bit targets
     the byte uses padding, so the table stays 48 bytes. */
  uint8_t needs_eval;
  /* The index of a constructor in its data declaration, and zero for every
     other kind. A case with many alternatives selects its alternative from
     this field. The field uses padding, so the table stays 48 bytes on a
     64-bit target and 28 bytes on wasm32. */
  uint16_t constructor_index;
};

struct AihcValue {
  /* Ordinarily an unmodified info-table pointer. During semispace collection,
     a forwarded from-space object temporarily holds its to-space address. */
  AihcSlot header;
  /* A partial constructor and a boxed array both spend field zero on a count
     and hold their payload from field one. Reach that payload through
     aihc_partial_fields and aihc_array_elements rather than by hand. */
  AihcSlot fields[];
};

/* A block of an old generation: a run of regions that copied objects fill
   with a bump pointer. The blocks of a generation form a list in
   allocation order. */
typedef struct AihcHeapBlock {
  uint8_t *start;
  uint8_t *next;
  uint8_t *limit;
  struct AihcHeapBlock *link;
} AihcHeapBlock;

/* An old generation. Gen1 is a list of blocks: bytes is their capacity, and
   the scan fields are the Cheney cursor of a collection that copies into
   the generation. Gen2 is a set of segments that the collector keeps, and
   only bytes is used: the bytes of its occupied slots. */
typedef struct {
  AihcHeapBlock *first;
  AihcHeapBlock *last;
  uint64_t bytes;
  AihcHeapBlock *scan_block;
  uint8_t *scan;
} AihcGeneration;

typedef struct AihcForeignFrame {
  struct AihcForeignFrame *previous;
  AihcSlot *roots;
  uint64_t count;
  const AihcSrt *srt;
  /* AIHC_FOREIGN_UNSAFE for an unsafe call, AIHC_FOREIGN_SAFE for a safe
     call that holds the capability, and AIHC_FOREIGN_RELEASED for a safe
     call that released it. Compiled code reserves five words for the frame,
     so this word keeps both facts. */
  uint64_t allow_callbacks;
} AihcForeignFrame;

enum {
  AIHC_FOREIGN_UNSAFE = 0,
  AIHC_FOREIGN_SAFE = 1,
  AIHC_FOREIGN_RELEASED = 2
};

typedef struct AihcCallbackFrame AihcCallbackFrame;
typedef struct AihcCallbackSlot AihcCallbackSlot;

_Static_assert(sizeof(AihcForeignFrame) <= 40,
               "foreign frame exceeds its LIR stack space");

/* The execution state of one capability. */
struct AihcCapability {
  AihcMachine *machine;
  AihcEntry exit_code;
  /* Compiled code keeps the heap pointer and the heap limit in registers.
     heap_next is a copy that compiled code stores before a call that can
     allocate, collect, or read it, and loads again after the call. Only
     runtime functions change heap_limit, so compiled code loads it again
     after such a call and never stores it. */
  uint8_t *heap_next;
  uint8_t *heap_limit;
  /* The first free byte of the stack of the running thread. Continuation
     frames live in stack chunks, not in the managed heap. Compiled code
     keeps the stack pointer in a register: it pushes a frame there, and the
     continue helpers set it to the address of the frame they enter. That
     pops the frame and every frame above it. This field is a copy that
     compiled code stores before a call that can read it. A runtime function
     that selects the stack of a thread writes this field, and compiled code
     loads it again. See aihc_stack_push. */
  uint8_t *stack_next;
  /* The nursery starts at heap_start. The heap limit bounds the current
     block, less the fixed allocation and stack charges. */
  uint8_t *heap_start;
  /* Compiled code reads this field at a fixed offset: six pointer words on
   * every target. */
  AihcThread *current_thread;
  /* Compiled barriers read this size at six pointer words plus eight bytes. */
  uint64_t nursery_bytes;
  /* The top frame of the running thread, which compiled code stores before
     a store that can be an escape, at six pointer words plus sixteen bytes.
     The promotion of the escape runs the pause walk from it first, so a
     thunk under evaluation is claimed and is not copied. NULL when the
     caller does not know the top frame: a promotion then takes the
     collection for a young thunk. See "Local heaps" in
     docs/multi-threaded-rts-design.md. */
  AihcValue *pause_top;
  /* The code and the continuation of the function that started last. Only
     a program built with --profile-time stores them, at the entry of each
     function: the continuation first, then the code. The time profiler
     reads them when a sample interrupts the thread. See aihc_profile.c. */
  const void *profile_code;
  AihcValue *profile_frame;
  /* The block cursor counts reserved bytes, including fixed allocation and
     stack charges. A refill extends the limit without a gap in the heap. */
  uint64_t nursery_taken;
  uint64_t nursery_block_bytes;
  uint64_t heap_allocated_bytes;
  AihcThread *run_queue_head;
  AihcThread *run_queue_tail;
  AihcIoRequest *io_requests_head;
  AihcIoRequest *io_requests_tail;
  uint64_t io_request_count;
  uint64_t allocation_count;
  AihcResume selected_resume;
  /* Allocation statistics include movable objects and complete pinned blocks.
     heap_alloc_base marks the start of uncounted mutator allocation.
     Collection and statistics queries add the span below heap_next.
     The pinned allocator adds its charge directly.
     heap_peak_bytes records maximum occupied space, including pinned blocks.
     The collector also counts collections and their monotonic duration. */
  uint8_t *heap_alloc_base;
  /* The remembered set: old objects that may hold a pointer to a younger
     object. The barrier appends, and a collection empties the list. */
  AihcValue **remembered;
  struct AihcRootFrame *root_frames;
  AihcForeignFrame *foreign_frames;
  AihcCallbackFrame *callback_frames;
  /* Released chunks that the next stack growth can use again. */
  struct AihcStackChunk *spare_chunks;
  uint64_t spare_chunk_count;
  uint64_t remembered_count;
  uint64_t remembered_capacity;
  /* The entries that aihc_remember added since the last compaction. Only
     those can repeat an object: an update and the collector each add an
     object once. */
  uint64_t remembered_repeatable;
  /* The bytes of fixed allocations and of new young stack chunks since the
     last collection. */
  uint64_t fixed_since_gc;
  /* The gen1 blocks of this capability: the survivors of its last
     collections. gen1_age is the number of collections since the last one
     that copied gen1 of this capability. See "Gen1" in
     docs/multi-threaded-rts-design.md. */
  AihcGeneration gen1;
  uint64_t gen1_age;
  /* With several capabilities, gen1 is the oldest local generation, and a
     collection copies gen1 when it reaches this size. See
     aihc_choose_generation. */
  uint64_t gen1_limit_bytes;
  /* The stores that gave an object outside the local heap a value of the
     local heap. In the end state each one promotes the value. */
  uint64_t escape_count;
  /* The scan work that the write barrier added since the last collection,
     in bytes: the size of each object it put in the remembered set, and
     one run of elements for each card it made dirty. */
  uint64_t barrier_work;
  /* The position of the capability in the array of the machine. */
  uint64_t index;
  /* Not zero while the worker of the capability waits in its idle wait. */
  uint64_t idle;
  /* The threads that this capability pushed to an idle capability. */
  uint64_t push_count;
  /* The commit count of the machine when the last transaction of this
     capability that retried was still valid. See aihc_stm_wait_request. */
  uint64_t stm_retry_token;
  /* The inbox: the threads that other capabilities push or wake into this
     capability. inbox_lock guards the inbox, and the worker moves it into
     the run queue at each schedule. Without workers, the lock is NULL and
     the inbox stays empty. See "Run queues" in
     docs/multi-threaded-rts-design.md. */
  AihcThread *inbox_head;
  AihcThread *inbox_tail;
  void *inbox_lock;
  /* The levels of the stop that this capability leads. */
  uint64_t stop_depth;
  /* Not zero while this capability collects. The time profiler then gives
     the sample a collection frame and does not read heap objects. */
  uint64_t profile_collecting;
  /* The young pinned blocks and large objects that this capability
     allocated. A collection of this capability sweeps this list alone and
     moves each block that is now in gen2 to the list of the machine. See
     aihc_sweep_pinned. */
  struct AihcPinnedBlock *young_pinned;
};

/* The reason that starts a collection. The statistics report a count for
   each reason. */
typedef enum {
  /* A reservation found no room in the nursery. */
  AIHC_GC_REASON_NURSERY,
  /* The write barrier work reached the -R budget. */
  AIHC_GC_REASON_BARRIER,
  /* The fixed allocations since the last collection reached the nursery
     size. */
  AIHC_GC_REASON_LARGE,
  /* A promotion reached an object that a copy cannot promote. */
  AIHC_GC_REASON_ESCAPE,
  /* A push of threads to an idle capability. */
  AIHC_GC_REASON_PUSH,
  /* The -M limit needs a full collection. */
  AIHC_GC_REASON_HEAP_LIMIT,
  /* A request of the program or of the runtime start. */
  AIHC_GC_REASON_REQUEST,
  AIHC_GC_REASON_COUNT
} AihcGcReason;

/* The process state shared by all capabilities. */
struct AihcMachine {
  /* The capability records, which never move: compiled code finds its
     record through the header of its stack chunk. */
  AihcCapability **capabilities;
  uint64_t capability_count;
  /* Not zero when the machine allocated the capabilities array. */
  uint8_t capabilities_owned;
  /* The machine owns the nursery arena. Capability i has the aligned window
     of nursery_stride_bytes bytes at i times the stride. */
  uint8_t *nursery_arena;
  uint64_t nursery_stride_bytes;
  AihcSlot *globals;
  uint64_t global_count;
  /* The info table of the frame at the bottom of each forked thread. Each
     thread gets its own copy of the frame on its own stack. */
  const AihcInfo *thread_done_info;
  uint64_t heap_max_bytes;
  uint8_t heap_limit_enabled;
  uint8_t program_started;
  AihcStableName *stable_names;
  uint64_t next_stable_name;
  /* The number of the next new thread. The counter starts at one, it gives the
     main thread the number one, and it does not give a number again. */
  uint64_t next_thread_id;
  int64_t exit_status;
  uint64_t heap_peak_bytes;
  uint64_t gc_count;
  uint64_t gc_time_ns;
  AihcTransactionTimer *transaction_timers;
  /* The transactions that wait in retry for a commit, and the count of
     those among them without a timer. stm_wait_lock guards both and takes
     no other lock. Each commit increments stm_commit_count. */
  struct AihcStmWaiter *stm_waiters;
  uint64_t stm_forever_count;
  uint64_t stm_commit_count;
  void *stm_wait_lock;
  /* The pinned blocks and large objects of gen2. A block that a promotion
     moved into gen2 can stay on the young list of its capability until the
     next collection of that capability. */
  struct AihcPinnedBlock *pinned_blocks;
  /* The bytes of the pinned lists: pinned blocks and large objects. */
  uint64_t fixed_bytes;
  AihcValue *global_array;
  /* The stack of each thread that has not finished. The collector releases
     the stack of a thread that it did not retain. */
  struct AihcStack *stacks;
  const AihcIoBackend *io_backend;
  /* The longest collection so far, in monotonic nanoseconds, and the bytes
     the last collection kept: the copied objects and the pinned blocks. */
  uint64_t gc_max_pause_ns;
  uint64_t heap_live_bytes;
  /* Gen2, which every capability shares. Only bytes is used: the bytes of
     its occupied slots. */
  AihcGeneration gen2;
  /* The -B count: a survivor stays in gen1 for this many collections of its
     capability. Zero copies the survivors of the nursery into gen2
     directly. */
  uint64_t gen1_collections;
  /* The -R budget of barrier work between two collections of a capability,
     in bytes. */
  uint64_t barrier_budget;
  uint64_t gen2_limit_bytes;
  uint64_t gen2_factor;
  uint64_t gc_minor_count;
  uint64_t gc_gen1_count;
  uint64_t gc_full_count;
  /* The incremental gen2 cycle: the number of the cycle, whether one is
     active, the bytes it has marked, and the mark work the promotions
     since the last slice ask for. See docs/gc-design.md. */
  uint64_t gen2_epoch;
  uint64_t gen2_cycle_active;
  uint64_t gen2_marked_bytes;
  uint64_t mark_debt;
  uint64_t mark_factor;
  uint64_t mark_slice_floor;
  uint64_t mark_slice_cap;
  /* The mark work that local collections without a stop owe the active
     gen2 cycle. A later stop does it in one slice. */
  uint64_t deferred_mark_bytes;
  /* The bytes of gen2 when the active cycle took its snapshot. The cycle
     is finished in one pause when gen2 doubles while it runs. */
  uint64_t gen2_cycle_start_bytes;
  /* The oldest generation the last collection copied. */
  uint64_t gc_last_generation;
  /* The charges of the fixed blocks of each generation, zero to two. Their
     sum is fixed_bytes. The gen1 and gen2 tests add the count of their
     generation, so dead old fixed blocks start a collection. */
  uint64_t fixed_generation_bytes[3];
  /* Not zero when each capability has a worker on its own OS thread. See
     "Workers" in docs/multi-threaded-rts-design.md. */
  uint64_t workers_active;
  /* The number of capabilities in the idle wait. */
  uint64_t idle_count;
  /* The locks of the workers: see aihc_heap_lock and aihc_object_lock. */
  void *heap_lock;
  void *object_lock;
  /* The stop protocol: stop_lock guards the three counts below, and
     stop_condition signals a change. running_count is the number of
     capabilities outside the idle wait, and parked_count is the number of
     those that wait for the end of a stop. */
  void *stop_lock;
  void *stop_condition;
  uint64_t stop_requested;
  uint64_t running_count;
  uint64_t parked_count;
  /* The number of capabilities in a safe foreign call. Such a capability
     does not count as running, so a stop does not wait for it. It can give
     work when the call returns, so the deadlock test must not ignore it. */
  uint64_t foreign_count;
  /* The number of collections for each reason. See AihcGcReason. */
  uint64_t gc_reason_counts[AIHC_GC_REASON_COUNT];
  /* The collections that copy gen1 only because gen2 is above its limit. */
  uint64_t gc_gen2_limit_count;
};

/* Compiled code calls the write barrier before a pointer store outside the
   local nursery. The C runtime calls it before each pointer store into an
   existing object. An array store gives the element index. An array copy
   gives the element range. A large array has one card for each 128 elements.
   A collection scans only the cards that stores touched. The barrier without
   an index marks every card of a large array. */
void aihc_write_barrier(AihcCapability *capability, AihcValue *object);
/* The barriers of a compiled store site. They also take the value that the
   site stores, and they count an escape: a store of a value of the local
   heap of the capability into an object outside it. See "Local heaps" in
   docs/multi-threaded-rts-design.md. */
/* The barrier of a pointer store into an object. The result is the value to
   store: the value, or its global copy when the store is an escape. A
   result of AIHC_ESCAPE_COLLECT asks the caller to run
   aihc_escape_collect with its roots and to try the store again. The
   barrier records nothing then. */
#define AIHC_ESCAPE_COLLECT ((AihcSlot)1)
AihcSlot aihc_write_barrier_store(AihcCapability *capability, AihcValue *object,
                                  AihcSlot value);
AihcSlot aihc_write_barrier_store_at(AihcCapability *capability,
                                     AihcValue *object, uint64_t index,
                                     AihcSlot value);
/* Count an escape when the value is in the local heap of the capability and
   the object is not. */
void aihc_count_escape(AihcCapability *capability, const AihcValue *object,
                       AihcSlot value);
void aihc_write_barrier_at(AihcCapability *capability, AihcValue *object,
                           uint64_t index);
void aihc_write_barrier_range(AihcCapability *capability, AihcValue *object,
                              uint64_t offset, uint64_t count);
/* A continue helper enters a frame in another stack chunk than the one the
   stack pointer was in. New frames go above that frame, so the chunk is
   young again. */
void aihc_stack_enter_chunk(AihcCapability *capability, const AihcValue *frame);

_Static_assert(sizeof(AihcValue) == sizeof(AihcSlot),
               "AIHC objects must have a one-word base header");
_Static_assert(sizeof(AihcInfo) == 5 * sizeof(void *) + 8,
               "the info table layout must match the Lir lowering");

/* Foreign frames protect suspended Haskell values across C callbacks. */
void aihc_foreign_enter(AihcCapability *capability, AihcForeignFrame *frame,
                        AihcSlot *roots, uint64_t count, const AihcSrt *srt,
                        uint64_t allow_callbacks);
void aihc_foreign_leave(AihcCapability *capability, AihcForeignFrame *frame);
AihcBackendEntry aihc_callback_create(AihcCapability *capability,
                                      AihcValue *closure,
                                      AihcCallbackSlot *slots, uint64_t count);
void aihc_free_haskell_fun_ptr(AihcBackendEntry entry);
void aihc_callback_enter(AihcCallbackFrame *frame, AihcBackendEntry entry,
                         const AihcInfo *stop_info);
AihcCapability *aihc_callback_machine(AihcCallbackFrame *frame);
AihcValue *aihc_callback_closure(AihcCallbackFrame *frame);
AihcValue *aihc_callback_continuation(AihcCallbackFrame *frame);
void aihc_callback_return(AihcCapability *capability, uint64_t result);
uint64_t aihc_callback_leave(AihcCallbackFrame *frame);

/* Transfer a scheduler record to fixed-width Lir slots. */
void aihc_lir_take_resume(AihcCapability *capability, AihcResume *resume,
                          uint64_t *slots);

/* Info tables have at least four-byte alignment on every target.
   These bits belong to the thunk header, not to references to the thunk.
   Bit zero alone marks a claimed or evaluated thunk: field zero names its
   owner or its value. Both bits mark a claim between its two stores. Bit one
   of a frame marks a frame that a pause processed. */
#define AIHC_HEADER_EVALUATING UINT64_C(1)
#define AIHC_HEADER_CLAIMING UINT64_C(2)
#define AIHC_FRAME_PROCESSED UINT64_C(2)
#define AIHC_HEADER_TAG_MASK UINT64_C(3)

static inline const AihcInfo *aihc_value_info_table(const AihcValue *value) {
  return (const AihcInfo *)(uintptr_t)(value->header & ~AIHC_HEADER_TAG_MASK);
}

static inline AihcObjectKind aihc_value_kind(const AihcValue *value) {
  return (value->header & AIHC_HEADER_EVALUATING) != 0
             ? AIHC_OBJECT_BLACKHOLE
             : aihc_value_info_table(value)->object_kind;
}

static inline uintptr_t aihc_value_info(const AihcValue *value) {
  return aihc_value_info_table(value)->identity;
}

static inline uint64_t aihc_value_arity(const AihcValue *value) {
  return aihc_value_info_table(value)->remaining_arity;
}

/* The slots a partial constructor has filled. It lives in the object because
   every stage of one constructor shares a single info table. */
static inline uint64_t aihc_partial_applied(const AihcValue *value) {
  return value->fields[0];
}

/* The slots a partial constructor still needs. The saturated info table names
   the full width. */
static inline uint64_t aihc_partial_total(const AihcValue *value) {
  return aihc_value_info_table(value)->next->field_count;
}

static inline AihcSlot *aihc_partial_fields(AihcValue *value) {
  return value->fields + 1;
}

static inline const AihcSlot *
aihc_partial_fields_const(const AihcValue *value) {
  return value->fields + 1;
}

static inline uint64_t aihc_value_count(const AihcValue *value) {
  if (aihc_value_kind(value) == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return aihc_partial_applied(value);
  }
  return aihc_value_info_table(value)->field_count;
}

static inline AihcSlot *aihc_value_fields(AihcValue *value) {
  return value->fields;
}

static inline const AihcSlot *aihc_value_fields_const(const AihcValue *value) {
  return value->fields;
}

/* Reserve nursery space for a batch of objects, regardless of its total size.
   Compiled code then advances the heap pointer for each object. */
/* Refill or collect after the heap check fails. Compiled code uses this
   entry point. Dynamic allocation uses aihc_heap_allocate instead. It can
   return nursery memory or separately allocated memory.

   The table is the static reference table of the code that requests the
   collection, and the collector marks everything it names. A running function
   has no heap object to carry its table, so the safepoint of a compiled
   function passes the function's own table. Every other request passes NULL:
   a runtime helper is reached by a tail call, or by a call whose only
   continuation is a transfer to heap objects, so the static references of the
   function that called it are dead by the time it collects. Suspended code
   needs no entry either: its continuation closure carries the table in its
   info table. */
void aihc_heap_collect(AihcCapability *capability, uint64_t words,
                       uint64_t root_count, AihcSlot *roots,
                       const AihcSrt *srt);
void aihc_ensure_heap(AihcCapability *capability, uint64_t words,
                      uint64_t root_count, AihcSlot *roots, const AihcSrt *srt);
/* Allocate one object and return its address. A nursery allocation advances
   the heap pointer. The caller must initialize the object before collection. */
AihcValue *aihc_heap_allocate(AihcCapability *capability, uint64_t words,
                              uint64_t pinned, uint64_t root_count,
                              AihcSlot *roots, const AihcSrt *srt);

/* Initialize the process and return its initial capability. */
AihcCapability *aihc_machine_initialize(void);
AihcCapability *aihc_machine_new(uint64_t global_count);
uint64_t aihc_allocation_count(const AihcCapability *capability);
void aihc_reset_allocation_count(AihcCapability *capability);
/* The bytes compiled code has taken from the managed heap. Compiled code
   bumps the heap pointer itself, so both of these account for the span since
   the last account before they read or clear the total. */
uint64_t aihc_heap_allocated_bytes(AihcCapability *capability);
void aihc_reset_heap_allocated_bytes(AihcCapability *capability);
void aihc_no_match(void);
/* The runtime removes RTS options before the Haskell program starts. argv[0]
   stays because getProgName and withProgName use the same mutable vector.
   aihc_program_arguments_initialize flattens argv into one buffer of
   zero-terminated strings; the parser and the argument store behind the other
   four functions live in core-libs/aihc-rts/native/aihc_runtime_options.lir. */
void aihc_program_arguments_initialize(int argc, char *const argv[]);
int64_t aihc_runtime_arguments_initialize(const void *buffer, int64_t length);
/* The runtime settings that come from the environment. The host reads the
   process environment and flattens it like argv; the parser in
   aihc_runtime_options.lir keeps the AIHC_RTS_STATS value, and keeps the
   flattened buffer itself for lookupEnv and getEnvironment. The WASI P3 host
   reads no environment: see aihc_host_wasip3.c. */
void aihc_program_environment_initialize(void);
int64_t aihc_runtime_environment_initialize(const void *buffer, int64_t length);
/* Write the runtime statistics to the AIHC_RTS_STATS file, once, when the
   environment names one. The generated main calls this when the capability
   halts, and aihc_exit_process calls it before the process exits. */
void aihc_runtime_statistics_report(void);
/* The allocation counters of a program built with --profile-allocations:
   a name for each info table, a pair of counters for each (the objects and
   the words), and the number of info tables. The entry registers them
   before the capability starts, and the statistics report writes them. */
void aihc_allocation_profile_register(const char *const *names,
                                      const uint64_t *counts,
                                      const uint64_t *size);
/* The tables of a program built with --profile-time: the code of each
   function, its name, and their number, then the info tables of the
   functions and their number. The entry registers them before the
   capability starts, and the registration starts the sample timer. Only the
   POSIX hosts define it. See aihc_profile.c. */
void aihc_time_profile_register(AihcCapability *capability,
                                const void *const *codes,
                                const char *const *names,
                                const uint64_t *code_count,
                                const AihcInfo *const *infos,
                                const uint64_t *info_count);
int64_t aihc_program_environment_size(void);
int64_t aihc_program_environment_copy(void *buffer, int64_t capacity);
/* Install an immutable managed byte array without allocation. */
int64_t aihc_program_environment_replace(const void *array, int64_t length);
/* The first program argument that the host gave, or NULL when it gave no
   arguments. withArgs and withProgName do not change it. */
const char *aihc_program_initial_name(void);
int64_t aihc_program_arguments_size(void);
int64_t aihc_program_arguments_copy(void *buffer, int64_t capacity);
/* Install an immutable managed byte array without allocation. */
int64_t aihc_program_arguments_replace(const void *array, int64_t length);
void aihc_set_field(AihcCapability *capability, AihcValue *value,
                    uint64_t index, AihcSlot field);
/* Boxed arrays are contiguous managed objects. GrinAllocate returns their
   storage before initialization. The unchecked wrappers below require a
   reservation and advance the heap pointer themselves.

   The boxed arrays, the mutable references, the stable names, and the byte
   arrays below are defined by the Lir runtime units in
   core-libs/aihc-rts/native. See the "Runtime units" section of docs/lir.md. */
AihcValue *aihc_array_new(AihcCapability *capability, int64_t count,
                          AihcSlot initial);
/* The caller reserves heap for delay creation, transaction creation, and
   transaction writes. These operations and their callees must not collect.
   Aihc.Grin.Primitive defines the maximum slot counts. */
AihcValue *aihc_tvar_delay(AihcCapability *capability, int64_t delay,
                           AihcSlot initial, AihcSlot final);
AihcSlot aihc_tvar_read(AihcCapability *capability, AihcValue *variable);
/* The read of readTVarIO#, outside a transaction. It allocates nothing. */
AihcSlot aihc_tvar_read_io(AihcCapability *capability, AihcValue *variable);
void *aihc_stm_wait_request(AihcCapability *capability);
int64_t aihc_stm_wait_result(AihcCapability *capability, void *request);
uint64_t aihc_stm_begin(AihcCapability *capability);
uint64_t aihc_stm_commit(AihcCapability *capability);
uint64_t aihc_stm_abort(AihcCapability *capability);
uint64_t aihc_stm_active(AihcCapability *capability);
uint64_t aihc_tvar_write(AihcCapability *capability, AihcValue *variable,
                         AihcSlot value);

AihcSlot aihc_array_copy(AihcCapability *capability, AihcValue *source,
                         int64_t source_offset, AihcValue *target,
                         int64_t target_offset, int64_t count);
AihcValue *aihc_mutvar_new(AihcCapability *capability, AihcSlot initial);
/* Stable names are managed records with weak referents and lookup links.
   Consume at most four reserved slots without collection. */
void *aihc_stable_name_make(AihcCapability *capability, AihcValue *value);
/* State and allocation helpers used by native code. None of these functions
   transfers control to a generated user function. */
/* Apply `stages` argument groups with `count` slots in total. The function
   must take more groups than the application supplies, or be a partial
   constructor. The result is one partial application. */
AihcValue *aihc_apply_slow(AihcCapability *capability, AihcValue *function,
                           uint64_t stages, uint64_t count,
                           const AihcSlot *arguments, AihcValue **continuation);
/* Consume at most fourteen reserved heap slots without collection. */
/* Consume at most four reserved heap slots without collection. */
const AihcResume *aihc_block_on_blackhole(AihcCapability *capability,
                                          AihcValue *value,
                                          AihcValue *continuation);
/* The updates give the stored value, or NULL when the update is an escape
   whose promotion needs a collection. The caller then runs
   aihc_escape_collect with its roots and updates again. */
AihcValue *aihc_update(AihcCapability *capability, AihcValue *object,
                       AihcValue *value);
AihcValue *aihc_update_blackhole(AihcCapability *capability, AihcValue *object,
                                 AihcValue *value);
/* Delimited continuations. See "Delimited continuations" in
   docs/exceptions.md. A prompt tag is a heap node without fields, compared
   by address. control0# walks the continuation to the nearest prompt frame
   with its tag, records the frames above that prompt in a heap node, and
   resumes by applying the function to that node in the prompt's context.
   Applying the captured continuation copies the recorded frames onto the
   caller's continuation, so a capture can be resumed any number of times. */
/* aihc_prompt_tag.lir defines this operation. The caller reserves one heap
   slot. This operation must not collect. */
AihcValue *aihc_prompt_tag_new(AihcCapability *capability);
const AihcResume *aihc_control0(AihcCapability *capability, AihcValue *tag,
                                AihcValue *function, AihcValue *continuation);
const AihcResume *aihc_continuation_resume(AihcCapability *capability,
                                           AihcValue *captured,
                                           AihcValue *action,
                                           AihcValue *continuation);
const AihcResume *aihc_raise(AihcCapability *capability, AihcValue *exception,
                             AihcValue *continuation);
/* The caller reserves nine heap slots. This operation must not collect. */
AihcSlot aihc_fork(AihcCapability *capability, AihcValue *action);
AihcThread *aihc_my_thread_id(AihcCapability *capability);
void *aihc_mvar_new(AihcCapability *capability);
const AihcResume *aihc_mvar_read(AihcCapability *capability, void *mvar,
                                 AihcValue *continuation);
const AihcResume *aihc_mvar_take(AihcCapability *capability, void *mvar,
                                 AihcValue *continuation);
AihcSlot aihc_mvar_try_take(AihcCapability *capability, void *mvar);
AihcSlot aihc_mvar_try_read(AihcCapability *capability, void *mvar);
uint64_t aihc_mvar_try_put(AihcCapability *capability, void *mvar,
                           AihcSlot value);
const AihcResume *aihc_mvar_put(AihcCapability *capability, void *mvar,
                                AihcSlot value, AihcValue *continuation);
/* noDuplicate#: claim the thunks of the thread, and give up a duplicate
   evaluation. */
const AihcResume *aihc_no_duplicate(AihcCapability *capability,
                                    AihcValue *continuation);
const AihcResume *aihc_yield(AihcCapability *capability,
                             AihcValue *continuation);
const AihcResume *aihc_await_io(AihcCapability *capability, void *request,
                                AihcValue *continuation);
const AihcResume *aihc_thread_done(AihcCapability *capability);
void *aihc_io_stdin(void);
void *aihc_io_stdout(void);
void *aihc_io_stderr(void);
/* The access mode of a descriptor the program already has, numbered as an
   open request numbers its mode, or a negative aihc_io_error. */
int64_t aihc_io_descriptor_mode(int64_t descriptor);
/* An IO handle over a descriptor the program already has, or an open error.
   The host that has no descriptors to adopt reports one. */
void *aihc_io_adopt(AihcCapability *capability, int64_t descriptor,
                    int64_t mode);
int64_t aihc_io_handle_descriptor(void *handle);
/* The position of a handle that the runtime itself keeps, or -1 for a handle
   whose position the operating system keeps in its descriptor. */
int64_t aihc_io_handle_position(void *handle);
/* Move the position that the runtime keeps. This gives 0, or -1 for a handle
   that has no such position. */
int64_t aihc_io_handle_set_position(void *handle, int64_t position);
/* A monotonic clock in nanoseconds, for GHC.Clock. The start point is not
   specified. */
uint64_t aihc_clock_monotonic_ns(void);
/* The number of capabilities of the machine, for getNumCapabilities. */
int64_t aihc_capability_count(void);
int64_t aihc_io_open_result_error(void *result);
int64_t aihc_io_close(void *handle);
int64_t aihc_errno_get(void);
int64_t aihc_errno_set(int64_t value);
int64_t aihc_memory_write_byte(void *buffer, int64_t offset, int64_t value);
int64_t aihc_memory_read_byte(const void *buffer, int64_t offset);
_Noreturn int64_t aihc_io_raise_error(int64_t error);
uint64_t aihc_byte_array_copy_to_addr(void *opaque_array, int64_t offset,
                                      void *destination, int64_t length);
uint64_t aihc_byte_array_compare(void *opaque_left, int64_t left_offset,
                                 void *opaque_right, int64_t right_offset,
                                 int64_t length);
/* Byte arrays consume prior reservations and cannot collect.
   Size helpers include descriptor, payload, padding, and pinned metadata.
   Freeze and thaw preserve the representation. */
/* The slots of a boxed array of count elements: the header, the length, and
   the elements. A negative count or one whose slots cannot fit the address
   space stops the program. The caller reserves these slots before the
   allocation. */
uint64_t aihc_array_words(int64_t count);
uint64_t aihc_byte_array_words(int64_t size, uint64_t pinned,
                               int64_t alignment);
uint64_t aihc_byte_array_resize_words(void *array, int64_t size);
void *aihc_byte_array_new(AihcCapability *capability, int64_t size);
void *aihc_byte_array_new_pinned(AihcCapability *capability, int64_t size);
void *aihc_byte_array_new_aligned_pinned(AihcCapability *capability,
                                         int64_t size, int64_t alignment);
uint64_t aihc_byte_array_is_pinned(void *array);
void *aihc_byte_array_contents(void *array);
uint64_t aihc_byte_array_shrink(void *array, int64_t size);
void *aihc_byte_array_resize(AihcCapability *capability, void *array,
                             int64_t size);
uint64_t aihc_byte_array_get_size(void *array);
uint64_t aihc_byte_array_copy_from_addr(void *source, void *array,
                                        int64_t offset, int64_t length);
uint64_t aihc_byte_array_fetch_add_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_sub_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_and_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_fetch_nand_word(void *array, int64_t index,
                                         uint64_t value);
uint64_t aihc_byte_array_fetch_or_word(void *array, int64_t index,
                                       uint64_t value);
uint64_t aihc_byte_array_fetch_xor_word(void *array, int64_t index,
                                        uint64_t value);
uint64_t aihc_byte_array_compare_and_swap_word(void *array, int64_t index,
                                               uint64_t expected,
                                               uint64_t replacement);
uint64_t aihc_byte_array_copy(void *source, int64_t source_offset,
                              void *destination, int64_t destination_offset,
                              int64_t length);
uint64_t aihc_byte_array_set(void *array, int64_t offset, int64_t length,
                             uint64_t value);
void *aihc_io_submit_read(AihcCapability *capability, void *handle,
                          void *buffer, int64_t offset, int64_t length);
void *aihc_io_submit_write(AihcCapability *capability, void *handle,
                           void *buffer, int64_t offset, int64_t length);
void *aihc_io_submit_open(AihcCapability *capability, void *path,
                          int64_t length, int64_t mode);
int64_t aihc_io_take_result(void *request);
void *aihc_io_take_open_result(void *request);
void aihc_set_thread_done_continuation(AihcCapability *capability,
                                       AihcValue *thread_done_continuation);
void aihc_set_exit_status(AihcCapability *capability, int64_t status);
int64_t aihc_get_exit_status(const AihcCapability *capability);
_Noreturn void aihc_exit_process(int64_t status);
AihcEntry aihc_halt(AihcCapability *capability);
/* The entry points of GHC's RTS API that package C code calls, declared for
   it by the Rts.h that Aihc.Hackage.Headers writes. Their names are GHC's,
   because the callers are not aihc's to rename. */
void startTimer(void);
void stopTimer(void);
void blockUserSignals(void);
void unblockUserSignals(void);
int rtsSupportsBoundThreads(void);
/* System.Posix.Signals sets the default or ignore action of a signal through
   stg_sig_install and reads or writes nocldstop. Each host defines
   stg_sig_install, and aihc_runtime.c defines nocldstop. */
int stg_sig_install(int signal_number, int action, void *mask);
extern int64_t nocldstop;
#endif
