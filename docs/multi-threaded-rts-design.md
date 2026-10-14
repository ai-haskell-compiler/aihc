# Multi-threaded runtime system

This document gives the design of the multi-threaded runtime system (RTS).
It gives the options that we examined and the decisions that we made.
It also compares each decision with the threaded RTS of GHC.

The current RTS is in `core-libs/aihc-rts/native`.
It runs every Haskell thread on one operating system (OS) thread.
It uses no OS threads and no atomic operations.
Refer to [gc-design.md](gc-design.md) and
[native-runtime-objects.md](native-runtime-objects.md) for the current
design.

## Implementation status

The first part of build step 1 separates `AihcCapability` from `AihcMachine`.
Compiled code reads its capability from the stack chunk header.
The C runtime takes the capability as an explicit argument.
Programs still run with one capability on every target.
Each capability reserves nursery space in blocks.
The default block size is 32 KiB, and `-n<size>` selects another size.
A refill retains the heap pointer and does not collect until the nursery budget is full.
Fixed allocations and stack charges use that same budget.
The machine owns an aligned nursery arena.
The current capability uses one power-of-two window in this arena.
The runtime rounds `-A` up to a power of two.
A smaller heap limit or test budget can use part of that window.
The region allocator keeps alignment fragments for reuse.
Compiled stores and inline thunk updates test the nursery of the active capability.
The test reads the capability from the stack chunk, then reads its nursery base and size.
It uses an exclusive or with the aligned base and an unsigned size compare.
The C runtime uses the same test.
The runtime has no global nursery bounds.
Gen1 uses the `-B` count rule and has no byte maximum.
Each capability has its own gen1 blocks and its own count of collections. The machine keeps gen2.
Thread records are in gen2 from the fork and never move.
Their resumption and transaction fields are roots of the capability, so a store into them takes no barrier.
Each thunk has the owner word as field zero, and its free variables start at field one.
The entry writes nothing into the thunk. A pause claims the thunks of the update frames with plain stores.
A collection claims the thunk of each update frame that it scans.
The update keeps the state in field zero, and the inline evaluation check follows an evaluated thunk.
Blackhole queues and the queue list of each thread replace the blackhole table.
The claim is a compare-and-swap, and the stores and loads of the access rules have their orders.
The rules cover the thunk header and field zero, the pointer fields of references and boxed arrays, the card bytes, the full byte of an `MVar`, the value of a `TVar`, and the atomic primitives that compiled code does.
The message protocol, the queue recovery, and `noDuplicate#` remain for step 3.
The default count is one, and a count of zero promotes nursery survivors into gen2 directly.
The runtime has no worker threads and does not enable `-N`.

Lir has the atomic instructions of the memory model section on every backend.
No compiled code or runtime unit uses them yet.

The rest of step 1 remains necessary before step 2.
The write barrier counts its scan work against the `-R` budget. When the work reaches the budget, the next reservation collects.
It includes the barrier safepoints at the store sites, the barrier bounds of the runtime operations, and the copy of an array in pieces.
The cold path of the write barrier at a compiled store site gets the stored value and counts escapes.
The runtime statistics report the count as `escape_count`.
The first part of build step 2 gives the machine an array of capabilities.
The arena has one window for each capability, and each capability has its own gen1 blocks and remembered set.
A collection on one OS thread copies the nurseries of all capabilities.
It copies a survivor into the gen1 of the capability whose nursery held it.
The write barrier ignores an object in the nursery of any capability.
`aihc_machine_set_capabilities` changes the count when all nurseries are empty.
A removed capability gives its gen1 blocks and its remembered set to the first capability.
Only the collector fuzz driver uses more than one capability.
`aihc_promote` copies an escaping value and the young objects it reaches into gen2.
An immutable original stays as it is, and an unclaimed thunk becomes an indirection to its copy.
A value that reaches a mutable object or a claimed thunk runs a collection that moves every young survivor into gen2.
Until local collection is enforced, every young object counts as local.
The fuzz driver plays a promotion into a global. No runtime operation and no compiled store site promotes yet.
Native parallel execution starts at step 6, after the local collection of step 3.
WASM keeps one capability until WASI supports the required thread operations.

## Goals

- One RTS. There is no `-threaded` build flavour. One runtime library and one
  compiled program run on one core or on many cores. The `-N` RTS option
  selects the number of cores at start time.
- Low cost for single-threaded work. A program with one Haskell thread must
  run within 10 percent of the current RTS. This limit applies with `-N1`
  and with `-N<n>` for `n` above one.
- The same semantics as GHC for `MVar`, `STM`, thunks, exceptions, and the
  foreign function interface (FFI), as far as the current RTS gives them.
- One design for all backends. The AMD64, ARM64, and LLVM backends run the
  same runtime code. The wasm32 backend runs the same code with one
  capability and no OS threads.
- Bounded garbage collection pauses stay as [gc-design.md](gc-design.md)
  gives them. No pause term grows with the number of capabilities in the
  end state.
- Local heaps. In the end state, each capability collects its own nursery
  and its own gen1 alone, without a stop of any other capability. Only
  gen2 is shared, and a concurrent marker collects it. Local collection
  comes before parallel execution, so no version runs Haskell threads in
  parallel with a stop-the-world young collection. Refer to the garbage
  collection section.

## Non-goals

- Compaction of gen2. Gen2 does not move objects, because a concurrent
  marker needs that. A bounded evacuation of sparse segments is a later
  option.
- A read barrier. Compiled code loads pointers without a test.
- Bound threads and `forkOS` in the first version.
- Sparks and `par#` in the first version.
- Asynchronous exceptions. They are not in the current RTS. The design makes
  room for them, but a different document gives their design.

## Vocabulary

| Term | Meaning |
| --- | --- |
| Haskell thread | A green thread with a thread record and a stack of chunks. |
| Capability | The permission to run Haskell code, with the per-core state. One OS thread holds a capability at a time. |
| Worker | An OS thread that can hold a capability. |
| Machine | The process-global state: the heap, the options, the globals, and the statistics. |
| Safepoint | A point where the roots of a Haskell thread are known to the runtime. |
| Stop-the-world (STW) | Every capability is stopped at a safepoint. |
| Ragged barrier | Every capability passes one safepoint at its own pace. No capability waits for another. |
| Sequential mode | The machine has one capability. |
| Local heap | The nursery sub-range and the gen1 blocks of one capability. |
| Global heap | Gen2, the large objects, the pinned blocks, and the static objects. Every capability can reach them. |
| Escape | The first point where another capability can reach a local object. |
| Bridge collection | A STW collection of every local heap by one thread. It promotes every survivor into gen2. It serves a change of the capability count and the degradation mode. |

## Summary of decisions

| Topic | Decision | GHC |
| --- | --- | --- |
| Execution model | Capabilities with a shared heap. One worker holds one capability. | Same |
| Capability pointer | Loaded from the header of the stack chunk of `%sp`. No new register. | `BaseReg` is a pinned register. |
| Nursery | One arena with one sub-range of `-A` bytes for each capability. A capability fills its own sub-range in 32 KiB blocks, in the same order in every round. | One nursery for each capability, optional `-n` chunks. |
| Gen1 | Local blocks of each capability. A survivor stays for `-B` collections, one by default. No byte maximum. | Gen0 to-space, emptied at every collection. |
| Write barrier | The fast path tests whether the object is in the sub-range of this capability. The test uses the aligned nursery base and size from the capability. | A generation test through the block descriptor. |
| Safepoints for GC | The block refill, the write barrier cold path, the scheduler calls, and idle capabilities. | `HpLim` in memory. A remote store forces the next heap check to fail. |
| Context switch | At `aihc_lir_eval` on a thunk, at scheduler calls, and at `yield#`. A ticker sets a flag. | At every heap check, through `HpLim`. |
| Young collection | Each capability collects its local heap alone, with the sequential collector and no stop. An escape promotes the local object graph into gen2. No space is copied by more than one thread. Local collection comes before parallel execution. | STW parallel copy by default. Local heaps were a prototype in 2011, not merged. |
| Bridge collection | One thread copies every local heap in a stop, only for a change of the capability count and for the degradation mode under `-M`. | None |
| Old generation | Non-moving gen2. A concurrent marker on a spare worker, with ragged barriers at the snapshot and at the termination. The mutator slice is the assist. | Optional concurrent non-moving collector. |
| Blackholing | Lazy. A pause claims each update frame with a compare-and-swap. The state is one word: field zero names the owner, a queue, or the value. A waiter sends a message to the capability of the owner. | Lazy by default. The owner is in the thunk, and the indirectee gives the state. |
| Thunk layout | One owner word after the header. | One padding word in each thunk. |
| Memory model | Every shared location is an atomic access with an explicit order, relaxed where a plain instruction is enough. | Release and acquire barriers. |
| `MVar` | One lock byte in the record. | A lock on the info pointer. |
| `STM` | Optimistic transactions with buffered writes. One global commit lock. | Optimistic transactions with a lock for each `TVar` at commit. |
| IO manager | Each capability polls its own requests, as today. | Haskell IO manager threads with `epoll` or `kqueue`. |
| FFI | `safe` calls release the capability. Callbacks run on the stack of the caller and do not suspend. | `safe` calls release the capability. Callbacks run a nested scheduler. |
| Preemption of tight loops | None without an evaluation or an allocation. | None without an allocation. |

## Execution model

### Options

1. **Capabilities with a shared heap.** This is the GHC model. Each
   capability has a run queue and an allocation area. Haskell threads move
   between capabilities. Every capability sees every heap object.
2. **One global lock.** Every OS thread takes one lock to run Haskell code.
   This model gives no parallel speed-up. We rejected it.
3. **Separate machines.** Each OS thread has a heap of its own, and
   messages move data between them. This model cannot give the semantics
   of a shared `MVar` or `TVar`. We rejected it.

### Decision

We use option 1. The process has one machine and `n` capabilities. The RTS
option `-N<n>` sets `n`. The option `-N` without a number sets `n` to the
number of cores. The default is one capability. Thus a program without RTS
options runs as it runs today, with the fixed costs that this document
gives.

`setNumCapabilities` can increase and decrease `n` at run time. The change
happens in a stop-the-world step.

### Capability state

The machine struct `AihcMachine` keeps the process-global state. A new struct
`AihcCapability` keeps the state that one worker owns:

- The nursery block of the capability: `heap_next`, `heap_limit`, and the
  bytes the capability has taken since the last collection.
- The context copies `stack_next` and `current_thread`.
- The run queue and the inbox. Refer to the scheduler section.
- The remembered set, the mark buffer, and the spare stack chunks.
- The selected resumption, the foreign frames, the suspended foreign
  frames of `safe` calls, and the callback frames.
- The interrupt flags: `stop_requested` and `context_switch`.
- The allocation and collection counters of the capability.
- The wake-up descriptor for the idle wait. Refer to the IO section.

Every field that compiled code reads moves from `AihcMachine` to
`AihcCapability`. A C assertion checks each offset, as today.

## The capability pointer in compiled code

Compiled code names the machine by the symbol `@aihc_machine` today. A
capability has no fixed address, so compiled code must find its capability.

### Options

1. **A fifth context register.** GHC does this with `BaseReg`. Each function
   passes the capability with the other four context values. This costs one
   register in every function and one more move at every transfer.
2. **Thread-local storage.** A C `__thread` variable names the capability of
   the worker. The access is one instruction on Linux AMD64. It is a
   function call on Apple ARM64. Each object emitter needs new relocation
   types.
3. **The stack chunk header.** The chunk of the stack pointer names the
   capability. The stack limit `%sp_limit` is the end of that chunk, so the
   header is at `%sp_limit - 4096`. The load is one instruction on every
   target.

### Decision

We use option 3. The header of each stack chunk gets a `capability` field.
The rule is: the chunk that holds the stack pointer of a running thread names
the capability that runs it. The existing cold paths keep this rule:

- `aihc_lir_take_resume` writes the capability into the chunk of the
  continuation that it resumes.
- `aihc_stack_grow` writes it into the new chunk.
- `aihc_stack_enter_chunk` writes it into the entered chunk.
- The callback entry writes it after it pushes the stop frame.

No hot path changes. The lowering emits the load only next to a C call that
took `@aihc_machine` before. Such a call now takes the capability. The
machine stays reachable through `capability->machine`.

The chunk header has 64 bytes and is full on 64-bit targets. The fields
`depth` and `charged_gc` share one word. Each needs at most 32 bits.

The C runtime takes the capability as an explicit argument everywhere. The
functions `aihc_update` and `aihc_set_field`, which use the process machine
today, get the argument too. A foreign callback from an OS thread that holds
no capability finds a free capability through the machine. Refer to the FFI
section.

## The nursery and the write barrier

### Options

1. **One nursery for each capability.** This is the GHC model. The write
   barrier must then know every nursery. The current barrier is one
   subtraction and one unsigned compare against one address range.
2. **One arena with a shared cursor.** The arena is one run of regions. A
   capability takes blocks from the arena with an atomic add on one
   cursor. The write barrier keeps one range test. After each collection
   the blocks are handed out again in a different interleaving, so a
   block moves between cores from one round to the next. The first write
   to each line then comes from the cache of another core, from another
   die, or from memory on another NUMA node. We rejected it.
3. **One arena with one sub-range for each capability.** The arena is one
   run of `-N` sub-ranges of `-A` bytes. Capability `i` fills sub-range
   `i` and nothing else. The write barrier tests the sub-range of this
   capability. Each capability writes the same addresses in every round,
   so its L2 reuse, its prefetch pattern, and its page placement are the
   ones of a per-capability nursery. The sub-range is also the local heap
   boundary that the end state needs.

### Decision

We use option 3. `-A` is rounded up to a power of two, and the arena is
aligned to `-A`. Thus sub-range `i` is the `-A`-aligned window at
`arena_start + i * A`, and every `-A`-aligned window that holds a nursery
address holds nothing else. Each capability has its own cursor in its
sub-range. It takes blocks of 32 KiB in address order from the start of
the sub-range after each collection. No atomic operation is on the refill
path. The RTS option `-n<size>` sets the block size.

The write barrier fast path tests whether the object is in the nursery of
this capability. It reads the capability from the stack chunk header, then
reads `heap_start` and `nursery_bytes` from the capability. The test is:

```text
(object xor heap_start) < nursery_bytes
```

The compare is unsigned. The base has power-of-two window alignment.
Within the window, the exclusive or gives the byte offset from the base.
Outside the window, it gives a value at least as large as the window.
The nursery budget never exceeds the window size.
Thus a smaller heap limit or test budget also gives the correct result.

The original design used `(object xor %hp) >> log2(A) == 0` without loads.
However, `%hp` can equal the nursery end after an allocation.
At that point, it names the next aligned window.
Also, the runtime selects `-A`, so compiled code cannot use a constant shift.
The implemented test uses the stable base and reads the size at runtime.
It needs three loads, one exclusive or, one compare, and one branch before optimization.
The previous test needed two global loads, one subtraction, one compare, and one branch.

A store into an object outside the sub-range takes
the cold path. Until step 2 of the build order the cold path records the
object in the remembered set when it is old, as today, and counts the
store when the object is outside the local heap. From step 2 that count is
the escape path. Refer to the garbage collection section.

The compiled allocation fast path does not change. It is a bump and a
compare in registers. The slow path, `aihc_heap_collect`, first refills the
block of the capability from its sub-range. It collects only when:

- the capability has taken `-A` bytes since its last collection, which is
  the end of its sub-range, or
- another capability requested a stop.

The first rule keeps the collection frequency of a single-threaded program
as it is today. One thread fills `-A` bytes of consecutive blocks before it
collects, so its cache behaviour does not change with `-N`. A full
sub-range starts a local collection of that capability alone, and the rule
has no effect on any other capability.

A pinned worker places its own pages. The first write to a page places it
on the NUMA node of the core that writes it, so the sub-range of a pinned
worker lands on its own node without a NUMA library.

The refill is the one new cost on the allocation path. A refill is one slow
path call for each 32 KiB of allocation. The slow path spills the live
roots, calls the runtime, and loads the roots again. We estimate this at
one percent of the allocation work or less. The block size trades this cost
against the safepoint latency that the next section gives.

A stack growth charges a new chunk to the nursery today. The charge lowers
`heap_limit` of the capability that grows the stack. The capability counts
the charge in its own `-A` budget.

A larger arena for a larger `n` needs no reservation in advance. A
`setNumCapabilities` call runs a collection, after which the arena holds no
object, and the collector then maps an arena of the new size.

## Safepoints, preemption, and context switches

Compiled code keeps `%hp_limit` in a register. Nothing outside the worker can
change a register. Thus another capability cannot force the next heap check
to fail, as GHC does when it sets `HpLim` to zero.

### Options

1. **Heap limit in memory.** Replace the `%hp_limit` register with a
   capability register and read the limit from memory at each heap check.
   This is the GHC model. It costs one load at every allocation and a new
   pinned register on every backend. A remote store then stops a capability
   at its next allocation.
2. **Block refill as the safepoint.** Keep the registers. A capability
   reaches the runtime at the end of each 32 KiB block. The runtime reads the
   interrupt flags there. A stop request waits for the allocation of at most
   one block on each running capability.
3. **Signals that change the saved registers.** A signal handler sets the
   `%hp_limit` register of the interrupted code to zero. The handler cannot
   tell Haskell code from C code safely, and wasm32 has no signals. We
   rejected it.

### Decision

We use option 2. The safepoints of a capability are:

- The block refill in `aihc_heap_collect`.
- The cold path of the write barrier. The lowering spills the live roots
  at that call as it does at a reservation. Refer to the write barrier
  section.
- Each scheduler primitive: `yield#`, `fork#`, the blocking `MVar`
  operations, `awaitIO#`, the blackhole wait, the STM wait, and
  `noDuplicate#`.
- The idle wait of a capability without runnable threads.
- The release of the capability by a `safe` foreign call.

A loop that neither allocates, nor evaluates a thunk, nor stores a pointer
into an old object reaches no safepoint. A stop request then waits for it.
GHC has the same limit for a loop that does not allocate.

### Context switches

A stop request parks the worker inside the runtime call. The Haskell thread
keeps its roots in the spilled root array of the slow path. No continuation
is necessary. The block refill therefore serves the collector.

The pause before a collection can decide that the thread must not continue
its computation: the thread is a duplicate, or a duplicate finished first.
Refer to the pause transition of the blackholing section. The refill call
then cannot return into the interrupted code. The refill contract has two
results. `aihc_heap_collect` returns zero when the computation continues,
and the slow block loads its roots and its context again. It returns a
resumption when the computation is given up. The slow block then
tail-calls `aihc_lir_resume` with it, as the code after a scheduler
primitive does. The spilled roots of the given-up computation are dropped.
The cost is one test of the result on the slow path. The same contract
applies to the cold path of the write barrier.

A context switch between two Haskell threads needs a continuation. The block
refill has none. The thunk branch of `aihc_lir_eval` has one. The thunk and
the continuation describe the rest of the computation. The resume form
`Eval thunk continuation` of a blackhole waiter uses the same description
today.

We considered a continuation at each heap reservation. The lowering would
outline the block after each reservation into a function. The fast path
would then transfer to that function with a tail call instead of a jump.
We estimate one to five percent on allocation-heavy code. We keep this as
the fallback if the evaluation safepoint proves too coarse.

Decision: a context switch happens at the thunk branch of `aihc_lir_eval`,
at every scheduler primitive, and at `yield#`. The thunk branch loads the
`context_switch` flag of the capability and tests it. This costs two loads
and one predictable branch for each thunk evaluation. When the flag is set,
the branch suspends the thread with `Eval thunk continuation` and enters the
scheduler.

A ticker OS thread sets the `context_switch` flag of each running capability
every 20 ms. The RTS option `-C<time>` sets the interval. The ticker starts
when the program forks its second Haskell thread, and it stops when every
capability is idle. A program with one Haskell thread starts no ticker.

## Scheduler

### Run queues

Each capability has a run queue that only its worker touches. No lock and
no atomic operation guards it. Each capability also has an inbox: a queue
with a lock that other capabilities push threads and messages into. The
worker moves the inbox into its run queue at each schedule.

### Thread movement

We considered work stealing, where an idle capability takes threads from
the queue of a busy one. Stealing makes every run queue operation of the
owner publication-safe, which costs atomics on the single-threaded path.

Decision: the busy capability pushes. This is the GHC model. At each
schedule, a capability with two or more runnable threads reads the global
count of idle capabilities. When the count is above zero, it takes idle
capabilities from the idle list under the idle list lock. It moves half of
its surplus threads into their inboxes and wakes them. A capability with
one runnable thread pays one load and one compare.

Each thread record names its capability: the capability whose run queue or
inbox holds it, or that runs it. Only the worker of that capability changes
the field, when it pushes the thread to another capability. A thread that
`forkOn#` pinned stays on its capability.

A woken thread goes back to its own capability. The waker pushes it into
the inbox of that capability, or into its own run queue when that is the
same capability. Thus the capability field of a blocked thread stays valid
while it is blocked. The blackhole protocol below depends on this rule.

In the end state a push is an escape. The stack of the pushed thread names
objects of the local heap of the pushing capability, and the push must
promote everything the stack reaches into gen2 first. A thread that has
not run since its `fork#` has one closure and no stack, so its push is
cheap. A thread that has run can reach a large part of the local heap. The
scheduler therefore pushes the threads of the run queue that have not run
first. It pushes a thread that has run only when the idle capabilities
would stay idle otherwise, and it pushes at most one such thread for each
schedule. The promotion runs under a host root frame, so it can collect at
a host safepoint. The statistics count the pushes of each kind and the
bytes that each push promotes.

### Idle capabilities

A capability without runnable threads polls its pending IO requests. When
it has none, it waits on its condition variable. A push into its inbox, a
stop request, or a shutdown wakes it.

The last capability to become idle tests for a deadlock. The machine has a
deadlock when four conditions hold. Every capability is idle. No IO request
is pending. No timer is active. No thread is in a `safe` foreign call. The count of threads
in `safe` foreign calls is an atomic counter of the machine. A thread in a
foreign call can return and run, so it is not blocked. When the test finds
a deadlock, the runtime reports it, as the current RTS does.

### Workers

Each capability has one worker at start. A `safe` foreign call releases the
capability, and another worker takes it. Refer to the FFI section. The
number of workers can therefore be above `n`.

## Stop-the-world synchronisation

### Requests

A young collection stops nobody, because local collection comes before
parallel execution in the build order. A stop remains for three cases: a
change of the capability count, the full collection of the degradation
mode, and the oversized commit of the STM section. The gen2 snapshot and
the mark termination use a ragged barrier and no stop. Refer to the
garbage collection section.

A capability requests a stop for one of these cases. Every request comes from a safepoint of the
requester: `aihc_heap_collect` with its spilled roots, a scheduler call
with its continuation, or the idle wait. The requester holds no lock at
that point.

The protocol is:

1. The requester reads `stop_requested` of its own capability. When the
   flag is set, another capability leads a stop already. The requester
   parks as in step 6 below, and after the stop it tests whether its own
   request still holds. A collection by the leader can have made it void.
2. The requester takes the machine sync lock with a try-lock. When the
   try-lock fails, another capability became the leader between step 1
   and step 2. The requester goes to step 1.
3. The leader sets `stop_requested` on every capability and writes to the
   wake-up descriptor of each idle capability.
4. The leader takes the lock of each idle or released capability. Such a
   capability is already at a safepoint. Its roots are in its run queue,
   its inbox, and its suspended foreign frames.
5. The leader waits until each running capability parks.
6. A running capability parks at its next safepoint. It publishes its
   roots, increments the parked count, and waits on the sync condition
   variable. The leader is a parked capability too, for the count.
7. The leader runs the collection with the workers. Refer to the garbage
   collection section.
8. The leader clears `stop_requested` on every capability, releases every
   capability lock and the sync lock, and signals the condition variable.
   The parked workers return from their safepoint call and load their
   context again.

Two requesters cannot wait for each other. A requester waits for the sync
lock only in a try-lock, so it never blocks while it runs. A requester that
loses the try-lock parks and gives the leader what the leader waits for.

### Lock order

A worker takes locks in this order. It never takes a lock that is earlier
in the list while it holds a later one.

| Order | Lock | Holders |
| --- | --- | --- |
| 1 | The machine sync lock | The stop leader |
| 2 | The capability locks, in capability index order | The stop leader, a released capability, a worker that takes a capability |
| 3 | The object locks: the `MVar` byte, the STM commit lock, the timer list | One at a time for each worker |
| 4 | The idle list lock | The push, the idle wait |
| 5 | The inbox locks | The push, a wake, a message |
| 6 | The allocator locks: the region table, the gen1 blocks, the gen2 segments, the large objects, the pinned list, the stack regions, the stable names, the callback slots | A large or pinned allocation, a stack growth, a collection worker |

Three rules complete the order:

- A worker holds at most one object lock at a time.
- No runtime operation requests a stop while it holds a lock. The
  primitives consume their reservations and cannot collect, as today, so
  the only stop requests come from the safepoints above. A write barrier
  under a lock never collects either: a runtime operation reserves its
  barrier work before it takes a lock, as the write barrier section gives.
- A worker that holds an object lock does not run Haskell code. It waits
  for another worker only on an inbox lock or an allocator lock below it.
- In the end state, a runtime operation promotes before it takes a lock.
  The value it stores and the records it links under the lock are then
  global, and no promotion and no local collection runs under a lock.

### Stop latency

The latency of step 5 is the time until each running capability reaches a
safepoint. A capability that allocates reaches one after at most one
nursery block, 32 KiB by default. This bounds allocation, not time. A
capability that allocates slowly takes longer. A capability in a loop
without allocation or evaluation never reaches one. An `unsafe` foreign
call blocks the stop for its duration. GHC has the same limits, except that
its `HpLim` store stops an allocating loop at its next heap check.

The protocol itself visits every capability: one flag store and one lock
or wake-up for each. Its cost grows with `-N`, not with the live data.

## Garbage collection

The pause bound of [gc-design.md](gc-design.md) is a hard requirement. No
term of a pause may depend on the live data in gen2. No term may depend on
the depth of a stack or on the size of a large object. This document adds
one requirement: no term of a young collection may depend on the number of
capabilities. This section gives the design and the path to it.

### The principle

A moving collection of a space must stop every mutator that can hold an
unrecorded pointer into that space. Registers and stacks hold pointers
without a barrier, so a copy of a nursery stops every capability that can
name an object in it. There are three ways out, and each one removes one
word of that sentence.

- Remove *every*: make the nursery of a capability reachable from that
  capability alone. Its collection then stops nobody else. These are
  local heaps.
- Remove *moving*: mark the young objects in place under a snapshot
  barrier. Marking is correct under concurrency.
- Remove *unrecorded*: make every load tolerate a moved object through a
  read barrier.

A design that keeps all three words scales only through the parallel
efficiency of a stop-the-world copy. That efficiency depends on the shape
of the live graph, and the GHC experience with it is poor. We do not
accept it. No space is copied by more than one thread.

### Options

1. **STW, one collector.** The requester collects alone. The other
   capabilities park. A collection copies the live young objects of every
   capability on one worker, so the pause grows with the number of busy
   capabilities. We rejected it for young collections. It remains as the
   bridge collection for rare stops.
2. **STW, parallel copy.** Every worker takes part in the collection. Each
   worker copies the roots of its capability and the objects it reaches.
   The workers steal scan work from each other. The work grows with the
   number of busy capabilities, and the number of workers grows with it.
   The pause stays flat only when the live young graph divides among the
   workers. A structure that many threads consume lands on one worker, and
   the others wait for it. GHC gets a speed-up of two to four on eight or
   more threads and added `-qn` and `-qg1` because of this. OCaml 5 gets a
   near-linear speed-up to about 24 domains, because each domain reaches
   mostly its own data. The result depends on the workload. This is the
   GHC default. We rejected it: no space is copied by more than one
   thread, and the work distribution, the forwarding races, and the
   worker wake-ups are a cost that local heaps make unnecessary.
3. **Local heaps with promotion on escape.** Each capability collects its
   own nursery and its own gen1 alone, with the sequential collector and
   no stop. An invariant keeps every pointer into a local heap inside the
   roots and the objects of its capability. An escape, the first point
   where another capability can reach a local object, promotes the object
   and its local graph into gen2. The work of a local collection is the
   survivors of one capability. The cost that grows with sharing is the
   promotion at each escape, on the thread that shares. GHC built this in
   2011 and did not merge it. Two decisions of this document remove the two
   reasons that the previous revision gave against it: the sub-range test
   of the write barrier separates the local heap from everything else, and
   the cold path of the barrier is a safepoint with spilled roots that can
   move objects already. This is the end state.
4. **Sticky mark bits in a non-moving young space.** Young objects are
   marked in place and survivors stay where they are. The allocator fills
   free lines instead of fresh memory, and every store into a young object
   that is older than the current snapshot takes the deletion barrier. For
   Haskell that is one test on every thunk update. We rejected it.
5. **Concurrent copying with forwarding.** Mutators run while objects move
   and tolerate a moved object in every load path. Haskell code is pointer
   load heavy, so the read barrier costs on every path. We rejected it.
6. **Non-generational concurrent mark and sweep.** This is the Go design.
   A cycle traces the whole live heap once for each heap size of
   allocation. At Haskell allocation rates the marker rate is impossible
   unless the heap is many times the live data, and the allocator loses its
   bump path. We rejected it.

### Decision

We use option 3. The build order brings local collection before parallel
execution, so no version of the runtime runs Haskell threads in parallel
with a stop-the-world young collection. Each capability copies its own
local heap with the sequential collector, which uses no atomic operation.
Two capabilities can collect at the same time, but each one copies only
its own space.

The bridge collection is option 1 for three rare cases: a change of the
capability count, the full collection of the degradation mode under `-M`,
and the collections before step 3 of the build order, when only the
collector fuzz driver has more than one capability. It runs on the thread
that requests it, and it promotes every survivor into gen2, so every local
heap is empty after it and the invariant holds without a check.

### Local heaps

The local heap of a capability is its nursery sub-range, its gen1 blocks,
and its young fixed blocks. A young fixed block belongs to the capability
that allocated it, and its header names that capability.

**Invariant.** No object of the global heap and no object of another local
heap names an object of this local heap. Only the roots of this capability
and the objects of this local heap name its objects. The roots of a
capability are the stacks of its threads, its run queue, its inbox, its
selected resumption, its foreign frames, its suspended foreign frames, its
spilled roots, and its remembered set.

A thread record is always in the global heap, because a `ThreadId#` moves
freely. Its fields that name the stack, the closure, and the saved context
of the thread are roots of the capability of the thread, not fields of a
global object. The marker does not follow them, and the owner scans them.
Every other runtime record is an ordinary object: local when the capability
allocated it, and promoted when it escapes.

Under the invariant a capability collects its local heap alone. The
collection is the sequential copy of today with the roots of this
capability. No other capability stops, and no atomic operation runs. The
remembered set of the capability holds its gen1 objects that name nursery
objects. With `-B` equal to one, gen1 is copied at every collection and
the cold path records no such entry.

**Escapes.** An escape is a store, a message, or a push that gives another
capability a path to a local object. Every escape is a cold path or a
runtime call that holds the capability. The cold path promotes the local
object and its local graph into gen2 before it stores.

| Escape | Where it is today | What the end state does |
| --- | --- | --- |
| Store into an object outside the local heap | The write barrier cold path | Promote the stored value, then store |
| Update of a thunk outside the local heap, or of a CAF | `aihc_update`, the inline update cold path | Promote the result, then update |
| Attach of a blackhole queue to a thunk outside the local heap | The pause walk, the blackhole wait | Promote the queue, then attach |
| `putMVar#`, `writeTVar#`, a commit, a wake | Runtime calls on shared records | Promote the value and the waiter records, as a store |
| A blackhole message, an inbox message | The scheduler | Promote the records that the message names |
| `fork#` | The scheduler | The thread record is global. The closure stays local until the thread is pushed. |
| A push of a thread to another capability | The scheduler | Promote everything the stack and the closure reach |
| `makeStablePtr#`, `makeStableName#`, a callback slot | Runtime calls on global tables | Promote the referent through a local collection |
| A `safe` foreign call | The FFI | Nothing. C holds no heap pointer except through a stable pointer. |

A store of a global pointer into a local object is not an escape. A store
of a local pointer into a local object is not an escape. The fast path
covers the second case for the nursery, and the cold path records the first
case for gen1 only in the remembered set.

**Promotion.** A promotion is the copy of a collection with one root, done
by the escaping capability under its spilled roots or under a host root
frame. It copies into gen2 through the segment allocator, allocated marked
while a cycle is active, as a collection does. The copied objects leave
forwarding headers, as in a collection, and the next local collection
follows them. Three kinds of object need three rules, because the mutator
can hold a stale pointer to the original in a frame until its next local
collection:

- **An immutable object.** The original stays as it is, with no forwarding
  header. Every reader of the original reads the same values as the copy,
  so a stale pointer is harmless. Until the next local collection the
  original and the copy both exist, and that collection copies the
  original into gen1 when a root still names it. This floating garbage is
  bounded by the escaped bytes.
- **A thunk.** The original becomes an indirection to the copy, so the
  evaluation of a stale pointer reaches the copy and the work is shared.
  The follow path of the evaluation, the update, and the attach follow an
  indirection already, so the rule adds no case to them. The rest of the
  original becomes slop, as after an update. A claimed thunk has an owner
  and can have waiters, so it takes the rule of a mutable object.
- **A mutable object.** A `MutVar#`, an array, an `MVar#`, or a `TVar#`
  must not be written through a stale pointer. Its escape runs a local
  collection, in which the escaping value is promoted into gen2 and every
  root and local object is updated. No stale pointer survives. A stable
  name takes the same route, because two names for one object must be
  equal.

Promotion adds no copy work. Every promoted object is live, so the next
local collection would have copied it. The work moves from a stop onto the
timeline of the thread that shares the data. What promotion adds is
floating garbage in gen2: an escaped object that dies soon after lives in
gen2 until the next cycle. The statistics report the escaped bytes. Step 1
counts the escapes before step 2 pays them.

**Verification.** Under `AIHC_GC_VERIFY` a walk after each collection
checks the invariant: no object of the global heap and no object of
another local heap names an object of the collected local heap, except
through the root fields of a thread record. The collector fuzz driver runs
this walk with several capabilities.

### Gen1

Gen1 is local. Each capability fills its own gen1 blocks with the survivors
of its nursery. A survivor stays in gen1 for `-B` collections of its
capability, and the `-B`-th collection copies the whole gen1 of the
capability into gen2. The default is one: every collection copies gen1
into gen2, and gen1 holds the survivors of the last collection and nothing
else. Gen1 has no byte maximum.

The byte maximum of today is the wrong knob for two reasons. A copying
generation cannot retire its oldest part alone, because younger gen1
objects name older gen1 objects without a barrier record, so the whole
gen1 is copied at once and the maximum is one pause term. That term is
far above every other term: 16 MiB against the survivors of one nursery.
And the maximum is a machine quantity. With more capabilities the machine
promotes faster, gen1 fills after fewer collections, and the aging falls
as one over the capability count.

The count rule has neither problem. Its pause term is `-B` survivor sets
of one capability, and its aging is `-B` collections of the own capability,
which is `-B` times `-A` bytes of own allocation. Neither moves with the
capability count. With a concurrent marker the trade of gen1 changes too:
aging buys less marker work and less floating garbage, and it pays with
copy work in a pause. A count of zero promotes nursery survivors into gen2
directly and is allowed. The step 6 measurement decides the default.

### Bridge collection

The bridge collection runs on one thread while every other capability is
stopped. It visits the roots of every capability, scans the remembered set
of every capability, and copies the live objects of every local heap. A
survivor goes into gen2. The collection is the sequential collector with
the roots of all capabilities, so it needs no lock, no stealing, and no
atomic forwarding.

### Forwarding

A copied object keeps its new address in its old header with the low two
bits set to two, as today. Only one thread copies a space: the owner of a
local heap in a local collection or a promotion, or the requester of a
bridge collection. Thus the install of the forwarding header is a plain
store. The marker does not copy. It marks a gen2 object in the segment
bitmap with an atomic or on its word, and the thread whose atomic or set
the bit queues the object for a scan. A large object and a pinned block
keep their mark in their block header, set the same way.

### Indirections and update frames

An evaluated thunk is an indirection: its header has bit zero set, and its
field zero names the value. Refer to the blackholing section. The collector
follows such a thunk of a copied generation and stores its target, so the
copied objects hold no indirection, as today. When the collector must keep
the object, it copies it as a two-word indirection with the indirection
info table. With duplicate evaluation, the follow rule has one exception.

Two threads can evaluate one thunk. The first update makes the thunk
evaluated. The update frame of the second thread still names the thunk. A
collection between the two updates would then replace the thunk in the
frame with the value of the first update. The second update would then
write into the value.

Rule: the thunk field of an update frame is not shortened. The collector
copies the thunk as a two-word indirection, and the field keeps naming the
copy. The same rule applies to the thunk field of a blackhole message.
Every other reference follows the thunk to its value. GHC has the same rule
in its `evacuate_BLACKHOLE`.

The second update then stores its value into field zero of the copy, which
is the field that the follow path reads. Both results are the same value,
because the thunk is pure. The continuation of the second thread receives
its own result. Refer to the blackholing section for the thunk states.

A claimed thunk has a thread record or a blackhole queue in field zero. The
collector copies such a thunk whole, with its free variables, and traces
field zero as a pointer. Every thread pauses before a collection, and the
pause claims the thunks of its update frames. The rule above does not
depend on that: an update frame can name a thunk in any state, and the
collector keeps the object that the frame names in every state. A thunk in
the state Thunk is an ordinary object. A claimed thunk is copied whole. An
evaluated thunk is copied as a two-word indirection.

### Remembered set and mark buffer

The write barrier appends to the remembered set of the capability. The
deletion barrier appends to the mark buffer of the capability. Neither needs
an atomic operation. The worker of the capability scans the lists of its
capability. An object that goes back to the remembered set goes to the list
of the worker that scanned it.

A card of a large boxed array is one byte. Two capabilities can set the same
card at the same time. Both store the same value with a relaxed atomic byte
store, so the race has a defined result. A dirty card is a unit of scan
work. The worker that clears a card with an atomic exchange scans its run.

The barrier work is budgeted. The cold path of the write barrier counts
the scan work it adds: one object for a remembered set entry, and one run
for a card that it turns dirty. The card store is an exchange, so the
barrier sees whether the card was clean. A hot object that enters the
remembered set again does not count, because the compaction removes it.
The budget is the RTS option `-R<size>`, with the nursery size as the
default. The deletion barrier has the same budget for its mark buffer.

The budget has two kinds of consumer, with one rule each:

**Compiled store sites.** The cold path of the write barrier at a store
site is a safepoint. The lowering spills the live roots of the store site
before the call, as it does at a reservation. The call gives the
capability its context. The call does these steps in this order:

1. It tests the budget. When the count of the capability is at the budget,
   it runs a collection, or it parks in a stop that another capability
   leads. The collection can give the computation up, and the call then
   returns a resumption as the refill does. Otherwise the call loads the
   roots again: the object, the value, and the index of the pending store
   are among them.
2. In the end state, when the value is local and the object is not, it
   promotes the value as the local heaps section gives. Then it records
   the pending store for the object as it is after the collection and the
   promotion: the remembered set entry or the card, and the deletion
   barrier of the old value of the field as it is after the collection.
   A collection can start a gen2 cycle, so the deletion barrier reads the
   state of the cycle after step 1. While the snapshot scan of this
   capability is pending, it also shades the stored value. Refer to the
   concurrent marking section.
3. It returns, and the store site loads its roots and its context again
   and stores the value.

The collection comes before the record of the pending store, never after
it. A record before the collection is lost: the collection scans the
object, clears the card or drops the entry, and the store then lands
without protection. The spilled value survives the collection as a root
either way, but the reference from the old object would be missed by the
next collection.

After every barrier call, the store site loads `%hp_limit` again. The cold
path sets `heap_limit` of the capability to zero when the remaining budget
falls below the reserve of the runtime operations. The next paragraph
gives the reserve. The next reservation of the thread then takes the slow
path and collects. This is one load on the cold path.

**Runtime operations.** A runtime operation never collects while it holds
a lock. It obtains its barrier capacity before it takes a lock, through a
reservation that the compiler emits and keeps, as it does for heap:

- The GRIN primitive description gives each primitive a barrier bound next
  to its heap bound. The bound counts the entries and card runs the
  primitive records while it holds a lock. The bound is a constant for the
  `MVar` operations, the thread handoffs, the blackhole attach, and the
  timer list. The largest constant is the reserve, eight entries at most.
  A primitive without a heap bound, such as `tryPutMVar#`, has a barrier
  bound all the same.
- The GC-GRIN pass gives `ensure-heap` a second operand, the barrier
  bound. It emits the node before every primitive with a barrier bound,
  also before a primitive without heap words, with zero heap words then.
  The normalization sums the static barrier bounds of a region as it sums
  the heap words. It places the sum at the top of the region. A
  dynamic barrier bound is a reservation of its own, as a dynamic heap
  reservation is. The merge is sound because the budget decreases only
  through barrier work, which the bounds count, and a collection inside
  the region only increases it.
- The fast path of a reservation does not change for a static barrier
  bound. The cold path of the write barrier sets `heap_limit` of the
  capability to zero when the remaining budget falls below the reserve.
  The next reservation of the thread then takes the slow path, also a
  reservation of zero words. The slow path tests the barrier bound against
  the remaining budget and collects when it is short. A dynamic
  barrier bound adds one load and one compare to its fast path, because
  the bound can be above the reserve. Thus every primitive starts with at
  least its bound, and its work under a lock fits.
- A barrier safepoint at a store site lies inside a reservation that
  spans it, because a pointer store is not a reservation boundary. The
  collection at the safepoint must not invalidate the rest of that
  reservation. The GC-GRIN pass therefore gives each barrier call the
  words that the stores after the site still need from the spanning
  reservation. This is a constant of the site. The collection ensures
  these words before it returns, as `aihc_heap_collect` ensures the words
  of its reservation today. A barrier safepoint keeps the reservation
  intact in this way. The normalization needs no new boundary at a store
  site.
- A primitive whose barrier work depends on data gets a dynamic bound. The
  commit of a transaction declares the length of its write log, which a
  helper computes before the reservation. The reservation collects when
  the remaining budget does not cover the bound.
- An operation that wakes a data-dependent number of threads does the
  wakes outside its lock. It takes the lock, moves the waiters to a
  private list, and releases the lock. Then it wakes them one by one under
  a host root frame, where it can collect at a host safepoint when the
  budget is exhausted. `putMVar#` wakes all blocked readers this way, and
  a commit wakes the watchers of its written variables this way. The
  scheduler reads the messages of its inbox one at a time in the same way.
  Each message has the reserve, and the scheduler can collect between two
  messages.

**Oversized operations.** An operation whose work exceeds the whole budget
cannot get its capacity from one collection. Two operations can be that
large, and each has a rule that keeps its excess out of the bound:

- `copyArray#` and `copySmallArray#` dirty one card run for each 128
  elements of the target. The copy helper in `aihc_array.lir` copies in
  pieces of at most half the budget of card runs. Before each piece it
  calls the range barrier with the spilled roots of the copy: the source,
  the target, the offsets, and the count. The barrier call is a safepoint.
  It collects when the budget is short, and it records the cards of the
  piece after the collection, as a store site does. Thus a copy of any size
  exceeds the budget by nothing.
- A transaction whose write log is longer than the whole budget commits in
  a stop-the-world step. The commit reservation finds that no budget can
  cover the bound, so it requests a stop as the collection safepoint does.
  When every capability is parked, the thread commits without the commit
  lock, and the leader runs a collection directly after the commit. The
  entries of the commit are consumed by that collection, so no later
  collection sees them. Such a transaction writes more variables than the
  budget holds entries. With the default budget, that is several hundred
  thousand writes in one transaction. The stop is the cost of its size.

**Permitted excess.** A collection can find the count of a capability
above the budget by two amounts. One is the reserve. The other is the
allowance of the pause walk, which the blackholing section gives. Nothing
else exceeds the budget. The work bound names both.

Thus the budget bounds the scan work of the remembered set and the dirty
cards at a collection. The size of the arrays and of the old heap does not
change that bound. A loop that stores into an old array without an
allocation collects after one budget of barrier work.

The barrier fast path is the sub-range test of the nursery section. The cold
path gains one counter add, one compare, and one load of `%hp_limit`. The
spill code at each store site costs code size and no time, because it is
outside the fast path.

### Concurrent marking of gen2

Gen2 keeps the snapshot-at-the-beginning marking of
[gc-design.md](gc-design.md), and the marker moves off the pause. A marker
worker drains the mark stack and the mark buffers while the capabilities
run. The deletion barrier, the marked allocation of promotions during a
cycle, the atomic or on the bitmap word, and the pop-time scan of a
deferred chunk through `aihc_stack_enter_chunk` are the pieces that make
this safe, and they exist already. The rules that the concurrency adds
are:

- **The flip is the one stop of a cycle, and it does no work.** A cycle
  starts with a stop-the-world step that turns the deletion barrier on in
  every capability. Without it, a capability with the barrier off can
  delete a pointer that a capability with the barrier on has copied into a
  fresh local object, and the marker sees neither. The step costs one
  rendezvous for each cycle, and a cycle runs once for each live heap of
  promotion.
- **The root scans are ragged.** After the flip each capability, at its
  next safepoint, runs a local collection and then scans its gen1 and the
  top chunk of each of its stacks as roots, as the snapshot does today.
  Its lower chunks go to the pending frames. An idle capability is at a
  safepoint, and the marker takes its capability lock and scans for it.
- **A pending snapshot scan adds an insertion barrier on the cold path.**
  Until a capability has done its snapshot scan, its stores into objects
  outside its local heap shade the stored value as well as the old value.
  Without it, a capability can load a pointer from a gen2 object, store it
  into another gen2 object that the marker scanned already, and drop its
  own path to it before its scan. The barrier is on the cold path only,
  and it turns off at the scan. This is the hybrid barrier of Go, applied
  while a stack is grey.
- **The marker never scans a running stack.** Each capability scans its
  own pending frames in bounded slices at its safepoints, as the mutator
  slice does today. The marker scans a stack only under the lock of an
  idle capability, where no thread runs. Otherwise it consumes the mark
  stack and the buffers only.
- **A full mark buffer does not stop.** The capability swaps its buffer
  with an empty one and hands the full buffer to the marker.
- **The termination is ragged.** The marker asks every capability to hand
  over its buffer and to report at a safepoint. When every capability has
  reported and the mark stack and every buffer are empty, the cycle ends.
  When any buffer held work, the marker drains it and asks again.
- **The mutator slice is the assist.** The pacing rule of today gives the
  mark work that each promotion owes. The marker does that work. When the
  marker is behind, the collection that promotes runs a slice itself, as
  today. Thus a capability that promotes faster than the marker marks
  slows itself down, and no capability waits for another.

The mark stack is a set of segments, and a full segment is a unit of work
for a second marker worker of step 9. Each slice marks at most the slice
cap.

**Degradation.** When gen2 doubles since the snapshot while a cycle is
active, the collector has two choices and the user picks one. Without a
heap limit the heap grows past the growth factor and the assists grow with
it, so the pause bound holds and the heap does not. With `-M` set, the
next collection is a full stop-the-world collection, so the heap limit
holds and the pause bound does not. A collector cannot hold a hard heap
limit and a hard pause bound at the same time when promotion outruns
marking. The default is the pause bound.

### Pause bound

A local collection stops one capability. Its work is the
sum of these terms, and its time is that work on one worker:

| Term | Bound |
| --- | --- |
| Copy of the live nursery objects | The `-A` budget of the capability |
| Copy of gen1 | `-B` survivor sets of the capability |
| Root scan | The runnable threads and messages of the capability |
| Stack scan | One chunk for each live thread of the capability plus its young chunks, which the `-A` budget bounds |
| Remembered set and dirty cards | The barrier budget of the capability, plus the reserve and the pause walk allowance |
| Mark slice | The slice cap, only when the marker is behind |

No term depends on the number of capabilities, on the live data in gen2, on
the depth of a stack, or on the size of a large object. A promotion at an
escape is not a pause: it runs on the thread that shares, and its work is
bounded by the local heap of that capability. The escape of a mutable
object costs one local collection. The stops that remain, the flip of a
cycle, a change of the capability count, and the oversized commit, do no
copy work except the full collection of the degradation mode under `-M`.

The bridge collection is not under this bound. Its work is the sum of the
terms above over every capability, so it grows with `-N`. It happens only
at a change of the capability count and in the degradation mode under
`-M`.

The barrier budget bounds the remembered set and dirty card term. The
collector before step 1 scanned every remembered object and every dirty
card at a minor collection. A program can dirty every card of a large old
array without one more nursery of allocation. That work depends on the
size of the array and on the mutation, not on `-A`. The budget bounds it:
each capability counts the barrier work it adds since the last collection,
and the barrier call of a capability above the budget runs a collection at
once. Refer to the write barrier section.

**Stop latency.** The time before a collection is the stop latency of the
previous section. It is a separate bound, in allocation and not in time,
and the statistics file reports it separately from the collection time.

### Shared allocators

The region table, the gen1 blocks, the gen2 segments, the large object
allocator, the pinned list, and the stack regions get one lock each. A
local collection takes the region lock only when it needs a new gen1 block
or gen2 segment. Two capabilities can collect at the same time. The mutator reaches these locks only for a large or pinned
allocation and for a stack region, which are rare. In the end state a
promotion reaches the gen2 segment lock too. Each capability fills one
segment for each size class of its own, so a promotion takes the lock once
for each segment and not once for each object.

The fixed charges, the `-M` limit test, and the allocation statistics use
one atomic add for each large or pinned allocation. The movable allocation
statistics stay in the capability.

## Blackholing

A thunk entry sets bit zero of the header today. The runtime keeps the
original info table and payload. An update writes the result into field
zero and the indirection info table into the header. A thread that enters a
thunk under evaluation becomes a waiter. The waiters of a thunk are in a
hash table.

With more than one capability, three races appear:

- Two capabilities enter the same thunk before either sets the bit. Both
  evaluate it.
- A capability becomes a waiter while the owner updates the thunk. The
  update can miss the waiter.
- An update of one capability lands between the header load and the bit
  store of another capability. The bit store then puts the thunk info table
  back, with the result of the update in field zero. The second capability
  then reads the result as its first free variable.

### Options

1. **Eager claims with plain stores and an owner word.** Each thunk gets
   one word after the header. The entry writes the owner thread into it
   with a plain store, and the update writes the result into it. A waiter
   sends a message to the capability of the owner. No atomic operation is
   on the hot path. A plain store at entry cannot take part in any claim
   protocol: a late duplicate entry overwrites a claim on the header and a
   claim on the owner word alike. Thus `noDuplicate#` cannot give its
   guarantee, and a late duplicate entry can displace a waiter queue. We
   rejected it in the review of the previous revision.
2. **Eager claims with a compare-and-swap at every entry.** The entry
   claims the thunk atomically, so nothing overwrites a claim. This costs
   about twenty cycles on every thunk entry. Thunk entries are frequent in
   lazy code, so we estimate a cost far above the budget. We rejected it.
3. **Lazy claims with an owner word.** This is the GHC default. The entry
   writes nothing. A thread claims the thunks of its update frames when it
   pauses: before a collection, at a context switch, when it blocks, and at
   `noDuplicate#`. Each claim is a compare-and-swap on the header. A thunk
   whose evaluation ends without a pause is never claimed. Only an update
   writes without a claim, and an update is the end of an evaluation.
4. **Ownerless blackholes.** Keep the layout. The entry re-checks the
   header before it sets the bit. A waiter registers in a global table and
   runs a process-wide barrier. The update loads one byte of a filter
   array. An abandoned evaluation must freeze its frames into a resumption
   object. This option is eager, so it has the claim problem of option 1.

### Decision

We use option 3. The reasons are:

- A claim is always a compare-and-swap, and nothing but an update writes
  the header or field zero without one. The claim protocol has no window.
- `noDuplicate#` keeps the GHC guarantee: it claims every update frame of
  the current stack, whatever code pushed it. No compiler recognition is
  needed.
- The entry stores nothing. This removes the bit store of the current
  entry, so the entry is cheaper than today.
- The write barrier work of a claim happens at a pause, not at an entry.
- An abandoned evaluation puts the thunk back, because the free variables
  stay intact in the owner word layout.

The costs are the owner word and the claim walk at each pause. We lose the
loop detection at the first re-entry: a self-re-entry is detected at the
first pause after it, as in GHC. A loop grows the stack, and a stack growth
charges the nursery, so the first collection safepoint detects it.

Step 1 measures the owner word on the thunk-heavy workloads against the
current layout, with the entry store removed. The ownerless alternative is
rejected, not deferred: its claim is a plain store, which the review of
option 1 showed unsafe. A corrected alternative needs a new design review
before it can replace this decision.

### Thunk layout

A thunk has the header, the owner word, and its free variables. A thunk
without free variables has the header and the owner word, as today. The
owner word is field zero. The pointer bitmap of a thunk marks field zero as
a pointer. The collector traces it only when bit zero of the header is set.
A new thunk does not initialize the word.

### One word of state

The state of a thunk after a claim or an update is in field zero alone. The
header keeps the thunk info table with bit zero set from the claim or the
update to the next collection. This is the GHC model: an updated thunk
keeps the `BLACKHOLE` info table, and the identity of its indirectee gives
the state.

A two-word state, with a header store at the update, has no safe order of
stores. A reader can load a blackhole header and then field zero. It then
finds the value of an update in progress and takes it for an owner. A
duplicate can store its owner under the indirection header of a completed
update. A reader then takes a thread record for a value. One word of state
removes both cases.

Every reader classifies field zero by the object kind of the record that it
names:

| Kind of field zero | State | Reader action |
| --- | --- | --- |
| Thread record | Claimed by that thread | Block |
| Blackhole queue | Claimed by the owner of the queue, with waiters | Block |
| Any other object | Evaluated. Field zero is the value. | Use the value |

A thread record and a blackhole queue have object kinds that no Haskell
value has. A thunk has a lifted type, so its value is never a bare thread
record.

The inline evaluation check changes in one place. Today a thunk info table
sends an object to the slow path, and an indirection info table sends it to
the follow path. Now the check tests the two low bits of the header of a
thunk together. Bits `00` are the state Thunk, and the object goes to the
slow path. Bits `11` are the state Claiming, and the object goes to the
slow path too. Bits `01` are the states Claimed and Evaluated. The check
then loads field zero with a dependent load and loads the header of that
object. A thread record or a blackhole queue goes to the slow path. Any
other object is the value, and the check continues with it. The cost is
one compare of the two bits and one compare of the kind byte on the follow
path. The collector still makes two-word indirections with the indirection
info table, which keep their follow path.

Every reader and every updater handles the state Claiming in one way: it
loads the header again until the bits change. The state lasts two stores
of the claiming thread, the owner store and the header store, so the wait
is short. No reader dereferences field zero in the state Claiming, so no
reader sees the unused word. The slow path of the inline check,
`aihc_lir_eval`, does this wait. The claim itself never waits for a reader.

### States

| State | Header | Field zero |
| --- | --- | --- |
| Thunk | Thunk info, bit zero clear | Unused or stale |
| Claiming | Thunk info, bits zero and one | In transition |
| Claimed | Thunk info, bit zero | The owner thread |
| Claimed with waiters | Thunk info, bit zero | A blackhole queue: the owner thread, the thunk, and the waiters |
| Evaluated | Thunk info, bit zero | The value |
| Copied | Indirection info | The value |

The transitions are:

- **Entry.** `aihc_lir_eval` pushes the update frame and transfers to the
  thunk entry. It writes nothing into the thunk. A thunk under evaluation
  without a pause stays in the state Thunk, and a second thread that enters
  it evaluates it too.
- **Pause.** A thread pauses before a collection, at a context switch, when
  a scheduler call suspends it, and at `noDuplicate#`. The pause walks the
  update frames of its stack from the top. An update frame has a processed
  mark: bit one of its header word. A push writes the header without the
  mark, and the walk sets the mark when it has processed the frame. The
  mark is not a pointer store, so it needs no collector barrier, and a
  frame reader masks the bit as every header reader does. The walk stops at
  the first frame with the mark. The walk that set the mark processed
  every frame below it too, by this rule. The walk does not stop for any
  other reason. For each frame without the mark it loads the header of
  the thunk and takes one case, then sets the mark:
  - Thunk: it claims. The claim is a compare-and-swap of the header from
    the thunk info to the thunk info with bits zero and one. On success it
    stores the current thread into field zero with release order. Then it
    stores the header with bit zero alone with release order. Both
    stores are release stores: a reader can see the owner under a header
    that an update stored, so the owner store must publish the thread
    record itself. On failure it loads the header again and takes the case
    that it finds.
  - Claiming: another thread is between its swap and its header store. The
    walk loads the header again until it changes.
  - Claimed by this thread, or a queue of this thread: the thread has two
    update frames for one thunk, because the frame has no mark. This is a
    loop. The walk records the loop and continues, and after the walk the
    thread raises the loop exception.
  - Claimed by another thread, or a queue of another thread: this thread
    is a duplicate. The walk records the frame as a duplicate frame and
    continues.
  - Evaluated: a duplicate finished first. The walk records the frame as
    an evaluated frame and continues.

  After the walk, the thread acts on the lowest recorded frame, because
  the frames above it belong to evaluations that it gives up. For a
  duplicate frame, it abandons every frame above it as below and drops the
  frame. Then it sends a blackhole message for the thunk with the parent
  continuation of the frame, and it parks. For an evaluated frame, it
  abandons every frame above it, drops the frame, and continues with the
  value in the parent continuation of the frame. The frames below the
  lowest recorded frame keep their claims and their marks. Thus the walk
  completes for every live frame before the thread parks or continues
  elsewhere.
- **Attach.** The capability of the owner attaches the first waiter. It
  allocates a blackhole queue with the owner thread, the thunk, and the
  waiter. It links the queue into the queue list of the owner thread and
  stores the queue into field zero with a release store. A later waiter is
  appended to the queue by the same capability.
- **Update.** The updater loads the header. In the state Claiming, it
  loads the header again until the state changes. In the state Thunk, it
  stores the value into field zero and then the header with bit zero, both
  with release order. In the states Claimed and Evaluated, it loads field
  zero and stores the value with a release store. A queue of the updater
  is woken. A thread record or a queue of another thread means a
  duplicate: the value stands, and the owner recovers its queue as below.

  An update in the state Thunk can interleave with a claim of a duplicate.
  The claim cannot exclude the update, because the update loaded the
  header before the swap. Every interleaving ends in a valid state:

  | Order | Final state | Reason |
  | --- | --- | --- |
  | Update stores the value and the header, then the swap | Evaluated | The swap fails. The duplicate loads the header again and finds the value. |
  | Update stores the value, the swap, the owner store, the header stores in any order | Claimed by the duplicate | The owner store replaced the value. The duplicate evaluates again, and its update writes an equal value. |
  | The swap, the owner store, update stores the value, the header stores in any order | Evaluated | The value replaced the owner. The duplicate finds the value at its update. |
  | The swap, update stores the value and the header, the owner store, the header store | Claimed by the duplicate | The header store of the update ends the state Claiming early. Field zero holds the value at that instant, so a reader takes it as evaluated, which is correct. A reader that loads field zero after the owner store sees the owner under the header of the update. The owner store is a release store, so the dependent load of the reader publishes the thread record. |

  The release order of the update puts the value into field zero before
  its header store, so a reader never finds the unused word under bits
  `01`. The claim puts the owner into field zero before its header store
  for the same reason.
- **Recovery.** Each thread keeps the list of the queues it owns. At each
  pause, at each update, and at each abandon, the thread walks the list.
  A queue whose thunk is evaluated is unlinked. A queue that field zero of
  its thunk no longer names is unlinked too. The waiters of an unlinked
  queue are woken with the value of the thunk. Thus a queue that an update
  of a duplicate displaced wakes at the next pause of its owner at the
  latest. The owner pauses at least at its own update of the thunk. The
  waiters of a displaced queue wait for
  the owner, not for the duplicate, as in GHC.
- **Blocking.** A thread that finds a thread record or a queue in field
  zero reads the owner. When the owner is the thread itself, the runtime
  reports a loop. Otherwise the thread sends a blackhole message to the
  capability of the owner. The message holds the thunk and the
  continuation. The thread then parks as blocked on the thunk.
- **Message.** The capability of the owner reads the message at its next
  schedule. It loads the header and field zero of the thunk. A value wakes
  the sender with the value. A thunk with bit zero clear wakes the sender
  with `Eval thunk continuation`, because the claim was abandoned. A
  thread record or a queue with an owner on this capability attaches the
  sender. An owner on another capability forwards the message there. The
  owner cannot move while its capability reads the message, because only
  that capability moves it.
- **Abandon.** An exception unwinds an update frame. For a thunk that the
  thread claimed, the thread unlinks its queue. It stores the header with
  bit zero clear with a release store. Then it wakes the waiters of the
  queue. A
  synchronous exception wakes them with `Raise exception continuation`. An
  asynchronous exception wakes them with `Eval thunk continuation`. For a
  thunk in the state Thunk, nothing is written. For a thunk that a
  duplicate evaluated, nothing is written either. Field zero keeps a stale
  owner, which no reader loads while bit zero is clear and the next claim
  overwrites.
- **Stale reader.** A reader can load the header before an abandon and
  field zero after it. It then finds the old owner, blocks, and sends a
  message. The message handler finds bit zero clear and wakes the reader
  with `Eval`. The two loads of a reader have no safepoint between them,
  so no collection runs between them. Thus the stale owner is a valid
  record when the reader loads it.

The blackhole table of the current RTS disappears. The queue is in the
thunk and in the list of its owner. The message queue is the inbox of the
capability. A thread that parks on a blackhole is retained by the queue, and
the queue is retained by its owner thread. A queue whose owner thread dies
cannot exist, because a thread abandons every claim before it ends.

### Collector barriers

A claim, an attach, an update, and an abandon store a pointer into a thunk
that can be old or static. Each takes the write barriers of
[gc-design.md](gc-design.md):

| Transition | Store | Remembered set | Deletion barrier |
| --- | --- | --- | --- |
| Claim | A young thread record into field zero | When the thunk is outside the nursery | None. Field zero held no pointer. |
| Attach | A young queue into field zero | When the thunk is outside the nursery | The thread record that the queue replaces |
| Update | The value into field zero | When the thunk is outside the nursery, as today | Every pointer field of the thunk, as today, which includes the owner or the queue |
| Abandon | Bit zero clear | None | The queue that field zero names, when the thunk is in gen2 and a cycle is active |

The claim and the attach happen at a pause, so their barrier cost is not on
the entry path. The barrier fast path is the local nursery test, and a
thunk in the nursery of this capability pays nothing more. The update
barrier is the one of today.

In the end state the attach and the update are escapes when the thunk is
outside the local heap of this capability: the attach stores a local
queue, and the update stores a local value, into an object that other
capabilities can reach. The cold path promotes the queue or the value and
its local graph first, then stores. A thread record is always in the
global heap, so the claim is never an escape.

The pause walk claims one thunk for each unmarked update frame, and each
claim of a thunk outside the nursery records one remembered set entry. The
walk cannot collect in the middle, because a collection starts with a
pause. Its capacity comes from an allowance instead of a reservation. An
unmarked frame is in a young stack chunk: every frame in a chunk that a
collection scanned was marked by the pause before that collection, because
a walk marks every frame below the first marked one. A stack growth
charges each young chunk to the `-A` budget of the capability. Thus the
young chunks hold at most the `-A` budget of frames, and an update frame
has three words. The walk therefore records at most that many entries
above the barrier budget. The work bound of the collection names this
allowance. The walk does not count against the budget, and the barrier
that it takes never collects. After the walk, the usual rule applies: a
remaining budget below the reserve sets `heap_limit` to zero, and the next
reservation collects.

### Race tests

The collector fuzz driver and the scheduler stress suite cover these
interleavings, with real OS workers and with a model of the states above:

- Two entries, two updates, with a collection between the updates.
- A pause of one thread between the entry and the update of a duplicate.
  The duplicate must find the claim at its own pause or update.
- Two pauses that claim one thunk at the same time. One must win and the
  other must become a waiter.
- A reader that loads the header before an update and field zero after it.
  The reader must take the value.
- An update of a duplicate that displaces a queue. The waiters must wake
  at the next pause of the owner.
- A message that arrives after the update, after an abandon, and after the
  owner moved to another capability.
- A waiter whose owner abandons the evaluation, with a synchronous and
  with an asynchronous exception.
- A loop through one thread and a loop through two threads. The first must
  be reported at the first pause. The second stays a deadlock, as in GHC.
- A reader and an updater that find the state Claiming. Both must wait for
  the two stores and then act on the claimed state.
- Each row of the interleaving table of the update transition.
- A recursion with two update frames for one thunk, with a pause between
  the two entries and with a pause after both. The lower frames must be
  claimed in both cases.
- A duplicate frame found at a collection safepoint. The refill must
  return a resumption, and the dropped computation must not run.
- A duplicate frame with claimed frames below it. The lower frames must
  stay claimed and marked after the thread parks.
- A loop that stores into an old array without an allocation. The barrier
  must collect after one budget.
- A store of a young value into an old array whose barrier call collects.
  The next collection must find the reference through the card or the
  remembered set.
- A barrier call that collects while a gen2 cycle starts. The deletion
  barrier of the pending store must shade the old value.
- A commit with a write log longer than the remaining budget, and one
  longer than the whole budget. The first must collect before the lock,
  and the second must commit in a stop-the-world step with a collection
  directly after it.
- A `copyArray#` with more card runs than the whole budget. The copy must
  proceed in pieces. A collection inside a piece must keep the cards of
  the earlier pieces and record the cards of the piece after it.
- A pause walk through more unmarked frames than the remaining budget.
  The walk must claim every frame, and the next reservation must collect.
- A store site inside a reservation that spans it, where the barrier call
  collects. The stores after the site must find their words.
- `tryPutMVar#` after barriers that left less than the reserve. The
  reservation of zero words before it must collect first.
- `putMVar#` with more blocked readers than the remaining budget. The
  wake must collect outside the lock and wake every reader.
- A reader that sees the owner of a claim under the header of an update.
  The thread record must be complete.
- The `noDuplicate#` sequences of the next section.

### `noDuplicate#`

Two threads can evaluate one thunk, and both update it with the same
value. `noDuplicate#` makes this impossible for a computation with a side
effect. Its guarantee is the one of GHC: the thunks under evaluation by
the current thread are claimed, and a thread that finds one of them
claimed by another thread does not continue its evaluation.

`noDuplicate#` is a pause. It is a CPS primitive with a continuation, as
`yield#` is, so the thread can park in it. It walks the update frames of
the current stack and claims each thunk with the compare-and-swap of the
pause transition. The walk stops at the first frame with the processed
mark. A frame whose thunk another thread claimed makes the current thread
a duplicate, and it parks as the pause transition gives. A frame whose
thunk is evaluated makes the current thread continue with the value.

The guarantee holds across calls that are not inlined and across separate
compilation, because the walk reads the stack and not the code. It holds
when two evaluations of an outer thunk allocate separate inner thunks,
because the walk claims the outer frame. The second evaluation then finds
the claim at its own `noDuplicate#` and stops before its effect.

The race tests for this section are:

- Two threads evaluate one thunk whose body calls `unsafePerformIO`. The
  effect must run once.
- Two threads evaluate one outer thunk that allocates an inner
  `unsafePerformIO` thunk. The effect must run once.
- Thread A runs `noDuplicate#` after thread B updated the thunk. Thread A
  must take the value and not run the effect.
- `noDuplicate#` in a function that is not inlined and in a separately
  compiled module.

### Base library

The base library does not use the primitive today. `GHC.IO.noDuplicate` is
`return ()`, and `unsafePerformIO` runs its action without it. The change
lands with step 3:

- `GHC.IO.noDuplicate` calls `noDuplicate#`.
- `unsafePerformIO m` is `unsafeDupablePerformIO (noDuplicate >> m)`.
- `unsafeInterleaveIO m` is `unsafeDupableInterleaveIO (noDuplicate >> m)`.
- `GHC.ST` keeps its `noDuplicateST`.

Without this change the primitive protects nothing.

### Alternative: ownerless blackholes

This section keeps option 4 as a record of the review. It is rejected. Its
claim is an eager plain store, so it has the claim problem of option 1. Its
states are:

| State | Header | Field zero | Table entry |
| --- | --- | --- | --- |
| Thunk | Thunk info, no bits | First free variable | None |
| Blackhole | Thunk info, bit zero | First free variable | None, or `registered`, `parked`, or `woken` waiters |
| Indirection | Indirection info | The value | None |

The entry loads its free variables, loads the header again, compares it
with its own info table, and stores the header with bit zero. The update
stores the result and stores the header with release semantics. Then it
loads one byte of a filter array at the hash of the thunk address. A set byte sends
the update to the C path, which locks the table and wakes the waiters. A
waiter inserts a `registered` entry, increments the byte, runs a
process-wide barrier, and loads the header again. An indirection lets it
continue with the value. A blackhole lets it park, unless the update set
its entry to `woken` first. The process-wide barrier is `membarrier` on
Linux, a signal to each worker elsewhere, and nothing on wasm32.

An abandoned evaluation cannot put the thunk back, because field zero can
hold the result of a racing update. It freezes the frames above the update
frame into a resumption object and updates the thunk with an indirection
to it. This is the `AP_STACK` model of GHC.

The race tests above apply to this alternative too, with two more: a
waiter that registers between the header store and the filter load of an
update, and an abandon after a racing update.

## `MVar`

### Options

1. **One global lock.** Simple, with contention between unrelated `MVar`
   operations. We rejected it.
2. **A lock on the info pointer.** GHC swaps the info pointer with a
   `WHITEHOLE` and spins. The collector and every reader must then expect a
   `WHITEHOLE`. We rejected it.
3. **A lock byte in the record.** The `full` slot of the record has seven
   spare bytes. One byte becomes a spin lock.

### Decision

We use option 3. Each `MVar` operation acquires the byte with a
compare-and-swap, does its work, and releases it with a release store. The
uncontended cost is about forty cycles on a C path that costs several
hundred today.

`isEmptyMVar#` stays an inline acquire load of the `full` byte. The result
can change directly after the load, as in GHC. `tryReadMVar#` and
`tryTakeMVar#` become runtime calls that take the lock, because they read
two fields.

A wake sends the woken thread back to its own capability, as the scheduler
section gives. An operation that wakes one thread does so under the `MVar`
lock, within its declared barrier bound. It pushes into the inbox, which
the lock order permits. `putMVar#` wakes every blocked reader. It moves
the readers to a private list under the lock and releases the lock. Then
it wakes them one by one under a host root frame, where it can collect.
Refer to the write barrier section.

Sequential mode can skip the lock, because one worker holds the one
capability at a time, and the capability handover is a lock. We keep the
lock in both modes unless a measurement shows a cost above one percent.

## `STM`

The current transactions write each `TVar` in place and keep an undo log.
This is correct only because no other thread runs during a transaction.
With more than one capability, two transactions can interleave.

### Options

1. **One global STM lock.** A transaction holds the lock from `atomically#`
   to its commit. A transaction that evaluates a thunk that another thread
   owns then holds the lock while it waits. That thread can wait for the
   lock. We rejected it.
2. **Optimistic transactions with a global commit lock.** Writes go to a
   log. Reads record the value that they saw. The commit takes one lock,
   validates every read against the current value, applies the writes, and
   wakes the watchers.
3. **Optimistic transactions with a lock for each `TVar`.** This is the GHC
   model with `STM_FG_LOCKS`. It scales better under heavy commit traffic
   and costs more code.

### Decision

We use option 2. Commit sections are short, and contention on the commit
lock appears only with many committing capabilities. Option 3 is a later
refinement with the same transaction records.

A `TVar` becomes a record with a value and a watcher list. The value store
at commit has release semantics. `readTVarIO#` is one acquire load.

### Logs

A transaction record holds a read log and a write log. Nested transactions
for `catchRetry#` and `catchSTM#` keep a record of their own with a link to
the parent. The lookup order of `readTVar#` is:

1. The write log of the transaction, then the write logs of its parents up
   to the root. A nested transaction sees the writes of its parents.
2. The read log of the transaction, then the read logs of its parents.
3. The `TVar` itself, with an acquire load. The value goes into the read
   log of the transaction.

`writeTVar#` appends to the write log of the innermost transaction. When a
nested transaction commits, its read log and its write log merge into the
parent. The read log merges too, because the parent validates every read
of the whole transaction at its commit.

A nested transaction aborts for `retry#` under `catchRetry#` or for an
exception under `catchSTM#`. Its write log is dropped. Its read log merges
into the parent with the recorded values. The reads of an aborted
branch are dependencies of the whole transaction. The left branch of
`orElse` reads `a` and retries, and the right branch reads `b` and
retries. The transaction must then watch `a` and `b`, because a change to
`a` can let the left branch succeed. The exception of a `catchSTM#` branch
was a consequence of its reads, so the commit of the parent must validate
them too. GHC merges the read set of every aborted nested transaction in
the same way.

A merged read whose recorded value differs from an entry of the parent for
the same `TVar` makes the transaction invalid. The transaction then runs
again from `atomically#` at its next validation.

### Validation

A transaction is valid when the current value of each `TVar` in its read
logs is the recorded value. The commit validates under the commit lock.
The log keeps each recorded value alive, so an address cannot be reused
while it is recorded.

A running transaction can read an inconsistent state. It can read one
`TVar` before a commit of another transaction and a second `TVar` after
it. Such a transaction can loop or raise an exception that a consistent
state would not produce. The rules are:

- **Exception.** Before an exception leaves `atomically#` or a `catchSTM#`
  frame, the runtime validates the transaction. An invalid transaction
  drops the exception and runs again from `atomically#`.
- **Safepoint.** A thread inside a transaction reaches a context switch or
  a collection safepoint. The runtime then validates the transaction with
  a try-lock on the commit lock. An invalid transaction is condemned.
  The thread unwinds to the `atomically#` frame and runs the transaction
  again. A thread that validates often pays one try-lock for each safepoint
  inside a transaction, which the context switch interval bounds.
- **Retry.** `retry#` validates under the commit lock. An invalid
  transaction runs again at once. A valid one registers its thread as a
  watcher on each `TVar` of its read logs, releases the lock, and parks. A
  commit that writes a `TVar` wakes every watcher of it, sends each to its
  own capability, and removes their entries.
- **Blackhole.** A transaction that blocks on a blackhole holds no lock, so
  the owner of the thunk can commit. The blocked transaction validates at
  its resume, as at a safepoint.

Delay variables keep their timer list. The list is global under the commit
lock. The capability that polls a timer applies its expiry as a transaction
that writes the variable.

In sequential mode, validation always succeeds, and the lock is
uncontended. One mutex acquisition is the cost of a transaction, plus one
try-lock for each safepoint inside it.

### Barrier capacity of a commit

The commit stores into every written `TVar` under the commit lock, and
each store takes the write barrier. The commit cannot collect under the
lock, so it reserves its barrier work first. A helper computes the length
of the write log before the reservation of the commit primitive. The
reservation declares that length as a dynamic barrier bound. When the
remaining budget of the capability does not cover it, the reservation
collects first. When the whole budget does not cover it, the commit runs
in a stop-the-world step with a collection directly after it. The wake of
the watchers happens after the lock is released, under a host root frame.
Refer to the write barrier section.

### Tests

- A shared counter with `n` writers.
- Two `TVar` values with an invariant, a writer that changes both, and a
  reader that loops while the invariant does not hold. The reader must
  finish, through the safepoint validation.
- A reader that raises an exception from an inconsistent read. The
  exception must not escape.
- `orElse` with a parent write that the nested branch reads.
- `retry#` with `n` watchers and one writer.

## Memory model

Haskell code with a data race stays type safe. A pointer that a thread loads
from a shared field names an object whose fields are complete. The C
runtime and the LLVM backend must give the same under their language rules.
In C11 and in LLVM, a plain access that races with a write is undefined.
The hardware guarantee of a word-sized access does not apply there. Every
access to a location that another worker can access at the same time is
therefore an atomic access with an explicit order. A relaxed atomic access
compiles to a plain load or store on every target, so this rule costs
nothing on the hot paths.

### Lir instructions

Lir gets `load.atomic` and `store.atomic` with an order, `load.dependent`,
`cas`, `xchg`, `atomic.add`, `atomic.or`, and `fence`. The orders are
`relaxed`, `acquire`, `release`, and `seq_cst`. A plain `load` or `store`
names a location that no other worker can access at the same time: a new
object before its publication, a frame of the own stack, a local block, or
the own capability fields. The Lir lint checks the sites below. The wasm32
target lowers every atomic instruction to a plain operation, because a
module without shared memory runs one thread.

`load.dependent` loads a pointer and gives every later load through it an
address dependency. The native AMD64 and ARM64 backends emit a plain load
and keep the dependency, which both architectures order. The LLVM backend
emits an acquire load, because LLVM gives no guarantee for a dependency.

### Access rules

| Location | Writer | Reader | Order |
| --- | --- | --- | --- |
| Object header, claim | The pause of the claiming thread | Every reader, every other pause | `cas acq_rel` to the claiming state, then `store.atomic release` to the claimed state. Readers use `load.dependent`. |
| Object header, update and abandon | The updater, the thread that abandons | Every reader | `store.atomic release`, `load.dependent` |
| Object header, forwarding | A collection worker | A collection worker | `cas acq_rel`, `load.atomic acquire` |
| Field zero of a thunk, owner | The claiming thread, between its swap and its header store | Every reader that saw bits `01` | `store.atomic release`, then the release store of the header. Readers use `load.dependent` after the header load, then `load.dependent` of the kind of the record. The owner store is a release store, because a reader can see the owner under the header store of an update, which publishes nothing of the claiming thread. GHC uses a release store here too. |
| Field zero of a thunk, queue and value | The capability of the owner, the updater | Every reader that saw bit zero, the recovery walk | `store.atomic release`, `load.dependent` |
| Capability field of a thread record | The capability of the thread | A blocking thread that routes a message | `store.atomic release`, `load.atomic acquire`. A stale value is forwarded by the handler. |
| Fields of a new object | The allocating thread | Nobody until publication | Plain |
| Fields of an immutable object after publication | Nobody | Every reader | Plain, after a `load.dependent` or an acquire load of the pointer |
| Pointer field of a mutable object | `writeMutVar#`, `writeArray#`, the CAS primitives, the runtime | `readMutVar#`, `readArray#`, the collector | `store.atomic release`, `load.dependent` |
| Unboxed field of a mutable array | `writeByteArray#` and the like | `readByteArray#` | Plain. A race is a program error, as in GHC. |
| `atomicReadIntArray#` and the atomic primitives | The primitive | The primitive | `seq_cst` |
| Card byte | The write barrier | A collection worker | `store.atomic relaxed`, `xchg` |
| Capability flags | The ticker, the stop leader | The owner worker | `store.atomic relaxed`, `load.atomic relaxed`. The park is a mutex, which orders the roots. |
| Sub-range cursor | The owner capability | The owner capability | Plain |
| Mark bitmap word, large object mark | A collection worker | A collection worker | `atomic.or relaxed` with the scan queued by the setter |
| `MVar` lock byte | Every worker | Every worker | `cas acquire`, `store.atomic release` |
| `MVar` full byte | The holder of the lock | `isEmptyMVar#` | `store.atomic release`, `load.atomic acquire` |
| `TVar` value | The commit under the lock | `readTVar#`, `readTVarIO#` | `store.atomic release`, `load.atomic acquire` |
| Other thread record fields | The capability of the thread | The capability of the thread, the collector | Plain under the capability handover, which is a lock |
| Inbox, idle list, tables | The lock holder | The lock holder | Plain under the lock |
| Counters of the machine | Every worker | The statistics report | `atomic.add relaxed` |

A reader of an immutable object relies on the publication order: the
allocating thread wrote the fields before the release store that published
the pointer, and the reader loaded the pointer with a dependency or an
acquire. The inline evaluation check loads the header of an object and then
field zero of the same object. Then it loads the header of the record that
field zero names. The first two loads have no address dependency of their own.
The lowering uses `load.dependent` for the header, so the field load
depends on it, and the third load depends on the second.

The C runtime uses the C11 atomic operations with the same orders. C11 has
`memory_order_consume`, but the compilers promote it to acquire, so the C
runtime uses `memory_order_acquire` for every load that Lir does with
`load.dependent`. The C runtime runs cold paths, so the cost is not on the
budget. The collector workers use the sequential instance without atomic
operations when one worker collects.

## IO manager

### Options

1. **One IO manager OS thread.** It polls every request of every capability
   and pushes each completed thread into an inbox. An idle capability then
   waits for the IO thread. Each completion costs one thread handover,
   which is several microseconds. An IO-bound program with one Haskell
   thread can lose more than ten percent.
2. **Each capability polls its own requests.** This is the current model
   for each capability. A request belongs to the capability that submitted
   it. The capability polls with a zero timeout at each schedule and blocks
   in the poll when it is idle.

### Decision

We use option 2. The single-threaded path does not change. The idle wait
of a capability is a poll on its pending descriptors and its wake-up
descriptor. The wake-up descriptor is an `eventfd` on Linux and a pipe on
other POSIX hosts. A push into the inbox or a stop request writes to it.

A thread that migrates after it submitted a request still completes on the
capability that polls the request. That capability sends the thread to its
own capability. The `threadDelay` timers stay with the capability that set
them, with the same rule.

On wasm32 the host driver pumps the one capability, as today.

## Foreign function interface

### `unsafe` and `safe` calls

An `unsafe` call keeps the capability. A long `unsafe` call blocks the
capability and every stop request, as in GHC. Its foreign frame stays on
the foreign frame list of the capability, as today, and the worker of the
capability scans it.

A `safe` call releases the capability before the call and takes a
capability back after it:

1. The runtime moves the foreign frame of the thread from the foreign
   frame list of the capability to its suspended list, under the
   capability lock. The frame holds the roots and the static reference
   table of the call site. The capability owns these roots until the call
   returns, and its worker scans the suspended list in each collection.
   The thread record names the capability, so the collector keeps the
   record and the stack.
2. The runtime increments the count of threads in `safe` foreign calls.
3. The worker releases the capability. When the capability has runnable
   threads, an idle worker takes it, or the runtime starts a worker.
4. The worker makes the C call.
5. After the call, the worker takes a free capability. It prefers the
   capability that it released. It waits when none is free.
6. The worker removes the foreign frame from the suspended list of the
   capability that owns it, under that capability lock, and decrements
   the count. The thread continues on the new capability, and its record
   names the new capability. The chunk header of its stack pointer names
   the new capability.

A capability can own several suspended frames at once, one for each thread
of it that is in a `safe` call. The suspended list has no order
requirement. A stop takes the capability lock, so the list cannot change
during a collection.

`rtsSupportsBoundThreads` stays zero until bound threads exist.

### Callbacks

A foreign callback runs on the stack of the thread that made the foreign
call and may not suspend, as today. The callback entry takes a capability
first. In a `safe` call, it takes the released capability of the caller. On
a foreign OS thread, it takes a free capability. The entry then pushes the
stop frame and writes the capability into the chunk header.

GHC runs a callback as a new bound thread under a nested scheduler loop, so
the callback can block. We keep this as a later step with bound threads.

### Bound threads

`forkOS` and the bound main thread of GHC are not in the first version. A
bound thread runs only on its own OS thread, and the capability moves to
that OS thread when the scheduler selects the thread. The design has room
for it: the worker that owns the thread parks on a condition variable, and
the scheduler hands the capability to it.

## Threads, thread identifiers, and other global state

- `next_thread_id` becomes an atomic counter.
- The blackhole table of the current RTS disappears. A blackhole queue
  lives in field zero of its thunk.
- The stable name list is global under one lock. The hash counter is
  atomic.
- The callback slots are global under one lock.
- Stack chunks come from the spare list of the capability. A capability
  with more than 64 spare chunks returns regions to the machine.
- The statistics file sums the counters of every capability. It gains the
  capability count, the number of workers, the stop latency, and the
  collection time as separate fields. It also gains the fields that the
  garbage collection section names: the stop count, the rendezvous time,
  the escape counts and bytes, the pushes of each
  kind, and the occupied bytes against the segment bytes of each gen2 size
  class.

## RTS options

| Option | Meaning | Default |
| --- | --- | --- |
| `-N<n>` | The number of capabilities. `-N` alone is the number of cores. | 1 |
| `-A<size>` | The nursery size of each capability, rounded up to a power of two. | Decided by measurement |
| `-B<count>` | The number of collections that a survivor spends in gen1. This replaces the byte maximum of today. | 1 |
| `-n<size>` | The nursery block size. | 32 KiB |
| `-C<time>` | The context switch interval. | 20 ms |
| `-R<size>` | The barrier budget of a capability between two collections. | The nursery size |

`getNumCapabilities` returns `n`. `threadCapability#` returns the capability
of the thread and whether it is pinned. `forkOn#` pins the thread to
capability `k` modulo `n`.

## Cost budget for single-threaded work

The table gives each new fixed cost and our estimate for a program with one
Haskell thread. The measurements of build step 1 replace the estimates.

| Cost | Where | Estimate |
| --- | --- | --- |
| Block refill | One slow path for each 32 KiB of allocation | Below 1 percent |
| Sub-range barrier test | Capability, base, and size loads, then an exclusive or, a compare, and a branch | Measurement required |
| Escape count | One counter add on the cold path of the write barrier | Near zero |
| Capability load | One load next to each C call that took the machine and each compiled barrier | Near zero |
| Owner word | One word in each thunk with free variables | 2 to 4 percent on lazy code |
| Entry | The bit store of today disappears | A gain |
| Claim walk | One compare-and-swap, two barriers, and one mark store for each update frame that is live at a pause, once for each frame | Below 1 percent |
| Release stores | `stlr` on ARM64 at update, nothing on AMD64 | Below 1 percent |
| Inline check of an evaluated thunk | One header bit test and one kind compare on the follow path | Below 1 percent |
| Context switch flag | Two loads and one branch for each thunk evaluation | Below 1 percent |
| Barrier budget | One counter add, one compare, and one load of `%hp_limit` on the cold path of the write barrier | Near zero |
| `MVar` lock | One compare-and-swap pair for each `MVar` operation | Below 1 percent of such programs |
| STM commit lock | One mutex for each transaction, one try-lock for each safepoint inside it | Small |
| Atomic primitives | Real atomic operations for `casMutVar#` and the array atomics | Program dependent, as in GHC |
| Collection with `-N1` | The sequential instance, no change | Zero |
| Collection with idle capabilities | One lock and one worker wake-up for each idle capability for each collection | Measured, see below |
| Idle workers | `n - 1` parked OS threads | Zero CPU |

The barrier load cost still needs measurement. The gate is 10 percent against
the RTS before step 1. The workloads are the `gc-*` examples and the compile
workloads of the benchmark suite. Each runs at `-N1` and at `-N<cores>`.

Sequential mode gives a switch for C paths that measure above budget: a
C path tests `machine->capability_count == 1` and skips its lock. Compiled
code has no such switch. Its hot paths need no atomic operation by design.

### Performance tests

The workloads above do not exercise every estimate. These tests measure
the estimates one at a time, each at `-N1` and `-N<cores>` with one
Haskell thread, against the RTS before step 1:

| Test | Estimate under test |
| --- | --- |
| `MVar` ping-pong between two threads on one capability, and `takeMVar` and `putMVar` in a loop from one thread | The `MVar` lock |
| An STM counter from one thread, and a transaction with ten reads | The commit lock and the safepoint validation |
| A file copy with small reads and writes | The IO polling model |
| A thunk-heavy workload with `-A256k`, at `-N8` with one busy thread | Frequent collections with idle capabilities, including the worker wake-ups |
| A program that forks one thread that waits on an `MVar` for the whole run | The ticker and the context switch flag |
| `unsafePerformIO` in a loop | The pause walk of `noDuplicate#` |

The frequent collection test checks that idle capabilities add no cost to
the collections of a busy one. A local collection wakes no other
capability.

## Comparison with GHC

| Topic | GHC threaded RTS | This design |
| --- | --- | --- |
| Build flavours | Non-threaded and threaded runtime libraries. | One runtime library. |
| Default `-N` | 1 | 1 |
| Capability access | `BaseReg`, a pinned register. | A load from the stack chunk header, only next to C calls. |
| Heap check | `Hp` against `HpLim` in memory. | Two registers. |
| Remote interrupt | Store zero to `HpLim`. | A flag read at the block refill and at thunk evaluation. |
| Ticker | Timer thread or signal, 20 ms. | Timer thread, 20 ms, started at the second Haskell thread. |
| Nursery | A block list for each capability, `-n` chunks optional. | One arena, one sub-range of `-A` bytes for each capability, 32 KiB blocks in address order. |
| Gen1 | Gen0 to-space, emptied at every collection. | Local blocks, emptied every `-B` collections of the own capability, one by default. |
| Write barrier | Generation test through the block descriptor, `mut_list` for each capability. | An exclusive or with the local nursery base and a size compare, remembered set for each capability. |
| Local heaps | A prototype in 2011, not merged. | The end state. Promotion on escape at the cold paths. |
| Minor collection | STW, parallel copy, work stealing. | A local sequential copy for each capability, no stop. No parallel copy. |
| Old generation | Copying by default, concurrent non-moving optional. | Non-moving mark and sweep with a concurrent marker. One stop for the flip, ragged root scans and termination. |
| Thunk header | Two words. | Two words. |
| Blackholing | Lazy by default, claims at `threadPaused` with a compare-and-swap, owner in the thunk. | Lazy, claims at a pause with a compare-and-swap, owner in the thunk. |
| Evaluated thunk | `BLACKHOLE` info with the value as indirectee. | Thunk info with bit zero and the value in field zero. |
| Blackhole waiting | A message to the capability of the owner, `BLOCKING_QUEUE` as indirectee, `tso->bq` for recovery. | A message to the capability of the owner, a blackhole queue in field zero, the queue list of the owner for recovery. |
| Update frame and indirection | `evacuate_BLACKHOLE` keeps the thunk. | The thunk field of an update frame is not shortened. |
| `noDuplicate#` | Claims each update frame at `threadPaused`. | Claims each update frame at the pause walk. The same guarantee. |
| Abandoned evaluation | `AP_STACK` object. | The thunk is put back, as today. |
| `MVar` | Lock on the info pointer with `WHITEHOLE`. | Lock byte in the record. |
| `STM` | Optimistic, per-`TVar` locks at commit, watch queues, validation at exceptions and context switches. | Optimistic, one commit lock, watch queues, validation at exceptions and safepoints. |
| IO manager | Haskell event manager threads, `epoll`, `kqueue`, timer manager. | Poll in each capability, timers in each capability. |
| `safe` FFI | Releases the capability, `suspended_ccalls` list for each capability. | Same, the suspended list of the capability. |
| Callbacks | New bound thread, nested scheduler, can block. | On the caller stack, cannot block. |
| Bound threads | `forkOS`, bound main thread. | Later. |
| Sparks | Spark pool deque for each capability. | Later. |
| Thread movement | Push to idle capabilities. | Push to idle capabilities. A woken thread goes back to its own capability. |
| Deadlock | `BlockedIndefinitelyOnMVar` after a collection. | Failure when every capability is idle and no foreign call is in flight, as today. |
| Memory model | Release and acquire barriers since GHC 9.2. | Atomic accesses with explicit orders at every shared location, dependency loads. |
| Non-allocating loops | Not preemptible without `-fno-omit-yields`. | Not preemptible. Loops that allocate but do not evaluate are not preemptible either. |

The GHC threaded RTS costs a few percent on single-threaded programs. The
parallel collector can cost much more on them, because its idle workers
spin, so users set `-qg` or `-qn`. This design has no parallel collector:
a capability collects its own local heap, and idle capabilities take no
part.

## Build order

Every step builds a part of the end state. No step builds a part that the
end state removes.

1. **Capability split and local heap shape.** Add `AihcCapability` and the
   chunk header field. Move the per-worker fields. Add the arena with one
   sub-range and one cursor for each capability, and the block refill. Add
   the sub-range test of the write barrier and the escape count on its cold
   path. Add the gen1 blocks of each capability and the `-B` count rule,
   and remove the byte maximum. Put thread records in the global heap. Add
   the Lir atomic instructions and the access rules. Add the owner word,
   the lazy claims with the pause walk, the queue list of a thread, and the
   new update. Add the barrier budget, the barrier bound of `ensure-heap`
   in GC-GRIN, and the remaining words of a barrier safepoint. Add the
   update frame rule to the collector. Still one OS thread and `-N1` only.
   Measure the cost budget and the three entries. This step carries most
   fixed costs, and its escape count measures the cost of step 2 before
   step 2 exists.
2. **Several capabilities and promotion.** Give the machine an array of
   capabilities with one arena window, one gen1, and one remembered set
   for each. Add the bridge collection. Add the promotion on escape at
   each row of the escape table, with the three promotion rules. Still one
   OS thread. Only the collector fuzz driver uses more than one
   capability, and it plays the escapes of each row.
3. **Local collection.** Enforce the invariant of the local heaps section.
   Make a full sub-range start a local collection of its own capability
   and nothing else. Add the invariant walk to the verifier. The fuzz
   driver collects one capability while the others keep their local
   heaps. This step removes the stop from the young side before any
   Haskell thread runs in parallel.
4. **Workers and the scheduler.** Add the workers on OS threads, the
   capability locks, the sync protocol, the lock order, and
   `setNumCapabilities`. Add the inboxes, the push of threads with the
   promotion of a pushed stack, the push counts, the idle wait, the
   wake-up descriptors, and the ticker. Add the locks of the shared
   allocators. The RTS refuses `-N` above one.
5. **Synchronisation primitives.** Add the `MVar` lock. Add the blackhole
   message protocol, the queue recovery, `noDuplicate#`, and the base
   library change. Add the optimistic STM with the commit lock and the
   validation rules. Add the real atomic primitives. Add the race tests and
   the STM tests. Every shared runtime record follows the escape table.
6. **Parallel execution.** Accept `-N` above one. Run the scheduler stress
   suite and the performance tests. This is the first version that runs
   Haskell threads in parallel. Each capability collects its own local
   heap.
7. **Concurrent marker.** Add the marker worker with the flip, the ragged
   root scans, the insertion barrier of a pending scan, the buffer swap,
   the ragged termination, and the assist. Until this step the mutator
   slices mark gen2, as today.
8. **FFI.** Add `safe` calls that release the capability, the suspended
   frame lists, the worker pool, the deadlock rule, and callbacks from
   foreign OS threads.
9. **Later.** Bound threads, blocking callbacks, sparks, a second marker
   worker, and the bounded evacuation of sparse gen2 segments.

Each step leaves the compiler shippable and is its own PR series. Steps 2
to 5 do not expose parallel execution, so no intermediate version runs
Haskell threads in parallel without the protocols that they need or with
a stop-the-world young collection.

## Tests

- The collector fuzz driver plays compiled code for several threads today.
  It gains several capabilities, the escapes of each row of the escape
  table, local collections of one capability, the thunk states of the
  blackholing section, and the race tests of that section. It runs under
  the thread sanitizer when the C compiler supports it.
- A scheduler stress suite under `examples/concurrency-*`. The programs
  are: `MVar` ping-pong between capabilities, a shared lazy list that `n`
  threads consume, and the STM tests of the STM section. The suite also has
  `noDuplicate#` with `n` evaluators of one `unsafePerformIO` thunk, and
  `safe` calls that block while other threads run.
- The cost budget gate and the performance tests of the cost budget
  section.
- The GRIN `gc-stress` fixtures and the verifier under `AIHC_GC_VERIFY`
  run with `-N2` as well. From step 1 the verifier reports the local heap
  invariant, and from step 3 it enforces it.
- The escape tests of step 2: a producer that sends a lazy list through an
  `MVar`, two threads that share one `IORef`, a thread pushed after it has
  run, `makeStableName#` on a local object before and after its escape, and
  a thunk that escapes while it is under evaluation. Each runs under the
  verifier and under the thread sanitizer.

## Open points

- The nursery block size. The measurement of step 1 decides between 16 KiB,
  32 KiB, and 64 KiB.
- The context switch at the thunk branch. If the measurements show that
  programs need a finer switch, the outlined reservation continuation is
  the fallback.
- The owner word. Step 1 measures it against the budget. There is no
  fallback: a cheaper protocol needs a new design review.
- The nursery size pulls two ways. A small nursery fits L2 and bounds the
  memory bandwidth of many capabilities. A large nursery lowers the
  survival rate and gives longer aging under the `-B` count rule. The
  step 6 measurement weighs both.
- The default of `-B`. One is the GHC aging. Zero promotes into gen2
  directly and moves work from the pause to the marker. The step 2
  measurement decides.
- The push policy of the end state. A push of a thread that has run
  promotes what its stack reaches. The push counts of step 2 decide
  whether the scheduler needs a stricter rule than one such push for each
  schedule.
- The fragmentation of gen2. The statistics report the occupied bytes
  against the segment bytes of each size class. The bounded evacuation of
  sparse segments comes only when a workload shows the need.
