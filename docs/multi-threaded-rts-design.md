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
  gives them.

## Non-goals

- Thread-local heaps. Every capability shares one heap.
- A concurrent collector thread. The collector runs only when every
  capability is stopped.
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
| Sequential mode | The machine has one capability. |

## Summary of decisions

| Topic | Decision | GHC |
| --- | --- | --- |
| Execution model | Capabilities with a shared heap. One worker holds one capability. | Same |
| Capability pointer | Loaded from the header of the stack chunk of `%sp`. No new register. | `BaseReg` is a pinned register. |
| Nursery | One arena for all capabilities. A capability takes 32 KiB blocks from it. | One nursery for each capability, optional `-n` chunks. |
| Write barrier | Unchanged. The arena is one address range. | A generation test through the block descriptor. |
| Safepoints for GC | The block refill, the scheduler calls, and idle capabilities. | `HpLim` in memory. A remote store forces the next heap check to fail. |
| Context switch | At `aihc_lir_eval` on a thunk, at scheduler calls, and at `yield#`. A ticker sets a flag. | At every heap check, through `HpLim`. |
| Garbage collection | STW parallel copy. Every worker collects. A sequential instance without atomics serves one capability. The work has a hard bound, the time has a stated assumption. | STW parallel copy by default. |
| Old generation | Incremental marking in STW slices, as today. | Optional concurrent non-moving collector. |
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
2. **One arena for all capabilities.** The arena is one run of regions. A
   capability takes blocks from the arena. The write barrier keeps its one
   range test.

### Decision

We use option 2. The nursery arena is one region run of `-N` times `-A`
bytes. `aihc_nursery_start` and `aihc_nursery_bytes` describe the arena, so
the write barrier does not change. A capability takes blocks of 32 KiB from
the arena with one atomic add on the arena cursor. The RTS option `-n<size>`
sets the block size.

The compiled allocation fast path does not change. It is a bump and a
compare in registers. The slow path, `aihc_heap_collect`, first refills the
block of the capability from the arena. It collects only when:

- the capability has taken `-A` bytes since the last collection, or
- the capability has passed its barrier budget, or
- the arena has no free block, or
- another capability requested a stop.

The first rule keeps the collection frequency of a single-threaded program
as it is today. One thread fills `-A` bytes of consecutive blocks before it
collects, so its cache behaviour does not change with `-N`.

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
- Each scheduler primitive: `yield#`, `fork#`, the blocking `MVar`
  operations, `awaitIO#`, the blackhole wait, and the STM wait.
- The idle wait of a capability without runnable threads.
- The release of the capability by a `safe` foreign call.

A loop that neither allocates nor evaluates a thunk reaches no safepoint.
A stop request then waits for it. GHC has the same limit for a loop that
does not allocate.

### Context switches

A stop request parks the worker inside the runtime call. The Haskell thread
keeps its roots in the spilled root array of the slow path. No continuation
is necessary. The block refill therefore serves the collector.

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

A capability requests a stop for a collection, a mark slice, or a change
of the capability count. Every request comes from a safepoint of the
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
  the only stop requests come from the safepoints above.
- A worker that holds an object lock does not run Haskell code. It waits
  for another worker only on an inbox lock or an allocator lock below it.

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
the depth of a stack or on the size of a large object. This section gives
what the parallel collector adds to that bound and what it cannot promise.

### Options

1. **STW, one collector.** The requester collects alone. The other
   capabilities park. The collector keeps its property that it uses no
   atomic operations. A collection copies the live young objects of every
   capability, so the pause grows with the number of busy capabilities.
   We rejected it.
2. **STW, parallel copy.** Every worker takes part in the collection. Each
   worker copies the roots of its capability and the objects it reaches.
   The workers steal scan work from each other. The work of a collection
   grows with the number of busy capabilities, and the number of workers
   grows with it. This is the GHC default.
3. **Thread-local collection.** Each capability collects its own nursery
   without a stop. This needs a promotion of each object at the point where
   another thread can see it. It changes the GRIN safepoint model and the
   write barrier. We rejected it.

### Decision

We use option 2 from the first version with more than one capability. The
collector keeps a sequential path for one worker. The copy loop is one
function that a macro parameter instantiates twice. The sequential instance
uses no atomic operation, so a program with `-N1` pays nothing. The RTS
option `-qn<k>` limits the number of workers of a collection, and `-qg`
selects the sequential instance.

### Work distribution

Each worker owns its allocation and its scan work:

- **To-space blocks.** Each worker fills gen1 blocks of its own and one gen2
  segment for each size class of its own. No two workers write the same
  block. A worker scans the blocks it filled with a Cheney cursor, one
  block at a time.
- **Stealing.** A full block that its owner has not scanned yet is a unit
  of work. Each worker keeps its full unscanned blocks on a list with a
  lock. A worker without local work takes a block from another worker.
- **Roots.** The leader divides the capabilities among the workers of the
  collection before it starts: capability `i` goes to worker `i` modulo
  `k`. A worker scans the roots of each capability it got: the current
  thread, the run queue, the inbox, the selected resumption, the foreign
  frames, the suspended foreign frames, the spilled roots of its parked
  safepoint, the remembered set, and the mark buffer. The global roots are
  divided among the workers in fixed shares. With one worker, that worker
  gets every capability. Thus the root coverage does not depend on `-qn`
  or on `-qg`.
- **Stacks.** A stack is reached through its thread record. The worker that
  claims the thread record scans the stack and sets the ages of its chunks.
  Thus one worker scans each stack, and a thread record is a unit of work.
- **Termination.** A worker without local work and without a block to
  steal increments the idle count and waits. The collection ends when every
  worker is idle. A worker that frees work wakes the idle workers.

### Forwarding

A copied object keeps its new address in its old header with the low two
bits set to two, as today. With several workers, the install of this
forwarding header is a compare-and-swap:

- An immutable object is copied first. Then the worker installs the
  forwarding header with a compare-and-swap against the header it loaded.
  When the swap fails, another worker copied the object first. The loser
  takes the address from the winner and leaves its copy as slop. The slop
  reads as zero words, which no header is, as the slop of an update does
  today.
- A mutable object, a thread record, and every runtime record is claimed
  first. The worker swaps the header with a claim pattern, copies the
  object, and installs the forwarding header with a release store. A worker
  that reads the claim pattern waits for the forwarding header. Two copies
  of a mutable object cannot exist.
- A large object and a pinned block keep their mark in their block header.
  The mark is set with an atomic or. The worker whose atomic or set the
  mark sets the generation and moves the charge.
- A static object is marked in an address set, as today. The set becomes a
  table with compare-and-swap insert. A collection only inserts, and the
  table is resized between collections.
- A gen2 object is marked in the segment bitmap with an atomic or on its
  word. The worker whose atomic or set the bit queues the object for a
  scan.

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
pause claims the thunks of its update frames. Thus every update frame names
a claimed thunk or an evaluated thunk when the collection runs. No frame
names a thunk in the state Thunk.

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
default. When the count of a capability passes the budget, the capability
requests a collection at its next safepoint. Thus the budget bounds the
scan work of the remembered set and the dirty cards at a collection. The
size of the arrays and of the old heap does not change that bound.

### Incremental marking of gen2

The marking slices run in STW steps, at the end of each collection, as
today. A full mark buffer requests a STW step that runs one slice and does
not copy. The slice is parallel in the same way as the copy. The mark stack
is a set of segments, and a full segment is a unit of work that a worker
can steal. Each worker marks at most the slice cap. The mark debt of a
collection is the sum of the promotions of every worker, so the pacing
keeps its meaning.

We considered slices on a mutator worker while the other workers run. The
snapshot-at-the-beginning invariant holds under concurrency, but the marker
then reads frames that another worker pops. We keep this as a later option.

### Pause bound

The bound has two parts: the work of a collection, and the time that the
workers need for it. The work has a hard bound. The time has the bound of
the work divided by the workers only under an assumption about the live
graph. This section gives both, and the measured limits that decide the
defaults.

**Work.** A collection costs the sum of these terms over all workers:

| Term | Bound |
| --- | --- |
| Copy of live young objects | `-N` times the `-A` budget |
| Copy of gen1 | The gen1 maximum, `-B` |
| Root scan | The runnable threads and messages of every capability, plus the global roots |
| Stack scan | One chunk for each live thread plus the young chunks, which the `-A` budgets bound |
| Remembered set and dirty cards | `-N` times the barrier budget |
| Mark slice | `-N` times the slice cap |

The barrier budget is new. The current collector scans every remembered
object and every dirty card at a minor collection. A program can dirty
every card of a large old array without one more nursery of allocation.
That work depends on the size of the array and on the mutation, not on
`-A`. The budget bounds it: each capability counts the barrier work it
adds since the last collection, and a capability above the budget requests
a collection at its next safepoint. Refer to the write barrier section.

No term depends on the live data in gen2, on the depth of a stack, or on
the size of a large object. The sum grows with `-N`. A user who needs a
work bound that does not grow with `-N` lowers `-A` and the barrier budget
as `-N` grows. Both are budgets for each capability.

**Time.** The time bound has an assumption: the live young graph has at
least `k` independent parts of similar size, for `k` workers. Under it, the
pause is the work divided by `k` plus the imbalance. Stealing moves full
blocks. A worker that runs out of work waits for the block that another
worker still fills. Thus the imbalance is at most one block of copy work
for each such worker.

This assumption can fail. A linked chain through the young data is a serial
dependency: a worker reaches object `i + 1` only when it scans object `i`.
One worker then copies the chain while the others idle. The worst case is
a chain through the young data of every capability, and its time is the
whole work bound on one worker. This is the same as option 1. GHC has the
same limit. No parallel copying collector removes it.

The root scan has unequal shares too. A capability with many threads has
more thread records to claim. A thread record is a unit of work, so the
imbalance is at most one stack scan.

**Measured limits.** Step 2 of the build order measures the pause against
`-N` on four shapes of young data: independent trees, one chain, many
short lists from one producer, and many threads with small stacks. The
measurements decide the default of `-qn` and whether `-A` gets a default
that shrinks with `-N`.

**Stop latency.** The time before a collection is the stop latency of the
previous section. It is a separate bound, in allocation and not in time,
and the statistics file reports it separately from the collection time.

### Shared allocators

The region table, the gen1 blocks, the gen2 segments, the large object
allocator, the pinned list, and the stack regions get one lock each. A
collection worker takes the region lock only when it needs a new gen1 block
or gen2 segment. The mutator reaches these locks only for a large or pinned
allocation and for a stack region, which are rare.

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
current layout, with the entry store removed.

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
the follow path. Now a thunk info table with bit zero clear goes to the
slow path. A thunk info table with bit zero set loads field zero with a
dependent load and loads the header of that object. A thread record or a
blackhole queue goes to the slow path. Any other object is the value, and
the check continues with it. The cost is one test of the header bit and
one compare of the kind byte on the follow path. The collector still makes
two-word indirections with the indirection info table, which keep their
follow path.

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
  update frames of its stack from the top. For each frame it loads the
  header of the thunk:
  - Thunk: it claims. The claim is a compare-and-swap of the header from
    the thunk info to the thunk info with bits zero and one. On success it
    stores the current thread into field zero and stores the header with
    bit zero alone, with release order. On failure it loads the header
    again and takes the case that it finds.
  - Claiming: another thread is between its swap and its header store. The
    pause spins until the header changes, which takes two stores.
  - Claimed by this thread, or a queue of this thread: an earlier pause
    claimed this frame and every frame below it. The walk stops.
  - Claimed by another thread, or a queue of another thread: this thread
    is a duplicate. It abandons the frames above this one as below and
    drops this frame. Then it sends a blackhole message for the thunk with
    the parent continuation of the frame, and it parks.
  - Evaluated: a duplicate finished first. The thread abandons the frames
    above this one, drops this frame, and continues with the value in the
    parent continuation of the frame.
- **Attach.** The capability of the owner attaches the first waiter. It
  allocates a blackhole queue with the owner thread, the thunk, and the
  waiter. It links the queue into the queue list of the owner thread and
  stores the queue into field zero with a release store. A later waiter is
  appended to the queue by the same capability.
- **Update.** The updater loads the header. When bit zero is clear, it
  stores the value into field zero and the header with bit zero, both with
  release order. When bit zero is set, it loads field zero and stores the
  value with a release store. A queue of the updater is woken. A thread
  record or a queue of another thread means a duplicate: the value stands,
  and the owner recovers its queue as below.
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
the entry path. The barrier fast path is the nursery range test, and a
thunk in the nursery pays nothing more. The update barrier is the one of
today.

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
- The `noDuplicate#` sequences of the next section.

### `noDuplicate#`

Two threads can evaluate one thunk, and both update it with the same
value. `noDuplicate#` makes this impossible for a computation with a side
effect. Its guarantee is the one of GHC: the thunks under evaluation by
the current thread are claimed, and a thread that finds one of them
claimed by another thread does not continue its evaluation.

`noDuplicate#` is a pause. It walks the update frames of the current stack
and claims each thunk with the compare-and-swap of the pause transition.
The walk stops at the first frame that an earlier pause claimed. A frame
whose thunk another thread claimed makes the current thread a duplicate,
and it parks as the pause transition gives. A frame whose thunk is
evaluated makes the current thread continue with the value.

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

This section keeps option 4 for the comparison of step 1. Its states are:

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
section gives. The waker holds the `MVar` lock while it pushes into the
inbox, which the lock order permits.

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
| Field zero of a thunk, owner | The claiming thread, between its swap and its header store | Every reader that saw bit zero | `store.atomic relaxed` before the release store of the header, `load.dependent` after the header load, then `load.dependent` of the kind of the record |
| Field zero of a thunk, queue and value | The capability of the owner, the updater | Every reader that saw bit zero, the recovery walk | `store.atomic release`, `load.dependent` |
| Capability field of a thread record | The capability of the thread | A blocking thread that routes a message | `store.atomic release`, `load.atomic acquire`. A stale value is forwarded by the handler. |
| Fields of a new object | The allocating thread | Nobody until publication | Plain |
| Fields of an immutable object after publication | Nobody | Every reader | Plain, after a `load.dependent` or an acquire load of the pointer |
| Pointer field of a mutable object | `writeMutVar#`, `writeArray#`, the CAS primitives, the runtime | `readMutVar#`, `readArray#`, the collector | `store.atomic release`, `load.dependent` |
| Unboxed field of a mutable array | `writeByteArray#` and the like | `readByteArray#` | Plain. A race is a program error, as in GHC. |
| `atomicReadIntArray#` and the atomic primitives | The primitive | The primitive | `seq_cst` |
| Card byte | The write barrier | A collection worker | `store.atomic relaxed`, `xchg` |
| Capability flags | The ticker, the stop leader | The owner worker | `store.atomic relaxed`, `load.atomic relaxed`. The park is a mutex, which orders the roots. |
| Arena cursor | Every capability | Every capability | `atomic.add relaxed` |
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
  collection time as separate fields.

## RTS options

| Option | Meaning | Default |
| --- | --- | --- |
| `-N<n>` | The number of capabilities. `-N` alone is the number of cores. | 1 |
| `-n<size>` | The nursery block size. | 32 KiB |
| `-C<time>` | The context switch interval. | 20 ms |
| `-R<size>` | The barrier budget of a capability between two collections. | The nursery size |
| `-qn<k>` | The maximum number of workers in a collection. | `-N` |
| `-qg` | The sequential collector instance also with many capabilities. | Off |

`getNumCapabilities` returns `n`. `threadCapability#` returns the capability
of the thread and whether it is pinned. `forkOn#` pins the thread to
capability `k` modulo `n`.

## Cost budget for single-threaded work

The table gives each new fixed cost and our estimate for a program with one
Haskell thread. The measurements of build step 1 replace the estimates.

| Cost | Where | Estimate |
| --- | --- | --- |
| Block refill | One slow path for each 32 KiB of allocation | Below 1 percent |
| Capability load | One load next to each C call that took the machine | Near zero |
| Owner word | One word in each thunk with free variables | 2 to 4 percent on lazy code |
| Entry | The bit store of today disappears | A gain |
| Claim walk | One compare-and-swap and two barriers for each update frame that is live at a pause, once for each frame | Below 1 percent |
| Release stores | `stlr` on ARM64 at update, nothing on AMD64 | Below 1 percent |
| Inline check of an evaluated thunk | One header bit test and one kind compare on the follow path | Below 1 percent |
| Context switch flag | Two loads and one branch for each thunk evaluation | Below 1 percent |
| Barrier budget | One counter add on the cold path of the write barrier | Near zero |
| `MVar` lock | One compare-and-swap pair for each `MVar` operation | Below 1 percent of such programs |
| STM commit lock | One mutex for each transaction, one try-lock for each safepoint inside it | Small |
| Atomic primitives | Real atomic operations for `casMutVar#` and the array atomics | Program dependent, as in GHC |
| Collection with `-N1` | The sequential instance, no change | Zero |
| Collection with idle capabilities | One lock and one worker wake-up for each idle capability for each collection | Measured, see below |
| Idle workers | `n - 1` parked OS threads | Zero CPU |

The sum of the estimates is below 7 percent. The gate is 10 percent against
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

The frequent collection test is the one that decides whether idle workers
join a collection. A wake-up of a parked worker costs several microseconds.
With `-A256k` and one busy thread, a collection runs every few hundred
microseconds, so `-qn` may need a default below `-N` for this case.

## Comparison with GHC

| Topic | GHC threaded RTS | This design |
| --- | --- | --- |
| Build flavours | Non-threaded and threaded runtime libraries. | One runtime library. |
| Default `-N` | 1 | 1 |
| Capability access | `BaseReg`, a pinned register. | A load from the stack chunk header, only next to C calls. |
| Heap check | `Hp` against `HpLim` in memory. | Two registers. |
| Remote interrupt | Store zero to `HpLim`. | A flag read at the block refill and at thunk evaluation. |
| Ticker | Timer thread or signal, 20 ms. | Timer thread, 20 ms, started at the second Haskell thread. |
| Nursery | A block list for each capability, `-n` chunks optional. | One arena, 32 KiB blocks, `-A` budget for each capability. |
| Write barrier | Generation test through the block descriptor, `mut_list` for each capability. | One range test, remembered set for each capability. |
| Minor collection | STW, parallel copy, work stealing. | STW, parallel copy, work stealing, sequential instance for one worker. |
| Old generation | Copying by default, concurrent non-moving optional. | Incremental mark and sweep in STW slices. |
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
spin, so users set `-qg` or `-qn`. This design uses the sequential instance
for `-N1`, parks idle workers without a spin, and keeps the per-capability
`-A` budget, so that effect stays small.

## Build order

1. **Capability split.** Add `AihcCapability` and the chunk header field.
   Move the per-worker fields. Add the arena and the block refill. Add the
   Lir atomic instructions and the access rules. Add the owner word, the
   lazy claims with the pause walk, the queue list of a thread, and the new
   update. Add the barrier budget. Add the update frame rule to the
   collector. Still
   one OS thread and `-N1` only. Measure the cost budget and the three
   entries. This step carries most fixed costs.
2. **Workers and stop-the-world.** Add the workers, the capability locks,
   the sync protocol, the lock order, and `setNumCapabilities`. Add the
   inboxes, the push of threads, the idle wait, the wake-up descriptors,
   and the ticker. Add the locks of the shared allocators. Add the parallel
   copy with the sequential instance, the work stealing, and the parallel
   mark slice. The RTS refuses `-N` above one, and only the collector fuzz
   driver and the pause measurements use more than one capability. This
   step measures the pause against `-N`.
3. **Synchronisation primitives.** Add the `MVar` lock. Add the blackhole
   message protocol, the queue recovery, `noDuplicate#`, and the base
   library change. Add the optimistic STM with the commit lock and the
   validation rules. Add the real atomic primitives. Add the race tests and
   the STM tests.
4. **Parallel execution.** Accept `-N` above one. Run the scheduler stress
   suite and the performance tests. This is the first version that runs
   Haskell threads in parallel.
5. **FFI.** Add `safe` calls that release the capability, the suspended
   frame lists, the worker pool, the deadlock rule, and callbacks from
   foreign OS threads.
6. **Later.** Bound threads, blocking callbacks, sparks, and concurrent
   marking slices.

Each step leaves the compiler shippable and is its own PR series. Steps 2
and 3 do not expose parallel execution, so no intermediate version runs
Haskell threads in parallel without the protocols that they need.

## Tests

- The collector fuzz driver plays compiled code for several threads today.
  It gains several capabilities and OS workers, the thunk states of the
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
  run with `-N2` as well.

## Open points

- The nursery block size. The measurement of step 1 decides between 16 KiB,
  32 KiB, and 64 KiB.
- The context switch at the thunk branch. If the measurements show that
  programs need a finer switch, the outlined reservation continuation is
  the fallback.
- The blackhole protocol. Step 1 measures the owner word against the
  budget. The ownerless alternative replaces it only above three percent
  and only with its race tests.
- The number of collection workers when few capabilities are busy. The
  frequent collection test decides whether `-qn` gets a default below
  `-N`, and the pause measurements decide whether `-A` shrinks with `-N`.
