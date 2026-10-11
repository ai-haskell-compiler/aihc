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
| Garbage collection | STW. One capability collects. The other capabilities park. Parallel copy later. | STW parallel copy by default. |
| Old generation | Incremental marking in STW slices, as today. | Optional concurrent non-moving collector. |
| Blackholing | Eager, with a plain store. No owner in the thunk. Waiters go to a global table. | Lazy by default. The owner is in the thunk. |
| Thunk layout | Unchanged. No padding word. | One padding word in each thunk. |
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
- The selected resumption, the foreign frames, and the callback frames.
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
with a lock that other capabilities push threads into. The worker moves the
inbox into its run queue at each schedule.

### Thread movement

We considered work stealing, where an idle capability takes threads from
the queue of a busy one. Stealing makes every run queue operation of the
owner publication-safe, which costs atomics on the single-threaded path.

Decision: the busy capability pushes. This is the GHC model. At each
schedule, a capability with two or more runnable threads reads the global
count of idle capabilities. When the count is above zero, it takes idle
capabilities from the idle list under the machine lock. It moves half of its
surplus threads into their inboxes and wakes them. A capability with one
runnable thread pays one load and one compare.

A thread that `forkOn#` pinned stays on its capability. A woken thread goes
to the run queue of the capability that wakes it, unless it is pinned. Then
it goes to the inbox of its capability.

### Idle capabilities

A capability without runnable threads polls its pending IO requests. When
it has none, it waits on its condition variable. A push into its inbox, a
stop request, or a shutdown wakes it. The last capability to become idle
checks for pending IO requests and active timers. When there are none, it
reports a deadlock, as the current RTS does.

### Workers

Each capability has one worker at start. A `safe` foreign call releases the
capability, and another worker takes it. Refer to the FFI section. The
number of workers can therefore be above `n`.

## Stop-the-world synchronisation

A capability that needs a collection or a change of the capability count
requests a stop:

1. It takes the machine sync lock. A second requester waits on this lock,
   and it checks afterwards whether its request still holds.
2. It sets `stop_requested` on every capability and writes to the wake-up
   descriptor of each idle capability.
3. It takes the lock of each idle or released capability. Such a capability
   is already at a safepoint.
4. It waits until each running capability parks at its next safepoint.
5. It runs the collection.
6. It releases every capability and the sync lock. The parked workers
   return from their safepoint call and load their context again.

The latency of step 4 is the time until each running capability reaches a
safepoint. The block size of the nursery bounds this time for code that
allocates.

## Garbage collection

### Options

1. **STW, one collector.** The requester collects alone. The other
   capabilities park. The collector keeps its property that it uses no
   atomic operations. A collection copies the live objects of every
   capability, so the pause grows with the number of busy capabilities.
2. **STW, parallel copy.** Each worker copies the roots of its capability
   and the blocks it allocated. The workers steal scan work. Forwarding a
   copied object needs a compare-and-swap on its header. This is the GHC
   default.
3. **Thread-local collection.** Each capability collects its own nursery
   without a stop. This needs a promotion of each object at the point where
   another thread can see it. It changes the GRIN safepoint model and the
   write barrier. We rejected it.

### Decision

We use option 1 first and option 2 later, after a measurement. A single
Haskell thread on `-N8` produces the same live data as on `-N1`, so the pause
of option 1 does not grow for it. The idle capabilities cost one lock
acquisition each.

Option 2 gets a sequential fast path. The collector copies without atomic
operations when one worker collects. The copy loop is one function that a
macro parameter instantiates twice, so the sequential path pays nothing.

### Roots

The roots of a collection are the roots of every capability and the global
roots:

- For each capability: the current thread, the run queue, the inbox, the
  selected resumption, the foreign frames, the callback frames, and the
  spilled roots of its parked safepoint.
- Global: the globals, the blackhole table, the stable names, the pinned
  list, the timers, the callback slots, and the RTS roots.

### Remembered set and mark buffer

The write barrier appends to the remembered set of the capability. The
deletion barrier appends to the mark buffer of the capability. Neither needs
an atomic operation. A collection takes the lists of every capability.

A card of a large boxed array is one byte. Two capabilities can set the same
card at the same time. Both write the same value, so the race is benign.

### Incremental marking of gen2

The marking slices run in STW steps, at the end of each collection, as
today. A full mark buffer requests a STW step that runs one slice and does
not copy. A slice runs on one worker, so the marker keeps its property that
it uses no atomic operations.

We considered slices on a mutator worker while the other workers run. The
snapshot-at-the-beginning invariant holds under concurrency, but the marker
then reads frames that another worker pops. We keep this as a later option.

### Shared allocators

The region table, the gen1 blocks, the gen2 segments, the large object
allocator, the pinned list, and the stack regions get one lock each. The
collector runs in STW and takes none of them. The mutator reaches them only
for a large or pinned allocation and for a stack region, which are rare.

The fixed charges, the `-M` limit test, and the allocation statistics use
one atomic add for each large or pinned allocation. The movable allocation
statistics stay in the capability.

## Blackholing

A thunk entry sets bit zero of the header. The runtime keeps the original
info table and payload. An update writes the result into field zero and the
indirection info table into the header. A thread that enters a thunk under
evaluation becomes a waiter. The waiters of a thunk are in a hash table.

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

1. **A padding word in each thunk.** This is the GHC model. The update
   writes the result into the padding word, and the blackholing writes the
   owner thread there. No free variable is ever overwritten. A waiter sends
   a message to the capability of the owner, which attaches the waiter and
   updates on one worker. No atomic operation is necessary. The cost is one
   word in each thunk with one or more free variables.
2. **Compare-and-swap at thunk entry.** The bit store becomes a
   compare-and-swap. The entry then sees an update that landed before it.
   This costs about twenty cycles on every thunk entry. We rejected it.
3. **A lazy blackhole.** The collector and the context switch write the
   blackholes by a walk of the update frames. This saves the store at entry.
   It does not solve the update race, and it loses the early detection of a
   loop. We rejected it.
4. **Ownerless blackholes.** Keep the layout. The thunk entry loads its free
   variables, loads the header again, compares it with its own info table,
   and only then sets the bit. A waiter registers in the global table and
   runs a process-wide barrier. The update stores the result, stores the
   header with release semantics, and loads one byte of a filter array.

### Decision

We use option 4. The reasons are the memory cost of option 1 and the
compatibility of option 4 with the current layout. Option 1 is the fallback
if the measurements of option 4 show a cost above one percent.

#### Thunk entry

`aihc_lir_eval` pushes the update frame and transfers to the thunk entry. It
no longer sets the bit. The generated entry does these steps:

1. Load the free variables into registers.
2. Load the header again.
3. Compare it with the info table of the thunk. The address is a constant.
4. If the header differs, pop the update frame and transfer to
   `aihc_lir_eval` with the parent continuation. The thunk is then under
   evaluation or updated.
5. Store the header with bit zero set.
6. Run the body.

The free variables are loaded before the bit store. An update that lands
between step 3 and step 5 overwrites field zero after the load. The entry
then evaluates with correct free variables, and its own update later writes
its result into field zero. Between the two, the thunk reads as under
evaluation by this thread, and readers wait for it. Both results are the
same value, because the thunk is pure.

The cost is one load, one compare, and one predictable branch for each
thunk entry.

A thunk that `aihc_lir_eval_single_entry` enters runs the same steps. Its
bit store is harmless, because no other evaluation reaches the thunk.

#### Update

The update stores the result into field zero, then stores the indirection
header. The header store has release semantics. On AMD64 it is a plain
store. On ARM64 it is `stlr`. Then the update loads one byte of the global
filter array at the hash of the thunk address. When the byte is zero, the
update is complete. When it is not zero, the update takes the C path, which
locks the table, looks the thunk up, and wakes its waiters.

The filter array has 64 Ki bytes. A byte is the number of waiter entries
whose thunk hashes to it, saturated. A waiter on one thunk slows only the
updates of thunks with the same hash, which is one in 65536.

#### Waiting

`aihc_block_on_blackhole` does these steps:

1. Lock the table. Insert a waiter entry in the state `registered`.
   Increment the filter byte. Unlock.
2. Run the process barrier. Refer to the next section.
3. Load the header again.
4. If the header is an indirection: lock the table. If the entry is still
   `registered`, remove it, decrement the byte, unlock, and continue with
   the value. If the entry is `woken`, unlock and continue with the value.
5. Otherwise: lock the table. If the entry is `woken`, unlock and continue.
   Else set it to `parked`, unlock, and enter the scheduler.

The update on the C path sets a `registered` entry to `woken` and enqueues
the thread of a `parked` entry. Thus no thread is enqueued while it runs.

Self-re-entry detection walks the chain of the current thread, as today.
The waiter bit, bit one of the header, is no longer used. The forwarding
pattern of the collector, low bits `10`, stays unique.

#### The process barrier

The update stores the header and then loads the filter byte. The waiter
stores the filter byte and then loads the header. Without a fence on both
sides, both loads can miss the store of the other side. The update is hot,
so it gets no fence. The waiter is rare, so it runs a process-wide barrier.
This is the asymmetric form of the Dekker protocol.

| Host | Barrier |
| --- | --- |
| Linux | `membarrier(MEMBARRIER_CMD_PRIVATE_EXPEDITED)` |
| Other POSIX | A signal to each worker. The handler runs a full fence and increments an acknowledgement count. The waiter spins on the count. |
| Windows | `FlushProcessWriteBuffers` |
| wasm32 | Nothing. One capability. |

Each worker has a barrier point during the call. After the call, every
store that preceded the barrier point of its worker is visible. Every load
that follows the barrier point of its worker sees the filter byte. The case
analysis in the design review showed no lost wake-up.

#### Duplicate evaluation and `noDuplicate#`

Two threads can evaluate one thunk at the same time. Both update it with the
same value. `noDuplicate#` makes this impossible for a thunk with a side
effect. For each update frame on its stack, the thread locks the table and
claims the thunk with an entry that names it as the owner. When another
thread owns the thunk, the thread becomes a waiter on that thunk. It pops
its frames down to the update frame of the thunk and parks. The owner wakes
it with the value. This is the GHC model.

#### Abandoned evaluation

An asynchronous exception can stop an evaluation. The current RTS puts the
original header back and lets the waiters evaluate again. With option 4,
field zero can hold the result of a racing update instead of the first free
variable. The thunk can then not be evaluated again from its payload.

Decision: an abandoned evaluation freezes the frames between the update
frame and the raise point into a heap object, as `control0#` copies frames
today. The thunk becomes an indirection to a resumption thunk that pushes
these frames again when it is entered. This is the `AP_STACK` model of GHC.
It keeps the work that the evaluation did. Asynchronous exceptions are not
in the current RTS, so this change lands with them.

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

A wake moves the woken thread to the run queue of the capability that
wakes it. A pinned thread goes to the inbox of its own capability instead.

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

A transaction record holds a read log and a write log. `readTVar#` first
looks in the write log, then in the read log, then loads the `TVar` and
records the value. `writeTVar#` appends to the write log. Nested
transactions for `catchRetry#` keep a log of their own and merge it into the
parent at their commit, as today.

The commit validates by value. For each read entry, the current value of the
`TVar` must be the recorded value. The log keeps the recorded value alive,
so an address cannot be reused while it is recorded.

`retry#` validates under the commit lock. An invalid transaction runs again
at once. A valid one registers its thread as a watcher on each `TVar` of its
read log, releases the lock, and parks. A commit that writes a `TVar` wakes
every watcher of it and removes their entries.

Delay variables keep their timer list. The list is global under the commit
lock. The capability that polls a timer applies its expiry as a transaction
that writes the variable.

In sequential mode, validation always succeeds, and the lock is
uncontended. One mutex acquisition is the cost of a transaction.

## Memory model

Haskell code with a data race stays type safe. A pointer that a thread loads
from a shared field names an object whose fields are complete. The rules
are:

- A pointer store into a shared mutable object has release semantics. The
  sites are `writeMutVar#`, `writeArray#`, `writeSmallArray#`, the
  compare-and-swap primitives, the thunk update, and every runtime store
  into an `MVar`, a `TVar`, or a thread record.
- A reader loads a pointer and then loads the fields of the object that it
  names. This is an address dependency, which AMD64 and ARM64 order.
- The inline evaluation check loads the header of an object and then field
  zero of the same object. This has no address dependency. The lowering
  adds one: the field address is computed from the header value with an
  `and` against zero. The native ARM64 backend keeps this dependency. The
  LLVM backend emits an acquire load for the header instead, because LLVM
  gives no guarantee for a dependency.
- `atomicReadIntArray#`, `atomicWriteIntArray#`, `fetchAddIntArray#`,
  `casIntArray#`, and `casMutVar#` are sequentially consistent atomic
  operations. The lowering emits them as plain operations today.
- A word-sized aligned load or store is single-copy atomic on every target.
  The header of a thunk is written by its evaluator and read by every
  reader with plain operations under this rule.

Lir gets the instructions `load.acquire`, `store.release`, `cas`, `xchg`,
`atomic.add`, and `fence`. The wasm32 target lowers them to plain
operations, because a module without shared memory runs one thread.

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
capability that polls the request. That capability enqueues it. The
`threadDelay` timers stay with the capability that set them, with the same
rule.

On wasm32 the host driver pumps the one capability, as today.

## Foreign function interface

### `unsafe` and `safe` calls

An `unsafe` call keeps the capability. A long `unsafe` call blocks the
capability and every stop request, as in GHC.

A `safe` call releases the capability before the call and takes a
capability back after it:

1. The runtime records the Haskell thread as in a foreign call. Its roots
   are the roots of its foreign frame, as today.
2. The worker releases the capability. When the capability has runnable
   threads, an idle worker takes it, or the runtime starts a worker.
3. The worker makes the C call.
4. After the call, the worker takes a free capability. It prefers the
   capability that it released. It waits when none is free.
5. The thread continues on that capability. The chunk header of its stack
   pointer names the new capability.

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
- The blackhole table is global under one lock. Only contention reaches it.
- The stable name list is global under one lock. The hash counter is
  atomic.
- The callback slots are global under one lock.
- Stack chunks come from the spare list of the capability. A capability
  with more than 64 spare chunks returns regions to the machine.
- The statistics file sums the counters of every capability. It gains the
  capability count and the number of workers.

## RTS options

| Option | Meaning | Default |
| --- | --- | --- |
| `-N<n>` | The number of capabilities. `-N` alone is the number of cores. | 1 |
| `-n<size>` | The nursery block size. | 32 KiB |
| `-C<time>` | The context switch interval. | 20 ms |
| `-qg` | Sequential collection also with many capabilities. Applies once parallel collection exists. | Off |

`getNumCapabilities` returns `n`. `threadCapability#` returns the capability
that last ran the thread and whether it is pinned. `forkOn#` pins the thread
to capability `k` modulo `n`.

## Cost budget for single-threaded work

The table gives each new fixed cost and our estimate for a program with one
Haskell thread. The measurements of build step 1 replace the estimates.

| Cost | Where | Estimate |
| --- | --- | --- |
| Block refill | One slow path for each 32 KiB of allocation | Below 1 percent |
| Capability load | One load next to each C call that took the machine | Near zero |
| Thunk entry check | One load, one compare, one branch for each thunk entry | Below 1 percent |
| Update filter | One load for each thunk update, plus `stlr` on ARM64 | Below 1 percent |
| Context switch flag | Two loads and one branch for each thunk evaluation | Below 1 percent |
| `MVar` lock | One compare-and-swap pair for each `MVar` operation | Below 1 percent of such programs |
| STM commit lock | One mutex for each transaction | Small |
| Atomic primitives | Real atomic operations for `casMutVar#` and the array atomics | Program dependent, as in GHC |
| Collection with `-N1` | No change | Zero |
| Collection with idle capabilities | One lock for each idle capability for each collection | Near zero |
| Idle workers | `n - 1` parked OS threads | Zero CPU |

The sum of the estimates is below 5 percent. The gate is 10 percent against
the RTS before step 1. The workloads are the `gc-*` examples and the compile
workloads of the benchmark suite. Each runs at `-N1` and at `-N<cores>`.

Sequential mode gives a switch for C paths that measure above budget: a
C path tests `machine->capability_count == 1` and skips its lock. Compiled
code has no such switch. Its hot paths need no atomic operation by design.

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
| Minor collection | STW, parallel copy, work stealing. | STW, one worker first, parallel copy later. |
| Old generation | Copying by default, concurrent non-moving optional. | Incremental mark and sweep in STW slices. |
| Thunk header | Two words. | One word. |
| Blackholing | Lazy by default, eager optional, owner in the thunk. | Eager, no owner, filter byte and global table. |
| Blackhole waiting | A message to the capability of the owner. | A table entry, a process barrier, and a header reload. |
| `noDuplicate#` | Claims each update frame at `threadPaused`. | Claims each update frame in the table. |
| Abandoned evaluation | `AP_STACK` object. | `AP_STACK` object, with asynchronous exceptions. |
| `MVar` | Lock on the info pointer with `WHITEHOLE`. | Lock byte in the record. |
| `STM` | Optimistic, per-`TVar` locks at commit, watch queues. | Optimistic, one commit lock, watch queues. |
| IO manager | Haskell event manager threads, `epoll`, `kqueue`, timer manager. | Poll in each capability, timers in each capability. |
| `safe` FFI | Releases the capability, worker pool. | Same. |
| Callbacks | New bound thread, nested scheduler, can block. | On the caller stack, cannot block. |
| Bound threads | `forkOS`, bound main thread. | Later. |
| Sparks | Spark pool deque for each capability. | Later. |
| Thread movement | Push to idle capabilities. | Push to idle capabilities. |
| Deadlock | `BlockedIndefinitelyOnMVar` after a collection. | Failure when every capability is idle, as today. |
| Memory model | Release and acquire barriers since GHC 9.2. | Release stores at publication, dependency loads. |
| Non-allocating loops | Not preemptible without `-fno-omit-yields`. | Not preemptible. Loops that allocate but do not evaluate are not preemptible either. |

The GHC threaded RTS costs a few percent on single-threaded programs. The
parallel collector can cost much more on them, so users set `-qg` or `-qn`.
This design keeps the sequential collector and the per-capability `-A`
budget to avoid that effect.

## Build order

1. **Capability split.** Add `AihcCapability` and the chunk header field.
   Move the per-worker fields. Add the arena and the block refill. Add the
   Lir atomic instructions and the release stores. Change the thunk entry
   and the update. Still one OS thread and `-N1` only. Measure the cost
   budget. This step carries most fixed costs.
2. **Workers and stop-the-world.** Add the workers, the capability locks,
   the sync protocol, `-N`, and `setNumCapabilities`. Add the inboxes, the
   push of threads, the idle wait, the wake-up descriptors, and the ticker.
   Add the locks of the shared allocators. The collector runs on one worker.
3. **Synchronisation primitives.** Add the `MVar` lock. Add the blackhole
   table protocol with the process barrier and `noDuplicate#`. Add the
   optimistic STM with the commit lock. Add the real atomic primitives.
4. **FFI.** Add `safe` calls that release the capability, the worker pool,
   and callbacks from foreign OS threads.
5. **Parallel copy.** Measure the pause with busy capabilities. Add the
   parallel copy with the sequential fast path when the measurement asks
   for it.
6. **Later.** Bound threads, blocking callbacks, sparks, and concurrent
   marking slices.

Each step leaves the compiler shippable and is its own PR series.

## Tests

- The collector fuzz driver plays compiled code for several threads today.
  It gains several capabilities and OS workers, and it runs under the
  thread sanitizer when the C compiler supports it.
- A scheduler stress suite under `examples/concurrency-*`. The programs
  are: `MVar` ping-pong between capabilities, a shared lazy list that `n`
  threads consume, and an STM counter with `n` writers. The suite also has
  `noDuplicate#` with `n` evaluators of one `unsafePerformIO` thunk, and
  `safe` calls that block while other threads run.
- The cost budget gate: the `gc-*` examples and the compile workloads at
  `-N1` and `-N<cores>` with one Haskell thread.
- The GRIN `gc-stress` fixtures and the verifier under `AIHC_GC_VERIFY`
  run with `-N2` as well.

## Open points

- The nursery block size. The measurement of step 1 decides between 16 KiB,
  32 KiB, and 64 KiB.
- The context switch at the thunk branch. If the measurements show that
  programs need a finer switch, the outlined reservation continuation is
  the fallback.
- The parallel copy. Step 5 decides it from the pause measurement.
- The process barrier on hosts without `membarrier`. The signal fallback
  needs a measurement of its latency with many workers.
