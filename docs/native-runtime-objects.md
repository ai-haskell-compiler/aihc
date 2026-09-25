# Native runtime objects

The Apple ARM64 and Linux AMD64 backends share the runtime ABI, C runtime,
constructor/global link layout, and snapshot support from `aihc-native`. Both
consume Lir (see `docs/lir.md`); only instruction selection and object
emission belong to the architecture packages.

Both backends are built on every platform. `aihc compile` defaults to the host
target on supported hosts and accepts an explicit target for cross-compilation:

```text
aihc compile Main.hs --target apple-arm64
aihc compile Main.hs --target linux-amd64
```

The selected LLVM target triple is passed to Clang for dependency objects, the
shared runtime, and the final executable. Cross-linking therefore requires a
Clang installation with the corresponding target linker and sysroot.

## RTS options

Compiled programs accept RTS options between `+RTS` and `-RTS` arguments.
The runtime removes these arguments before `getArgs` reads the argument vector.
Use `--RTS` to stop RTS option processing for all subsequent arguments.

The `-M<size>` option sets the maximum managed heap size in bytes.
The size accepts an optional `K`, `M`, `G`, or `T` binary unit.
Lower-case units have the same meaning.
The default heap size is unlimited.

The semispace collector starts with a small space and grows it on demand.
After each collection, the target capacity doubles until it holds twice the live data.
The `-M` limit caps the shared budget for movable objects and pinned blocks.
Pinned block charges include their allocation metadata and alignment padding.
The collector stops when live data and the next reservation exceed this budget.
It excludes the second space, unused capacity, collector metadata, and static objects.

Static reference tables determine static object liveness for every collection.

## Initialization and host scopes

One static machine controls one program heap.
The host initializes this heap before it imports arguments or environment data.
Startup uses explicit root scopes for temporary buffers.
After these scopes end, the runtime copies retained data into movable arrays.
These arrays contain only the retained bytes.
The collector then removes temporary imports.
The machine then applies the parsed heap limit and starts the main thread.
After this transition, the runtime rejects further startup imports.
Retained arguments and environment data count toward this limit.
A limit below the retained startup data causes failure before user code starts.

The machine holds the global table in a managed pinned byte array.
The collector traces each global table slot.
Argument replacement installs an immutable `ByteArray#` as a process root.
It does not allocate or copy the array.
The collector updates this root when the array moves and reclaims previous arrays when they become unreachable.

Normal runtime primitives consume reservations from GRIN safepoints.
They cannot collect inside their C calls.
The scheduler and startup can also establish explicit host safepoints with `AihcRootFrame`.
Each frame publishes all live managed C references before a host call can collect.
The collector updates these reference slots after relocation.
The runtime must reload relocated references from their slots.

`aihc_host_byte_array` requires an active root scope and uses the same heap and statistics as program allocation.
Its pinned buffers hold raw data for host interfaces that need fixed addresses.
Paths and POSIX poll arrays use call scopes.
WASI canonical ABI buffers use a scope that remains active until the asynchronous operation ends.
Generated buffer destructors defer memory reclamation until that scope ends.
Resource destructors still close their host resources.
The next collection can reclaim buffers after their scope ends.
No allocation occurs before GC initialization or outside a reservation or explicit host safepoint.

The collector alone obtains backing storage and metadata from the C allocator.
The runtime has no second heap or general auxiliary allocator.
The thread stacks below are the only other storage, and the collector file owns them too.

## Thread stacks

Continuation frames are not in the managed heap.
Each thread has a stack, and the frames of the thread are on this stack.
A stack is a doubly linked list of chunks.
Each chunk has 4096 bytes and the same alignment.
Thus the chunk of an address is the address with the low 12 bits cleared.
A chunk starts with a 32-byte header: the owner stack and the chunks below and above.
The frames of a chunk follow the header.

The stack pointer is the first free byte of the stack of the running thread.
The stack limit is the end of the chunk that holds the byte before the stack pointer.
Compiled code keeps both values in registers and passes them to each function that it transfers control to.
Refer to "Lowering from GC-GRIN" in [lir.md](lir.md).
A push writes the frame at the stack pointer and increases it.
The frame fits when the new stack pointer is not above the stack limit.
If the frame does not fit, `aihc_stack_grow` puts it at the start of the next chunk.
Then the stack limit is the end of that chunk.
A new chunk comes from the C allocator, so a push never collects.
A frame keeps the address of its parent in field zero, in any chunk.
Thus a chunk boundary needs no link frame.
The largest frame has 256 words, so a frame always fits in an empty chunk.

A continue helper sets the stack pointer to the address of the frame that it enters.
This pops the frame and every frame above it.
The stack limit becomes the end of the chunk of that frame.
An application resume sets the stack pointer to the first byte after its continuation.
These two rules are correct because code always pushes a frame directly above its current continuation.
The current continuation is thus always the topmost live frame.

The machine field `stack_next` holds a copy of the stack pointer.
Compiled code stores the stack pointer there before a runtime call that can read it, such as a collection or a scheduler operation.
The scheduler writes `stack_next` when it selects a thread, and `aihc_lir_resume` loads the stack pointer from there.
The runtime pushes the frames of the list below through `stack_next` while no compiled code runs.
The machine fields `heap_next` and `heap_limit` are the same kind of copy of the heap pointer and the heap limit.

The frame layouts do not change, because each frame has its info table.
The collector finds a live frame through a pointer to it.
It marks the frame as it marks a static object and scans the frame in place.
Frames do not move, so a pointer to a frame needs no relocation.
A stack stays while the collector retains its thread record.
After each collection, the collector releases the stacks of the threads that it did not retain.
It also releases the chunks above the chunk after the running chunk.
When a thread finishes, the runtime releases its stack immediately.
The machine keeps up to 64 released chunks for later growth.

Stack chunks are not in the heap statistics or in the `-M` limit.

The runtime puts these frames on the stack as well:

- The update frame that `aihc_lir_eval` pushes when it enters a thunk.
- The final and top continuations of the main thread, and the thread-done frame at the bottom of each forked thread.
- The stop frame of a foreign callback. The callback runs on the stack of the thread that made the foreign call.
- The frames that a `control0#` resume pushes again.

`control0#` copies the frames between the top and the prompt to the managed heap.
The copies link from the top down, and the lowest copy has a null parent.
A resume pushes new copies of these frames on the stack from the bottom up.
It never writes the heap copies, so a captured continuation can resume any number of times.

## Runtime statistics

Set the environment variable `AIHC_RTS_STATS` to a file path to get the
runtime statistics of a program. When the program exits normally, the runtime
writes one JSON object to that file:

```json
{"schema": 1, "peak_heap_bytes": 0, "allocated_bytes": 0, "gc_count": 0, "gc_time_ns": 0}
```

A normal exit is a return from `main` or an `exitWith` call. A runtime failure
writes no file. An empty value counts as an unset variable.

- `peak_heap_bytes` records the maximum occupied space for movable objects and pinned blocks.
  The runtime samples this count before collection and at exit.
- `allocated_bytes` counts actual movable allocations and complete pinned block charges.
  Unused reservations and collector copies do not increase this count.
  Host buffers use the same count and heap budget.
- `gc_count` is the number of collections.
- `gc_time_ns` is the monotonic time the collections took, in nanoseconds.

The environment parser lives in `aihc_runtime_options.lir` next to the RTS
option parser. The POSIX host flattens `environ` into one buffer of
`NAME=VALUE` strings, and the C runtime reads the path through
`aihc_rts_stats_path`.

The `wasm32-wasip3` target does not implement the hook yet. The P3 driver
reads no environment, every file write on that host is one asynchronous
stream that the driver pumps. A
`wasm32-wasip3` program therefore never writes the file, even when the
runner passes `--env AIHC_RTS_STATS=<path>` and `--dir` to wasmtime.

STM delay variables use `wasi:clocks/monotonic-clock@0.3.0` on this target.
The runtime submits a timer request for the earliest deadline.
`awaitIO#` preserves the continuation while `wait-until` waits on the host.
The WASI callback completes the request and resumes the continuation.
The runtime then updates expired delay variables before the transaction starts again.
Timer variables remain garbage collection roots throughout the wait.

Native heap objects have an eight-byte header followed by payload slots.
The header contains an info-table address and two low tag bits.
Info tables have at least four-byte alignment on every target.
Bit zero marks a thunk under evaluation. Bit one marks a thunk with blocked waiters.
Header readers mask both bits before they read the info table.

```text
saturated constructor: [header] [fields...]
thunk:                 [header] [environment...]
partial application:   [header] [fields...]
indirection:           [header] [target]
blackhole:             [header] [environment / reserved target...]
```

Each info table records the object's identity, populated field count, remaining
logical arity, pointer bitmap, next application-stage table, an optional native
apply entry, and the static reference table of the object's code.
Application changes the header to the statically known next table.
Ordinary objects share this static metadata.
A thunk under evaluation retains its original info table and payload.

The Lir lowering gives each closure stage with a remaining arity from one to
four an apply entry, the `backend_entry` of the info table. The entry takes
the values of all remaining argument groups of the stage. Apply sites pass
the machine, the context of the thread, the closure, the continuation, and
the supplied values in the `aihc` convention and tail-call that entry. The entry loads captured fields
directly from the closure, takes the supplied values as parameters, and
tail-calls the target function. A stage whose fields and supplied values are
all pointers shares one of the runtime's enter functions, which reaches the
target through the identity field of the table; any other stage gets a
generated stub.

One application supplies from one to four argument groups, as in the GHC
eval/apply model. The apply helper for the shape of the groups compares the
remaining arity of a closure with the group count:

- The arity is equal to the group count: the helper tail-calls the entry
  with all the values.
- The arity `k` is less than the group count: the helper pushes a normal
  continuation frame on the thread stack. The frame holds the parent
  continuation and the other groups. Then it tail-calls the
  entry with the first `k` groups and the frame as the continuation. The
  frame applies the result to the groups it holds.
- The arity is more than the group count, or the function is a partial
  constructor: the shared C slow path builds one
  partial application with all the supplied values.

Primitive operations have no heap-object tag. A partially applied primitive is
lowered to an ordinary closure whose generated entry makes the saturated
primitive call.

Every updateable object reserves at least two words. Evaluating a thunk changes
its header to `BLACKHOLE`, executes the entry encoded by its old header, then
changes the same object to `INDIRECTION` and writes the returned heap pointer
into its first payload word. There is no separate cell allocation.

Exceptions have no native heap tag or object representation. They are removed
before native runtime lowering. The final physical tag is the semispace
collector's temporary forwarding marker.

The semispace collector does not copy heap indirections. When it forwards a
pointer to an indirection, it follows the chain and stores the final target.
The new space therefore holds no indirection after a collection. Static
indirections stay in place, because static objects do not move. The collector
forwards their targets instead.

The collector has a fuzz test in `Test.Native.GcFuzz`. The test generates
random scripts that build heaps through the runtime interface, change them,
and force collections. A C driver runs each script against the runtime and
reports the new space, the roots, and the static objects after every
collection. A model of the same script predicts the report. The scripts cover
constructors, closures, thunks, partial applications, arrays, indirection
chains, cycles, blackholes, static objects, reference tables, and every root
source the collector visits. The driver process stays alive across cases, so
the test can compile the driver with sanitizers when the C compiler supports
them.

The cooperative scheduler keeps pending IO requests in managed pinned objects. Suspended threads retain
ordinary action closures or pointers to continuation frames on their stacks. The scheduler hands a selected thread
back to generated code as a resume record, which the Lir resume helper
dispatches with a tail call. All retained closure values and pending-request
continuations are precise collector roots.
Generated code exposes live values to the collector through explicit root slots.

STM transactions, write logs, and timers use the managed heap.
Each record has a header and a distinct runtime object kind.
The collector obtains each size from its C structure.
It traces pointer fields through that structure on both 32-bit and 64-bit targets.
The current transaction of each live thread retains its parent transactions and write logs.
The machine timer list retains timer variables and final values until expiry.
Commit, abort, and expiry remove references without direct memory release.
These records count toward managed allocation statistics and the `-M` limit.

The GRIN primitive description gives fixed allocation bounds for ordinary calls and CPS calls.
The GC stage inserts `ensure-heap` before these calls.
The reservation protects call arguments and values needed after the call.
The existing GC transformation gives relocated roots fresh names.
Lir translates the explicit reservation and the call.

`stmBegin#` reserves three heap slots, and `writeTVar#` reserves four.
`newDelayTVar#` reserves eight slots for its TVar and optional timer.
`newPromptTag#` reserves one slot.
`makeStableName#` reserves four slots.
`newMVar#` and `fork#` each reserve nine slots.
`readMVar#`, `takeMVar#`, and `putMVar#` each reserve five slots for one optional waiter.
CPS call reservations protect the continuation and all pointer arguments.
Each slot has eight bytes on every target.
C size assertions check that runtime records fit these bounds.
Reservations can exceed actual allocation, which statistics measure separately.

These primitives consume reserved heap and must not collect.
Their callees must preserve this contract.

Managed heap allocation does not clear memory in release builds.
Each object initializer must write every field before collection can trace the object.
When the C preprocessor symbol `DEBUG` is defined, `aihc_gc_allocate` clears its allocation for diagnostics.
Runtime records explicitly initialize null links, empty queues, and inactive state in both builds.
`aihc_gc_allocate` checks the available space and cannot collect.
The delay-variable helper initializes its TVar and timer before any further collection.

GRIN snapshot fixtures request GC stress checks with `gc-stress: true`.
The test harness changes generated Lir to select the collector path at each reservation.
It identifies collector blocks by their calls to `aihc_heap_collect`.
This transformation exists only in test code.
Successful stress fixtures must report at least one collection.
They can specify heap limits through `rts-arguments`.

Thread records use the managed heap, including the initial thread.
Machine initialization reserves the initial record after GC initialization.
The record has the `AIHC_OBJECT_THREAD` kind.
The collector obtains its size and pointer fields from the C structure.
It traces the resume function, continuation, pointer value, transaction, and run queue link.

The machine retains the current thread and both ends of the run queue.
MVar waiters, blackhole waiters, and pending IO requests retain their threads.
The collector relocates each of these references.
An unreachable thread becomes reclaimable after it leaves these runtime queues and other live references.
Thread records count toward managed allocation statistics and the `-M` limit.
Snapshot fixtures reset managed allocation statistics after initialization, so their totals exclude the initial thread.
Process statistics include the initial thread.

Each thread record has a unique number.
The machine assigns number one to the initial thread and increments the number for each new thread.
Collection preserves this number even when it changes the record address.
The number remains at offset eight on every target.
`aihcThreadIdNumber#` reads it with one load.
`myThreadId#` reads the current thread from the machine.

Blackhole waiters use the managed heap.
Each waiter has a header, thread, continuation, and queue link.
The collector traces these fields through the C layout.
A machine-local hash table maps each contended thunk to both ends of its waiter queue.
The first contention allocates the table with C allocation.
The table grows as necessary and retains its capacity for reuse.
These arrays are runtime metadata, outside managed allocation statistics and the `-M` limit.
Waiter records count toward both statistics and the limit.
Ordinary thunk evaluation allocates no table entry or blackhole record.

`aihc_lir_eval` follows indirections before it selects a branch.
A ready value requires no reservation or update frame.
The thunk branch pushes a three-slot update frame on the thread stack.
This push does not collect, so the thunk branch needs no reservation.
The blackhole branch reserves four slots, which cover a waiter on every target.
It protects the resolved value and the continuation as roots across collection.
The thunk branch stores the parent continuation and resolved thunk in the update frame.
It sets the evaluation bit and transfers to the thunk entry.
The original info table and payload remain intact, including across suspension.
No owner record is necessary.

`aihc_block_on_blackhole` walks the current continuation chain to detect self-re-entry.
An update frame for the same thunk proves self-re-entry.
Otherwise, it inserts a waiter into the hash table and sets the waiter bit.
The runtime consumes the reserved waiter space without collection.
The scheduler suspends that thread until an update or exception wakes it.

The shared update continuation saves the tag bits and updates the thunk to an indirection.
If the waiter bit was set, it removes the table entry and wakes its waiters.
An uncontended update performs no table lookup.
The continuation then evaluates the result with the parent continuation.
Exception unwinding clears both bits and raises the exception in each waiter.
The restored thunk can be evaluated again.
No collection or scheduler switch occurs between a header change and removal of its waiter entry.

`aihc_lir_eval_single_entry` enters a thunk with no update frame, evaluation bit, or reservation.
The compiler uses it only for a thunk that no other evaluation can reach.
The thunk is not updated, and the collector can reclaim it as soon as its entry has loaded its fields.
A value with the evaluation bit, an indirection, or a blackhole goes to `aihc_lir_eval`.

The collector masks the header tags and traces the original thunk layout and static reference table.
An update continuation retains its thunk while evaluation is in progress.
The waiter table also retains contended thunks and their waiter queues.
Collection relocates the table keys and queue ends, then rebuilds the hash table in a reusable spare array.
This rule applies to both static and heap thunks.
The scheduler does not scan the table to find completed thunks.

`MVar#` uses a managed empty/full cell with separate FIFO queues for
blocked readers, takers, and putters.
A put into an empty cell wakes all blocked readers with the same value.
It gives the value to the first taker, or leaves the cell full.
A take from a full cell returns its old value.
If a putter is blocked, the take installs that putter's value and wakes that putter.

MVars and waiters have distinct object kinds and C layout visitors.
A live MVar retains its value and all queue heads and tails.
Each waiter retains its continuation, put value, queue link, and thread record.
The machine has no list that retains every MVar.
The collector can reclaim unreachable MVars, waiters, and their values, including cycles.
A wake removes the waiter from its queue without direct memory release.
MVars and waiters count toward managed allocation statistics and the `-M` limit.

## Static objects

Static objects occupy an object-file data section and never move.
An evaluated CAF contains an indirection into the managed heap.
The collector retains its target only while the CAF remains reachable.

Each info table contains a static reference table (SRT).
The SRT names static objects that the code can reach directly.
It also names the tables of called functions and the entry functions of objects that the code can create.
These entries retain the required CAF values before those objects exist.
This includes thunks, partial applications, and continuation closures.
The collector follows these tables from active code and live objects.
It also follows pointer fields in live objects.

A compiled function passes its table to the collector at each of its safepoints.
After CPS conversion, each call is a tail call.
The active function has no heap object to carry its table, and nothing else records it.
A collection inside a runtime helper passes no table: the helper is reached by a tail call, or by a call whose only continuation is a transfer to heap objects, so the calling function's static references are dead.
Suspended code uses a continuation closure with a table in its info table.

No section and no table lists the static objects. The collector finds them by
address: a pointer that is outside both spaces of the managed heap names an
object that never moves. Each collection records the addresses it marks in a
hash set and scans each object once through its info table, so an evaluated
CAF gets its target forwarded like any heap field. A nullary constructor has
no fields, so marking it does nothing.

Every object that compiled code can store in a pointer field carries an info
table. Byte arrays have the `AIHC_OBJECT_BYTE_ARRAY` kind.

Stable names use four managed slots on every target: header, weak referent, hash, and weak lookup link.
Their info tables have the `AIHC_OBJECT_STABLE_NAME` kind.
The GRIN GC stage reserves their memory and protects the referent and caller values before `makeStableName#`.
The Lir runtime unit consumes this reservation without collection.

The machine lookup list does not retain names.
A name does not retain its referent or the next name in the list.
After strong tracing, the collector rebuilds the lookup list from live names with live referents.
It updates moved referents and follows heap indirections to live targets.
It clears dead referents and removes dead entries from the lookup list.
This phase does not allocate memory or start further tracing.

A live name keeps its hash even if its referent dies.
Pointer equality between live names remains valid because collection relocates every strong reference to each name.
Dead names and referents become reclaimable, and name records count toward allocation statistics and heap limits.
The lifetime rule follows [System.Mem.StableName](https://downloads.haskell.org/~ghc/latest/docs/libraries/base-4.22.0.0-66f8/System-Mem-StableName.html).

## Byte arrays and pinned storage

A byte array has six eight-byte descriptor slots followed by its payload.
The slots contain the header, current size, contents address, pinned flag, alignment, and allocation size.
An empty array still has one payload byte.
The allocation size remains unchanged after shrink, so the collector can traverse the complete object.

`newByteArray#` uses the movable heap.
After relocation, the collector repairs the contents address to point after the descriptor.
Pinned allocations contain their descriptor and payload in one separate block.
The payload preserves the requested power-of-two alignment.
Two metadata slots precede the object and contain the list link and complete charge.
The metadata address is the allocation base.

The GRIN GC stage calls a size helper before each allocation or resize.
The helper checks the size and alignment and computes the complete reservation in slots.
The existing reservation transformation protects the source array and other live pointers.
The Lir allocation functions consume this reservation without collection.
Resize copies into a new array and preserves the pinned flag and alignment.

`aihc_gc_allocate_pinned` consumes the same budget as movable allocation.
It reduces the available space by the complete block charge and updates allocation statistics.
The machine records physical space capacity separately from the available space limit.
The pinned allocation list does not retain its objects.
Strong tracing marks pinned objects through ordinary roots and object fields.
After weak stable-name processing, the collector releases unmarked pinned blocks.
Their charges then become available to later reservations.

An `Addr#` does not retain an array.
A pinned address remains stable only while its array remains live.
Use `keepAlive#` around an action that uses the address, including the complete asynchronous IO operation.
A final `touch#` also keeps its operand live through earlier GC reservations on that control path.
These lifetime requirements follow the [GHC byte-array contract](https://downloads.haskell.org/~ghc/latest/docs/libraries/ghc-internal-9.1401.0-555c/src/GHC.Internal.Prim.html).

GRIN preserves `keepAlive#` as a lifetime scope through simplification.
CPS conversion gives each pointer owner a three-slot continuation frame.
GRIN GC reserves these frames and relocates their arguments.
The collector traces their parent and owner fields.
Continuation dispatch passes through these frames without a result-layout conversion.
This supports abstract result representations and results with multiple registers.
Exception unwinding treats them as ordinary frames.

Foreign operand conversion evaluates other operands before it obtains byte-array payload addresses.
Thus, a collection during operand evaluation cannot invalidate an extracted movable address.
The path and argument-buffer helpers retain their owners while raw addresses are in use.

## Request allocation and roots

`stmWaitRequest#`, `submitIORead#`, and `submitIOWrite#` reserve sixteen slots in GRIN GC.
`submitIOOpen#` reserves twenty-one slots, which include five slots for its result handle.
`adoptIOHandle#` reserves five slots for a handle.
The runtime consumes these reservations without collection.
The request bound includes two pinned metadata slots.
Requests remain pinned because host calls retain C request addresses across host safepoints.
Handles can move and use `IOHandle#` references.
The GC reclaims handle storage.
Programs must still close open host resources.
Standard handles are static objects with the same header.
Open errors reside in the result handle, so a result never uses an integer as a traced pointer.

`IORequest#` references retain requests before await and after completion.
Only pending operations have scheduler roots.
A request traces its handle, buffer owner, thread, continuation, and next pending request.
Completion removes the scheduler root.
An unreachable completed request can then be reclaimed, even when its result was not consumed.
Result consumption checks the completed state and prevents a second consumption.
The pinned allocation list does not retain objects.

## IO manager

The runtime ABI separates operation submission, scheduler suspension, and
result consumption:

1. A submission primitive consumes reserved memory for an opaque request in the `submitted` state.
2. `awaitIO#` asks the configured backend to make progress. Immediate
   completions continue directly; otherwise the request becomes `pending` and
   retains the current green thread and continuation.
3. Backend polling changes a ready request to `completed` and enqueues its
   thread. A primitive takes the result and changes the request to `consumed`.

Backend workers or readiness mechanisms produce only native completion data;
Haskell continuations are always reconstructed and enqueued on the scheduler
thread. This prevents moving-heap pointers from escaping to an asynchronous
backend. Managed `IORequest#` references supply Haskell heap roots.

IO operations target opaque runtime-owned handles rather than OS descriptor
numbers. Standard input and output are the first preopened handles, while each
backend owns their platform representation. The POSIX backend stores a file
descriptor in each handle, sets it nonblocking, and uses `poll` when buffer
reads or writes report that they would block. Windows can instead store `HANDLE` or
`SOCKET` resources without exposing either representation to generated code.

Reads and writes operate on an offset and length within a pinned `MutableByteArray#` payload.
The collector owns and can reclaim these arrays.
The caller must retain the array through the submission reservation.
A final `touch#` after submission can establish this lifetime.
Submission finds the pinned owner from the payload address and checks the requested slice against its bounds.
The request then retains that owner until result consumption.
Interior payload addresses also retain their owner.
A request rejects a buffer address in the movable heap.
External buffers remain the caller's responsibility.
Use `keepAlive#` around the complete action when a separate Haskell owner controls external storage.
Handles use the same collector and heap budget.
Callers must not access the submitted slice while the request is pending.

`copyAddrToByteArray#` copies an explicit byte count into a checked destination slice.
It does not search for a terminating zero.
The source address must remain valid until the synchronous copy returns.

A non-negative request result is the number of transferred bytes. A non-empty
read returns zero at end-of-file. Either operation can return fewer bytes than
requested, so the future `Handle` layer must resubmit the remaining slice when
it requires a complete transfer. Errors use `-(errno + 1)` in the POSIX proof
of concept. `GHC.IO.Runtime` owns the runtime bindings and generic
suspension (`awaitIO`). `GHC.IO.StdHandles`
exposes buffer allocation, address and indexed byte access, handle operations,
and the standard handles. Text encoding, locking, transfer loops, and full
`Handle` semantics remain above this boundary.
