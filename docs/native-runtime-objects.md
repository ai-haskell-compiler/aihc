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
It excludes the second space, unused capacity, auxiliary runtime allocations, and static objects.

Static reference tables determine static object liveness for every collection.

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
  Auxiliary runtime allocations remain outside this count.
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
The header contains an info-table address.
Most info tables are static.
A blackholed thunk uses the embedded info table in its managed scheduler record.

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
Each blackhole record also contains a copy of the original thunk info table.

The Lir lowering gives saturated closure stages an apply entry, the
`backend_entry` of the info table. Apply sites pass the machine, the closure,
the continuation, and the supplied values in the `aihc` convention and
tail-call that entry. The entry loads captured fields directly from the
closure, takes the supplied values as parameters, and tail-calls the target
function. A stage whose fields and supplied values are all pointers shares
one of the runtime's enter functions, which reaches the target through the
identity field of the table; any other stage gets a generated stub.
Non-saturating closures, partial constructors, and invalid applications
leave the apply entry empty and use the shared C slow path.

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

The cooperative scheduler keeps pending IO requests in auxiliary C allocations. Suspended threads retain
ordinary action or continuation closures. The scheduler hands a selected thread
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
MVar waiters, blackhole owners, blackhole waiters, and pending IO requests retain their threads.
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
Each active blackhole retains both ends of its waiter queue.
Thunk update and exception paths remove the blackhole from the active list and wake its waiters.
The waiters then become reclaimable without direct memory release.
Their memory counts toward managed allocation statistics and the `-M` limit.

`aihc_lir_eval` follows indirections before it selects a branch.
A ready value requires no reservation or update frame.
The thunk branch reserves seventeen slots: three for its update frame and fourteen for a blackhole record.
The blackhole branch reserves fourteen slots, which also cover a waiter on every target.
Both branches protect the resolved value and the continuation as roots across collection.
The thunk branch reloads the info table after collection and stores the resolved thunk in its update frame.
`aihc_begin_blackhole` and `aihc_block_on_blackhole` consume the reserved space without collection.
The shared update continuation completes the blackhole update, then calls evaluation with the result and parent continuation.
It allocates no frame itself.
Blackhole records use the managed heap and have a distinct object kind.
The machine retains the active record list.
The collector traces each record through its C layout, including both list links and both waiter queue ends.
It also traces the thunk and its owner thread.
The original info table remains static.

A blackholed thunk header contains a pointer to the info table inside its record.
The collector relocates this interior pointer when it scans the thunk.
This rule applies to both static and heap thunks.
It preserves the embedded info table, captured fields, and static reference table through collection.
The record needs no pinned address because its references remain under runtime control.
Thunk update and exception paths remove the record from the active list.
The collector reclaims unreachable records and includes their bytes in allocation statistics and heap limits.

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

## STM wait requests

`stmWaitRequest#` reserves seventeen slots in GRIN GC.
The runtime consumes this reservation without collection.
The bound includes the request and two pinned metadata slots.
The collector owns request storage and includes its charge in heap limits and statistics.

The request has an `Addr#` result, so the machine registers it as an explicit root.
The registration starts before the primitive returns.
It remains active before await, during suspension, and after completion.
Result consumption removes the registration.
The next collection can then reclaim the request.
The caller must consume each completed request exactly once.
An unconsumed request retains its registration and counts toward the heap limit.

The collector traces the saved thread and continuation through the request header.
The root list and the pinned allocation list have separate purposes.
The pinned allocation list does not retain requests.
Host IO requests still use the old allocation path until the next conversion.

## IO manager

The runtime ABI separates operation submission, scheduler suspension, and
result consumption:

1. An ordinary foreign call allocates an opaque request in the `submitted`
   state without blocking.
2. `awaitIO#` asks the configured backend to make progress. Immediate
   completions continue directly; otherwise the request becomes `pending` and
   retains the current green thread and continuation.
3. Backend polling changes a ready request to `completed` and enqueues its
   thread. A final ordinary foreign call takes the result, changes the request
   to `consumed`, and releases it.

Backend workers or readiness mechanisms produce only native completion data;
Haskell continuations are always reconstructed and enqueued on the scheduler
thread. This prevents moving-heap pointers from escaping to an asynchronous
backend. Pending requests are collector roots only for their saved continuation
and thread resume record. The opaque request pointer itself has `Addr#`
representation and is not traced as a Haskell heap pointer.

IO operations target opaque runtime-owned handles rather than OS descriptor
numbers. Standard input and output are the first preopened handles, while each
backend owns their platform representation. The POSIX backend stores a file
descriptor in each handle, sets it nonblocking, and uses `poll` when buffer
reads or writes report that they would block. Windows can instead store `HANDLE` or
`SOCKET` resources without exposing either representation to generated code.

Reads and writes operate on an offset and length within a pinned `MutableByteArray#` payload.
The collector owns and can reclaim these arrays.
The caller must retain the array through submission, suspension, completion, and result consumption.
`keepAlive#` can protect that complete action.
Pending requests retain their saved continuations, which retain active keep-alive frames.
The request's raw buffer pointer is not a GC root.
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
