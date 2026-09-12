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
After each collection, the target capacity doubles until it holds twice the
live data. The `-M` limit caps the capacity of one space. The collector stops
the program when the live data and the pending reservation do not fit in that
capacity. It does not count the second space, auxiliary runtime allocations, or
static objects.

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

- `peak_heap_bytes` is the most bytes the current semispace ever held. That is
  the live data after a collection plus the allocations since it, sampled
  before each collection and at exit. The value is comparable to the
  `max_live_bytes` field of the GHC runtime.
- `allocated_bytes` counts every byte reserved on the managed heap. Compiled
  code bumps the heap pointer itself within a reservation, so a reservation
  that a branch does not use in full is still counted. Auxiliary runtime
  allocations, such as byte arrays, are not counted.
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

Native heap objects use a one-word tagged header followed by shape-specific
payload words. The low three header bits are the physical tag. The remaining
bits point to an aligned, statically emitted info table.

```text
saturated constructor: [header] [fields...]
thunk:                 [header] [environment...]
partial application:   [header] [fields...]
indirection:           [header] [target]
blackhole:             [header] [environment / reserved target...]
```

Each info table records the object's identity, populated field count, remaining
logical arity, pointer bitmap, next application-stage table, an optional native
apply entry, and the static reference table of the object's code. Application changes the header to the statically known next
table. Consequently every managed object pays only for its tagged header and
payload; arity, tracing metadata, and apply code consume no per-object shape
word.

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

The cooperative scheduler keeps thread records, blackhole records, wait queues,
and pending IO requests in auxiliary C allocations. Suspended threads retain
ordinary action or continuation closures. The scheduler hands a selected thread
back to generated code as a resume record, which the Lir resume helper
dispatches with a tail call. All retained closure values and pending-request
continuations are precise collector roots; live values of generated code reach
the collector only through the root vector of an explicit safepoint.

`MVar#` uses a runtime-owned empty/full cell with separate FIFO queues for
blocked readers, takers, and putters. Putting into an empty cell wakes every
blocked reader with the same value and either hands the value directly to the
oldest taker or leaves the cell full. Taking from a full cell returns its old
value and, when a putter is waiting, installs that putter's value before waking
it. This direct handoff prevents a newly running thread from overtaking an
already blocked operation. Stored values, queued put values, continuations, and
their suspended threads are collector roots. The cells themselves are
auxiliary allocations owned for the machine's lifetime; weak pointers and
finalization are intentionally outside the initial interface.

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

Compiled functions publish their table in `aihc_current_srt` at entry.
After CPS conversion, each call is a tail call.
The active function has no heap object to carry its table.
A collection can occur at its safepoints or inside a runtime helper.
Suspended code uses a continuation closure with a table in its info table.

No section and no table lists the static objects. The collector finds them by
address: a pointer that is outside both spaces of the managed heap names an
object that never moves. Each collection records the addresses it marks in a
hash set and scans each object once through its info table, so an evaluated
CAF gets its target forwarded like any heap field. A nullary constructor has
no fields, so marking it does nothing.

Every object that compiled code can store in a pointer field carries an info
table. The byte arrays, MVars, stable names, and threads that the runtime
allocates outside the heap therefore also start with a header. Their info
tables have the kind `AIHC_OBJECT_RUNTIME` or `AIHC_OBJECT_THREAD`, and the
collector scans nothing behind them.

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

Reads and writes operate on an offset and length within the payload of a pinned
`MutableByteArray#`. The proof-of-concept runtime allocates each byte array
outside the Haskell heap and does not release it. A request retains that stable
allocation through completion. Callers must not access the submitted slice
while the request is pending. A future garbage collector can own the same
descriptor and payload layout without changing `awaitIO#` or the backend
request model.

`copyAddrToByteArray#` copies an explicit number of bytes from an `Addr#` into a
bounds-checked destination slice. It does not scan for a terminating zero. The
source address must remain valid until the synchronous copy returns; only the
stable byte-array payload is retained by later asynchronous requests.

A non-negative request result is the number of transferred bytes. A non-empty
read returns zero at end-of-file. Either operation can return fewer bytes than
requested, so the future `Handle` layer must resubmit the remaining slice when
it requires a complete transfer. Errors use `-(errno + 1)` in the POSIX proof
of concept. `GHC.IO.Runtime` owns the runtime bindings and generic
suspension (`awaitIO`). `GHC.IO.StdHandles`
exposes buffer allocation, address and indexed byte access, handle operations,
and the standard handles. Text encoding, locking, transfer loops, and full
`Handle` semantics remain above this boundary.
