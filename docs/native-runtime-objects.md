# Native runtime objects

The Apple ARM64 and Linux AMD64 backends share the runtime ABI, C runtime,
constructor/global link layout, and snapshot support from `aihc-native`. Only
instruction selection, register allocation, and assembly emission belong to
the architecture packages.

Both backends are built on every platform. `aihc compile` defaults to the host
target on supported hosts and accepts an explicit target for cross-compilation:

```text
aihc compile Main.hs --target apple-arm64
aihc compile Main.hs --target linux-amd64
```

The selected LLVM target triple is passed to Clang for dependency objects, the
shared runtime, and the final executable. Cross-linking therefore requires a
Clang installation with the corresponding target linker and sysroot.

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
logical arity, pointer bitmap, and next application-stage table. Application
changes the header to the statically known next table. Consequently every
managed object pays only for its tagged header and payload; arity and tracing
metadata consume no per-object shape word.

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

The cooperative scheduler keeps thread records, argument vectors, blackhole
records, wait queues, and pending IO requests in auxiliary C allocations.
Suspended argument vectors carry their static info table plus a count of
trailing continuation pointers, so the semispace collector can relocate roots
held by runnable and blocked threads as precisely as roots in the currently
executing function.

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
and thread arguments. The opaque request pointer itself has `Addr#`
representation and is not traced as a Haskell heap pointer.

IO operations target opaque runtime-owned handles rather than OS descriptor
numbers. Standard input and output are the first preopened handles, while each
backend owns their platform representation. The POSIX backend stores a file
descriptor in each handle, sets it nonblocking, and uses `poll` when byte reads
or writes report that they would block. Windows can instead store `HANDLE` or
`SOCKET` resources without exposing either representation to generated code.

The read result is a byte from 0 through 255, `-1` for end-of-file, or
`-(errno + 1)` for an error. A write returns zero on success and the same
negative error encoding on failure. `GHC.Event` owns only generic suspension;
`GHC.IO.StdHandles` is the first client and exposes generic handle operations
plus standard-stream convenience wrappers. Buffering, encoding, locking, and
full `Handle` semantics remain deliberately above this boundary.
