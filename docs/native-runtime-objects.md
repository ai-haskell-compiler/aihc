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
logical arity, pointer bitmap, next application-stage table, and an optional
native apply entry. Application changes the header to the statically known next
table. Consequently every managed object pays only for its tagged header and
payload; arity, tracing metadata, and apply code consume no per-object shape
word.

The native backends give saturated closure stages a generated apply entry.
Apply sites place the closure, continuation, and supplied values in the AIHC
register convention and branch through that info-table entry. The stage stub
loads captured fields directly from the closure, moves the supplied values into
their final argument registers, and tail-branches to the target function's
register entry. Non-saturating closures, partial constructors, and invalid
applications leave the apply entry empty and use the shared C slow path.

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

The cooperative scheduler keeps thread records, blackhole records, and wait
queues in auxiliary C allocations. Suspended threads retain ordinary action or
continuation closures, which are expanded into one machine-owned argument area
only when the scheduler selects them. The machine likewise reuses one locals
area because CPS transfers discard the preceding function frame. The current
argument area's static info table and trailing-pointer count let the semispace
collector relocate its roots precisely.
