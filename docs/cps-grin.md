# CPS-GRIN

AIHC represents continuations with ordinary GRIN values instead of adding a
second intermediate language. The compilation pipeline is:

```text
Haskell -> System FC -> GRIN -> CPS-GRIN -> GC-GRIN -> native backend
```

`CPS-GRIN` is GRIN satisfying an additional invariant. Every continuation
which was the body of a source `GrinBind` has been closure-converted into a
generated `GrinFunction`. The transformation stores a closure for that
function with `GrinStore` and invokes it with `GrinApply` when the preceding
expression produces its values.

For example, the shape

```text
%x <- operation
rest %captured %x
```

becomes the equivalent of

```text
%k <- store (P$cps$parent$0/1 %captured)
%x <- operation
apply %k %x

$cps$parent$0 %captured %x =
  rest %captured %x
```

The generated closure captures exactly the variables which are bound at the
original bind and free in its body. Generated names and capture order are
deterministic. The administrative binds needed to receive results from
`GrinStore` and the preceding operation remain GRIN sequencing; they are not
user continuations and are not recursively closure-converted.

## Evaluation

The runtime creates an update frame only when evaluation enters a thunk.
`GrinCpsEval` therefore takes a value and one continuation after its representation argument.
The runtime follows indirections, returns ready values, and suspends on blackholes.
The update frame stores the parent continuation and the resolved thunk.
The update continuation completes the blackhole update before it evaluates the result.

For a non-tail `eval`, CPS also emits `GrinIfWhnf` before normal continuation allocation.
This operation tests the object kind without allocation or suspension.
It sends thunks, indirections, and blackholes to its slow branch.
The other object kinds are already in weak-head normal form.
The generated code has this shape:

```text
if-whnf value
  call after_eval parent captures... value
else
  continuation <- store after_eval_frame(parent, captures...)
  cps-eval value continuation
```

Both branches use the same generated continuation function.
The ready branch passes captures directly and creates no heap frame.
The slow branch stores captures in the normal continuation frame.
The GC pass keeps heap reservations inside these branches.
It protects the value and captured pointers before the slow branch allocates its frame.
The runtime protects the value and continuation before it allocates an update frame or blackhole waiter.

## Boundary and invariants

`toCpsGrin` is the only constructor for `CpsGrinProgram`; its data constructor
is private. Backends accept `CpsGrinProgram`, not `GrinProgram`, so normal API
use cannot bypass the pass or accidentally apply it twice. Both the main unit
and every cached dependency SCC cross this boundary before native code
generation.

Throw and catch nodes are rejected at this boundary. Exception control must be
lowered to ordinary GRIN control flow before CPS conversion. This keeps
CPS-GRIN's continuation model singular instead of retaining an implicit second
exception continuation.

The ordinary `.grin` artifact remains the direct-style input to the pass. It
is the lowered program after `Aihc.Grin.Simplify`, which resolves what a body
statically knows about its heap objects: an `eval` of a constructor or closure
node is the pointer itself, an `apply` of a known closure is a direct call or a
larger closure, a `case` on a known constructor takes its alternative, and a
`store-rec` group keeps only its cycles. When
`--keep-grin` is used, AIHC also writes `.cps.grin` and `.gc.grin`. The latter
is the exact program consumed by the native backend: each managed `store` is
preceded by `ensure-heap`, whose live pointer operands are returned under fresh
SSA names, and is then represented by `store-unchecked`. A recursive store
group receives one reservation covering the complete group.

The native runtime uses a semispace collector, a stop-the-world copying
collector. Static constructor and function info tables describe object identity,
populated pointer fields, the next application stage, and the tracing layout the
collector walks. Application advances the header to the next static info table,
so heap objects need only a single tagged info-table pointer before their
payload fields.

## Cooperative scheduling

`fork#`, `yield#`, and the operation-independent `awaitIO#` are CPS primitive
calls. Concrete IO operations are ordinary foreign calls which submit opaque
runtime requests over stable pinned `MutableByteArray#` slices and later consume
their results.
Consequently, adding a file, socket, timer, or process operation does not
require a new compiler primitive.

Suspended computations remain ordinary continuation closures. Runnable and
blackhole-blocked threads retain those closure values in runtime resume
records; pending IO requests retain the blocked thread and continuation until
the backend reports completion. These ordinary heap pointers are collector
roots, so scheduling does not introduce a native stack-scanning convention.
Each request also retains its unmanaged buffer allocation and slice. That
allocation cannot move while a backend owns its address and is not a collector
root.

The central scheduler drains ready requests before selecting a runnable thread
and blocks in the configured IO backend only when requests are pending and no
green thread can run. The first native backend uses POSIX `poll`; the request
boundary is intended to admit io-uring, IOCP, kqueue, and WASI implementations
without changing Haskell code, System FC, or CPS-GRIN.

Native saturated applications bypass that area: the closure's application-stage
info table selects generated code which loads captured fields and supplied
values into backend argument registers before tail-entering the function.
The WebAssembly adapter grows and reuses a machine-owned argument vector
instead of requiring a whole-program arity bound or allocating a vector for
every transfer.
