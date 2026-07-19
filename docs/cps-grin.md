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

The ordinary `.grin` artifact remains the direct-style input to the pass. When
`--keep-grin` is used, AIHC also writes `.cps.grin` and `.gc.grin`. The latter
is the exact program consumed by the native backend: each managed `store` is
preceded by `ensure-heap`, whose live pointer operands are returned under fresh
SSA names, and is then represented by `store-unchecked`. A recursive store
group receives one reservation covering the complete group.

The native runtime is compiled with either `--gc calloc` (the default dummy
collector, which never frees nodes) or `--gc semispace` (a stop-the-world
copying collector). Static constructor and function info tables describe object
identity, populated pointer fields, and the next application stage to both
collectors; only the semispace collector uses the tracing layout. Application
advances the header to the next static info table, so heap objects need only a
single tagged info-table pointer before their payload fields.

## Cooperative scheduling

`fork#` and `yield#` are CPS primitive calls. The runtime stores their ordinary
continuation closures and argument vectors in auxiliary thread records, then
resumes them through the same apply/entry mechanism as other continuations.
Runnable and blackhole-blocked thread arguments are collector roots; their
static layouts let a moving collector update them without introducing a native
stack-scanning convention.

This first representation is intentionally allocation-heavy. Optimizations
such as eliminating immediately applied continuations belong after the
suspension semantics are established, where they can prove that a continuation
does not escape.
