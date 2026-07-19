# GRIN heap snapshot tests

GRIN snapshot fixtures execute the same hand-written program with the reference
interpreter and both native backends: Apple AArch64 and Linux AMD64. Each
fixture independently asserts the returned values and the reachable abstract
GRIN heap. Native execution runs in the test suite matching the host platform.

Fixtures shared by both native backends live under
`test/Test/Fixtures/grin-snapshot`:

```yaml
entry: $entry
program: |
  constructor I32#/1 [Int32Rep]

  $entry -> BoxedRep Lifted =
    store (CI32# (1 :: Int32Rep))
return: "@0"
heap: |
  @0 = CI32# 1
status: pass
reason: a stored constructor is returned as a location
```

Locations are numbered by traversal from the returned values, not by allocator
address. This makes native and interpreted snapshots comparable and preserves
sharing and cycles:

```text
return: @0
heap:
  @0 = CLoop @0
```

Stored nodes are the reachable heap objects themselves. When evaluation updates
a thunk, that same object becomes an explicit indirection:

```text
return: @0
heap:
  @0 = CPair @1 @2
  @1 = Indirection @2
  @2 = CI32# 7
```

Snapshotting reads heap objects but never enters them. A suspended thunk remains
`F$function`, and a closure remains `P$function/arity`. Only an explicit
`GrinEval` executed by the fixture may evaluate a thunk. Native code pointers
are recovered through descriptor tables generated beside the assembly, so
snapshot output does not depend on debug symbols or raw addresses.

Fixtures may replace `return` and `heap` with an `error` expectation. These
cases assert the same stable runtime diagnostic from the interpreter and native
execution. For example, a thunk that re-enters itself while it is marked as a
blackhole uses:

```yaml
error: blackholed thunk re-entered
```

The focused fixture parser currently accepts constructor and function
declarations plus `constant`, `store`, `store-rec`, `eval`, `apply`,
`call`, and inline binds. Variables and literals carry explicit runtime
representations.
