# GRIN heap snapshot tests

GRIN snapshot fixtures execute the same hand-written program with the reference
interpreter and the native AArch64 backend. Each fixture independently asserts
the returned values and the reachable abstract GRIN heap.

Fixtures live under
`components/aihc-arm64/test/Test/Fixtures/snapshot`:

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

Snapshotting reads heap cells but never enters them. A suspended thunk remains
`F$function`, and a closure remains `P$function/arity`. Only an explicit
`GrinEval` executed by the fixture may evaluate a thunk. Native code pointers
are recovered through descriptor tables generated beside the assembly, so
snapshot output does not depend on debug symbols or raw addresses.

The focused fixture parser currently accepts constructor and function
declarations plus `constant`, `store`, `store-rec`, `eval`, `apply`,
`call`, and inline binds. Variables and literals carry explicit runtime
representations.
