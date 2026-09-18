# Type checker structure and API

This analysis uses commit `eeb0fe548` as its base.
The changes keep the type checker algorithms and interface representation.

## Findings and changes

| Area | Initial structure | Change |
| --- | --- | --- |
| Package API | All 49 modules were public. | Expose 20 modules. Keep 32 implementation modules private. |
| Entry point | `Aihc.Tc` contained interface data, diagnostic attachment, and checker control. | Move interface data to `Aihc.Tc.Interface`. Move diagnostic attachment to private `Aihc.Tc.Diagnostics`. |
| Term identity | `TcTermKey` required the checker monad module. | Define term identities in `Aihc.Tc.Types`. Define `patSynKey` beside `PatSynInfo` in `Aihc.Tc.Env`. |
| Interface data | `Aihc.Tc.Share` imported the checker entry point. | Import the interface, annotation, environment, and type modules directly. |
| Type matcher | FC imported `Aihc.Tc.Solve.Dict` for one pure function. | Define `matchTypes` in `Aihc.Tc.Match`. Both FC and the solver use this module. |
| Merge API | `unionTcInterfaces` duplicated the trusted merge implementation. | Use `mergeTcInterfaces TrustMergedFacts` at each caller. |
| Result API | No function returned the public `TcResult` type. | Remove this unused type. Keep annotated modules as the result. |

`Aihc.Tc` still exports the interface types and operations for existing callers.
Clients that need only interface data can import `Aihc.Tc.Interface` directly.
The matcher substitutes pattern variables in checked types.
It does not infer Haskell types, solve constraints, or reduce type families.

The public module count includes modules that the test support package imports.
These include `Monad`, `Kind`, `Zonk`, `Generate.Bind`, `Generate.Pattern`, and `Deriving.StockClass`.
Thus, this change reduces the API but does not fully isolate checker state.
The test support package needs a separate API before these modules can become private.

## Performance properties

Interfaces retain strict maps with global identity keys.
Map union and difference can share unchanged subtrees.
The entry point still adopts imported maps where the state representation permits this.
The trusted merge still uses `Map.union` without structural equality checks.
The checked merge still rejects unequal facts with the same identity.
List views remain available for serialization and output.

These changes remove duplicate code and dependencies.
They do not establish a run-time speed increase.

Three paths merit measurement before further changes:

- Diagnostic attachment traverses the AST once for each located diagnostic.
  A span index could reduce this cost on modules with many errors.
  It must preserve diagnostic order and the exact source node.
- Instance insertion scans the instance list for duplicate identities.
  Interface import and export also rebuild instance indexes.
  An identity map could reduce these costs but must preserve instance search order.
- Type scheme extraction appends binders to a list.
  An accumulator could reduce list allocation for deeply nested `forall` types.

## Further structural work

`Generate.Decl` has 5,356 lines at the base commit.
It contains module control, declaration registration, kind completion, instance checks, and annotation extraction.
Separate these phases with explicit inputs and outputs before further API changes.
Keep Haskell type checks in `aihc-tc`.
Keep name resolution in `aihc-resolve` and System FC conversion in the FC component.

`Monad` has 1,179 lines at the base commit.
Most clients already use its operations instead of direct state access.
The remaining direct state clients are `Aihc.Tc`, `Generate.Decl`, `Kind`, `Zonk`, and `Solve.Family`.
Small operations for state snapshots and finalization can reduce these dependencies.
Do not replace the state representation without allocation measurements.

## Compatibility and checks

External callers of newly private modules must use the public API.
Replace `unionTcInterfaces` with `mergeTcInterfaces TrustMergedFacts`.
Replace solver imports of `matchTypes` with `Aihc.Tc.Match`.
Import term identity operations from `Aihc.Tc.Types` and `patSynKey` from `Aihc.Tc.Env`.
The interface data fields and serialized values remain the same.

The base type checker suite passes all 610 tests.
The existing fixtures cover module imports, import cycles, type families, and diagnostic source locations.
The full project checks also cover FC and compiler clients.
This refactor adds no tests and changes no progress counts.
