# Resolver structure analysis

## Component boundaries

`aihc-resolve` assigns identities, resolves names, and selects operator fixities.
It produces surface syntax with annotations and diagnostics.
The type checker and FC compiler consume these results.
The refactor keeps these component boundaries.

| Module | Responsibility | Assessment |
| --- | --- | --- |
| `Aihc.Resolve` | Public API and recursive syntax resolution | Large, but expression, pattern, declaration, and type resolution depend on each other. |
| `Scope` | Scope maps, declaration collection, imports, and exports | Several responsibilities share one module. A later split can separate declaration collection from module interfaces. |
| `Monad` | Scope environment, local identities, and diagnostics | A small internal interface centralizes state changes and annotation creation. |
| `Types` | Public identities, annotations, and results | Shared types give downstream components the facts that they need. |
| `Span` | Source span calculations | Separate from scope and identity rules. |
| `Traverse` | Typed annotation traversal | Exhaustive cases make syntax changes visible to the compiler. |
| `Generic` | Generic syntax traversal | Useful for callers that need more than annotations. It excludes large leaf values. |
| `Infix` | Infix chains, fixity validation, and tree construction | New internal module. It has no resolver state or diagnostic effects. |

## Selected changes

The previous infix representation used separate operand and operator lists.
Several functions used `zip`, which could silently discard unmatched elements.
Other functions needed an empty-list error or a fallback expression.

`InfixChain` contains a first operand and a list of operator/operand pairs.
Its constructor is internal to `Infix`.
Operand traversal and operator traversal preserve these pairs.
Expression and pattern resolution use the same chain functions.
The chain type removes the empty-list errors and the unmatched-list cases.

The previous collection functions appended one element to each list at each infix node.
This required quadratic list work for a long left-nested chain.
The new function prepends pairs to an accumulator in one pass.
It keeps parentheses and annotations as operand boundaries.

The previous ambiguity check compared operator pairs and scanned the chain between each pair.
A chain of equal-precedence operators could require cubic work.
The new check uses a stack of active operators.
Each operator enters and leaves the stack at most once.
The check therefore takes linear time.

The check retains the conflict with the first left operator in source order.
This preserves the previous diagnostic location when a chain has more than one conflict.
Operand resolution still occurs before operator resolution.
An ambiguous chain still receives a left-nested tree and one ambiguity annotation.

## Public API assessment

The refactor does not change the public API.
The compiler driver already calculates module exports once and reuses them for resolution and downstream interfaces.
A new convenience API must preserve this reuse.
A wrapper that calculates exports again would add cost.

`Scope` exposes its record fields.
The interface serializer and builtin-scope code use those fields.
An opaque replacement would require coordinated caller changes and a supported serialization API.
This analysis found no need for that change in the infix refactor.

## Further work

Declaration collection and module interfaces are possible future module boundaries within `Scope`.
Named fields could replace the seven positional fields in `DeclExports`.
Both changes could improve readability without a public API change.

The export fixed point repeats declaration collection.
However, imported record fields affect the names that top-level wildcard patterns declare.
A cache must distinguish declarations with fixed names from declarations that depend on imported fields.
This refactor does not change the fixed point.

The type-variable and telescope folds also append one item to the accumulated list at each step.
Reverse accumulation could remove quadratic list work there.
Those lists are usually much shorter than infix chains.
This analysis did not measure those paths.

The custom resolver monad collects diagnostics when it creates annotations.
This avoids a second AST traversal for errors.
A replacement should preserve this property and local identity order.

A split of recursive syntax resolution needs care.
Type syntax can contain expressions, and patterns can contain types and expressions.
A large callback record or cyclic imports could make that split harder to read.
The independent infix algorithm provides a smaller, useful first boundary.

## Validation

Resolver fixtures check precedence boundaries, non-associative operators, parentheses, and diagnostic selection across multiple conflicts.
An FC fixture checks the resulting tree structure for left, right, mixed, qualified, and parenthesized operators.
It also checks nested constructor patterns.
Existing fixtures cover local identities, record fields, exports, and downstream type resolution.

## Performance experiment

The input declares `infixl 5 %%` and `(%%) x y = x`.
One expression joins character literals with `%%`.
The timer excludes the parser and forces all output annotations and the diagnostic count.
The compiler is GHC 9.12.4 on arm64 macOS.
The benchmark driver uses `-O2` and the normal Cabal resolver library.
Each value below is the median CPU time from five process runs.

| Operators | Before | After |
| ---: | ---: | ---: |
| 250 | 8.849 ms | 0.355 ms |
| 500 | 56.220 ms | 0.222 ms |
| 1,000 | 405.594 ms | 0.449 ms |

Both versions produce the same annotation counts: 773, 1,523, and 3,023.
Small measurements include runtime and garbage collection variation.
These results describe long infix chains, not complete package builds.
