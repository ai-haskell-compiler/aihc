# aihc-tc overview

`aihc-tc` consumes a name-resolved surface `Module` and returns the same
surface `Module` with type-checker annotations added. It owns Haskell type
checking only: types, kinds, type-class dictionaries, equality evidence, and
type-checker diagnostics. It does not perform name resolution, and it does not
desugar to System FC.

The public entry points live in `Aihc.Tc`:

- `typecheck :: [Module] -> [TcModuleResult]`
- `typecheckModule :: Module -> TcModuleResult`
- `typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> TcModuleResult`

`TcModuleResult` contains the annotated module plus a success flag. Diagnostics
are also stored as annotations in the returned module, so rendering and
downstream inspection do not need a separate diagnostic side channel.

## Component Shape

The type checker keeps parser syntax as the output shape and attaches facts
with the parser annotation mechanism:

- `TcAnnotation` for expression elaboration, type arguments, evidence, and
  lambda/pattern type facts.
- `TcBindingAnnotation` for binders.
- Class, instance, method, and dictionary annotations for type-class metadata.
- `TcDiagnostic` for type-checker errors.

The important implementation modules are:

- `Aihc.Tc`: public API, module sequencing, extension options, result cleanup.
- `Aihc.Tc.Monad`: `TcM`, typing environment, fresh uniques, meta/evidence
  stores, diagnostics, and solve reports.
- `Aihc.Tc.Generate.Decl`: top-level declarations, signatures, binding groups,
  classes, instances, and circular solve boundaries for declaration bodies.
- `Aihc.Tc.Generate.Expr`: expression/RHS constraint generation and annotated
  syntax rebuilding.
- `Aihc.Tc.Generate.Bind`: shared planning and generalization helpers for
  expression-local `let`/`where` binding groups.
- `Aihc.Tc.Generate.Annotate`: lazy report-aware annotation constructors.
- `Aihc.Tc.Generate.Pattern`: pattern checking and GADT givens.
- `Aihc.Tc.Kind`: surface type conversion and kind checking.
- `Aihc.Tc.Solve`: equality, implication, and dictionary solving.
- `Aihc.Tc.Zonk`: state/report-based replacement of solved metas.

## The Ideal Model

The type checker has one way to annotate value syntax:

1. The typing rule that checks a syntax node returns the rebuilt syntax for that
   node.
2. The same rule emits the constraints that justify the annotations.
3. The rebuilt syntax contains lazy annotation thunks that read a
   `TcSolveReport`.
4. Solving and post-solve generalization produce the report.
5. When the returned module is rendered or inspected, the thunks zonk types,
   look up evidence, and either attach successful facts or diagnostics.

There is no later source-span search and no replay queue of occurrence facts.
The generator does not record “work to do later”; it returns the syntax it owns.
The only reason this can still see solved types/evidence is Haskell laziness:
the report is passed into generation before it exists and is tied to the state
that solving and finalization produce.

## Module Pipeline

At module level, `tcModule` runs these phases:

1. Register type constructors, data constructors, classes, instances, and
   foreign imports in the global environment.
2. Collect and kind-check user type signatures.
3. Group value bindings, generate constraints, solve them, generalize successful
   inferred bindings, and return rebuilt value syntax with annotations already
   attached.
4. Type-check instance method bodies the same way.
5. Add declaration-level metadata for data, foreign, class, and instance
   declarations.

Value declarations are checked in dependency order but applied back to the
original module shape. That apply step is not an annotation replay pass; it only
places the already rebuilt declaration syntax back into source order and wraps
successful declarations with their binding annotation. Failed bindings keep
diagnostic annotations but have successful TC annotations stripped.

## Tie-The-Knot

The central helper is:

```haskell
solveCircularWithPost
  :: ([Ct] -> [Implication] -> TcM ())
  -> (TcSolveReport -> TcM (a, [Ct], [Implication]))
  -> (a -> TcM b)
  -> TcM b
```

Conceptually:

```haskell
result =
  let report = solveReportFromState finalState
      (syntax, constraints, implications, generatedState) =
        runGeneration report startState
      solvedState =
        runSolver constraints implications generatedState
      (result, finalState) =
        runPostSolve syntax solvedState
   in result
```

Generation must not inspect the report while creating constraints. It may only
store report-dependent thunks in returned syntax. This allows the same AST node
to contain:

- successful annotations whose types/evidence are filled from the final report;
- diagnostics keyed by the evidence variable of a failed constraint.

For inferred bindings, the post-solve step generalizes the type and commits the
meta-to-type-variable mapping before the report is observed. That is why binder,
pattern, tuple, list, and occurrence annotations render final names like `a`
instead of raw metas like `?3`.

## Diagnostic Ownership

The rule that creates a constraint owns the diagnostic attachment point for that
constraint.

Examples:

- A list element equality attaches failure to the later element that must match
  the earlier element.
- A case branch result equality attaches failure to the later branch RHS.
- A binding signature equality attaches failure to the binding RHS.
- A list-comprehension guard equality attaches failure to the guard expression.
- A kind diagnostic from a type signature attaches to the signature declaration.

`SourceSpan` remains best-effort display metadata in diagnostics. It is not used
to decide where annotations are attached, so type-checking still works for
resolved modules whose surface syntax has no source-span annotations.

## Invariants

- The returned module preserves incoming annotations.
- Type-checker annotations are added semantically by the typing rule that owns
  the syntax.
- No temporary node IDs or annotation queues appear in returned syntax or
  `TcState`.
- Every diagnostic that contributes to `tcmSuccess` is present as an
  annotation in the returned module; missing attachment is an internal error.
- Successful annotations are not emitted for failed value bindings.
- Missing solved evidence for a successful annotation is an internal error, not
  a rendered fallback.
- Declaration metadata for data/class/instance/foreign declarations remains
  declaration-level metadata; it does not walk value bodies.
