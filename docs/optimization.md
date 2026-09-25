# Optimization

This document is the design of the optimizer: what each `-O` level is for,
how a level turns into a plan of passes, how the System FC inliner decides,
and the rules that keep the passes from becoming a pile of special cases.
The implementation lives in `bin/aihc/compiler/fc` (the passes), in
`Aihc.Grin.PointsTo` (the GRIN analysis), and in
`Aihc.Cli.OptimizationPlan` (the plans). The document and the code change
together.

## The four modes

A level names what the build is for. Nothing else about it is fixed here.

| Level | Purpose | Scope |
| ----- | ------- | ----- |
| `-O0` | Finish as soon as possible. No pass runs that is not needed for correctness. | Each module on its own. |
| `-O1` | Optimize without whole-program transformations. | Each module on its own, in a future version in import order with facts from the interfaces of its imports. |
| `-O2` | Produce fast code. Every optimization is on. | The whole program, merged into one System FC program. |
| `-Os` | Produce a small executable, with the whole program in view. | The whole program. |

`-O2` and `-Os` merge the System FC of every module of the program, drop the
declarations the entry does not reach, and run the passes once on the result.
`-O0` and `-O1` run the passes on each module as it is lowered. `--lto` at
`-O0` or `-O1` merges the program without running any pass that the level
does not name.

## Three axes, one plan

A level conflates three things that are separate in the code:

- **Scope**: per module or whole program. It decides where the passes run and
  which values are roots: the public values of a module, or the entry of a
  program.
- **Objective**: speed or size. It decides how the inliner prices a site.
- **Effort**: which passes run, and how many rounds each gets.

`Aihc.Cli.OptimizationPlan.optimizationPlan` expands a level and the `--lto`
flag into an `OptimizationPlan` once, at the command line. The plan holds the
scope, the list of System FC passes, and whether the heap points-to analysis
of GRIN runs. Everything downstream reads the plan.

**No pass and no driver reads the level.** The level still reaches Clang,
which gets it for the C sources of a package and for LLVM output, and it is
part of the identity of an installed package. Nothing in the System FC, GRIN,
Lir or object pipeline branches on it. A new optimization is a pass with a
place in one or more plans, never a condition on the level inside another
pass.

## Passes

A pass is a value of `Aihc.Fc.Pass.Pass`. Each one is a pure function from a
program to a program and a report, run by `runPass` with the roots of the
scope. The driver in `Aihc.Cli.Install.optimizeFcProgram` runs the passes of
the plan in order, logs each report under `--verbose`, and lints the program
after each pass under `--lint`.

| Pass | What it does |
| ---- | ------------ |
| `PassEtaExpand` | Arity analysis, then eta expansion of every top-level value to the arity it finds. `Aihc.Fc.Arity`. |
| `PassInline policy rounds phase` | The inliner under a policy, for at most that many rounds, in a phase. `Aihc.Fc.Inline`. |
| `PassSimplify phase` | One walk over every body with the local rewrites and no copy of any callee, in a phase. `Aihc.Fc.Simplify`. |

A phase is a number that counts down as GHC's phases do: the shrinking
inliner runs in phase 2, the growing inliner in phase 1, and the final
simplifying walk in phase 0. Nothing else reads the phase: it decides which
rewrite rules fire (see below).

The plans are:

| Level | Passes |
| ----- | ------ |
| `-O0` | none |
| `-O1` | eta expand, inline `shrinkPolicy` [2], inline `growPolicy` [1], eta expand, simplify [0] |
| `-O2` | the same as `-O1`, on the whole program |
| `-Os` | eta expand, inline `shrinkPolicy` [2], eta expand, simplify [0] |

`-O2` and `-Os` also run the heap points-to analysis of GRIN on the lowered
whole program. See "Heap points-to analysis" below.

`-Os` is a prefix of `-O2`: the growing phase of `-O2` starts from the
program that `-Os` would have produced. Eta expansion runs before the
inliner so that a value it turns into a function is a saturated call, and
after it because a call of a class method hides the arity of the method until
the selection is inlined. The final simplifying walk reduces the applications
and casts that the second expansion leaves behind.

The simplifier is its own module because it is a different thing from the
inliner. It holds the rewrites that need no copy of a callee: beta reduction,
a let in the head of an application, the case of a known constructor, a case
on a comparison with a literal, common strict primitive calls, case of case
with join points, and cancelling casts. The inliner calls it on every copy it
makes, and the plan runs it standalone. A new local rewrite goes there. A new
rule about *which* copies to make goes in the inliner.

For a default case on a variable, the simplifier uses the evaluated case
binder in the case body. This gives a strict constructor field one use before
the case. The simplifier can then move a single-use thunk into the case.

## The inliner

The inliner follows the non-recursive inliner of MLton. It walks the values
from the leaves of the call graph to the roots. At a use of a non-recursive
value it puts a copy of the body in place, simplifies the copy, and keeps it
when the policy accepts the site. A value that nothing uses after a round is
dropped, unless it is a root.

### The policy

Every decision is local. A site is accepted or rejected from the callee, the
arguments at that site, and the value the site sits in. No decision depends on
what the walk did to any other value, so the result of one value is stable
under edits to unrelated values, and any single decision can be read off a
dump of the program.

`Aihc.Fc.Inline.InlinePolicy` has six knobs:

| Knob | Meaning |
| ---- | ------- |
| `policyCalleeLimit` | The largest body that is a candidate. A larger value is never copied, whatever the site. A value used once is copied whatever its size, because the copy replaces the value. |
| `policySiteLimit` | The largest growth one site may cause, after discounts. Zero means a site is taken only when the program does not grow. |
| `policyFunctionArgumentDiscount` | What a site earns for each argument that names a function the callee applies. The saving is a closure not allocated and a call made direct, which no size of the result shows. |
| `policyValueGrowth` | How far one top-level value may grow in the pass, as a percentage of its size when the pass began. |
| `policyValueSlack` | Nodes every value may grow by in the pass, whatever its size, so that a small value can still take one useful copy. |
| `policyTakeRequested` | Whether an `INLINE` value within the callee limit is copied at every site that the site rule admits, whatever the growth and the allowance of the value. |

`shrinkPolicy` sets the callee limit to 80, every other limit to zero, and
does not take requested sites.
The callee limit reduces work on large copies that the site rule would reject.
A removable value bypasses this limit when its copies together replace it.
`growPolicy` is the speed policy. Its numbers are in the code.

The inliner decides a scrutinee site before it simplifies the case alternatives.
The alternatives then use the allowance that remains after this decision.
A rejected site simplifies only the original alternatives.
This prevents duplicate work in nested cases.

The growth of a site is the size of the simplified copy less the size of the
call it replaces, less the discounts. The size is `Aihc.Fc.Size.exprSize`,
which follows the code the CPS conversion of GRIN makes: a case in bind
position copies its continuation into each alternative, so a case whose
scrutinee has several tail leaves counts its alternatives once per leaf.

A value that is copied at every use and then dropped is exempt from the
callee limit and from the growth of the value it lands in: the copies
together are no larger than the value, so the program shrinks whatever the
value's size.

Why these rules bound the program without a global counter: every accepted
site adds at most the callee limit, every value grows at most to its own
multiple, an exempt copy never grows the program, a requested copy is a
small `INLINE` value at a call that was already in the body, recursive groups are never
copied into themselves, and the round count is fixed. Total growth is
bounded by construction. There is no program budget and no backstop: a
program that grows more than expected is a mis-tuned knob, found by reading
the pass reports, not by bisecting a limit.

### What is deliberately not a rule

- A global size budget. It made a site's fate depend on how much the walk had
  spent before reaching it, so early sites starved later ones and an edit in
  one module changed the inlining of another.
- A budget relative to a separate `-Os` build. `-Os` is a prefix of `-O2`
  instead, so the shrink phase gives the base size for free.
- A ratio backstop over the whole program. It would only ever fire when a
  local rule is wrong, and then it would hide the wrong rule.

## Rewrite rules

A `{-# RULES #-}` pragma reaches System FC as a `DeclRule`: the rule's type
binders, the dictionaries of its constraints, and its pattern variables, the
shared type of its sides, and the two sides as expressions (`docs/system-fc.md`).
The simplifier fires rules, in `Aihc.Fc.Simplify.fireRule`, and the matcher
is `Aihc.Fc.Rules`.

- A rule is tried at an application whose head names the head of its
  left-hand side, after the arguments are simplified and before the head is
  inlined, so that a rule written for a function sees its calls. Because the
  inliner simplifies every copy it makes, rules fire on inlined code too.
- Matching is first-order and syntactic, modulo the names of binders both
  sides bind in the same place. Type binders of the rule match the type
  arguments; a ground type of the pattern is compared up to synonyms. No
  beta reduction or eta expansion is attempted. An application may give the
  head more arguments than the left-hand side names; the surplus applies to
  the result.
- A pattern variable never takes an expression that names a variable the
  application binds inside the part being matched, as in GHC, because that
  variable would escape into the right-hand side.
- The right-hand side is copied with fresh binders, instantiated, and
  simplified again in place. Each walk over one body fires at most
  `ruleFuel` rules, so a looping pair of rules stays finite.
- A rule fires only in the phases its activation names: `[n]` from phase
  `n` down to 0, `[~n]` before phase `n`, `[~]` never. `Aihc.Fc.Rules.ruleTable`
  selects the rules of a pass.
- A value a rule names is a root of the inliner: it is never dropped while
  the rule may still put it in place.
- Every pass report counts the rules it fired; `--verbose` prints it.
- A template that is an eta-expansion of a binder, `Λb. g @b` or `λx. g x`,
  is matched eta-reduced: the desugarer expands a binder passed at a
  polymorphic or a function type, and the source meant the binder alone.

### List fusion in the core libraries

`GHC.Base`, `Prelude` and `GHC.Enum` carry GHC's foldr/build scheme: a
producer (`map`, `filter`, `++`, an `Int` range) turns into its `build` form
in phase 2 by a `[~1]` rule, `foldr` over a `build` fuses there, and from
phase 1 a `[1]` rule turns what did not fuse back into the plain function.
Two things differ from GHC:

- Each producer also has a `[1]` rule from its `build` form straight back to
  the plain call (`"map/build"`, `"filter/build"`, `"++/augment"`,
  `"enumIntFromTo/build"`), so the round trip does not depend on `build`
  being inlined, which a size-bound pass or the `-Os` plan may not do.
- `foldr` takes all three arguments in its head, so that a consumer written
  as a partial application, `sum = foldr (+) 0`, has the arity of its type
  and is copied at its calls. The arity pass reads arity from the body, and
  GHC's `foldr k z = go` gives it arity 2.

Rules are matched in the program the pass is given. At the per-module scope
that is the module's own rules; at the whole-program scope it is every rule
of every module, which is what makes rules from a library fire in a program
that uses it. Per-module firing of imported rules waits on the same import
facts as "-O1 in import order" below.

### Inline pragmas

A top-level value and an instance method carry their `INLINE`, `INLINABLE`
or `NOINLINE` pragma into System FC as the `valInline` of the `ValDecl`
(`inline [2]`, `inlinable`, `noinline [~1]` in the text form). The inliner
reads it per phase, with the activation read as a rule's is:

| Pragma | In the phases the activation names | In the other phases |
| ------ | ---------------------------------- | ------------------- |
| `INLINE` | a candidate whatever its size; copied at each admitted site when the policy takes requested sites and the value is within the callee limit; otherwise the site policy decides each copy | never copied |
| `INLINABLE` | the usual policy | never copied |
| `NOINLINE` | the usual policy | never copied |
| none | the usual policy | the usual policy |

A plain `NOINLINE` names no phase, so the value is never copied (the text form
leaves its `[~]` unsaid); a plain `INLINE` names every phase.

GHC copies an `INLINE` value at every saturated call whatever the growth.
`growPolicy` does the same for an `INLINE` value within the callee limit.
Such a site does not charge the allowance of the value it lands in, so the
other sites of that value keep their room. The sites inside the copy still
charge it. The core libraries mark the small wrappers `INLINE`: `(.)`,
`thenIO`, `bindIO`, `returnIO`, and the `Monad IO` methods. Without the
pragma, a large value such as `bufWrite` used its allowance before it
reached them, and `>>` and `(.)` stayed calls and partial applications.

Two things differ from GHC:

- The site rule decides which sites are admitted. A call that gives every
  parameter is admitted, and so is a partial call with an interesting
  argument. System FC does not keep the arity of the left-hand side apart
  from the lambdas of the body, so `(.) f g = \x -> f (g x)` has arity three.
- A larger `INLINE` value is only a candidate, and the site policy decides
  each copy. The `text` package marks large functions `INLINE`, and
  honouring them GHC's way made its example two and a half times larger at
  `-O2`.

`shrinkPolicy` does not take requested sites, so it keeps its invariant
below, and an `INLINE` value that it rejects stays a call. This is what lets a rule beat the inliner to a
call: `NOINLINE [1] f` keeps `f` a call through phase 2, where a rule on
`f` fires, and lets the growing inliner copy it afterwards. `CONLIKE` is
read and ignored. A recursive value is never copied whatever its pragma.

## Heap points-to analysis

`Aihc.Grin.PointsTo` is the heap points-to analysis of Boquist's GRIN
thesis, in the inclusion-based form of Andersen's analysis. It runs on the
GRIN of a whole program, after lowering and before the CPS conversion.
`Aihc.Cli.Install.optimizeGrinPointsTo` runs it when the plan sets
`planGrinPointsTo`, and it prints one report line under `--verbose`.

The analysis finds the heap locations that each pointer variable can point
at. A location is a `store`, a binding of a `store-rec`, an `apply` that
makes a partial application, a global, or the shared object of a nullary
constructor. The analysis also finds the nodes of each location and the
locations of each field. Two locations stand for objects that the program
cannot see: one that can be a thunk, and one in weak-head normal form. A
value that a primitive, a foreign call, `catch#`, or the runtime gives is
one of them. A value that goes to such code, and each public global,
escapes: the unknown code can read, apply, and evaluate it.

The solver is sequential. A variable, a parameter, a result, and a field
are each a set node. A copy is an edge. `eval`, `apply`, `case`, and `fetch`
are triggers on the set node of their operand. The worklist gives each set
node only its new locations (difference propagation). The solver does not
merge cycles. The analysis refuses a program that has an explicit `update`,
because an update can change a value node into an indirection.

The rewrites are:

| Rewrite | Condition |
| ------- | --------- |
| Remove a case alternative | No location of the scrutinee holds its constructor. |
| Replace `eval x` with `x` | Each location of `x` holds only nodes in weak-head normal form. |
| Replace a saturated `apply` with `fetch` and `call` | Each location holds the same closure tag, and the argument and result layouts match. |
| Replace `eval` with `eval-once` | Each thunk that `eval` can enter is single-entry, and its function gives a value in weak-head normal form. |

The pass can join consecutive applications of a known closure into one
call. Each intermediate partial application must occur only as the next
callee. The pass preserves logical argument groups, including empty groups
and groups with several values. It fetches captured fields from the original
closure. It does not cross an evaluation or effect. An oversaturated call
keeps the application of its remaining arguments.

A thunk is single-entry when its location is not shared. A location is
shared when a variable that points at it has two uses, when a shared or an
escaped node holds it in a field, when it is a static object, or when it is
the result of a shared thunk. Alternatives count as the largest use of any
one of them. A case binder and the binders of a default alternative are
other names for the scrutinee. A program that calls `aihcControl0#` gets no
single-entry evaluations, because a captured continuation can resume an
evaluation more than one time.

Each rewrite removes code or replaces a runtime dispatch with a direct
jump. An `eval` that CPS would change into `if-whnf` and a continuation
frame goes away completely. Thus the analysis runs at `-Os` as well as at
`-O2`. After the rewrites, the program gets the normalization, the
simplification, the sweep, and the renumbering that lowering gives it.

The report line gives the time of the analysis and of the rewrites, the
number of solver iterations (set nodes that the worklist gave new
locations), the numbers of variables, set nodes, locations, shared locations
and single-entry thunks, and the number of rewrites of each kind.

The fixtures are in `compiler/grin/test/Test/Fixtures/grin-points-to`. The
shared evaluation fixtures also run as whole programs with the rewrites, in
the GRIN interpreter. The interpreter marks a thunk that a single-entry
evaluation entered, and a second evaluation of it fails.

## Invariants

These are the properties the structure is meant to keep. Each is checkable,
and a change that breaks one needs a reason in its pull request.

- A pass at `-O0` exists only for correctness. The `-O0` plan is empty.
- `shrinkPolicy` never grows the program under `programSize`.
- Running a plan twice on its own output changes nothing but local names.
  A rewrite that ping-pongs shows up here.
- A per-module result depends only on that module and the interfaces of its
  imports.
- Every pass has golden fixtures under
  `compiler/fc/test/Test/Fixtures/golden`, run through `passes:` in the
  fixture, in the order a plan would run them. A `simplify` entry may carry a
  phase (`simplify: 2`), and an `inline` object a `phase` knob.

## Not done

- **Faster points-to solving.** The solver does not merge cycles of copy
  edges, and it does not use more than one core. Constraint generation for
  each function is independent, so it can run in parallel. The solve can
  get lazy cycle detection. Measure first: the analysis takes about 100 ms
  for the largest example.
- **More precise points-to analysis.** The analysis is not context
  sensitive, and a value that goes through a mutable reference or an array
  is unknown. A model of `MutVar#` and array cells per allocation site would
  keep such values known.

- **Growth in context.** A site's growth is measured on the copy alone. A
  copy that adds tail leaves to a strict let or a case scrutinee multiplies
  the enclosing body under the CPS-aware size without charging the site. The
  fix is to measure at the enclosing let or case, or to give the CPS
  conversion a join point for a case in bind position.
- **`-O1` in import order.** The inliner takes its candidates from the
  program it is given. The DAG mode is a second source of candidates: the
  small bodies that the System FC of an import exports, under the callee
  limit. Nothing else in the plan changes.
- **The dictionary field rule.** A method whose one use is its dictionary
  field is treated as used once and copied unconditionally, which the
  case-of-known rule then selects at every site. The rule should count only
  saturated calls as the uses that make a value unconditional.
