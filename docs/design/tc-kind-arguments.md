# Kind arguments on type constructor applications in aihc-tc

## 1. Problem

`aihc-tc` keeps kind arguments only on a bare constructor.
It drops them on a saturated application.
Downstream code then calculates them again from the visible arguments.
This fails when a kind variable occurs only in the result kind.

Example: `Param :: Nat -> j -> k` in `generic-lens-core-2.3.0.0`.
In `paramIso :: Traversal (Param n a) (Param n b) a b`, no visible argument gives `k`.
The FC conversion stops with `cannot infer the invisible kind argument ... for Param`.
This blocks `generic-lens` and `lsp-test`.

Earlier faults with the same cause: `Same NP` in sop-core (#3162), `Snd` in vinyl (#3172), `TypeError` result kinds (#3173),
Quick Look of a bare constructor (#3171), the promoted tuple (#3254, open), and `Any` defaults (#3199).
Each fix added one more local rule.

## 2. The present representation

`TcType` in `components/aihc-tc/src/Aihc/Tc/Types.hs`:

- `TcTyCon tc args` is a constructor application with only the visible arguments.
- `TcKindedTyCon tc kinds` is a bare poly-kinded constructor with its kind arguments, in scheme order.
- `TcAppTy f a` is an application with a head that is not a constructor, or a partial `TcKindedTyCon` spine.

Places that keep kind arguments:

- `Kind.convertSurfaceType` and `kindedTyConUse` instantiate a poly-kinded constructor as `TcKindedTyCon` with kind metas.
- `mkAppTy` keeps a `TcKindedTyCon` head for a partial application (`TcAppTy` spine).
- `mkAppTy` keeps the spine of `Any` when saturated (`isAnyTyCon`).

Places that drop kind arguments:

- `mkAppTy`: a saturated application of a `TcKindedTyCon` head becomes `TcTyCon tc args`.
- `canonicalApplications`: each kinded spine and each bare `TcKindedTyCon` becomes `TcTyCon`.
- `typeShape`: a shape has no kinds.
- Type family applications: `TcTyCon fam args` never has kind arguments.

Functions that calculate kind arguments again:

- `Types.typeApplicationKinds`: matches the scheme against the argument kinds and an optional expected kind.
- `Finalize.recordKindArguments`, `recordKindArgumentsIn`, `recordPredKindArguments`,
  `openResultKindScheme`, `typeHasOpenResultKind`:
  they write `TcKindedTyCon` spines back for open result kinds, from the reduct or the place of the type.
- `Solve/Family.hs` `bareKindArguments` (in `reduceTypeFamilies`): kinds of a bare constructor in an equation.
- `Solve/Congruence.hs` `kindArguments`: kind proofs for congruence.
- `Fc/Convert.hs` `invisibleKindArgs`, `kindVarToType`, `extraKindVars`, `visibleArgumentKinds`:
  each application in FC calculates its kind arguments again, with an expected kind if one is known.
  `kindVarToType` has a special `Any` rule that uses `Type`.
- `Fc/Desugar/Value.hs` `newtypeFieldType`: kind substitution for a newtype field.

Unification and matching (`Kind.unifyKindsAt`, `Match.matchOne`) accept a bare `TcTyCon tc []` as equal to each `TcKindedTyCon tc ks`.
`reflexiveWitness` selects the kinded form for a proof.

The root defect: the type checker knows the kind arguments, drops them, and other code must find them again.
If the expected kind at that place is not known, they cannot be found.
In `paramIso`, `Param n b` is an argument of the type variable `f`.
`recordKindArgumentsIn` gives `Nothing` as the expected kind under `TcAppTy`.

## 3. Options

### (a) Each application of a poly-kinded constructor has all its kind arguments (as GHC)

Change `TcTyCon !TyCon ![TcType]` to `TcTyCon !TyCon ![TcType] ![TcType]` (kinds, visible arguments).
The kinds list is full for each poly-kinded constructor, also for type families.

- Unification: unify the kinds lists before the visible arguments.
- Zonking and substitution: walk the kinds list too.
- Family reduction: equations get kind patterns. Matching binds kind variables directly. `bareKindArguments` goes.
- Instance matching: match the kinds lists. Two instances that differ only in kind get distinct heads.
- Interfaces: `TypeArtifact.putType`/`getType` change. Bump `packageArtifactFormatVersion` (now `aihc-artifacts-57`).
- FC: `convertTyConApplication` reads the kinds. `invisibleKindArgs` and `typeApplicationKinds` leave FC.
- Each site that builds `TcTyCon` from nothing (deriving, wired-in types, Template Haskell reify) must supply kinds.
  Some sites do not have them and need fresh metas, thus a monad.
- Allocation: one more pointer field on each `TcTyCon` node (3 to 4 words).
  Also a kinds list on each poly-kinded application (`Proxy`, `M1`, `Const`, `Generic` code, families).
  Estimate: 5 to 15 % more type checker allocation.

### (b) Only applications whose visible arguments do not give the kinds have them

The same new field as (a).
The field is not empty only when the scheme at this arity has a kind variable that no visible formal kind mentions.
`openResultKindScheme` already gives this condition.
Then the field has all kind arguments of the scheme, in scheme order.
Otherwise the field is `[]`, and the present calculation stays correct and total.

- Unification: if both sides have kinds, unify them. If one side has `[]`, the visible arguments decide.
  This is the present rule for a bare constructor.
- Zonking and substitution: as (a), but most lists are the shared `[]`.
- Family reduction: an equation right side keeps its kinds. Instantiation substitutes them.
- Instance matching: as today, plus the kinds list if both sides have one.
- Interfaces: as (a). FC: read the kinds when present, otherwise call `invisibleKindArgs` as today.
- Construction: few constructors have result-only kind variables (`Any`, `TypeError`, some families, `Param`).
  Only the sites that build those need kinds.
- Allocation: the extra field only, almost no lists. Estimate: 1 to 3 % more type checker allocation.

One decision is open: how the pure `mkAppTy` knows the condition.
(i) A flag on `TyCon`, set from the kind scheme at the 31 places that build a `TyCon`.
(ii) A `TcM` smart constructor that reads `tcsGlobalTyCons`.
Use (i), because `mkAppTy` is pure and substitution calls it.
(i) is a risk if a `TyCon` exists before its kind is known (a recursive group). See section 5.

### (c) Keep the representation and record kind arguments at more places

Make `recordKindArgumentsIn` give an expected kind in more places.
For `Param`: under `TcAppTy f x`, use the argument kind of the kind of `f`.

- Unification, zonking, family reduction, instance matching, interfaces, and FC: no change.
- Allocation: almost zero in the type checker. A small cost in finalization.
- It corrects `Param` with about 20 lines.
- It does not correct the root defect.
  A kind that only unification gave (for example through a given) is lost before finalization.
  Each new place needs a new rule. #3162, #3171, #3172, #3173, #3199, and #3254 show this pattern.

### How to measure allocation

Build the compiler on the base commit and on the branch.
With each compiler, install `lens-5.3.6` and `generic-lens-core-2.3.0.0` on a new scratch store.
Use `+RTS -s -N1` and compare the "bytes allocated" line. Do two runs of each, in alternation.
Give the numbers in each PR description.

## 4. Recommendation

Use option (b).
It keeps the kinds where they cannot be calculated again, and it removes the local rules.
It costs less allocation than (a), and a later move to (a) keeps the same representation.

If `generic-lens` must work before (b) is done, apply (c) for `Param` as a separate fix. Step 6 removes it.

PR sequence. Each PR passes `just check`.

1. `refactor(tc)`: rename the constructor to `TcTyConApp tc kinds args`.
   Add a bidirectional pattern synonym `TcTyCon tc args` that matches each kinds list and builds `[]`.
   Add a `COMPLETE` pragma. No behavior change, because each kinds list is `[]`. No format bump.
2. `refactor(tc)`: change each traversal that builds a type again to `TcTyConApp`, so that it keeps the kinds.
   Examples are zonking, substitution, `expandTcTypeSynonyms`, `tidyType`, the share, and the unpack walks.
   Find them with a search for `TcTyCon tc (map`. Still no behavior change.
3. `refactor(tc)`: unification, `Match.matchOne`, and `sameType` compare the kinds lists when both are not empty.
   Still no behavior change.
4. `fix(tc)`: fill the kinds. Add the `TyCon` flag.
   `mkAppTy` keeps the head kinds on saturation if the flag is set.
   This replaces the `isAnyTyCon` rule.
   Serialize the kinds and bump `packageArtifactFormatVersion`.
   FC `convertTyConApplication` reads the kinds when present.
   Add the `Param` fixture (section 5). It must fail before this PR.
5. `refactor(tc)`: merge `TcKindedTyCon tc ks` into `TcTyConApp tc ks []`.
   Remove `canonicalApplications` rules for bare forms, `reflexiveWitness`, and the bare-constructor rules
   in `unifyKindsAt` and `matchOne`.
6. `refactor(tc)`: remove `Finalize.recordKindArguments` and its helpers,
   the FC `Any` rule in `kindVarToType`, and the (c) rule if it exists.
   All fixtures in section 5 must still pass. If one fails, step 4 is not complete.

Workarounds that each step can remove:

| Step | Removes |
| --- | --- |
| 4 | the `isAnyTyCon` rule in `mkAppTy` |
| 5 | `canonicalApplications` (bare forms), `reflexiveWitness`, the bare rules in `unifyKindsAt` and `matchOne`, `bareKindArguments` in `Solve/Family.hs` |
| 6 | `recordKindArguments`, `recordKindArgumentsIn`, `recordPredKindArguments`, `openResultKindScheme` (moves to the flag), `typeHasOpenResultKind`, the reduct scope check of #3173, the `Any` rule in `kindVarToType` |

`typeApplicationKinds` stays. FC still uses it for applications whose kinds the visible arguments give.

## 5. Risks and required fixtures

Risks:

- A rebuilding site that uses the pattern synonym silently drops kinds.
  Step 2 must find each site before step 4 fills kinds.
- Derived `Eq` and `Ord` on `TcType` see `[]` and a full list as different.
  `Map` keys (family instance keys, given matching, `reduced /= application`) can then miss.
  Step 3 must cover each comparison that is not structural on purpose.
- The `TyCon` flag can be wrong for a `TyCon` that exists before its kind is inferred.
  Examples are a recursive declaration group, wired-in names, and resolver output.
  If so, use option (ii) in step 4.
- Allocation can be higher than the estimate. Measure in step 2 and step 4.

These fixtures exist and must pass at each step:

- FC golden (`bin/aihc/compiler/fc/test/Test/Fixtures/golden/`):
  `type-error-reduced-argument.yaml`, `type-instance-polymorphic-result-kind.yaml`,
  `closed-family-inferred-result-kind-variable.yaml`, `higher-kind-meta-default.yaml`,
  `data-family-instance-result-kind.yaml`, `instance-head-promoted-nil-kind.yaml`.
- tc annotated (`components/aihc-tc/test/Test/Fixtures/annotated/`):
  `promoted-list-kind.yaml`, `class-promoted-parameter-kind.yaml`,
  and the Quick Look fixture of #3171 (`rank-n-argument-poly-kinded-family-constraint`).
- `promoted-constructor-kind-arguments.yaml` from #3254, when it merges.

Add before or with the change:

- An FC golden fixture for `Param`: `data family Param :: N -> j -> k`, used as `f (Param n a)` under a type variable `f`.
  It fails on `main` today. Add it in step 4 (or with the (c) fix).
- An FC golden fixture where only a given equality fixes a result-only kind.
  Option (c) cannot correct this case, and (b) must.
- A reduced `M1` case from lens `Control.Lens.Wrapped`
  (`lens-wrapped-reduced-Red.hs` in the shared session notes).
  It fails on `main` with `cannot infer the invisible kind argument ... for M1`.
  Reduce it to the lowest layer before step 4.
- An allocation baseline on `main`, measured as in section 3, in the step 1 PR description.
