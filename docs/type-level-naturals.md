# Type-level naturals

`aihc` has no type-level literals at all. The kind checker's fallthrough
rejects the surface node outright:

```haskell
{-# LANGUAGE DataKinds, KindSignatures #-}
data Vec (n :: Type) = Vec
type P = Vec 3
-- error: unsupported surface type in kind checker: TTypeLit (TypeLitInteger 3 "3")
```

`GHC.TypeLits`, `GHC.TypeNats`, `GHC.TypeError` and their `GHC.Internal.*`
counterparts are consequently empty shells (`module GHC.TypeLits () where`).
The empty modules are the symptom; the missing feature is the literal itself.

The motivating consumer is `random-1.3.1`:

```haskell
import GHC.TypeLits (Nat, KnownNat, natVal', type (<=))
class (KnownNat (SeedSize g), 1 <= SeedSize g, Typeable g) => SeedGen g where
  type SeedSize g :: Nat
seedSize = fromInteger $ natVal' (proxy# :: Proxy# (SeedSize g))
```

`cabal run -v0 exe:aihc -- install --target apple-arm64 --lint random` fails
name resolution on `System/Random/Seed.hs` with eight resolver errors plus a
cascade. `System.Random.Stateful` then aborts with "missing checked type
constructor for class predicate SeedGen"; that abort's diagnostics are a
separate task. The `random` install will still fail afterwards on unrelated
root causes (missing `Generic` deriving, injective type families, unlifted
expression signatures) — out of scope here.

## Design decisions

### Literals are a new node, not encoded type constructors

`TcType` gains

```haskell
data TyLit = TyLitNat !Integer | TyLitSymbol !Text | TyLitChar !Char
data TcType = ... | TcTyLit !TyLit
```

and `Aihc.Fc.Syntax.Type` gains a matching `TyLit Name TyLit`, where the
`Name` is the type constructor that is the literal's kind. Naming the kind
rather than deriving it is what keeps the FC side free of special cases: a
literal then refers to its sort exactly as every other type refers to a
constructor, so `Fc.Imports`, the scope table and `Fc.Lint` need no rule of
their own, and the lint reads the kind off the node instead of having to
know where `Natural` is declared.

The alternative — encoding `3` as a nullary `TyCon` named `"3"` — was
rejected on two counts. The FC lint resolves the kind of every `TyCon`
through `lookupHeaderType` (`Aihc.Fc.Lint.lintType`), so each distinct
literal would need a synthesized declaration in the module header and an
entry in `InstallV2.addReferencedFacts`; and the solver's arithmetic would
have to recover the value by re-parsing name text. A real constructor costs
one `case` per consumer and the `-Wincomplete-patterns` errors enumerate the
work exactly. The consumer set is small: about a dozen files for `TcType`
(`Tc.Types`, `Tc.Kind`, `Tc.Unify`, `Tc.Zonk`, `Tc.Tidy`, `Tc.Solve.*`,
`Cli.TypeArtifact`) and about fifteen for the FC type (`Fc.TypeOf`,
`Fc.Lint`, `Fc.Pretty`, `Fc.Parser`, `Fc.Share`, `Fc.Convert`, `Fc.Normalize`,
`Fc.Inline`, `Fc.Tidy`, `Fc.Prune`, `Fc.Merge`, `Fc.Imports`, `Grin.Lower`).

Everything the compiler is built on is an asset of `aihc-prim`: nothing in
`aihc-base` may be fundamental to it, and `Install` must not name `aihc-base`
at all. The kind of a natural literal is such an asset, so `data Natural`
is declared in `GHC.Prim.Natural`, beside the `Integer` that is already
there, and `GHC.Num.Natural` re-exports it and keeps its instances. That is
a deliberate departure from GHC's layout, where `Natural` sits in
`ghc-bignum`'s own `GHC.Num.Natural`.

### Kinds follow GHC 9.12

A natural literal has kind `GHC.Num.Natural.Natural` — the data type, exactly
as in GHC, where the type is both a runtime `Natural` and the kind of type
literals. `Nat` is a synonym for it. A string literal has kind
`GHC.Types.Symbol`, which `aihc-prim` does not yet declare (only the
`TypeLitSort` payload of `KindRep` mentions the name); adding `data Symbol`
to `GHC/Types.hs` narrows, rather than widens, the divergence from `ghc-prim`.
A character literal has kind `GHC.Types.Char`, which already exists.

`Natural` already has GHC's representation (`NS Word# | NB BigNat#`,
`core-libs/aihc-base/src/GHC/Num/Natural.hs`), so `natVal` returns a real
value with no new runtime work.

### Symbol lands with Nat, `KnownChar` does not

The literal plumbing is one change for all three sorts; splitting it would
mean touching the same thirty files twice. `Symbol` is also wanted by the
parallel `GHC.Generics` metadata work, so `TyLitSymbol`, `KnownSymbol` and
`symbolVal` ride along in the same stack (PR 3). `TyLitChar` gets the
constructor and the kind, but `KnownChar`/`charVal` are deferred — nothing
needs them yet.

### `KnownNat` is solver-synthesized, like `Typeable`

`Typeable` is the precedent: `Tc.Solve.Dict` special-cases the class name,
builds `EvTypeable`, and `Fc.Desugar.Value.desugarTypeableEvidence` emits
`$Dict$Typeable @t <representation>`. `KnownNat` follows the same shape with a
new `EvTypeLit !(Maybe (Text, Text)) !TcType !TyLit` evidence term, desugared
to `$Dict$KnownNat @n <natural>`. The natural value reuses the existing
big-literal builder (`desugarIntegerLiteral`, which already handles values
past `maxWord` via `integerShiftL#`/`integerAdd`) through
`naturalFromInteger`.

`someNatVal`, `withSomeSNat` and friends are expressible in the library with
`Unsafe.Coerce.unsafeCoerce`, as GHC writes them; no further compiler support
is needed for them.

### `<=` is GHC 9.12's, and the machinery it needs comes first

GHC 9.12 defines the comparison chain in `GHC.Internal.Data.Type.Ord` and
`GHC.Internal.TypeError` as

```haskell
type x <= y  = Assert (x <=? y) (LeErrMsg x y)
type x <=? y = OrdCond (Compare x y) 'True 'True 'False
type LeErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: …)

type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _      = ()
  Assert _     errMsg = errMsg

type family TypeError (a :: ErrorMessage) :: b where   -- no equations; solver magic
```

`Compare`, `OrdCond`, `(<=?)`, `CmpNat` and the arithmetic families cost
nothing beyond the kind-polymorphic closed families this stack adds anyway.
`(<=)` is the exception, and it is worth being precise about why.

`Assert (x <=? y) (LeErrMsg x y)` is a **bare type-family application at kind
`Constraint`** in constraint position. In `random` it is stuck:
`1 <= SeedSize g` is a superclass of `SeedGen`, with `g` the class variable,
so nothing reduces until an instance fixes `SeedSize`. Today
`Tc.Kind.surfaceClassPredToPred` expands a constraint synonym and demands
exactly one `Pred`, and `Pred` is `ClassPred`/`EqPred`/`QuantifiedPred`/
`IParamPred` — there is no irreducible form. The pre-9.4 spelling
`type x <= y = (x <=? y) ~ True` would sidestep the whole problem, because a
stuck `<=?` application is an `EqPred` whose evidence is an erased coercion;
this stack deliberately does not take that exit.

Four capabilities are therefore prerequisites rather than follow-ups:

1. **`IrredPred !TcType`** in `Pred`: a stuck constraint-kinded application,
   with evidence, reduced when its arguments become known. `SeedGen`'s
   dictionary gains a field of that type, so the FC side has to carry a
   `Constraint`-kinded family application as a binder type.
2. **Constraint tuples.** `Assert 'True _ = ()` reduces to the empty
   *constraint*; `Tc.Kind.convertTupleType` currently yields the boxed `Unit`
   at kind `Type`. This is independently valuable: a constraint synonym
   standing for more than one constraint is a known aihc-base limit, and it is
   the same machinery.
3. **`TypeError` reporting.** A wanted whose head is the `TypeError` family
   must render its `ErrorMessage` rather than fail as a missing instance.
   `ErrorMessage` also needs the existential `ShowType` promoted and the
   infix `:<>:`/`:$$:` constructors.
4. **Kind-indexed family instances.** `Compare` is selected purely by the
   kind of its arguments (`type instance Compare (a :: Natural) b = CmpNat a b`),
   and `Tc.Solve.Family.matchTypes` matches types while ignoring kinds. With
   only `CmpNat` wired the misfire is invisible; it becomes wrong the moment
   `CmpSymbol` exists.

`Unsatisfiable` is exported and so must exist by name. Its GHC definition
instantiates `unsatisfiableLifted` at `(##) -> a` for representation
polymorphism; the first cut gives it a lifted-only signature and records the
divergence, since the ratchet checks names.

## PR stack

Each PR is independently buildable and testable. Conventional Commits; branch
per PR off the previous one.

### PR 0 — `feat(tc): irreducible predicates and constraint tuples`

A spike that settles the riskiest piece before anything is built on it. It
needs no type-level literals and no core-libs changes: the fixtures declare
their own `Assert`-shaped closed family.

- `Aihc.Tc.Types`: `IrredPred !TcType` in `Pred`, and `ShapePred` alongside.
- `Tc.Kind.surfaceClassPredToPred`: a constraint-kinded application that is
  neither a class nor an equality becomes an `IrredPred` instead of the
  current "constraint synonym does not expand to one constraint" abort.
- `Tc.Solve`: an `IrredPred` wanted is reduced through
  `Solve.Family.reducePredFamilies` and re-canonicalized; a stuck one is kept,
  matched against givens structurally, and reported readably when unsolved.
- `Tc.Kind.convertTupleType`: a boxed tuple checked against kind `Constraint`
  becomes the constraint tuple; `CTuple0` solves trivially and `CTupleN`
  splits into its components. Declare the constraint tuples in `aihc-prim`.
- Evidence and FC: an `IrredPred` field carries a dictionary-shaped value; the
  reduction from `Assert 'True _` to the empty constraint tuple has to be a
  well-typed cast in FC, not a silent retype.
- Tests: an annotated fixture shaped exactly like the target —
  `type family F (b :: Bool) (c :: Constraint) :: Constraint` with
  `F 'True _ = ()`, a class `class F (G g) () => C g` with an associated
  family, and an instance that fixes `G` so the constraint reduces; plus the
  unsolved case, and an FC lint fixture for the dictionary field.

**Exit criterion.** If FC cannot carry a `Constraint`-kinded family
application as a binder type without a coercion story, stop and revisit the
sequencing before PR 1.

### PR 1 — `feat(tc): type-level literals in the kind checker` — landed

The literal plumbing, no user-visible feature beyond kind-checking a literal.

- `Aihc.Tc.Types`: `TyLit`, `TcTyLit`, and the `TypeShape` counterpart
  (`ShapeTyLit`) so structural `Eq`/`Ord` stay honest.
- `Aihc.Tc.Kind.convertNonSynonymTypeWithKinds`: a `TTypeLit` case returning
  the literal and its kind, from new `tcWiringNaturalTyCon` /
  `tcWiringSymbolTyCon` entries in `TcWiring` (`Aihc.Prim.Wiring`).
- `Aihc.Tc.Unify`, `Zonk`, `Tidy`, `Solve.Decompose`, `Solve.Congruence`:
  a literal unifies only with an equal literal.
- `Aihc.Fc.Syntax`: `TyLit`, and every FC consumer listed above;
  `Fc.Lint.lintType` gives a literal its sort's kind; `Fc.Parser`/`Pretty`
  get a round-trippable token (regenerate the FC goldens from the tasty
  `actual:` blocks).
- `Cli.TypeArtifact` CBOR codec; bump `packageArtifactFormatVersion`.
- `core-libs/aihc-prim/src/GHC/Types.hs`: `data Symbol`, exported;
  `core-libs/aihc-prim/src/GHC/Prim/Natural.hs`: `data Natural`, which
  `GHC.Num.Natural` re-exports.
- Tests: annotated fixtures for a natural, symbol and character literal as
  data-type arguments, and a kind mismatch (`Tagged 'x'` where
  `Tagged :: Symbol -> Type`). All three sorts are fixtures because all
  three kinds are in `aihc-prim`, which is what the annotated harness
  loads. A natural literal is also covered end to end, by installing a
  package that uses one against a real `aihc-base` with `--lint`.
- The FC parse/render round-trip property already covers types; `genType`
  gains literals, which is what found that `stringChar` did not accept the
  braced hex escape the renderer writes for a symbol.

### PR 2 — `feat(base): GHC.TypeError` — landed

- `GHC.TypeError`: `ErrorMessage` (`Text`, `ShowType`, `:<>:`, `:$$:` with
  GHC's fixities), `TypeError` as an equation-less family, `Assert`,
  `Unsatisfiable`/`unsatisfiable`. GHC declares these in
  `GHC.Internal.TypeError` and re-exports them; `aihc-internal` depends on
  `aihc-base` rather than the other way round, so the declarations live in
  `GHC.TypeError` and the internal module re-exports them. Promoting the
  existential `ShowType` turned out to need nothing new.
- `Tc.Solve.Dict`: a wanted whose head is `TypeError` is reported as the
  message its argument spells, through new `tcWiringTypeErrorFamily` and
  `tcWiringErrorMessageCons` entries. Like the recognized deriving classes
  these are module-and-name pairs with no package, so the compiler stays
  free of library identities.
- Three compiler gaps had to be fixed on the way, each with its own
  fixture: a `_` on a family equation's left-hand side took a fresh
  `TYPE rep` instead of the parameter kind and then survived as a meta into
  FC, so `Assert 'True _ = ()` could not be written at all; a family's kind
  scheme dropped the variables its standalone signature quantified, so
  `TypeError :: forall b. ErrorMessage -> b` emitted a result kind nothing
  bound; and an irreducible constraint was converted with no expected kind,
  so a family polymorphic in its result kind had no kind argument.
- Tests: an annotated fixture for a demanded assertion, and an install
  `code-quality` fixture whose `expect-error` is the rendered text.

**Not done here.** GHC reports a `TypeError` that stands as a *given* of a
function signature at the declaration, and defers one in an instance
context until the instance is selected. This PR reports only the demanded
case, which is what `(<=)` needs. A never-demanded given is still carried
into FC, where the lint compares the `Type` synonym against its expansion
and rejects it; reporting givens the way GHC does removes that path.

### PR 3 — `feat(base): KnownNat, KnownSymbol and GHC.TypeNats`

- `Aihc.Tc.Evidence`: `EvTypeLit`, with `Tc.Finalize` and `Tc.Solve.Dict`
  cases beside the `Typeable` ones.
- `Fc.Desugar.Value.desugarTypeLitEvidence`, building
  `$Dict$KnownNat @n (naturalFromInteger …)` — reusing
  `desugarIntegerLiteral`, which already handles values past `maxWord` —
  and `$Dict$KnownSymbol @s (unpackCString# …)`.
- `GHC.Internal.TypeNats` gets `Natural`, `Nat`, `KnownNat`, `natVal`,
  `natVal'`, `SNat`, `withSomeSNat`, `SomeNat`, `someNatVal`, `fromSNat`;
  `GHC.TypeNats` re-exports, per the 9.12 layering. Same for
  `GHC.Internal.TypeLits`/`GHC.TypeLits` (`Symbol`, `KnownSymbol`,
  `symbolVal`, `symbolVal'`, `SSymbol`, `SomeSymbol`, `someSymbolVal`, and the
  `Integer`-returning `natVal` that 9.12's `GHC.TypeLits` exports).
- Tests: annotated fixtures for `natVal (Proxy :: Proxy 3)`; an eval fixture
  printing the result; a local reproducer package built with `aihc build`.

`natVal'` takes a `Proxy#`. Its *definition* needs nothing new; only a call
site written as `natVal' (proxy# :: Proxy# (SeedSize g))` needs the parallel
unlifted-expression-signature fix. PR 3 does not block on it; PR 5 does.

### PR 4 — `feat(tc): type-level comparison and arithmetic`

- `tcWiringTypeNatFamilies` and the builtin reduction in
  `Tc.Solve.Family.reduceHead`, applied only when every argument is a literal.
- Kind-indexed family instance matching, so `Compare`'s `Natural`, `Symbol`
  and `Char` instances are told apart.
- `Data.Type.Ord` with `Compare`, `OrderingI`, `OrdCond`, `(<=?)`, `(<=)`,
  `(<)`, `(>=)`, `(>)`, `Max`, `Min`, verbatim from 9.12; `GHC.TypeNats`
  declarations for `CmpNat`, `(+)`, `(-)`, `(*)`, `(^)`, `Div`, `Mod`,
  `Log2`, and the `GHC.TypeLits` re-exports.
- Tests: annotated fixtures discharging `1 <= 4`, `2 + 3 ~ 5`,
  `CmpNat 1 2 ~ LT`, a literal-reducing associated family, and a failing
  `4 <= 2` asserting GHC's "Cannot satisfy" text.

Inverting an application — GHC's solving of `n + 1 ~ 5` for `n` — stays out
of scope. `random` needs only forward reduction.

### PR 5 — `feat(core-libs): resolve and check random's SeedGen`

- Verify `System/Random/Seed.hs` (read-only, in `~/.cache/aihc/hackage`)
  resolves and type-checks; record what the install fails on next in
  `docs/hackage-install-packages.md`.
- Depends on the parallel unlifted-expression-signature task for the
  `proxy# :: Proxy# (SeedSize g)` call site.

## Traps

- Editing a core-libs `.hs` does **not** invalidate its `~/.cache/aihc` store
  entry — the fingerprint ignores Haskell source content. Delete the entry, or
  use `install --store <scratch>` plus `rm -rf`.
- New exports in modules shared with `ghc-prim`/`base` trip the core-libs API
  divergence ratchet in dev-spec. `Symbol` in `GHC.Types` and the
  `GHC.TypeNats`/`GHC.TypeLits` names all exist in GHC 9.12, so they should
  *close* divergences; check with `--divergences`.
- `cabal build all` fails on `aihc-prim` (GHC.Prim). Build `exe:aihc`, test
  `aihc:spec` and the `aihc-tc` suite specifically.
- Any change to the FC type or to a stamp shape needs
  `packageArtifactFormatVersion` bumped. PR 0 and PR 1 each change a type
  shape; the later PRs do not.
- The FC golden fixtures have no accept flag; paste the tasty `actual:` block
  into the yaml.
