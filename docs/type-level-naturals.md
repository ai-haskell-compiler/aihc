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

and `Aihc.Fc.Syntax.Type` gains a matching `TyLit !TyLit`.

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
`symbolVal` ride along in the same stack (PR 2). `TyLitChar` gets the
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

### `<=` uses the pre-9.4 definition

GHC 9.12 defines `type x <= y = Assert (x <=? y) (LE x y)`, which needs
`GHC.TypeError` — itself an empty module here. This stack defines
`type x <= y = (x <=? y) ~ True` instead. The *exports* of `GHC.TypeNats`
and `GHC.TypeLits` still mirror 9.12, which is what the core-libs API
divergence ratchet checks; only the definition differs. Wiring `TypeError`
into the ratchet-visible modules is a follow-up.

### Arithmetic reduces in the solver, not through equations

`+`, `*`, `-`, `Div`, `Mod`, `Log2`, `CmpNat` and `OrdCond` are declared as
equation-less `type family`s in `GHC.TypeNats` and reduced by a new builtin
case in `Tc.Solve.Family.reduceHead`, keyed on type-constructor identity
through a new `tcWiringTypeNatFamilies` table in `Aihc.Prim.Wiring` (the type
checker knows no library; every such name reaches it as wiring). Reduction
applies only when every argument is a literal.

Inverting an application — GHC's solving of `n + 1 ~ 5` for `n` — is **out of
scope**. `random` needs `1 <= SeedSize g` with `SeedSize g` reducing to a
literal, which forward reduction covers.

## PR stack

Each PR is independently buildable and testable. Conventional Commits; branch
per PR off the previous one.

### PR 1 — `feat(tc): type-level literals in the kind checker`

The whole plumbing change, no user-visible feature beyond kind-checking a
literal.

- `Aihc.Tc.Types`: `TyLit`, `TcTyLit`, plus the `TypeShape` counterpart
  (`ShapeTyLit`) so structural `Eq`/`Ord` stay honest.
- `Aihc.Tc.Kind.convertNonSynonymTypeWithKinds`: a `TTypeLit` case returning
  the literal and its kind, replacing part of the unsupported fallthrough.
  Kind constructors come from new `tcWiringNaturalTyCon` / `tcWiringSymbolTyCon`
  entries in `TcWiring` (`Aihc.Prim.Wiring`).
- `Aihc.Tc.Unify`, `Zonk`, `Tidy`, `Solve.Decompose`, `Solve.Congruence`:
  literals unify only with equal literals.
- `Aihc.Fc.Syntax`: `TyLit`; every FC consumer listed above.
- `Aihc.Fc.Lint.lintType`: a literal's kind is its sort's type constructor.
- `Aihc.Fc.Parser`/`Pretty`: a round-trippable syntax for literals (the FC
  name-sort prefix convention is text-only, so the literal needs its own
  token shape; regenerate the FC golden fixtures from the tasty `actual:`
  blocks).
- `Cli.TypeArtifact` CBOR codec; bump `packageArtifactFormatVersion`.
- `core-libs/aihc-prim/src/GHC/Types.hs`: `data Symbol`, exported.
- Tests: annotated fixtures under
  `components/aihc-tc/test/Test/Fixtures/annotated/` for a literal in a kind
  signature, a literal argument to a data type, a literal in a type synonym,
  and a kind mismatch (`Vec "x"` where `Vec :: Natural -> Type`); an FC lint
  pass fixture; an FC parser round-trip property.

### PR 2 — `feat(base): KnownNat, KnownSymbol and GHC.TypeNats`

- `Aihc.Tc.Evidence`: `EvTypeLit`; `Tc.Finalize` and `Tc.Solve.Dict` cases
  alongside the `Typeable` ones.
- `Fc.Desugar.Value`: `desugarTypeLitEvidence`, building
  `$Dict$KnownNat @n (naturalFromInteger …)` and
  `$Dict$KnownSymbol @s (unpackCString# …)`.
- `GHC.Internal.TypeNats` gets the real definitions (`Natural`, `Nat`,
  `KnownNat`, `natVal`, `natVal'`, `SNat`, `withSomeSNat`, `SomeNat`,
  `someNatVal`, `fromSNat`); `GHC.TypeNats` re-exports, per the 9.12 layering.
  Same for `GHC.Internal.TypeLits`/`GHC.TypeLits` (`Symbol`, `KnownSymbol`,
  `symbolVal`, `symbolVal'`, `SSymbol`, `SomeSymbol`, `someSymbolVal`), which
  also re-exports the `GHC.TypeNats` names that 9.12's `GHC.TypeLits` exports
  with `Integer`-returning `natVal`.
- Tests: annotated fixtures for `natVal (Proxy :: Proxy 3)`; an eval fixture
  printing `natVal`'s result; a local reproducer package built with
  `aihc build`.

`natVal'` takes a `Proxy#`. Its *definition* needs nothing new; only a call
site written as `natVal' (proxy# :: Proxy# (SeedSize g))` needs the parallel
unlifted-expression-signature fix. PR 2 does not block on it; PR 4 does.

### PR 3 — `feat(tc): type-level natural arithmetic and comparison`

- `tcWiringTypeNatFamilies` and the builtin reduction in
  `Tc.Solve.Family.reduceHead`.
- `GHC.TypeNats` declarations for `CmpNat`, `OrdCond`, `(<=?)`, `(<=)`,
  `(+)`, `(-)`, `(*)`, `Div`, `Mod`, `Log2`, and the `GHC.TypeLits`
  re-exports.
- Tests: annotated fixtures discharging `1 <= 4`, `2 + 3 ~ 5`,
  `CmpNat 1 2 ~ LT`, a literal-reducing type family (`SeedSize`-shaped), and
  a *failing* `1 <= 0` with a readable message.

### PR 4 — `feat(core-libs): resolve and check random's SeedGen`

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
  `packageArtifactFormatVersion` bumped (PR 1 does it once; PR 2 and PR 3 add
  no type shape).
- The FC golden fixtures have no accept flag; paste the tasty `actual:` block
  into the yaml.
