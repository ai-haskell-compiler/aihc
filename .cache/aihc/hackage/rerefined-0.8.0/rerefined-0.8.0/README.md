# rerefined
[refined-nv-gh]:      https://github.com/nikita-volkov/refined
[refined-nv-hackage]: https://hackage.haskell.org/package/refined
[strongweak-hackage]: https://hackage.haskell.org/package/strongweak
[binrep-hackage]:     https://hackage.haskell.org/package/binrep
[refined1-hackage]:   https://hackage.haskell.org/package/refined1

Rewrite of Nikita Volkov's [refined][refined-nv-hackage] library.

* same concept
* same performance
* more instances
* better ergonomics (no insidious `Typeable` constraints)
* internals: fewer dependencies (no `aeson`), better errors, more concise

## Why?
I used the original [refined][refined-nv-hackage] library fairly extensively to
power other libraries (see [strongweak][strongweak-hackage],
[binrep][binrep-hackage]), though I moved to a fork [refined1][refined1-hackage]
some time ago to provide a feature I needed. I think the library has some flaws
and I want to contribute, but my tiny tweaks are still pending after a few
years. A good excuse to rewrite from the ground up.

All source code is original.

## Major changes from original refined
### Simplified errors
refined encoded the logical predicates in its error type. This doesn't enable
any further analysis, just turns a non-sum type into a sum type and complicates
consumption. Furthermore, this error type is first transformed into another
recursive ADT, which is then pretty printed. This is unnecessary (even mentioned
in the code).

rerefined has a single-constructor error type which can be easily and
efficiently turned into a string-like in a single pass.

### No insidious `Typeable` contexts
See [refined#101](https://github.com/nikita-volkov/refined/issues/101).
`Typeable` is useful, but the way it is used brings lots of `Typeable` contexts.

rerefined asks that you do a bit more work upfront, but gives you tools and
grants much more power. Predicates declare their "predicate name" explicitly as
a type-level `Symbol`. Precedence is supported and infix operators are welcomed.
(For now, the logical operators primarily look like their propositional logic
counterparts.)

### Cleaner design
What do `LessThan`, `GreaterThan`, `EqualTo` etc. have in common? They're all
relational binary operators where one value is a pre-filled `Natural`. rerefined
packs all of these into a single predicate that takes a type-level relational
operator. Only one instance for the same amount of code, and much easier to
reason about.

We take this even further and allow passing a type-level sign, to enable
comparing negative values.

We take this _even_ further and use the same relational operator definitions to
define length comparisons, where the other value is taken from the input's
length (rather than its numeric value). This does not take a sign, since length
must be non-negative.

### More instances
You know that length comparison predicate above? It has a _single instance_ for
each of `Refined1` and `Refined`:

```haskell
-- | Compare the length of a 'Foldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, Foldable f, ReifyRelOp op)
  => Refine1 (CompareLength op n) f where
    validate1 p = validateCompareLength p . length

-- | Compare the length of a 'MonoFoldable' to a type-level 'Natural' using the
--   given 'RelOp'.
instance (KnownNat n, MonoFoldable a, ReifyRelOp op)
  => Refine (CompareLength op n) a where
    validate p = validateCompareLength p . olength
```

We get a ton more instances for a ton less code. (Note that mono-foldable has a
surprisingly small footprint, as most of its transitive dependencies are core
libraries.)

## License
Provided under the MIT license. See `LICENSE` for license text.
