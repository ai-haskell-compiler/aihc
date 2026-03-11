one-liner-instances
===================

This package uses machinery from *[one-liner][]* in order to provide default
implementations for methods from `Num`, `Fractional`, `Floating`, `Semigroup`,
`Monoid`, `Bounded`, `Eq`, `Ord`, and `Random`.  These will work for any types
(deriving `Generic`) whose fields are all instances of that typeclass.

For `Num`, `Fractional`, `Floating`, `Semigroup`, and `Monoid`, the types also
must have only a single constructor.  `Random` methods offer variants with
single constructors (for performance) and with multiple constructors.

[one-liner]: https://hackage.haskell.org/package/one-liner

So, `gPlus` (generic addition) will work for:

```haskell
data Tup1 a b = Tup1 a b            -- requires Num a, Num b
data Tup2 a   = Tup2 Int a          -- requires Num a, Num b
data Tup3     = Tup3 Int Double
data Tup4 a b = Tup4 Int Double     -- no constraint on a or b
```

But not on:

```haskell
data Tup5 a   = Tup2 String a       -- String is not an instance of Num
```

These are implemented by applying the operation to every field.

Newtype wrappers
----------------

Similar to `WrappedMonoid` and `WarppedMonad` from *base*, some convenient
newtype wrappers are provided that will give free instances of `Num`, etc. for
appropriate types:

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Num`, then `GNum a` has a `Num` instance (and
same for `Fractional`, `Floating`, etc.).

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Semigroup`, then `GMonoid a` has a `Semigroup`
instance (and same for `Monoid`).

If `a` is a data type (deriving `Generic`) whose fields all have instances of
`Bounded`, then `GBounded a` has a `Bounded` instance.

If `a` is a data type (deriving `Generic`) whose fields all have instances of
`Eq`, then `GOrd a` has a `Eq` instance (and same for
`Ord`).

Comparisons
-----------

This package provides very similar functionality to *[generic-deriving][]*.

[generic-deriving]: http://hackage.haskell.org/package/generic-deriving

There are a few major design differences between *generic-deriving* and
*one-liner*, the package that this one is built on.

*generic-deriving* creates a *separate* "deriving" typeclass for every
typeclass one wants to generalize.  So, there is a separate `GMonoid`
typeclass, a separate `GEnum` typeclass, etc.

*one-liner* instead creates a single typeclass (`ADTRecord` and `Constraints`)
to unify all generalizable typeclasses.  Both the generic `Monoid` and generic
`Num` instances are built upon the same `Constraints` typeclass.  From a
usability standpoint, *one-liner* allows one to easily create generic versions
of their own, custom typeclasses -- something that *generic-deriving* does not
help with.

*one-liner-instances*, however, is simply a package using the *one-liner*
engine to provide generic instances for common classes where it is possible.

The main difference in practical usability between *one-liner-instances* and
*generic-deriving* themselves are few, but are mainly:

*   *one-liner-instances* has generic implementations for
    `Num`/`Fractional`/`Floating`, and *generic-deriving* doesn't.  This is a
    superficial difference, however, since nothing fundamental is preventing
    *generic-deriving* from adding them in the future.
*   *one-liner-instances* provides newtype wrappers that can automatically
    imbue appropriate types with instances, which can be used with the upcoming
    [DerivingVia][] syntax to automatically derive instances, or just used on
    their own for convenience purposes.

    *generic-deriving* does not aim to do this at this moment.
*   Integrates with the rest of the *one-liner* ecosystem, if one is already
    using it to provide constraints for custom typeclasses.

[DerivingVia]: https://twitter.com/Iceland_jack/status/959923603096719360
