# Changelog for unboxing-vector

## Version 0.2.0.0 (2020-09-27)

- Add coercion functions for mutable vectors.
- Add `Unboxable` instance for several mode types: `Data.Semigroup.Arg`, `Data.Monoid.Alt`, `Data.Functor.Compose`.
- The dependency on mono-traversable can be disabled via a package flag.

## Version 0.1.1.0 (2019-07-01)

- Support older GHC versions.
- Add `Enum` and `EnumRep` wrappers.
- Add `Unboxable` instance for `Ordering`.

## Version 0.1.0.0 (2019-06-17)

Initial release with

- Data.Vector.Unboxing
- Data.Vector.Unboxing.Mutable
