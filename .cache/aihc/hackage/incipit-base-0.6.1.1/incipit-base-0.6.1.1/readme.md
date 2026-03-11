# About

This library is part of [incipit], a set of `Prelude` modules for the [Polysemy] ecosystem.
It contains the reexports from `base` that are inherited by the other two packages.

# Usage

Using a custom `Prelude` requires the use of Cabal mixins to hide the module from `base` and replace it with
`IncipitBase`:

For `hpack`:
```yaml
dependencies:
  - name: base
    version: '>= 4 && < 5'
    mixin:
      - hiding (Prelude)
  - name: incipit-base
    version: '>= 0.3'
    mixin:
      - (IncipitBase as Prelude)
      - hiding (IncipitBase)
```

For `cabal`:
```cabal
build-depends:
    base >=4 && <5, incipit-base >= 0.3
mixins:
    base hiding (Prelude), incipit-base (IncipitBase as Prelude), incipit-base hiding (IncipitBase)
```

`incipit-base` used to export `Prelude`, but
[stack can't deal with that](https://github.com/commercialhaskell/stack/issues/5414).

[incipit]: https://hackage.haskell.org/package/incipit
[Polysemy]: https://hackage.haskell.org/package/polysemy
