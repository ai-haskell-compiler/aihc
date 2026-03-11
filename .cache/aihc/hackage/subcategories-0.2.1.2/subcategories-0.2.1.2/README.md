# The `subcategories` package

![Haskell CI](https://github.com/konn/subcategories/workflows/Haskell%20CI/badge.svg)
![Hackage](https://img.shields.io/hackage/v/subcategories)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/subcategories)

This package provides variants of functor-like structures,
with domain types are constrained.
In particular, this package provides an abstraction for functorial
containers, which can be expressed as a functor from a *full-subcategory*
of **Hask** to **Hask** itself [^1].

For example:

- We can treat `Set` as if it is a `Fuctor`, `Foldable`, `Applicative`,
  with their domain restricted to full-subcategory **Ord** of `Ord` instances
  of **Hask**.
- For `MonoFoldable` or `MonoTraversable` types (from `mono-traversable` package),
  we provide `WrapMono` wrapper with zero-cost coercion. Such `mono`s can be
  regarded as a functorial structure from the full subcategory consisting of just a single object,
  say `Element mono`.

[^1]: Strictly speaking, `CFoldable`, a constrained counterpart of `Foldable`, doesn't require a functoriality as with the original `Foldable`.

## Optimisation
This library is designed to keep the abstraction runtime overhead as minimum as possible.

Some notes:

- If a constrained term such as `cmap` or `czipWith` has concrete type, it must have exactly the same representation as the corresponding operation modulo (zero-cost) coercion.
  * The same still holds if the set of required constraints coincides.
  * Although the constructor of `WrapMono mono a` is hidden, its just a `newtype`-wrapper around `mono`;
    hence, constrained operators must have the same representations as the corresponding combinators
    in `mono-traversable` package.
- OTOH, for a polymorphic term, like `cmap :: (Ord a, Ord b) => (a -> b) Set a -> Set b`
  and `Set.map`, they can have different representations; indeed, `Set.map` doesn't require `a` to be `Ord`-instance and therefore the implementation of `cmap` discards the dictionary for `Ord a` to call `Set.map`.