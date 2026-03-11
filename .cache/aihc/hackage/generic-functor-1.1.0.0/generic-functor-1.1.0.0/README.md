# Generic functors [![Hackage](https://img.shields.io/hackage/v/generic-functor.svg)](https://hackage.haskell.org/package/generic-functor) [![pipeline status](https://gitlab.com/lysxia/generic-functor/badges/main/pipeline.svg)](https://gitlab.com/lysxia/generic-functor/-/commits/main)

Implementation of `Functor` instances and other functor-like structures
using `GHC.Generics`.

## Deriving `Functor`

This library enables `DerivingVia` to derive the `Functor` class generically,
via the newtype `GenericFunctor`.

```haskell
{-# LANGUAGE DeriveGeneric, DerivingVia #-}

import GHC.Generics (Generic)
import Generic.Functor (GenericFunctor(..))

data Twice a = Twice (Either a a)
  deriving Generic
  deriving Functor via (GenericFunctor Twice)
```

Note that there is already built-in support for deriving `Functor` in GHC with the
`DeriveFunctor` extension instead. If that extension ever chokes on a type, this
library might have a chance at handling it. (Open an issue if it does not!)

The `Twice` example just above is not handled by the `DeriveFunctor` extension:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data Twice a = Twice (Either a a) deriving Functor

{-
  error:
    • Can't make a derived instance of ‘Functor Twice’:
        Constructor ‘Twice’ must use the type variable only as the last argument of a data type
-}
```

The [*generic-data*][generic-data] library also includes a generic implementation of `Functor`,
but only for instances of `Generic1`, which applies to much more restricted shapes
of `data` than `Generic`.

## Deriving `Bifunctor`

Similarly, we can use `DerivingVia` for the `Bifunctor` class
(from *base*, module `Data.Bifunctor`).

```haskell
{-# LANGUAGE DeriveGeneric, DerivingVia #-}

import GHC.Generics (Generic)
import Generic.Functor (GenericFunctor(..), GenericBifunctor(..))

data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b
  deriving Generic
  deriving Functor via (GenericFunctor (Tree a))
  deriving Bifunctor via (GenericBifunctor Tree)
```

In summary, the newtype `GenericFunctor` can be used for `DerivingVia`
of the classes `Functor` and `Foldable`, and the newtype `GenericBifunctor`
for the classes `Bifunctor` and `Bifoldable`.

Default implementations for the above classes are also available as standalone
functions (`gfmap`, `gfoldMap`, `gbimap`, `gbifoldMap`) and also for
`Traversable` and `Bitraversable` (`gtraverse`, `gbitraverse`).

## Functors not over the last type parameter

The standard `Functor` class only applies to types that are functors over their
last type parameter. For example, in `Either e r`, `fmap` maps only `r`.

Using this library, `fmap`-like functions can be derived over any type
parameter of a `Generic` data type, all from the same definition `gsolomap`.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Generic.Functor.Multimap (gsolomap)

data Result a r = Error a | Ok r   -- Another name for Either
  deriving Generic

mapError :: (a -> b) -> Result a r -> Result b r
mapError = gsolomap

-- This one is fmap
mapOk :: (a -> b) -> Result e a -> Result e b
mapOk = gsolomap

mapBoth :: (a -> b) -> Result a a -> Result b b
mapBoth = gsolomap
```

`gsolomap` is **unsafe**. Misuse will break your program.
Read on for specifics.

### Usage

`gsolomap` should only be used to define **polymorphic** "`fmap`-like functions"
for `Generic` types.

The signature of `gsolomap` is:

```haskell
gsolomap :: (Generic x, Generic y, GSolomap a b x y) => (a -> b) -> (x -> y)
```

The types `x` and `y` must be specializations of the same user-defined `data`
type which is an instance of `Generic`, with some type parameters equal to `a`
or `b` respectively. At use sites of `gsolomap`, `a` and `b` must also be two
distinct universally quantified type variables, with no equality constraint
relating them with each other or any other type.

The guarantee is that `gsolomap` satisfies `gsolomap id = id`. Under the
condition that `a` and `b` are abstract, that equation uniquely determines the
implementation. (That uniqueness claim may be broken with GADTs and other
explicit uses of type equality constraints.)

In particular, `gsolomap` *must not* be specialized with types `a` and `b` that
are equal. A function defined using `gsolomap` is safe to specialize once
the `GSolomap` constraint has been discharged.

For instance the three functions above, `mapError`, `mapOk`, `mapBoth` are
sufficiently polymorphic.
They are each uniquely determined by their types and the equation `mapX id = id`.
(Without that equation, `mapBoth` has four implementations of the same type.)

## Compositions of functors

How many `fmap` do you need to map a function `a -> b` over
`(t, Maybe [Either Bool a])`?

You only need one `solomap`:

```haskell
type F t a = (t, Maybe [Either Bool a])

maps :: (a -> b) -> F t a -> F t b
maps = solomap
```

`solomap` can also see through bifunctors and there may be more than
one occurrence of the type parameter `a`.

```haskell
type F a = ([a], Either a ())

maps2 :: (a -> b) -> F a -> F b
maps2 = solomap
```

`solomap` is **unsafe**, subject to the same restrictions as `gsolomap`:
where `solomap` is used, the type of its first argument `(a -> b)` must refer
to two distinct universally quantified variables `a` and `b`.
Functions are safe to specialize only once the `Solomap` constraint is out of
their contexts.

```haskell
solomap :: Solomap a b x y => (a -> b) -> (x -> y)
```

## Functors of multiple parameters

You can also map with more than one function simultaneously.
For example with `a -> b` and `c -> d` over `(Maybe a, [(c, a)])`:

```haskell
type F a c = (Maybe a, [(c, a)])

bimaps :: (a -> b) -> (c -> d) -> F a c -> F b d
bimaps f g = multimap (f :+ g :+ ())
```

`multimap` takes a list of functions separated by `(:+)` and terminated by `()`.

There is also a `gmultimap`, generalizing `gsolomap`.

`gmultimap` and `multimap` are **unsafe**, similarly to `gsolomap` and `solomap`.


---

## Internal module policy

The public API is `Generic.Functor`. Don't use `Generic.Functor.Internal`.

## Future work

- Functors in arbitrary categories.

## Related links

- [*generic-data*][generic-data]: utilities for `GHC.Generics` and deriving for
  other standard classes.

- [*generic-lens*][generic-lens]: the `params` traversal uses a very similar implementation.

- [*one-liner*][one-liner]. [*product-profunctors*][pp]

- [*Deriving Bifunctors with Generics*](https://kcsongor.github.io/generic-deriving-bifunctor/),
  blogpost by Csongor Kiss,
  describing the main idea for the implementation (using incoherent instances).

[generic-data]: https://hackage.haskell.org/package/generic-data
[generic-lens]: https://hackage.haskell.org/package/generic-lens
[one-liner]: https://hackage.haskell.org/package/one-liner
[pp]: https://hackage.haskell.org/package/product-profunctors
