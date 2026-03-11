# unboxing-vector: A newtype-friendly variant of unboxed vectors

This package provides newtype-friendly wrappers for `Data.Vector.Unboxed` in [`vector` package](http://hackage.haskell.org/package/vector).

## Description

Suppose you define a newtype for `Int` and want to store them in an unboxed vector.

```haskell
import qualified Data.Vector.Unboxed as Unboxed

newtype Foo = Foo Int

vec :: Unboxed.Vector Foo
vec = Unboxed.generate 10 (\i -> Foo i)
```

With classic `Data.Vector.Unboxed`, you either write _two dozen of lines_ of code to get it work (the exact code is [here](https://github.com/minoki/unboxing-vector/blob/3a152014b9660ef1e2885d6b9c66423064223f63/test/Foo.hs#L36-L63)), or resort to Template Haskell ([`vector-th-unbox` package](http://hackage.haskell.org/package/vector-th-unbox)) to generate it.

Now you have the third option, namely `Data.Vector.Unboxing`.
With `Data.Vector.Unboxing`, the amount of code you write is just _two lines_:

```haskell
import qualified Data.Vector.Unboxing as Unboxing

instance Unboxing.Unboxable Foo where
  type Rep Foo = Int

vec :: Unboxing.Vector Foo
vec = Unboxing.generate 10 (\i -> Foo i)
```

...and if you want to be even more concise, you can derive `Unboxable` instance with `GeneralizedNewtypeDeriving`.

Note that the vector type provided by this package (`Data.Vector.Unboxing.Vector`) is *different* from `Data.Vector.Unboxed.Vector`.
If you need to convert `Unboxing.Vector` to `Unboxed.Vector`, or vice versa, use `Unboxing.toUnboxedVector` or `Unboxing.fromUnboxedVector`.

The module defining the type `Foo` does not need to export its constructor to enable use of `Unboxing.Vector Foo`.
In that case, the users of the abstract data type cannot convert between `Unboxing.Vector Int` and `Unboxing.Vector Foo` --- the abstraction is kept!

## For non-newtypes

Suppose you define a data type isomorphic to a tuple, like:

```haskell
data ComplexDouble = MkComplexDouble {-# UNPACK #-} !Double {-# UNPACK #-} !Double
```

In this example, `ComplexDouble` is isomorphic to `(Double, Double)`, but has a different representation. Thus, you cannot derive `Unboxing.Unboxable` from `(Double, Double)`.

For such cases, unboxing-vector provides a feature to derive `Unboxable` using `Generic`.
Use `Unboxing.Generics` newtype wrapper to derive it.

```haskell
{-# LANGUAGE DeriveGeneric, DerivingVia, UndecidableInstances #-}

data ComplexDouble = MkComplexDouble {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving Generic
  deriving Data.Vector.Unboxing.Unboxable via Data.Vector.Unboxing.Generics ComplexDouble
```

Unboxing via `fromEnum`/`toEnum` is also available.
Use `Unboxing.Enum` or `Unboxing.EnumRep` to derive it.

```haskell
{-# LANGUAGE DerivingVia, UndecidableInstances #-}

data Direction = North | South | East | West
  deriving Enum
  deriving Unboxing.Unboxable via Unboxing.EnumRep Int8 Direction
```

## Conversion

### Conversion from/to Unboxed vector

You can use `fromUnboxedVector` and `toUnboxedVector` to convert one vector type to another.

```haskell
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxing as Unboxing

convert :: Unboxed.Vector Int -> Unboxing.Vector Int
convert vec = Unboxing.fromUnboxedVector vec
```

### Coercion between Unboxing vectors

You can use `coerceVector` to convert vector types of different element types, if they have the same representation and have appropriate data constructors in scope.

```haskell
import qualified Data.Vector.Unboxing as Unboxing
import Data.MonoTraversable (ofold)
import Data.Monoid (Sum(..), All, getAll)

sum :: Unboxing.Vector Int -> Int
sum vec = getSum $ ofold (Unboxing.coerceVector vec :: Unboxing.Vector (Sum Int)) -- OK

and :: Unboxing.Vector Bool -> Bool
and vec = getAll $ ofold (Unboxing.coerceVector vec :: Unboxing.Vector All) -- fails because the data constructor is not in scope
```

## Supported GHC versions

The library itself is tested with GHC 8.0.2 or later.

To use `GeneralizedNewtypeDeriving` on `Unboxable` class, you need GHC 8.2 or later,
because GND for associated type families became available on that version.

`DerivingVia` is avaliable since GHC 8.6.1.
This means that, defining an `Unboxable` instance for user-defined `data` types (like `ComplexDouble` or `Direction` in this document) requires a latest GHC.

If you want a way to make your `data` types on older GHCs, please [open an issue on GitHub](https://github.com/minoki/unboxing-vector/issues).
