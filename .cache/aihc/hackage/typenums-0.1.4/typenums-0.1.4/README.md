# typenums

[![Hackage](https://img.shields.io/hackage/v/typenums.svg)](https://hackage.haskell.org/package/typenums)
![example workflow](https://github.com/adituv/typenums/actions/workflows/haskell-ci.yml/badge.svg)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/adituv/typenums/blob/master/LICENSE)

Type level numbers using existing Nat functionality. Uses kind-polymorphic
typeclasses and type families to facilitate more general code compatible with
existing code using type-level Naturals.

## Usage
Import either Data.TypeNums or Data.TypeLits instead of GHC.TypeLits.  Some
definitions conflict with GHC.TypeLits, so if you really must import it, use
an explicit import list.

This library is intended to be used in a kind-polymorphic way, such that
a type-level integer parameter can be written as a natural, and a rational
can be written as either of the other two.  As an example:

```haskell
{-# LANGUAGE PolyKinds #-}

data SomeType (n :: k) = SomeType

useSomeType :: KnownInt n => SomeType n -> _
useSomeType = -- ...
```

## Syntax
* Positive integers are written as natural numbers, as before.  Optionally
  they can also be written as `Pos n`.
* Negative integers are written as `Neg n`.
* Ratios are written as `n :% d`, where `n` can be a natural number, `Pos n`,
  or `Neg n`, and `d` is a natural number.

Addition, subtraction and multiplication at type level are all given as infix
operators with standard notation, and are compatible with any combination of
the above types.  Equality and comparison constraints are likewise available
for any combination of the above types.

N.B. The equality constraint conflicts with that in Data.Type.Equality.  The
(==) operator from Data.Type.Equality is re-exported as (==?) from both
Data.TypeNums and Data.TypeLits.
