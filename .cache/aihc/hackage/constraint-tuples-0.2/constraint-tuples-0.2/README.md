# `constraint-tuples`
[![Hackage](https://img.shields.io/hackage/v/constraint-tuples.svg)][Hackage: constraint-tuples]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/constraint-tuples.svg)](http://packdeps.haskellers.com/reverse/constraint-tuples)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/constraint-tuples/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/constraint-tuples/actions?query=workflow%3AHaskell-CI)

[Hackage: constraint-tuples]:
  http://hackage.haskell.org/package/constraint-tuples
  "constraint-tuples package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This library provides classes and type aliases that emulate the behavior of
GHC's constraint tuple syntax. Unlike GHC's built-in constraint tuples, the
types in this library can be partially applied.

This library exposes four different modules that provide essentially the same
API with slight differences in their implementation:

* `Data.Tuple.Constraint`: A `CTupleN` class compiles to a dictionary data type
   with `N` fields. (When building with GHC 9.10 or later, this will simply
   re-export the constraint tuples offered by `GHC.Classes`.)
* `Data.Tuple.Constraint.ClassNewtype`: A `CTupleN` class compiles to a newtype
   around the corresponding built-in constraint tuple type with `N` arguments.
* `Data.Tuple.Constraint.TypeFamily`: A `CTupleN` type alias is a constraint
  tuple type constructor with `N` arguments obtained by way of a type family.
  This will compile to a built-in constraint tuple, but casted with a
  type family axiom.
* `Data.Tuple.Constraint.TypeSynonym`: A `CTupleN` type alias is a constraint
  tuple type constructor with `N` arguments obtained by way of a type synonym.
  This will compile directly to a built-in constraint tuple.
