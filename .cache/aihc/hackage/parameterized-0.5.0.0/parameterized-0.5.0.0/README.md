[![Hackage](https://img.shields.io/hackage/v/parameterized.svg)](https://hackage.haskell.org/package/parameterized)
[![Build Status](https://secure.travis-ci.org/louispan/parameterized.png?branch=master)](http://travis-ci.org/louispan/parameterized)

Parameterized/indexed monoids and monads using only a single parameter type variable.

Refer to [ReaderSpec.hs](https://github.com/louispan/parameterized/blob/master/test/Parameterized/Control/Monad/Trans/Reader/ReaderSpec.hs) and [StateSpec.hs](https://github.com/louispan/parameterized/blob/master/test/Parameterized/Control/Monad/Trans/State/Strict/StateSpec.hs) for example usages.

# Changelog

* 0.5.0.0
  - Support data-diverse-2.0.0.0.

* 0.4.0.0
  - Added PNullary and PUnary injective type families.

* 0.3.0.0
  - PMonoid is now a class with a single instance (courtesy of georgew).
  - added fixities for backtick versions of pmappend, pappend, papply and pbind
  - added changingState constructor which sults in better type inference.

* 0.2.0.0
  - Renamed Pempty to PEmpty.
  - Added injective functional dependencies to PMEmpty, and PEmpty.
  - TypeLevel is not exported by default

* 0.1.0.0
  - Initial version with parameterized Semigroup, Monoid, Applicative, Alternative, Monad
  - Added instances for OverlappingWhichReader, DistinctWhichReader, ManyReader, ManyState, and ChangingState
