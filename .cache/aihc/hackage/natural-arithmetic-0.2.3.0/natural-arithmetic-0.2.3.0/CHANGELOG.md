# Revision history for natural-arithmetic

## 0.2.3.0 -- 2025-06-12

* Add Fin.greatest#
* Add Plus.commutative#
* Add substitution functions for unlifted less-than-or-equal-to
* Add Nat.substitute#
* Add Eq.symmetric#
* Add nativeFrom32#
* Add incrementL#
* Add testLessThanEqual#
* Add nativeTo32#
* Add Fin.ascendFromToM_#
* Add Fin.weaken#

## 0.2.2.0 -- 2025-04-07

* Add Fin.weakenL#
* Add Fin.weakenR#
* Add Fin.demote32#
* Add Fin.constant#
* Add Fin.equals#
* Add more nat constants (N8192# and N16384#)
* Add EitherFin#

## 0.2.1.0 -- 2024-02-02

* Add `fromInt` and `fromInt#` to `Arithmetic.Fin`.
* Update package metadata.

## 0.2.0.0 -- 2024-01-09

* Change the types of `with#` and `construct#` (both in `Arithmetic.Fin`)
  to use an unboxed "less than" instead of a boxed one. This is a breaking
  change.
* Add patterns for the natural numbers 5, 6, 7.
* Add a lot of primitives for working with unboxed natural and inequalities.
  GHC is unable to eliminate the boxed `Fin` type in a suprisingly large
  number of cases, and `Fin#` helps a lot with this.

## 0.1.4.0 -- 2023-05-31

* Add unboxed Nat type
* Add nominal role for Fin# type constructor. Technically, this is a breaking
  change, but if anyone was using coerce on a Fin#, they were already in a
  bunch of trouble. So, there is not going to be a major version bump for this.

## 0.1.3.0 -- 2022-05-23

* Add strict variant of descend.
* Add unboxed Fin type

## 0.1.2.0 -- 2019-01-20

* Add strict variant of descend.

## 0.1.1.0 -- 2019-11-22

* Undocumented

## 0.1.0.0 -- 2019-09-04

* Initial release.
