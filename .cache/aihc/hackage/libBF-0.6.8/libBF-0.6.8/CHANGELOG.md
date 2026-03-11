# Revision history for libBF-hs

## 0.6.8 -- 2024.06.05

* Fix a bug that would cause `(>)` and `(>=)` (in the `Ord BigFloat` instance)
  to return incorrect answers when one of the arguments is NaN.
* Fix a bug which could cause incorrect behavior (e.g., infinite loops) on
  32-bit architectures.

## 0.6.7 -- 2023.11.28

* Add `Data` instances for `BigFloat`, `Sign`, `BFRep`, and `BFNum`.

## 0.6.6 -- 2023.07.17

* Update the vendored version of `libbf` to include the changes from the
  `2021-03-27` version of `libbf` included in `quickjs`. This includes a bugfix
  that prevents the `bfIsSubnormal` function from infinitely looping on AArch64
  (and other non-x86 64-bit architectures) on certain inputs.

## 0.6.5.1 -- 2022.08.29

* Fix a build error on non-Windows OSes accidentally introduced in version
  0.6.5.

## 0.6.5 -- 2022.08.29

* Force static linking against `gcc_s` on Windows to avoid portability issues.

## 0.6.4 -- 2022.08.11

* Fix the build on Windows with GHC 9.4, which bundles a Clang-based C
  toolchain instead of a GCC-based one.

## 0.6.3 -- 2021-10-14

* Fix an additional compile issue on 32-bit systems. Previously
the argument to fmul2Exp was incorrectly specified as an `Int64`.

## 0.6.2 -- 2021-02-19

* Fix a bug related to incorrect marshalling of String to C in
  `setString` and `bfFromString`.

## 0.6.1 -- 2021-02-16

* Fix a linker issue on Windows when loading via the GHC dynamic linker.

## 0.6 -- 2021-01-29

* Fix a bug with `frint` and `bfRoundInt` that was causing incorrect
rounding modes to be selected.

* Implement additional operations on `BigFloat` values, including
some missing predicates, conversions to and from binary form
and fused-multiply-add.

* Fix a compile issue on 32-bit systems.

## 0.5.2 -- 2021-01-12

* Use `install-includes` over `c-sources` for header files to avoid linker
issues.

## 0.5.1 -- 2020-07-13

* Add header files to `c-sources` field to include them in `sdist`.

## 0.5.0 -- 2020-07-01

* First version. Released on an unsuspecting world.
