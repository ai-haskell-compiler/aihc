### 0.3.1 [2026.01.10]
* Allow building with `template-haskell-2.24.*` (GHC 9.14).
* Remove unused `ghc-prim` dependency.

## 0.3 [2024.10.22]
* Drop support for pre-8.0 versions of GHC.
* Remove `genericLiftWithPkg`, `genericLiftWithPkgFallback`, and related
  functions. These functions were only useful for defining `Generic`-based
  `lift` implementations on pre-8.0 versions of GHC. Now that `lift-generics`
  no longer supports these versions of GHC, they are no longer necessary or
  useful. Use `genericLift` and friends instead.

## 0.2.1 [2021.09.16]
* The lifting functions have been modified to reduce splice
  time for constructors with many fields from quadratic to
  linear. This probably only has a modest impact on total
  compilation time.

* `genericLift`, `genericLiftTyped`, `genericLiftTypedExp`, and
  `genericLiftTypedCompat` functions now work for GHC 7.4 and above
  for types that derive `Typeable`.

* `genericLiftWithPkgFallback`, `genericLiftTypedWithPkgFallback`,
  `genericLiftTypedExpWithPkgFallback`, and
  `genericLiftTypedCompatWithPkgFallback` functions have been added that take
  advantage of `Typeable` instances for GHC 7.4 and above but also take a
  user-provided package name for earlier versions.

## 0.2 [2020.09.30]
* `genericLiftTyped` and `genericLiftTypedWithPkg` now return a `Code` instead
  of a `TExp` to reflect the type of `liftTyped` changing in
  `template-haskell-2.17.0.0`. New functions `genericLiftTypedTExp` and
  `genericLiftTypedTExpWithPkg` have been added for those who wish to return
  `TExp` specifically. In addition, the functions `genericLiftTypedCompat` and
  `genericLiftTypedCompatWithPkg` have been introduced which return a `Code`
  on `template-haskell-2.17.0.0` or later, but a `TExp` on older versions of
  `template-haskell`. These functions are most useful for implementing
  `liftTyped` in a `Lift` instance in a backwards-compatible way.

  The `th-compat` library is used to backport the `Code` data type back to
  versions of `template-haskell` that do not define it.
* The functions in `Language.Haskell.TH.Lift.Generics` are now generalized to
  work over any `Quote` instance instead of hardcoding `Q`. Again, the
  `th-compat` library is used to backport `Quote` to old versions of
  `template-haskell` that do not define it.
* Make `genericLift` work properly for empty data types.

### 0.1.3 [2019.11.26]
* Add `genericLiftTyped` and `genericLiftTypedWithPkg`.

### 0.1.2
* Add a case for `V1` (i.e., empty data types), which diverges in the way
  you'd expect.

### 0.1.1
* Fix test suite on GHC 8.0

## 0.1
* Initial commit
