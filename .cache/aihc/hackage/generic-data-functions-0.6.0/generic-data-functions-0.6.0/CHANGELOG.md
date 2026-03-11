## 0.6.0 (2024-06-15)
* support parsing constructor names on type-level for `foldMap`, `traverse`
  * effectively requires Symparsec-- but we need not incur a dependency
  * previous behaviour is still present using `*Raw` functions
* add `FOnCstr`, for targeting a single constructor of a sum type
* bump minimum compiler to GHC 9.2 (base-4.16) because I can't easily test GHC
  9.0
* big cleanup

## 0.5.1 (2024-04-10)
* remove spurious constraint in Traverse.Sum (should be same semantics)
* bump text upper bound for GHC 9.8 compatibility

## 0.5.0 (2024-04-05)
* Remove `SumOpts` type used for switching whether or not to permit "singleton
  sum types" for sum handlers. This was a type-level switch which changed
  runtime behaviour. generic-data-asserts provides type-level asserts which fail
  at compile time. Now you can't fail at runtime on a singleton sum -- you must
  either allow it, or fail at compile time -- but I don't see this as a problem.
* Add "checked constructor names" to generic traverse fail.
* Clean up some errors, `error` usage:
  * `V1` errors are handled using `absurdV1 :: V1 p -> a` (like `absurd :: Void
    -> a`) instead of error messages -- since I suppose that's what you want if
    you're thrusting void data types at us
  * except for generic traverse, where the user can choose to wrap it into their
    functor (e.g. as a parse error)

## 0.4.1 (2024-04-04)
* Remove generic representation asserts. We can handle them elsewhere without
  cluttering type signatures here. (I strongly recommend them, but that's not a
  reason to keep them here.)
* Unwrap `D1` in generic type classes rather than handing off to user. This was
  never really clever or useful, as it turns out. Saved a an extra type class
  per function, but requires the user to write more weird stuff.

## 0.3.1 (2024-04-03)
* fix error in traverse types

## 0.3.0 (2024-04-03)
* Use type tags, push actual target types into a type family. It means more
  required type annotations, but this is fine by me as I think this library
  should be implemented using GHC 9.10 `TypeApplications`.
* Swap `NoRec0`, `EmptyRec0` for uninstantiated data types. Less unwrapping,
  more consistent to the rest of the type-heavy interface. (They were labelled
  "via" but never used with `DerivingVia`.)

## 0.2.0 (2023-08-04)
* Redesign interface, pushing certain checks out of type classes into top-level
  generic function type signature. It means busier top-level types and more code
  for wrapping them, but it allows for more flexibility and cleans up
  implementation. (And the busyness simply makes explicit the implicit checks
  that were being done before.)

## 0.1.1 (2023-07-20)
  * add work-in-progress store-style generic `foldMap`, encoding constructors by
    their index, at `Generic.Data.Function.FoldMap.SumConsByte`

## 0.1.0 (2023-06-23)
Initial release.

  * extracted from binrep
