## 0.2 [2024.05.03]
* Drop support for GHC 7.10 and earlier.
* When building with GHC 9.10 or later, `Data.Tuple.Constraint` will simply
  re-export constraint tuple definitions found in `GHC.Classes`. If building
  with an older version of GHC, `Data.Tuple.Constraint` will backport the
  definitions from `GHC.Classes.
* Provide nullary constraint tuples named `CUnit` and unary constraint tuples
  named `CSolo` for consistency with `GHC.Classes` in GHC 9.10+.
* Provide constraint tuples up to size 64 when building with GHC 9.2 or later.
  (Earlier versions of GHC impose a maximum tuple size of 62.)

### 0.1.2 [2019.11.24]
* Introduce the `Data.Tuple.Constraint.{TypeFamily,TypeSynonym}` modules that
  provide ways of directly accessing constraint tuple type constructors through
  various means of type-level trickery.
* Make `Data.Tuple.Constraint.ClassNewtype.CTuple0` actually be a class
  newtype.

### 0.1.1 [2019.10.21]
* Split `generator-script` out of the main `.cabal` file, as it is only used
  for development purposes.
* Minor Haddock fixes.

## 0.1 [2019.10.14]
* First version.
