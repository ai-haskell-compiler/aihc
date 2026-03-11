# 0.2.0.0

  - Add support for GHC 8.4

  - Removed `type instance a == b = IsIntBaseTypeEq a b` as GHC 8.4 made `(==)` a closed type-family ([#3](https://github.com/hvr/int-cast/issues/3))

  - Improved Haddock documentation

----

## 0.1.2.0

  - Add support for GHC 7.10

  - Add support for `Natural`

## 0.1.1.0

  * Fix `intCastIso` to be reflexive wrt non-fixed integer types

  * Simplified `IsIntBaseTypeEq` equations

  * Add `Data.Type.Equality.(==)` instance for `IntBaseTypeK`

# 0.1.0.0

  * initial release

----
