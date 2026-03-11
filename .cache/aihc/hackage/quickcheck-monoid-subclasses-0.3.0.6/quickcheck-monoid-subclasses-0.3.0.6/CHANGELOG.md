# 0.3.0.6

- Revised upper bounds for package dependencies.

# 0.3.0.5

- Added support for GHC `9.12`.

# 0.3.0.4

- Added support for GHC `9.10`.

# 0.3.0.3

- Revised upper bounds for package dependencies.

# 0.3.0.2

- Revised upper bounds for package dependencies.

# 0.3.0.1

- Added support for GHC `9.8`.

# 0.3.0.0

- Added laws for the following factorial semigroup and monoid classes:
    - `Data.Semigroup.Factorial.Factorial`
    - `Data.Semigroup.Factorial.StableFactorial`
    - `Data.Monoid.Factorial.FactorialMonoid`

- Added laws for the following GCD and LCM monoid classes:
    - `Data.Monoid.GCD.{Left,Right}DistributiveGCDMonoid`
    - `Data.Monoid.GCD.DistributiveGCDMonoid`
    - `Data.Monoid.LCM.DistributiveLCMMonoid`

- Added missing laws for the following class:
    - `Data.Monoid.GCD.OverlappingGCDMonoid`

- Removed `cancellativeGCDMonoidLaws`.
    - The documentation for the `GCDMonoid` class no longer states these laws.

# 0.2.0.0

- Improved generation of arbitrary `Semigroup` value combinations.
- Added derived laws for the `LeftGCDMonoid` type class.
- Added derived laws for the `RightGCDMonoid` type class.
- Removed hard-to-satisfy coverage check from `LCMMonoid` type class laws.
- Added support for building with GHC `9.6` series.

# 0.1.0.0

- Added laws for the `LCMMonoid` type class.
- Added laws for the `GCDMonoid` type class to match those of `LCMMonoid`.

# 0.0.0.1

- Revised lower dependency bound for the `vector` package.

# 0.0.0.0

- Provides support for testing instances of classes defined in the following
  modules:
    - `Data.Monoid.GCD`
    - `Data.Monoid.Monus`
    - `Data.Monoid.Null`
    - `Data.Semigroup.Cancellative`
