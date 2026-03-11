# Revision history for commutative-semigroups

## 0.2.0.2 -- 2025-03-01

- Loosen dependency bounds for containers-0.8

## 0.2.0.1 -- 2024-05-18

- Loosen dependency bounds for GHC 9.10, thanks to yaitskov

## 0.2 -- 2024-04-09

- Removed the instances for the `Event` and `Lifetime` types unavailable on Windows

## 0.1.1.0 -- 2024-03-24

- Added more trivial instances for semigroups from base

## 0.1.0.2 -- 2023-12-22

- Support GHC 9.8.1

## 0.1.0.1 -- 2023-04-17

- Loosen version bounds
- Support GHC 9.6.1

## 0.1.0.0 -- 2022-06-12

- `Commutative (Product a)` now requires `CommutativeProduct a`.
  `CommutativeProduct` is a new class to indicate `(*)` from `Num` is
  commutative, which is not required by `Num`. (Example:
  multiplication on
  [quaternions](https://en.wikipedia.org/wiki/Quaternion) is
  non-commutative, and the `Quaternion a` type from the `linear`
  package has a valid `instance RealFloat a => Num (Quaternion a)`.)

  **Remark:** There is also no canonical subclass class in the `Num`
  hierarchy which implies commutative `(*)`, as both `Integral` and
  `Floating` instances work here:

  - `Integral` instances are customarily Euclidean Domains, which are
    commutative rings with extra conditions.

  - `Floating` instances customarily expect `(+)`, `(*)`, and `exp` to
    form an exponential field, which is also a commutative ring with
    extra conditions.

## 0.0.2.0 -- 2022-03-26

- Add `instance Ord a => Commutative (Set a)`
- Add `instance Commutative IntSet`

## 0.0.1.0 -- 2021-01-28

- Add instance for `Maybe`.

## 0.0.0.0 -- 2021-01-06

Initial version, created from `groups` package.
