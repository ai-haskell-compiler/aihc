# Revision history for dependent-sum-template

## 0.2.0.2 - 2025-01-18

* Support GHC 9.12


## 0.2.0.1 - 2023-12-06

* Add back support for GHC 9.2 and 9.4.

  We were able to work around [the bug in `reifyInstances`](https://gitlab.haskell.org/ghc/ghc/-/issues/23743) by using a data family instead of a type family.

## 0.2.0.0 - 2023-08-01

* Recover compatibility with template-haskell 2.18, which was lost in 0.1.2.0
* deriveGShow will generate code that uses Show instances for every argument to a constructor, apart from those of the type that it is generating an instance for.
* Drop support for GHC 9.2 and 9.4 due to [a bug in `reifyInstances`](https://gitlab.haskell.org/ghc/ghc/-/issues/23743)

## 0.1.2.0 - 2023-07-11

* Rework a lot of the logic using th-abstraction to get structural information about data types and to
  normalize their representation. This should allow the deriving functions to work on a much wider range
  of types.

* Change dependency to just be on `some`, not `dependent-sum`, as we just need the reexported classes.

## 0.1.1.1 - 2021-12-30

* Fix warning with GHC 9.2 about non-canonical `return`.

## 0.1.1.0 revision 1 - 2021-11-30

* Add bound to `th-abstraction` to prevent build failure.

## 0.1.1.0 - 2021-11-25

* Support GHC 9.0

## 0.1.0.3 - 2020-03-24

* Relax version bounds on `dependent-sum` to include 0.7.

## 0.1.0.2 - 2020-03-23

* Update GitHub repository in cabal metadata.

## 0.1.0.1 - 2020-03-21

* Support GHC 8.8.

## 0.1.0.0 - 2019-03-21

* Remove code for generating instances of *Tag classes, as they were removed in dependent-sum-0.6.
