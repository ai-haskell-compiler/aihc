<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# Changelog for apply-merge

## 0.1.0.1

### Added
* Implementation of `applyMergeOn`
  ([#1](https://github.com/pgujjula/apply-merge/issues/1)) and `applyMergeBy`
  ([#3](https://github.com/pgujjula/apply-merge/issues/3)).
* Implementation of `applyMerge` for `NonEmpty`
  ([#2](https://github.com/pgujjula/apply-merge/issues/2)).
* Comprehensive benchmark suite for `applyMerge`
  ([#8](https://github.com/pgujjula/apply-merge/issues/8)).

### Changed
* Switched from falsify to QuickCheck in the test suite, allowing the
  project to be used in Stackage
  ([link](https://github.com/well-typed/falsify/issues/71)).
* CI configured to test GHC 9.2.8, 9.4.8, 9.6.5, 9.8.2.

## 0.1.0.0

### Added
* Implementation of `applyMerge`.
* A tasty-based test suite for `applyMerge`.
* A tasty-bench benchmark suite for `applyMerge`.
* Haddock documentation for `applyMerge`.
* Support for GHC 9.2, 9.4, 9.6, 9.8.
* CI testing for GHC 9.2.8, 9.4.8, 9.6.4.
* Documentation of the library in `README.md` and `docs/ALGORITHM.md`, with
  feedback from _meeeow_ on Haskell Discourse
  ([link](https://discourse.haskell.org/t/apply-merge-lift-a-binary-increasing-function-onto-ordered-lists-and-produce-ordered-output/9269/4)).
