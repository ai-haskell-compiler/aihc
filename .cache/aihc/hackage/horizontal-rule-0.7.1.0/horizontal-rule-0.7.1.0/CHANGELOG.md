# `hr-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.C.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## 0.7.1.0 (2026-01-10)

### Non-Breaking

* Bump `base` dependency version upper bound
* Bump `optparse-applicative` dependency version upper bound
* Bump `time` dependency version upper bound
* Remove mock tests, vendored `HMock` and `explainable-predicates`
  dependencies

## 0.7.0.0 (2024-12-04)

### Breaking

* Remove support for GHC 8.6, constraining lower bounds
* Remove support for GHC 8.4, constraining lower bounds
* Remove support for GHC 8.2, constraining lower bounds
* Change minimal Cabal from 1.24 to 3.0

### Non-Breaking

* Bump `base` dependency version upper bound
* Bump `tasty` dependency version upper bound
* Bump `text` dependency version upper bound
* Bump `time` dependency version upper bound
* Vendor `HMock` and `explainable-predicates`

## 0.6.0.0 (2023-05-28)

### Breaking

* Add support for `optparse-applicative` `0.18`

### Non-Breaking

* Bump `ansi-wl-pprint` dependency version upper bound
* Adjust dependency constraints to match tested versions

## 0.5.0.0 (2022-03-01)

### Breaking

* Rename Haskell package to `horizontal-rule`
* Refactor API to use `MonadTerminal` instead of `IO` directly

### Non-Breaking

* Add mock tests
* Bump `text` dependency version upper bound
* Bump `time` dependency version upper bound
* Bump `optparse-applicative` dependency version upper bound

## 0.4.0.0 (2021-06-25)

### Breaking

* Fix `--help` when using `optparse-applicative` `0.16`

### Non-Breaking

* Refactor Nix configuration

## 0.3.0.1 (2021-05-28)

### Non-Breaking

* Add library usage example

## 0.3.0.0 (2021-05-28)

### Breaking

* Add library
* Add `--width` and `--default` CLI options
* Add `--input` and `--timeout` CLI options

## 0.2.0.1 (2021-05-27)

### Non-Breaking

* Fix link in README
* Fix formatting in RPM description

## 0.2.0.0 (2021-05-26)

### Breaking

* Add support for `optparse-applicative` `0.16`

### Non-Breaking

* Add `.deb` and `.rpm` packaging
* Add Cabal support to `Makefile`
* Add Cabal tests to GitHub Actions
* Add [stan](https://hackage.haskell.org/package/stan) static analysis

## 0.1.0.3 (2020-11-21)

### Non-Breaking

* Use GitHub Actions instead of Travis CI

## 0.1.0.2 (2020-11-05)

### Non-Breaking

* Rename Git default branch to `main`
* Refactor `Makefile`, add `STACK_NIX_PATH` support
* Add `test-all` command to `Makefile`
* Add Nix configuration

## 0.1.0.1 (2019-12-29)

### Non-Breaking

* Fix operator for compatibility with GHC 8.2.2

## 0.1.0.0 (2019-12-29)

### Breaking

* Initial public release
