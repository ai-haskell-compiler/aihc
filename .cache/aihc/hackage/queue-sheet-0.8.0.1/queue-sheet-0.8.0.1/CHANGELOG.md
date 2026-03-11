# `queue-sheet-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.0.D`, for which
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

## 0.8.0.1 (2025-01-04)

### Non-Breaking

* Flip `optparse-applicative_ge_0_18` flag

## 0.8.0.0 (2025-01-03)

### Breaking

* Support whitespace-separated tags and items instead of CSV
* Remove support for GHC 8.6, constraining lower bounds
* Remove support for GHC 8.4, constraining lower bounds
* Remove support for GHC 8.2, constraining lower bounds
* Change minimal Cabal from 1.24 to 3.0

### Non-Breaking

* Bump `aeson` dependency version upper bound
* Bump `ansi-wl-pprint` dependency version upper bound
* Bump `base` dependency version upper bound
* Bump `bytestring` dependency version upper bound
* Bump `filepath` dependency version upper bound
* Add support for `optparse-applicative` `0.18`
* Bump `tasty` dependency version upper bound
* Bump `text` dependency version upper bound
* Bump `transformers` dependency version upper bound
* Bump `ttc` dependency version upper bound
* Bump `vector` dependency version upper bound
* Adjust dependency constraints to match tested versions

## 0.7.0.2 (2022-03-02)

### Non-Breaking

* Bump `text` dependency version upper bound
* Bump `optparse-applicative` dependency version upper bound

## 0.7.0.1 (2021-10-10)

### Non-Breaking

* Add dependency bounds

## 0.7.0.0 (2021-06-25)

### Breaking

* Fix `--help` when using `optparse-applicative` `0.16`

### Non-Breaking

* Use TTC 1.1.0.1
* Refactor Nix configuration

## 0.6.0.0 (2021-05-27)

### Breaking

* Add support for `optparse-applicative` `0.16`

### Non-Breaking

* Add `.deb` and `.rpm` packaging
* Add Cabal support to `Makefile`
* Add Cabal tests to GitHub Actions
* Add [stan](https://hackage.haskell.org/package/stan) static analysis

## 0.5.0.1 (2020-11-23)

### Non-Breaking

* Use GitHub Actions instead of Travis CI

## 0.5.0.0 (2020-11-08)

### Breaking

* Add simplified CSV support for tags and items

### Non-Breaking

* Rename Git default branch to `main`

## 0.4.0.0 (2020-09-12)

### Breaking

* Add item support for tags

## 0.3.0.0 (2020-08-23)

### Breaking

* Add default section support
* Add top-level queues array support
* Add import support
* Add user-defined tag support
* Remove split property
* Change templates to use snake-case properties

## 0.2.0.0 (2020-08-11)

### Breaking

* Add URL support

## 0.1.0.0 (2020-07-25)

### Breaking

* Configure sections and queues in a single YAML file

### Non-Breaking

* Add a template option
* Add an output option
* Refactor `Makefile`, add `STACK_NIX_PATH` support
* Add `test-all` command to `Makefile`
* Add Nix configuration

## 0.0.1.0 (2020-01-22)

### Breaking

* Initial public release
