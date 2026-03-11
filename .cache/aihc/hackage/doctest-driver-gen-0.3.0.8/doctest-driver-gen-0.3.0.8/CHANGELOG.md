# Changelog for doctest-driver-gen

## 0.3.0.8

* Relax `build-depends` to allow `base-4.x`.

## 0.3.0.7

* Update `build-depends` to allow `base-4.18.0.0`.

## 0.3.0.6

* Update `build-depends` to allow `base-4.17.0.0`.

## 0.3.0.5

* Update `build-depends` to allow `base-4.16.0.0`.

## 0.3.0.4

* Update `build-depends` to allow `base-4.15.0.0`.

## 0.3.0.3

* Update `build-depends` to allow `base-4.14.0.0`.

## 0.3.0.2

* Update `build-depends` to allow `base-4.13.0.0`.

## 0.3.0.1

* Add `build-tool-depends` to make `stack test` work normally (contribution by
  qrilka).

## 0.3.0.0

* Update the template file (in `ddgen_output`) to consider to
  `NoImplicitPrelude`.
* Fix the build fail when GHC's version is less than 8.0.x.

## 0.2.0.4

* Update `build-depends` to allow `base-4.12.0.0`.

## 0.2.0.3

* Update `build-depends` to allow `doctest-0.16.0.0`.

## 0.2.0.2

* Update `build-depends` to allow `doctest-0.15.0.0`.

## 0.2.0.1

* Update `build-depends` to allow `base-4.11.0.0`.

## 0.2.0.0

* Add `ddgen_run`.
* Update `ddgen_output` to reduce arguments for simplicity.
* Update `build-depends` to allow `doctest-0.14`.

## 0.1.0.1

* Update `build-depends` to be strict.

## 0.1.0.0 (Initial)

* Add a executable program: doctest-driver-gen.
* Add `Test.DocTest.Gen`, `ddgen`, `ddgen_output`, and `ddgen_usage`.
