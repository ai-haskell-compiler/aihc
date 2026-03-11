# Changelog for ENIG

## [Unreleased]

* Optimized `enigAuto`
  * Current version is just a proof of concept. This should be optimized.
  * Use `Builder`
* Enough test cases for `enigAuto`

## [0.0.1.0] -- 2019-05-14

### Added
* Add `enigAuto` which takes just string(`Text`) and process automatically. It returns just string(`Text`)

## [0.0.0.2] -- 2019-03-16

### Changed
* Could handle `아/야` and `으/null`

### Added
* Add Haddock documentation comments

## [0.0.0.1] -- 2018-06-20

### Changed
* Use `unicode-transforms` instead of `text-icu`

### Added
* Re-implement ENIG with features 2016 and 2018 only
  * Re-implementing features 2017 is postponed
* Implement handy functions
