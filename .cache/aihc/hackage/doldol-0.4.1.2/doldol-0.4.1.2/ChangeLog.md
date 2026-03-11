Changelog for doldol
====

## [Unreleased]

## [0.4.1.0] - 2019-07-21

### Added
* Re-enable `allReq`

### Changed
* Remove unnecessary constraint `NFData`

### Improved
* Improve and fix test codes

### Removed
* Remove unnecessary ghc-option for parallelism

## [0.4.0.0] - 2019-07-16

### Changed
* Change type of some types to handle `Foldable (Enum e)`

### Improved
* Automate test code with test-framework

## [0.3.0.1] - 2017-03-05

### Improved
* Uses pattern matching for `Data.Flag.Phantom`.

## [0.3.0.0] - 2017-03-03

### Changed
* Change type of `Flag` from `Int64` to `Word64` to use unsigned integer.

## [0.2.0.0] - 2017-03-02

### Removed
* Remove `allReq` becasue of `include`.

### Changed
* Change implementation of `include` as like as `allReq`.

## [0.1.5.0] - 2017-03-02

### Added
* Add `anyReq`, `allReq`.

### Fixed
* Fix: Remove wrong indents before CPP.

### Improved
* Relax `base` dependency to 4.8.

## [0.1.4.1] - 2017-02-28

### Added
* Add `NFData` class for `Data.Flag.Phantom`.

## [0.1.4.0] - 2017-01-27

### Added
* Implement `Data.Flag.Phantom`.

## [0.1.3.2] - 2017-01-27

### Improved
* Refactoring to implement `Data.Flag.Phantom`.

### Removed
* Remove not-using dependency.

## [0.1.3.0] - 2017-01-21

### Added
* Implement `eqAbout`, `includeAbout`, and `excludeAbout`.
* Define `Flag` as `Int64`.

## [0.1.2.0] - 2017-01-21

### Added
* Implement `readFlag` and `readEnum`.

## [0.1.1.0] - 2017-01-21

### Added
* Implement `showFlagFit` and `showFlagBy`.

## [0.1.0.0] - 2017-01-20

### Changed
* Changed Functions' name.

### Added
* Implement `decodeFlag` and `showFlag`.

## [0.0.0.0] - 2017-01-18

### Added
* A interface only for `List`.
