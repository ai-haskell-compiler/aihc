# TODO
* Add doctests
* Add TyFun from singletons (or just depend from)?

# CHANGELOG

## 0.1.3
### Fixed
* Buildability with GHC-8.10 (by enobayram)

## 0.1.2
### Added KnownPeano

## 0.1.1
### Fixed
* Compilable with ghc-8.0.1

## 0.1.0
### Added
* Type family `Substract` to remove elements of one list from another
* Unsafe helper functions to relax constraints in some safe points. But we use
  `unsafeCoerce` so this functions are potentially just break constants
### Changed
* Compilable under ghc-7.8

## 0.0.1
First version
