# CHANGELOG

## 1.1.1
### Fixed
* Compatibility with mtl-2.3 and hence newer GHC versions
* Migrate to Github

## 1.1.0
### Added
* instances of `MonadHReader` for `MaybeT` and `IdentityT`
* method 'hlocal' in `MonadHReader`

## 1.0.2
### Added
* added forgotten `MonadMask` instance

## 1.0.1
### Changed
* fix `Applicative` constraints for base < 4.8

## 1.0.0
### Added
* `MHRElemsConstraint`
### Changed
* depenes on `hset` > 2.0.0
* use `tagged` instead of Labeled
* some things renamed, so this version is not backwards compatible

## 0.2.0
### Added
* `subHSetHReaderT` run a local reader with a subset of HSet elements
### Changed
* minimum required `hset` version is `1.1.0`

## 0.1.0
### Changed
* Use `hset-1.0.0`

## 0.0.2
Make it compilable on base-4.7

## 0.0.1
First working version
