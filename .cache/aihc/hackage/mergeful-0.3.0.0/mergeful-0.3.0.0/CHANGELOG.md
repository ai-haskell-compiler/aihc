# Changelog

## [0.3.0.0] - 2022-06-19

### Added

* `autodocodec` support

### Changed

* Fixed a bug in which the `ValueSyncResponse` serialised conflicting values to JSON strangely.
* Fixed a bug in which the `ItemSyncResponseConflict` and `ItemSyncResponseConflict` serialised conflicting values to JSON strangely.

## [0.2.0.0] - 2020-05-21

## Added

* processServerSyncCustom and related
* mergeSyncResponseCustom and related

### Changed

* Another parameter: the client id so that you can use an sql id as the client id.
  This likely means that none of your code that works with mergeful-0.1 will compile anymore
  but it is easy to fix by adding the right parameter.

## [0.1.0.0] - 2019-09-23

### Changed

* Compatibility with validity-containers >=0.5
