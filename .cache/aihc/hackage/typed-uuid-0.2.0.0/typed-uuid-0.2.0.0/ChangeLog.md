# Changelog

## [0.2.0.0] - 2021-11-20

### Changed

* Replaced `yamlparse-applicative` by `autodocodec`.

## [0.1.0.0] - 2020-05-10

### Added

* `parseUUIDAsciiBytes`
* `parseUUIDLazyAsciiBytes`
* `instance YamlSchema (UUID a)`
* `instance YamlKeySchema (UUID a)`

### Changed

* Renamed `uuidBs` to `uuidASCIIBytes`
* Renamed `uuidLBs` to `uuidLazyASCIIBytes`
* Renamed `parseUUID` to `parseUUIDText`

## [0.0.0.2] - 2020-02-13

Put a lower bound on the base version

## [0.0.0.1] - 2020-02-12

Got typed-uuid to compile with GHC 8.8.x
