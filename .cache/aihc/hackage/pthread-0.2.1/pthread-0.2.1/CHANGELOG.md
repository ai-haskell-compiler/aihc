# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [0.2.1] - 2019-06-17

### Added

* Compatibility with ghc-8.6.
* Check for -threaded.

## [0.2.0] - 2017-11-06

### Added

* Bindings for managing threads (create, join, exit, cancel).
* Requirement to link with -threaded if using the new bindings.
* Tests for create, join and exit.
* An exception datatype for the boundness check.

### Changed

* Renamed module to System.Posix.Thread.
* Renamed functions to createKey and deleteKey.
* Show error code on failure.

## [0.1.1] - 2017-09-28

### Changed

* Fixed the URL to the pthread repo in the cabal file.

## [0.1] - 2017-09-28

### Added

* Bindings for thread-local storage management.
