# Revision history for wai-middleware-delegate

`wai-middleware-delegate` uses [PVP Versioning][1].

## 0.2.0.1 -- 2025-03-19

* Relax the upper bounds on the text dependency

## 0.2.0.0 -- 2024-11-05

* Relax the upper bounds of the data-default version used by the tests
* Remove the Default instance of ProxySettings

## 0.1.5.0 -- 2024-10-28

*  Upgrade 0.1.4.2 to a minor release

## 0.1.4.2 -- 2024-10-28

* Export defaultSettings as an alternative to Default#def

* Add comments 'deprecating' the Default instance of ProxySettings

## 0.1.4.1 -- 2024-03-17

* Relax the upper bounds on bytestring to allow bytestring 0.12.1

## 0.1.4.0 -- 2023-17-11

* Relax the upper bounds on bytestring to allow bytestring 0.12.1
* Remove dependencies on http-conduit and connection
* Switch to running the integration tests using http-bin in docker

## 0.1.3.1 -- 2022-08-09

* Relax the version bounds so that pre 2.0 versions of text are still supported

## 0.1.3.0 -- 2022-08-09

* Bump a dependency - switch to text-2.0

## 0.1.2.4 -- 2021-09-30

* Fix cabal reference to data files

## 0.1.2.3 -- 2021-09-30

* Add missing data files needed by the tests

## 0.1.2.2 -- 2021-09-29

* Fix the module documentation

## 0.1.2.1 -- 2021-09-29

* Relax some dependency upper-bounds
* Compile with a later version of GHC


## 0.1.2.0 -- 2020-10-20

* Relax some dependency upper-bounds


## 0.1.1.0 -- 2018-08-06

* Add configuration for the number of redirects


## 0.1.0.0 -- 2018-07-28

* Initial version.

[1]: https://pvp.haskell.org
