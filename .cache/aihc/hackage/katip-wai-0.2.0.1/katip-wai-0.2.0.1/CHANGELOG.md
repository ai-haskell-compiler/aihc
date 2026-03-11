# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org/).


<!-- Guiding Principles  -->
<!--   Changelogs are for humans, not machines. -->
<!--   There should be an entry for every single version. -->
<!--   The same types of changes should be grouped. -->
<!--   Versions and sections should be linkable. -->
<!--   The latest version comes first. -->
<!--   The release date of each version is displayed. -->

<!-- Types of changes -->
<!--   `Added` for new features.-->
<!--   `Changed` for changes in existing functionality. -->
<!--   `Deprecated` for soon-to-be removed features. -->
<!--   `Removed` for now removed features. -->
<!--   `Fixed` for any bug fixes. -->
<!--   `Security` in case of vulnerabilities. -->

<!-- ## [Unreleased] -->

## [0.2.0.1] - 2026-01-11

### Fixed
- Increased upper bound for `time` and `containers`

## [0.2.0.0] - 2024-09-11

### Added

- Added a `Request` type that contains all the info about a request including its trace id.
- Added a `Response` type that contains info about a response, including how long it took the server to respond.
- `traceRequest` to convert from a `Network.Wai.Request` to a `Request`.
- `traceResponse` to convert from a `Network.Wai.Response` to a `Response`.
- Added the `Options` type, which provides more flexibility to customize how the request and response are handled.
- Added more formatting options for both the request and response.
- Added `middlewareCustom` for full customization.

### Changed
- The default formats used by `middleware` have been changed. See `defaultRequestFormat` and `defaultResponseFormat` for the new formats.

### Removed
- `middlewareWithFormatter` has been removed. Please use `middlewareCustom` instead.

## [0.1.2.4] - 2024-05-16

### Fixed

- Increased upper bound for `network`

## [0.1.2.3] - 2024-01-01

### Fixed

- Increased upper bound for `text`
- Increased upper bound for `bytestring`

## [0.1.2.2] - 2023-06-29

### Fixed

- Increased upper bound for `aeson`


## [0.1.2.1] - 2022-11-12

### Added 

- Added [CHANGELOG.md](./CHANGELOG.md)
- Added [CONTRIBUTING.md](./CONTRIBUTING.md)

### Fixed

- Adjusted version bounds 
- Fixed haddock error


## [0.1.2.0] - 2022-06-04

### Fixed

- Adjusted version bound for text.


## [0.1.1.0] - 2022-02-02

### Added

- Added `middlewareWithFormatter` and `defaultFormat` so that you may customize the logging format.


## [0.1.0.0] - 2021-12-29

### Added

- Initial release


[unreleased]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.2.0.1...HEAD
[0.2.0.1]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.2.0.0...releases/0.2.0.1
[0.2.0.0]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.2.4...releases/0.2.0.0
[0.1.2.4]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.2.3...releases/0.1.2.4
[0.1.2.3]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.2.2...releases/0.1.2.3
[0.1.2.2]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.2.1...releases/0.1.2.2
[0.1.2.1]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.2.0...releases/0.1.2.1
[0.1.2.0]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.1.0...releases/0.1.2.0
[0.1.1.0]: https://github.com/Disco-Dave/katip-wai/compare/releases/0.1.0.0...releases/0.1.1.0
[0.1.0.0]: https://github.com/Disco-Dave/katip-wai/releases/tag/releases%2F0.1.0.0
