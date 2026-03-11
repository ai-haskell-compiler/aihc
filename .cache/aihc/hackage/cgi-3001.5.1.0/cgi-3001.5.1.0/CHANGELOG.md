# Change Log
All notable changes to this project will be documented in this file.
This project adheres to the [Package Versioning Policy](https://wiki.haskell.org/Package_versioning_policy).

## [3001.5.0.1]
- Allow building against bytestring version 0.11.x.
- Update our Cabal file to "cabal-version: >= 1.10" so that we can legally use
  the other-extensions field.

## [3001.5.0.0]
- Define a proper `MonadFail` instance for `CGIT`. This is necessary to compile
  successfully with `ghc-8.8.x` Since that change affects our public API, a
  major version bump is necessary.
- The build no longer supports ghc prior to version 8.x.

## [3001.4.0.0]
- Drop obsolete Network.CGI.Compat module. The code in that module relied on
  obsolete functions and types from `network` which have been dropped there in
  the latest 3.x release. Re-writing the Compat module to use the new types
  felt like it would defeat the purpose of the module, so we've dropped it
  instead.
- Dropped the dependency on `network` altogether. We need `network-uri`,
  really. Giving up support for ancient versions of `network` allows us to drop
  the `network-uri` flag, too, simplifying our builds.
- Dropped support for versions of `mtl` prior to 2.2.x. That version was
  released almost 5 years ago, so we can probably drop the compatibility code
  (and the `old-mtl` Cabal flag) without surprising anyone.
- Added new `cookieHttpOnly` flag to the `Cookies` type. When set, the client's
  browser will prevent client side scripts from accessing the cookie.

## [3001.3.1.0]
### Changed
- Our error handling functions `throwCGI`, `catchCGI`, `tryCGI`, and
  `handleExceptionCGI` are deprecated. These functions are trivial aliases for
  the corresponding functions from the `exceptions` library's `MonadCatch`
  class. Users should directly use those functions. They are more general and
  have better documentation.
- Relax version constraints to allow building with network 2.8.x.

## [3001.3.0.3]
### Changed
- Bumped upper version bounds for containers and time.
- Updated to exceptions 0.10.x. This meant extending our `MonadMask` instance
  to provide the `generalBracket` method that was added to the class in recent
  versions of the exceptions library.
- The doctest suite would not work reliably with different versions of Cabal.
  Instead of going all out with a custom written build system to support it
  properly, we now run doctests as part of our CI builds but not as a part of
  the Cabal build any more.
- Fixed several compiler warnings in our code.

## [3001.3.0.2]
### Changed
- Bumped QuickCheck upper bound to version < 2.10

## [3001.3.0.1]
### Changed
- Bumped doctest to version < 0.12

## [3001.3.0.0]
### Changed
- Cookie.hs: cookieExpires now has type `Maybe UTCTime` rather than
`Maybe CalendarTime`
- Protocol.hs: URL decoding functions no longer decode UTF-8 encoding
- Functor and Applicative instance of CGIT no longer constrain Functor or
Applicative parameter to be an instance of Monad

## [3001.2.2.3]
### Changed
* CGI.hs haddock: Use web.archive.org link for CGI specification

## [3001.2.2.2]
### Changed
- Added support for building with mtl < 2.2.1 via flags

## [3001.2.2.1]
### Changed
- Bumped exceptions version to < 0.9

## [3001.2.2.0]
### Added
- MonadMask instance for CGIT

## [3001.2.1.0]
### Added
- Applicative instance for CGI Monad

### Changed
- Deduplicate shared with `multipart` code

## 3001.2.0.0
### Changed
- GHC 7.8.3 support

[Unreleased]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.3.0.2...HEAD
[3001.3.0.2]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.3.0.1...3001.3.0.2
[3001.3.0.1]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.3.0.0...3001.3.0.1
[3001.3.0.0]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.2.3...3001.3.0.0
[3001.2.2.3]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.2.2...3001.2.2.3
[3001.2.2.2]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.2.1...3001.2.2.2
[3001.2.2.1]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.2.0...3001.2.2.1
[3001.2.2.0]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.1.0...3001.2.2.0
[3001.2.1.0]:
https://github.com/cheecheeo/haskell-cgi/compare/3001.2.0.0...3001.2.1.0
