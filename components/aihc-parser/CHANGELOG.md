# Changelog

All notable changes to `aihc-parser` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [1.0.0.3] - 2026-06-01

### Changed

- Simplified the Cabal package synopsis for cleaner Hackage metadata.

### Fixed

- Preserved source-span annotations on binder names so downstream consumers can
  locate binders parsed from declarations such as foreign imports.

## [1.0.0.2] - 2026-05-28

### Fixed

- Removed the internal `parser-tooling-common` and `parser-test-support`
  sublibraries from the published Cabal package so Hackage exposes only the
  core `aihc-parser` library.

## [1.0.0.1] - 2026-05-28

### Fixed

- Removed internal progress executables from the published Cabal package so
  Hackage lists `aihc-parser` as library-only.
- Included parser fixture files in the source distribution so Hackage can run
  the package test suite and report coverage.

## [1.0.0.0] - 2026-05-27

### Added

- Initial stable release of the from-scratch Haskell parser package.
- Public parser, lexer, syntax, pretty-printer, shorthand, token, and
  parenthesis-insertion modules.
- Oracle-backed parser validation against GHC with parse/pretty round-trip
  fingerprint checks.
- Haskell2010 and extension coverage tracking, including the current fully
  implemented Haskell2010 baseline and supported tracked extensions.
