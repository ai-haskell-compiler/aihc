# Changelog

All notable changes to `aihc-cpp` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [1.0.0.0] - 2026-05-27

### Added

- Initial stable release of the pure Haskell CPP package.
- Public preprocessing API with deterministic configuration, diagnostics,
  include continuations, and preprocessing results.
- Support for object-like and function-like macros, includes, conditionals,
  diagnostics, line directives, token pasting, stringification, predefined
  macro handling, and comment-aware scanning.
- Oracle-backed progress suite against cpphs with the current `46/46`
  implemented baseline.
