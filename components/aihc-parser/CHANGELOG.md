# Changelog

All notable changes to `aihc-parser` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [1.0.0.0] - 2026-05-27

### Added

- Initial stable release of the from-scratch Haskell parser package.
- Public parser, lexer, syntax, pretty-printer, shorthand, token, and
  parenthesis-insertion modules.
- Oracle-backed parser validation against GHC with parse/pretty round-trip
  fingerprint checks.
- Haskell2010 and extension coverage tracking, including the current fully
  implemented Haskell2010 baseline and supported tracked extensions.
