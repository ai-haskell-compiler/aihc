# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.3.0.4] - 2021-11-13

- Compatiblity with prettyprinter-1.7.1 deprecactions.

## [0.3.0.3] - 2011-06-19

- GHC 9.0 support.

## [0.3.0.2] - 2020-11-17

- Add liftTyped methods for template-haskell 2.16

## [0.3.0.1] - 2020-03-21

- GHC 8.8 warnings fixed

## [0.3.0.0] - 2019-09-29

### Added

- GHC 8.8 support

### Removed

- GHC 8.0 (Stackage LTS < 11) aren't supported due to upstream changes.
  You can still find a package version combination that'll work for you.

### Changed

- AST Parser `parseText` and EDN `decodeText` no longer rely on MonadFail
  and work in `Either String` instead.

## [0.2.0.1] - 2019-02-18

### Changed

- `uuid` swapped for lighter `uuid-types`

### Added

- `tintin`-generated documentation

## [0.2.0.0] - 2019-01-18

### Changed

- Complete rewrite in Megaparsec.
- Text types changed to, well, `Text`
- Document  with `prettyprinter`
- Test suite changed to Hedgehog
- Code repository moved to GitLab

### Removed

- GHC < 8.0 (Stackage LTS < 7) aren't supported due to Semigroup-Monoid changes.

## [0.1.9.1] - 2018-12-03

### Changed

- GHC 8.4 compatibility fixes

## [0.1.9.0] - 2018-04-05

### Fixed

- `<` and `>` are valid for symbols and keywords.

## [0.1.8.2] - 2016-05-14

### Changed

- Use compat wrappers to handle GHC from 7.4 to 8.0.

### Removed

- Drop obsolete `developer` flag.

## [0.1.8.1] - 2015-10-08

### Fixed

- Update dependencies, fix deprecations, time-1.5 compat.

## [0.1.8.0] - 2013-11-29

### Fixed

- Use utf8-string parsing for unicode literals.

[0.3.0.4]: https://gitlab.com/dpwiz/hedn/tree/v0.3.0.4
[0.3.0.3]: https://gitlab.com/dpwiz/hedn/tree/v0.3.0.3
[0.3.0.2]: https://gitlab.com/dpwiz/hedn/tree/v0.3.0.2
[0.3.0.1]: https://gitlab.com/dpwiz/hedn/tree/v0.3.0.1
[0.3.0.0]: https://gitlab.com/dpwiz/hedn/tree/v0.3.0.0
[0.2.0.1]: https://gitlab.com/dpwiz/hedn/tree/v0.2.0.1
[0.2.0.0]: https://gitlab.com/dpwiz/hedn/tree/0.2.0.0
[0.1.9.1]: https://gitlab.com/dpwiz/hedn/tree/0.1.9.1
[0.1.9.0]: https://gitlab.com/dpwiz/hedn/tree/0.1.9.0
[0.1.8.2]: https://gitlab.com/dpwiz/hedn/tree/0.1.8.2
[0.1.8.1]: https://gitlab.com/dpwiz/hedn/tree/0.1.8.1
[0.1.8.0]: https://gitlab.com/dpwiz/hedn/tree/0.1.8.0
