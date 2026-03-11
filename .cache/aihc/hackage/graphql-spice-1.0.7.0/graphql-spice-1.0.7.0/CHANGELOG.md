# Changelog
All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.0.7.0] - 2026-01-25
### Removed
- Deprecated `Language.GraphQL.Class.gql` (moved to `Language.GraphQL.TH`).

### Added
- `String` instances from `ToGraphQL` and `FromGraphQL`.

## [1.0.6.0] - 2024-12-06
### Added
- `Language.GraphQL.Class.gql` is moved to `Language.GraphQL.TH` where it was
  before in `graphql`.

## [1.0.5.0] - 2024-11-21
### Added
- Add `ToGraphQL` and `FromGraphQL` instances for `Value` and `HashMap`.

## [1.0.4.0] - 2024-10-24
### Added
- `gql` quasi quoter which generates a string literal with the first line
  starting at the first column and all following lines indented relative to the
  first line.

## [1.0.3.0] - 2024-07-20
### Added
- Add `deriveToGraphQL` for deriving `ToGraphQL` instances automatically.
- Add `deriveFromGraphQL`for deriving `FromGraphQL` instances automatically.

## [1.0.2.0] - 2023-07-07
### Added
- `ToGraphQL` and `FromGraphQL` instances for `Word` types, `Float`, `Double`,
  and `Scientific`.
- `ToGraphQL` and `FromGraphQL` instances for `Day`, `DiffTime`,
  `NominalDiffTime`, `UTCTime`, `LocalTime` and `TimeOfDay`.
- `Resolver`: Export `ServerException`.
- `Resolver.defaultResolver`: Throw `FieldNotResolvedException` if the requested
  field is not in the parent object.

## [1.0.1.0] - 2023-02-17
### Added
- `ToGraphQL` and `FromGraphQL` typeclasses with instances for basic types.
- `Resolver` module with `argument` and `defaultResolver` helper functions.

## 1.0.0.0 - 2022-03-29
### Added
- JSON serialization.
- Test helpers.

[1.0.7.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.6.0...v1.0.7.0
[1.0.6.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.5.0...v1.0.6.0
[1.0.5.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.4.0...v1.0.5.0
[1.0.4.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.3.0...v1.0.4.0
[1.0.3.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.2.0...v1.0.3.0
[1.0.2.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.1.0...v1.0.2.0
[1.0.1.0]: https://git.caraus.tech/OSS/graphql-spice/compare/v1.0.0.0...v1.0.1.0
