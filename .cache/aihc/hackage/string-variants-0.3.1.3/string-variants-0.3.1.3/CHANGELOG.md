# Changelog

## [0.3.1.3] - 2025-12-11

- Fix the comparator displayed in the error message for too-long `NullableNonEmptyText`.

## [0.3.1.2] - 2025-01-30

- Add the limit number to the decode error string for `NonEmptyText`.

## [0.3.1.1] - 2024-10-31

- Add `Hashable` and `NFData` instances to `NonEmptyText`.
- Add `Data`, `Ord`, `Hashable` and `NFData` instances to `NullableNonEmptyText`.
- Add dependencies on `deepseq` and `hashable`.

## [0.3.1.0] - 2024-02-29

- Add `Data` instance to `NonEmptyText`.

## [0.3.0.1] - 2024-01-03

- Add support for GHC 9.6
- Fix bug in `mkNullableNonEmptyText` that incorrectly counted leading and trailing whitespace against the character limit.
- Improve documentation for `NullableNonEmptyText`

## [0.3.0.0] - 2023-10-24

- Remove incorrect `Semigroup` instance for `NonEmptyText`

## [0.2.2.1] - 2023-06-20

- Add support for `aeson-2.2.0.0`.
- When built with `aeson >=2.2.0.0`, record fields with type `NullableNonEmptyText` will be omittable.

## [0.2.2.0] - 2023-02-10

- Add `proseFromNonEmptyText` to create `Prose` from `NonEmptyText`.

## [0.2.1.0] - 2023-01-23

- Use `NonEmptyList` for `chunksOfNonEmptyText`

## [0.2.0.0] - 2022-12-05

- Add `literalNonEmptyText`, `literalProse`, `literalNullableNonEmptyText` for creation of string variants from the type-level literals.
- Add `concatWithSpace` to concatenate NonEmptyText in a type-safe way.
- Drop support for GHC <9.2.

## [0.1.0.2] - 2022-10-18

- Fix `mkNonEmptyTextWithTruncate` to perform truncation after character stripping.

## [0.1.0.1] - 2022-09-29

- Fix building on GHC 8.10.7, and add it to CI.

## [0.1.0.0] - 2022-09-28

- Initial release
