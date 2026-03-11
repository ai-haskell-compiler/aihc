## 0.96.0.2 (2025-03-02)

- Drop support for GHC 7
- Make `Prelude` imports explicit, add `LANGUAGE NoImplicitPrelude`
- Make upper bounds of dependencies major-major (all are shipped with GHC)
- Tested with GHC 8.0 - 9.12.1

## 0.96.0.1 Revision 3 (2023-09-28)

- Allow `containers-0.7`

## 0.96.0.1 Revision 2 (2023-07-07)

- Allow `bytestring-0.12`

## 0.96.0.1 Revision 1 (2022-05-25)

- Allow `base >= 4.17` (GHC 9.4)

## 0.96.0.1 (2021-07-19)

- Compatibility with `base-4.16` (GHC 9.2)
- Fix stack installation problems on Windows around flag `_regex-posix-clib`
  (issues
  [#4](https://github.com/haskell-hvr/regex-posix/issues/4) and
  [#7](https://github.com/haskell-hvr/regex-posix/issues/7)).

## 0.96.0.0 Revision 2 (2021-02-20)

- Compatibility with `base-4.15` (GHC 9.0)

## 0.96.0.0 Revision 1 (2020-03-25)

- Compatibility with `base-4.14` (GHC 8.10)

## 0.96.0.0 (2019-09-30)

- Update to `regex-base-0.94.0.0` API
- Compatibility with `base-4.13.0`
- Remove internal regex C implementation
