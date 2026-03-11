<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# Changelog for primecount

## 0.1.0.2

### Release History

| Release         | Date       | Tag              |
| --------------- | ---------- | ---------------- |
| Initial Release | 2024-12-23 | [`0.1.0.2`]      |

### Added
* Made project REUSE compliant (https://reuse.software) with respect to
  copyright and licensing.

### Updated
* Supported GHC versions are now: 8.10, 9.0, 9.2, 9.4, 9.6, 9.8, 9.10, and 9.12.

## 0.1.0.1

### Release History

| Release         | Date       | Tag              |
| --------------- | ---------- | ---------------- |
| Initial Release | 2023-08-24 | [`0.1.0.1`]      |
| Revision 1      | 2023-09-11 | [`0.1.0.1-rev1`] |

### Updated
* Supported GHC versions are now: 8.10, 9.0, 9.2, 9.4, and 9.6.
  * Support for GHC 8.4, 8.6 and 8.8 is dropped.

## 0.1.0.0

| Release         | Date       | Tag              |
| --------------- | ---------- | ---------------- |
| Initial Release | 2022-01-07 | [`0.1.0.0`]      |

### Added
* `Math.NumberTheory.Prime.Count.FFI`, with FFI bindings to all functions from the
  `primecount` library.
* `Math.NumberTheory.Prime.Count`, with high-level wrappers (`primePi`,
  `nthPrime`, and `primePhi`) around the functions in
  `Math.NumberTheory.Prime.Count.FFI`.
* Test coverage of `primePi`, `nthPrime`, and `primePhi` with `tasty-hunit`.
* Benchmarks of `primePi`, `nthPrime`, and `primePhi` with `tasty-bench`.
* Full Haddock documentation coverage.
* Support for GHC 8.4, 8.6, 8.8, 8.10, 9.0, and 9.2.

[`0.1.0.2`]: https://github.com/pgujjula/primecount-haskell/releases/tag/0.1.0.2
[`0.1.0.1-rev1`]: https://github.com/pgujjula/primecount-haskell/releases/tag/0.1.0.1-rev1
[`0.1.0.1`]: https://github.com/pgujjula/primecount-haskell/releases/tag/0.1.0.1
[`0.1.0.0`]: https://github.com/pgujjula/primecount-haskell/releases/tag/0.1.0.0
