# Changelog

## [0.6.0.1] - 2020-06-04

### Changed

- Relaxed dependency version bounds

## [0.6] - 2019-06-12

### Changed

- simple-vec3 operators now bind tighter than `*` for `Num`, so you
  can write `2 * v .* d`.

## [0.5] - 2019-06-11

### Changed

- Operator precedence order is now as follows: `.^`, `.*`, `<+>`,
  `<->`, `><` (multiplication binds most tightly).

- Test suite dependencies bump

## [0.4.0.10] - 2019-01-18

### Changed

- Test suite dependencies bump

## [0.4.0.9] - 2018-10-29

### Changed

- GHC 8.6.x support

## [0.4.0.8] - 2018-07-21

### Changed

- Benchmark dependencies bump

## [0.4.0.7] - 2018-06-30

### Changed

- Test suite dependencies bump

## [0.4.0.6] - 2018-05-12

### Changed

- Test suite dependencies bump

## [0.4.0.5] - 2018-05-11

### Changed

- Test suite dependencies bump

## [0.4.0.4] - 2018-05-11

### Changed

- Test suite dependencies bump

## [0.4.0.3] - 2018-05-10

### Changed

- Library dependencies bump

## [0.4.0.2] - 2018-04-25

### Changed

- Use `doctest-driver-gen` to run doctests

## [0.4.0.1] - 2018-03-17

### Changed

- GHC 8.4.x support

## [0.4] - 2018-02-20

### Added

- Doctests

### Changed

- `TVec3` is now just a type synonym for `(Double, Double, Double)`

## [0.3.1] - 2018-02-18

### Added

- `Eq` and `Show` instances for `Matrix CVec3`

## [0.3] - 2018-02-18

### Added

- GHC 8.2.x support

### Changed

- `Data.Vec3.Unboxed.Contiguous` is now again `Data.Vec3.Unboxed`.
  `UVec3` was merged with `SVec3` and renamed to `CVec3`.

- `Data.Vec3.Unboxed` is now `Data.Vec3.Tupled` and renamed to
  `TVec3`.

- Benchmarks were reorganized to work well with Criterion group
  coloring

### Removed

- `SVec3` (merged into `CVec3`)

## [0.2] - 2016-10-14

### Added

- Tests

### Changed

- `Data.Vec3.Unboxed` is now `Data.Vec3.Unboxed.Contiguous`

- `Data.Vec3.TUnboxed` is now `Data.Vec3.Unboxed` and uses
  [vector-th-unbox][] to generate Unbox instance

## [0.1.0.1] - 2012-12-13

### Changed

- Benchmark is now an actual Cabal-friendly benchmark

## [0.1.0.0] - 2012-12-05

[0.6.0.1]:     https://github.com/dzhus/simple-vec3/compare/0.6...0.6.0.1
[0.6]:     https://github.com/dzhus/simple-vec3/compare/0.5...0.6
[0.5]:     https://github.com/dzhus/simple-vec3/compare/0.4.0.10...0.5
[0.4.0.10]:https://github.com/dzhus/simple-vec3/compare/0.4.0.9...0.4.0.10
[0.4.0.9]: https://github.com/dzhus/simple-vec3/compare/0.4.0.8...0.4.0.9
[0.4.0.8]: https://github.com/dzhus/simple-vec3/compare/0.4.0.7...0.4.0.8
[0.4.0.7]: https://github.com/dzhus/simple-vec3/compare/0.4.0.6...0.4.0.7
[0.4.0.6]: https://github.com/dzhus/simple-vec3/compare/0.4.0.5...0.4.0.6
[0.4.0.5]: https://github.com/dzhus/simple-vec3/compare/0.4.0.4...0.4.0.5
[0.4.0.4]: https://github.com/dzhus/simple-vec3/compare/0.4.0.3...0.4.0.4
[0.4.0.3]: https://github.com/dzhus/simple-vec3/compare/0.4.0.2...0.4.0.3
[0.4.0.2]: https://github.com/dzhus/simple-vec3/compare/0.4.0.1...0.4.0.2
[0.4.0.1]: https://github.com/dzhus/simple-vec3/compare/0.4...0.4.0.1
[0.4]:     https://github.com/dzhus/simple-vec3/compare/0.3.1...0.4
[0.3.1]:   https://github.com/dzhus/simple-vec3/compare/0.3...0.3.1
[0.3]:     https://github.com/dzhus/simple-vec3/compare/0.2...0.3
[0.2]:     https://github.com/dzhus/simple-vec3/compare/0.1.0.1...0.2
[0.1.0.1]: https://github.com/dzhus/simple-vec3/compare/0.1.0.0...0.1.0.1
[0.1.0.0]: https://github.com/dzhus/simple-vec3/tree/0.1.0.0

[vector-th-unbox]: https://hackage.haskell.org/package/vector-th-unbox
