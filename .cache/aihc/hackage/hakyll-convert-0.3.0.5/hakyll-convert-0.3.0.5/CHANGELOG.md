## 0.3.0.5

### Changed

- Allow bytestring 0.11
- Allow text 2.0 and 2.1
- Allow time 1.11, 1.12, and 1.14
- Allow tasty 1.5
- Allow bytestring 0.12
- Allow filepath 1.5
- Allow containers 0.7

### Fixed

- Some HLint lints
- `-Wx-partial` warning from GHC 9.8



## 0.3.0.4

### Changed
- Allow tasty 1.3 and 1.4
- Allow tasty-expected-failure 0.12

### Fixed
- Brittle golden tests for Wordpress (which broke because of an update to the
    "feed" package)


## 0.3.0.3

### Added
- Include golden outputs (and inputs) in the source distribution.


## 0.3.0.2

### Removed
- Dependency on `datetime`. That package is absent from Stackage, seems
    unmaintained, is licensed under GPL (limiting hakyll-convert's licensing),
    and is easy to replace.


## 0.3.0.1

### Added
- CHANGELOG.md is now part of sdist tarball.
- Cabal file now puts halyll-convert under "Tools" category.

### Fixed
- Version number in the Cabal file.


## 0.3.0.0

### Added
- New function, `IO.savePost`, that writes a given post onto the disk, complete
    with a "front matter" understood by Hakyll.

### Changed
- Use `Text` instead of `String` in all public interfaces.

### Fixed
- Comments in modern Blogger backups work again.
