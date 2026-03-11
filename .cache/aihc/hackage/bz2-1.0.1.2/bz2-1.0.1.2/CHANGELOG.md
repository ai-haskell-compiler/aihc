# bz2

## 1.0.1.2

  * Rollback dependency on `bzip-clib` since bundled sources were not vulnerable
    to CVE-2019-12900

## 1.0.1.1

  * Fix vulnerability https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2019-12900

## 1.0.1.0

  * Add `decompressErr`

## 1.0.0.3

  * Fix bug decompressing partial data

## 1.0.0.2

  * Change buffering parameters for speed
  * Add benchmarks relative to `bzlib`, `bzip-pipes`, `bzlib-conduit`

## 1.0.0.1

  * Pass `-O3` to `cc-options`

## 1.0.0.0

  * Remove `Codec.Compression.BZip.Foreign`
  * Use `ForeignPtr` under the hood
  * Lazier streaming; memory use should be sensible

## 0.1.1.2

  * Use `ForeignPtr`s under the hood in various places (over `bracket`)

## 0.1.1.1

  * Haddock improvements

## 0.1.1.0

  * Change type signature of `bZ2BzlibVersion`
  * Add `Codec.Compression.BZip` module

## 0.1.0.1

  * Fix distribution

## 0.1.0.0

Initial release
