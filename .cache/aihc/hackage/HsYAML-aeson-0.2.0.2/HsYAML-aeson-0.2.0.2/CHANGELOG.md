## 0.2.0.2

_2025-03-11, Andreas Abel_

* Drop support for GHC 7.
* Support latest dependencies.
* Tested with GHC 8.0 - 9.12.1.

## 0.2.0.1

_2021-11-07, Andreas Abel_

* Allow `aeson-2.0` and `bytestring-0.11`.
* Build tested with GHC 7.4 -- 9.0.

## 0.2.0.0

_2019-09-22, Herbert Valerio Riedel_

This release incorporates the work from [Vijay Tadikamalla's GSOC 2019 Project](https://vijayphoenix.github.io/blog/gsoc-the-conclusion/).

* **Breaking change**: The result types of `decode1`, `decode1'`, `decodeValue`, and `decodeValue'` have been changed from `Either String _` to `Either (Pos,String) _` to mirror the error-reporting change in the `HsYAML-0.2` API
* New functions `encode1`, `encode1Strict`, `encodeValue`, and `encodeValue'` for serializing JSON Values as YAML documents
* New convenience function `decode1Strict`
* New (orphan) `instance ToYAML Data.Aeson.Value`

----

## 0.1.0.0

* First release. Released on an unsuspecting world.
