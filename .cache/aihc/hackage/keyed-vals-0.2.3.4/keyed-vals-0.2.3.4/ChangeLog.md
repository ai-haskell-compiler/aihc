# Revision history for keyed-vals

`keyed-vals` uses [PVP Versioning][1].

## 0.2.3.4 -- 2026-01-14

Changed

* Relax upper bounds on the containers dependency

## 0.2.3.3 -- 2026-01-14

Changed

* Relax upper bounds on the http-api-data dependency

## 0.2.3.2 -- 2025-03-25

Changed

* Relax upper bounds on the bytestring dependency

## 0.2.3.1 -- 2024-02-28

Changed

* Extend bounds on the bytestring dependency

## 0.2.3.0 -- 2024-01-09

Changed

* Relax constraints on the containers dependency

## 0.2.2.0 -- 2023-07-11

Changed

* Relax constraints on the http-api-data dependency
* Relax constraints on the bytestring dependency

## 0.2.1.0 -- 2023-07-11

Changed

* Relax constraints on the aeson dependency

## 0.2.0.0 -- 2022-12-15

Changed

* generalized encoding and decoding using [KeyedVals.Handle.Codec][]; this
  replaces KeyedVals.Handle.Aeson


Added

* support for encoding and decoding typed keys and values constrained to
  specific paths in the key-value store in [KeyedVals.Handle.Typed][]

## 0.1.0.0 -- 2022-11-28

* Initial version.

[1]: https://pvp.haskell.org
[KeyedVals.Handle.Typed]: https://hackage.haskell.org/package/keyed-vals/docs/KeyedVals-Handle.Typed.html
[KeyedVals.Handle.Codec]: https://hackage.haskell.org/package/keyed-vals/docs/KeyedVals-Handle.Codec.html
