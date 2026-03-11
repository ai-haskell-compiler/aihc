# Version 0.4.4

* Support `transformers >= 0.7.0`.


# Version 0.4.3

* Builds with GHC 9.0.1.

* `Producer'` replaced with `Proxy`. See `pipes`
  issue #224.

* Use QuickCheck instead of SmallCheck for tests.


# Version 0.4.2

* Remove upper bound on dependencies other than base.


# Version 0.4.1

* Fix #24 - skip leading empty chunks when decoding.

* Upper bound dependency bumps (transformers, binary).


# Version 0.4.0.5

* Upper bound dependency bumps (tasty).


# Version 0.4.0.4

* Upper bound dependency bumps (lens-family-core).


# Version 0.4.0.3

* Upper bound dependency bumps (pipes-bytestring).


# Version 0.4.0.2

* Upper bound dependency bumps (pipes-bytestring).


# Version 0.4.0.1

* Upper bound dependency bumps (tests).


# Version 0.4.0

* Generalized `encode` and `encodePut` to `Producer'`s.

* `decoded` and `decodedL` are now `Lens'`, not `Iso'` anymore.


# Version 0.3.0.1

* Add dependency on `ghc-prim`, needed for GHC 7.4.2.


# Version 0.3.0

* API revamped in order to support `pipes-parse-3.0.0`.


# Version 0.2.1

* Re-export `Put` and `Binary` from the `binary` package.

* Use `isEndOfBytes` from the `pipes-bytestring` package.


# Version 0.2.0

* API revamped in order to support `pipes-4.0.0` and `pipes-parse-2.0.0`.

* Added functions that act directly upon `Put` and `Get` monads: `encodePut`,
  `decodeGet`, `decodeGetMany`.

* Decoding functions now report the number of bytes that were consumed in order
  to decode a value.

* Re-export `Get` and `ByteOffset` from the `binary` package.


# Version 0.1.0.0

* Initial version.
