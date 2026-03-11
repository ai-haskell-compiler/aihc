See also http://pvp.haskell.org/faq

## 0.11.103.0

 - add `Crypto.Hash.SHA384.hkdf` and `Crypto.Hash.SHA512.hkdf` functions
   providing HKDF-SHA384 and HKDF-SHA512 respectively, as per RFC5869

## 0.11.102.0

 - expose SHA512/t variant via new `Crypto.Hash.SHA512t` module
 - expose SHA384 variant via new `Crypto.Hash.SHA384` module

## 0.11.101.0

 - add `Eq` instance for `Ctx`
 - add `start` and `startlazy` producing a `Ctx`
 - remove ineffective `RULES`
 - declare `Crypto.Hash.SHA512` module as `-XTrustworthy`
 - convert to `CApiFFI`
 - add `...AndLength` variants of hashing functions:

      - `finalizeAndLength`
      - `hashlazyAndLength`
      - `hmaclazyAndLength`

 - minor optimizations in `hmac` and `hash`

### 0.11.100.1

 - first public release

## 0.11.100.0 *(unreleased)*

 - new `hmac` and `hmaclazy` functions providing HMAC-SHA-512
   computation conforming to RFC2104 and RFC4231
 - fix unaligned memory accesses
 - switch to 'safe' FFI for calls where overhead becomes negligible
 - remove inline assembly in favor of portable C constructs
 - fix 32-bit length overflow bug in `hash` function
 - fix inaccurate context size
 - add context size verification to incremental API operations

### 0.11.7.1 *(unreleased)*

 - first version forked off `cryptohash-0.11.7` release
