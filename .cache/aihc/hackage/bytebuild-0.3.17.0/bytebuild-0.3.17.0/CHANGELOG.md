# Revision history for bytebuild

Note: Prior to version 0.3.4.0, this library was named
`small-bytearray-builder`, not `bytebuild`. The library
`small-bytearray-builder` is now just a compatibility shim
to ease the migration process.

## 0.3.17.0 -- 2025-11-17

* Add `hPut` to help with common case of pushing logs to stderr.
* Add `ToBuilder` for `CInt`

## 0.3.16.3

* Add support for building with GHC 9.10

## 0.3.16.2 -- 2024-02-06

* Restore import statement for `liftA2` to fix build for GHC 9.4.

## 0.3.16.1 -- 2024-02-02

* Remove all CPP
* Drop support for GHC < 9.4
* Drop support for text < 2.0

## 0.3.16.0 -- 2024-01-29

* Add `wordPaddedDec3`.
* Add `Data.Bytes.Builder.Avro`.
* Add `word16LEB128`.
* Stop accepting versions of text lower than 2.0.

## 0.3.15.0 -- 2024-01-05

* Add `Data.Bytes.Builder.Unsafe.pasteUtf8TextJson#` for users who need
  to perform JSON string encoding without using a builder.
* Add `Data.Bytes.Builder.textJsonString` when building with text 2.0+

## 0.3.14.0 -- 2023-07-20

* Add `runOntoLength`.
* Allow building with GHC 9.6.

## 0.3.13.0 -- 2023-05-01

* Add VLQ builders for Word32 and Word64.
* Add chunks-to-builder function
* Add textUtf8 for copying text to builder with text-2.0 and newer

## 0.3.12.0 -- 2022-12-01

* Support GHC 9.4.

## 0.3.11.0 -- 2022-04-12

* Support GHC 9.2
* Inline pragmas for better LEB encoding perf
* Bump upper bound on tasty in test suite

## 0.3.10.0 -- 2022-03-01

* Add upper bound on base since this does not build with GHC 9.2.
* Add `rebuild`
* Add `copyCons` for copying small byte sequences with extra byte in front
* Use `zigzag` library from hackage
* Seven eights encoding
* Finish out LEB for 32-bit and signed 32/64

## 0.3.9.0 -- 2021-11-19

* Add `cstring#`
* Add `ToBuilder` and `ToBoundedBuilder` classes
* Add quasiquoter named `bldr` in `Data.Bytes.Builder.Template`.

## 0.3.8.0 -- 2021-06-25

* Fix `doubleDec`, which was encoding small numbers incorrectly.
* Add `runByteString` for producing `ByteString` from bounded builders.
* Add `cstring#` for producing builder from `Addr#`.
* Correct the required length calculation for json string encoding.

## 0.3.7.0 -- 2020-11-06

* Fix build error in test suite.
* Add `ascii7` and `ascii8`.
* Performance tweak for json string escape builder.

## 0.3.6.0 -- 2020-06-30

* Add `replicate`.
* Fix compatibility with `primitive-0.7.1.0`.

## 0.3.5.0 -- 2020-05-01

* Add `wordLEB128` and `word64LEB128`.
* Add `integerDec` and `naturalDec`.
* Add `word48PaddedLowerHex`.
* Add `newBuilderState`, `closeBuilderState`, `commitDistance`, and
  `commitDistance1` to the unsafe module.

## 0.3.4.0 -- 2020-02-27

* Rename the library from `small-bytearray-builder` to `bytebuild`, and
  move modules from the `Data.ByteArray` namespace to the `Data.Bytes`
  namespace. This is not considered a breaking change since
  `small-bytearray-builder` continues to exist as a compatibility shim,
  reexporting the modules with their old names.
* Add `wordPaddedDec4`.
* Add `reversedOnto` and `commitsOntoChunks`.
* Add `ascii(2|3|4|5|6)`.
* Add `cstringLen` unbounded builder.
* Add `copy2`.

## 0.3.3.0 -- 2020-02-10

* Add `word64PaddedLowerHex` and `word32PaddedLowerHex`
* Add `word256Array{LE,BE}` and `word256{LE,BE}`
* Add `word{128,256}Padded{Lower,Upper}Hex`

## 0.3.2.0 -- 2020-01-20

* Add `putMany`, which allows pasting into the same mutable byte
  array over and over.
* Add `consLength`.
* Add `putManyConsLength`, useful for chunked HTTP encoding.
* Add `runOnto`
* Add `Data.Bytes.Chunks.length`
* Add `wordPaddedDec2` and `wordPaddedDec9`.
* Add `word8PaddedLowerHex`.

## 0.3.1.0 -- 2019-11-20

* Add big-endian and little-endian parsers for `Word128`. This includes
  both the single and multiple element variants.
* Export `reverseCommitsOntoChunks` from the `Unsafe` module.
* Add `Semigroup` and `Monoid` instances for `Chunks`.
* Add `consLengthLE32`.
* Add `fromEffect` to the unsafe interface.

## 0.3.0.0 -- 2019-10-17

* Breaking change: Change the internal implementation of `Builder`. This
  now works a lot more like the builder from `bytestring`. It accumulates
  chunks and can do a zero-copy appends when working with a sufficiently
  large immutable chunk. This introduces a mild performance regression
  (around 10%), but it makes the libary more generally useful.
* Introduce `consLengthBE32` and `consLength64BE` for efficient serialization
  of wire protocols that require prefixing a payload with its length.
* Add `int{16,32,64}BE` and `int{16,32,64LE}` as conveniences.
* Add little-endian encoding functions for `Word16`, `Word32`, and `Word64`.
* Add big-endian and little-endian functions for copying a
  `PrimArray` of numbers (both signed and unsigned) into a builder.
* Add `flush`, `copy`, and `insert` for better control when
  converting byte sequences to builders.
* Add `shortByteString` to improve interoperability with the
  `bytestring` library.

## 0.2.1.0 -- 2019-09-05

* Stop exporting data constructor in `Data.ByteArray.Builder`.
  This is technically a breaking change, but it was only
  exported by accident. So, with this release, we will technically
  violate PVP, and the previous release will be deprecated on hackage.
* Add more functions for encoding unsigned words: `word16PaddedLowerHex`,
  `word16LowerHex`, `word16UpperHex`, `word8LowerHex`.
* Unroll loop for `word8Dec`.

## 0.2.0.0 -- 2019-09-04

* Use `natural-arithmetic` to make manipulation of bounds possible.
* Add more functions for encoding numbers. This includes
  `word8/16/32/64` and `int8/16/32/64`.
* Rename the modules.
* Correct a serious error in the implementation of `bytes`.
* Make `pasteGrowST` accept an initial offset.
* Add a `pasteGrowST` for length-indexed builders.
* Add function for rendering floating-point numbers in a slightly
  inaccurate way.
* Add functions for encoding `ShortText` as UTF-8 and as a JSON string.

## 0.1.1.0 -- 2019-07-30

* Add several additional functions for encoding numbers.

## 0.1.0.0 -- 2019-06-25

* First version
