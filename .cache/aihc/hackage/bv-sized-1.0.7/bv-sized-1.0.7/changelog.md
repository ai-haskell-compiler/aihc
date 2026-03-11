# Changelog for [`bv-sized` package](http://hackage.haskell.org/package/bv-sized)

## 1.0.7 -- 2026-01-24

* Support building with `parameterized-utils` 2.2.*

## 1.0.6 *March 2025*

* Support building with `random-1.3.*`

## 1.0.5 *January 2023*

* Support building with GHC 9.4
* Add `Lift`, `NFData`, and `Hashable` instances for `SignedBV` and
  `UnsignedBV`

## 1.0.4 *March 2022*

* Deprecates trunc' and adds two alternatives, sresize and zresize
* Support for GHC 9.2
* BV is now a newtype

## 1.0.3 *April 2021*

* New instances (`NFData`, `Random`)
* New functions for `BV` that create uniformly random bitvectors
* Fix: Adds `asBV` for `SignedBV` (should have been there to begin with)

## 1.0.2 *August 2020*

* Allows tasty 1.3 for test suite
* Fixes bug in signedClamp function which made it possible to violate
  the nonnegative invariant on the internal representation of BVs
* Fixes divide by zero error in rotateL and rotateR
* Enhances test suite to test well-formedness of all operators that
  return a BV
* Fixes some documentation

## 1.0.1 *May 2020*

This fixed a subtle bug in the test suite which occasionally caused a
divide by zero exception.

## 1.0.0 *May 2020*

This is a completely breaking change and it is completely incompatible
with any previous use for this library.

  * Bitvectors no longer track their own width. Every operations that
    relies on runtime awareness of the width (for instance,
    truncations) requires an expicit 'NatRepr' argument.
  * Bitvectors do not support any typical instances you might hope for
    (Num, Bits, etc.). This is because they are not interpreted by
    default as signed or unsigned, so any class that requires such an
    interpretation is not supported. We do provide wrapper types that
    supply those instances when the bitvector width is known
    (SignedBV/UnsignedBV).
  * Every operation has been renamed. Most are pretty straightforward
    (e.g. bvAdd ==> add).
  * Several previously unsupported operations have been added
    (e.g. count leading zeros, conversion to/from bit/bytestrings)
  * The App and BitLayout modules have been removed entirely. Both are
    potentially useful, but are out of date and probably should be in
    a different package anyway.
  * New modules
	  * Data.BitVector.Sized.{Signed,Unsigned}: wrappers for BV that
		provide many instances
	  * Data.BitVector.Sized.Overflow: wrappers for operations that
        provide overflow information as part of the output

## 0.7.0 *April 2019*
  * extractWithRepr now takes a NatRepr as an argument to specify the
    index, which it always should have.
  * Updated to recent parameterized-utils hackage release, which fixes
    the build failures in the previous bv-sized release.

## 0.6.0 *March 2019*
  * changed WithRepr functions to '
  * added Num, Bits instances
  * bitVector now takes arbitrary Integral argument
  * add 'bitLayoutAssignmentList' function (see haddocks for details
  * Hid BV constructor, exposed BitVector as pattern

## 0.5.1 *August 2018*
  * fixed github URL

## 0.5.0 *August 2018*
  * Added a lot of better support for the App module, including a type
    class for embedding BVApp expressions along with associated smart
    constructors

## 0.4.0 *April 2018*
  * Added App module for BitVector expression languages and evaluation

## 0.3.0 *April 2018*
  * fixed bug with bvShiftR, it was doing a left shift!
  * Division now rounds to zero for negative integers, bvDiv -> bvQuot
  * added Ix instance for BitVector w
  * added bv0, zero-width vector
  * bvConcatMany, bvGetBytesU (conversion to/from list of bytes)

## 0.2.1 *March 2018*
  * bvMulFSU
  * bvDivU, bvDivS
  * Added Read instance, fixed Show to be compatible. Using
    prettyclass for pretty printing. (I guess this is semi-breaking,
    but whatever.)

## 0.2 *March 2018*
  * bv -> bitVector, so this is very much a breaking change
  * bvShiftL, bvShiftRL, bvShiftRA
  * bvLTU, bvLTS

## 0.1.1.1 *March 2018*
  * added BitLayout

## 0.1.1.0 *March 2018*
  * added functions `bvMulFS`/`bvMulFU` for full bitvector
    multiplication without truncation
  * removed Internal module, now export all those functions in Data.BitVector.Sized
  * fixed the bv*WithRepr functions, which were not truncating the
    inputs properly

## 0.1.0.0 *March 2018*
  * First release
