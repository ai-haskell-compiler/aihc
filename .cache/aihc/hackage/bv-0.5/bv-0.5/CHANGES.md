
0.5.0
=====

Note that changes in `fromInteger` and `toInteger` implementations break backwards compatibility.
I hope this will not cause major problems, let me know otherwise.

* Make _bv_ with compatible with GHC 8.4.1 (_base_ 4.11). Thanks to Kosyrev Serge!
* Define `toInteger` as `int` rather than `nat`.
* Make `fromInteger` consistent and always encode in two's complement, also positive integers.
* As a result of the two previous changes, now `toInteger . fromInteger == id`, as it should be.
* Add `Read BV` instance (based on `Text.Read`, so GHC-only).
* Fix a few bugs in the non-GMP implementation. (Fortunately, GMP is the default.)
* Remove upper bounds on testing dependencies.

0.4.1
=====

Another maintenance release:

* Fix compilation error with GHC 8.0.1.
* Add `check-bounds' flag so the user decides whether to perform bounds checking.

0.4.0
=====

This is a maintenance release, but it introduces changes to the API that required a new major version.
In summary, I have fixed a few bugs, optimized a few functions, and added a few more properties (tests).
Apart from that, and the usual clean up, there are also a handful of new API functions that I judged useful.

For performance reasons, this release introduces GMP specific optimizations.
The GMP-based backend is automatically used if available, unless _-f -gmp_ is specified.

Dependencies
-----------

Only if the library is compiled with _-fgmp_ (it will, by default, if possible):

* Depend on the _ghc-prim_ package, the GHC's internal representation of primitive types.
* Depend on the _integer-gmp_ package, the Haskell bindings for GNU's GMP library.
* Use _MagicHash_ extension to work with unboxed machine integers.

Interface
---------

* Added _bitVecs_ (list of bit-vector literals).
* Added _@:_ (indexing of multiple bits).
* Added _pow_ as an optimized exponentiation function.
* Fixed _bitVec_ (value must fit bit-with).
* Fixed _negate_ (wrong on zero bit-vector).
* Define _and_ and _or_ for the case of an empty list.
* Declared Monoid instance for bit-vector (monoid under concanetation).
* Define _join_ for the case of an empty list (it must be equivalent to _mconcat_).
* Optimized when using the GMP backend: _fromBits_, _fromInteger_, and _lg2_.
* Remove uninteresting _maxNat_ function from export list.

0.3.0
=====

This is a maintenance release, but it introduces changes to the API that required a new major version.

Dependencies
-----------

* Increase base version to 4.6.
* Support base 4.7 (new methods were added to the _Bits_ type-class).
* Use of _CPP_ extension for conditional compilation.

Interface
---------

* Replace assertions by errors when checking preconditions of exported functions.
* Use proper names for functions and encourage qualified import, names ended with underscore are now deprecated.
* Add _lsb1_ function to complement _msb1_.
* Tweak code and documentation.

