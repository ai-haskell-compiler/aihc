A library for bit-vector arithmetic in Haskell
=========================================

Bit-vectors are represented as a pair of a _size_ and a _value_,
where sizes are of type _Int_ and values are _Integer_.
Operations on bit-vectors are translated into operations on integers.
Remarkably, most operations taking two or more bit-vectors, will
perform zero-padding to adjust the size of the input bit-vectors
when needed (eg. when adding bit-vectors of different sizes).
Indexing operators don't do this, to avoid masking _out of bounds_
errors.

Other libraries
-------------

There exist many Haskell libraries to handle bit-vectors, but to the
best of my knowledge _bv_ is the only one that adequately supports
bit-vector arithmetic.

If you do not need bit-vector arithmetic, then you may consider using
any of these other libraries, which could offer more compact and 
efficient implementations of bit arrays.

Importing and name clashes
-----------------------

Many exported functions name-clash with Prelude functions, it is
therefore recommended to do a qualified import:

    import           Data.BitVector ( BV )
    import qualified Data.BitVector as BV

Running the test suite
--------------------

If you wish to run the test suite simply:

    cabal configure -ftest
    cabal build

Then run:

    dist/build/bv-tester/bv-tester

Performance
----------

**Tip:** For best performance compile with _-fgmp_.

**Tip:** If you are brave enough, compile with _-f -check-bounds_ (disables index bounds checking).

The _BV_ datatype is simply a pair of an _Int_, to represent the
_size_, and an arbitrary-precision _Integer_, to represent the
_value_ of a bit-vector.
Both fields are strict, and we instruct GHC to unbox strict fields.
Further, we ask GHC to inline virtually all bit-vector operations.
When inlined, GHC should be able to remove any overhead associated
with the _BV_ data type, and unbox bit-vector sizes.
Performance should depend mostly on the _Integer_ data type
implementation.

