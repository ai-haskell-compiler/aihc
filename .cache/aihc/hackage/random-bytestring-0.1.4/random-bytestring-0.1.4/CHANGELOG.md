0.1.3.2 (2019-06-06)
====================

*   Relax upper bound on `base` to `<5`.
*   Include GHC-8.6.5 into test-matrix.

0.1.3.1 (2018-03-18)
====================

*   Put dependency on pcg-random behind a cabal flag, which is on by default.
    This enables builds stackage nightly.

0.1.3 (2018-03-17)
==================

*   Support for GHC-8.4.1

0.1.2
=====

*   Add support for PCG as PRNG in addition to MWC.

    The module `Data.ByteString.Random.PCG` uses PCG. The module
    `Data.ByteString.Random.MWC` uses MWC. The module `Data.ByteString.Random`
    uses MWC for backward compatibility.

*   Support usage of custom PRNGs through the type class `RandomWords` and the
    function `generate` in the module `Data.ByteString.Random.Internal`.

*   Add some benchmark results to package description

0.1.1
=====

*   Support for GHC-8.2.1 and Cabal-2.0

0.1.0
=====

*   In the implementation of `random`, use `withSystemGen` that seeds the PRNG
    from the system randomness source.

0.0.1
=====

*   First public release
