bytestring-lexing
=================
[![Hackage version](https://img.shields.io/hackage/v/bytestring-lexing.svg?style=flat)](https://hackage.haskell.org/package/bytestring-lexing) 
[![Build Status](https://github.com/wrengr/bytestring-lexing/workflows/ci/badge.svg)](https://github.com/wrengr/bytestring-lexing/actions?query=workflow%3Aci)
[![Dependencies](https://img.shields.io/hackage-deps/v/bytestring-lexing.svg?style=flat)](http://packdeps.haskellers.com/specific?package=bytestring-lexing)

The bytestring-lexing package offers extremely efficient `ByteString`
parsers for some common lexemes: namely integral and fractional
numbers. In addition, it provides efficient serializers for (some
of) the formats it parses.

As of version 0.3.0, bytestring-lexing offers the best-in-show
parsers for integral values. And as of version 0.5.0 it offers (to
my knowledge) the best-in-show parser for fractional/floating
numbers. A record of these benchmarks can be found
[here](https://github.com/wrengr/bytestring-lexing/tree/master/bench/html)


## Install

This is a simple package and should be easy to install. You should
be able to use the standard:

    $> cabal install bytestring-lexing


### Testing

To run the test suite (without coverage information), you can use
the standard method (with `runhaskell Setup.hs` in lieu of `cabal`,
if necessary):

    $> cd bytestring-lexing
    $> cabal configure --enable-tests
    $> cabal build
    $> cabal test

If you want coverage information as well, there are a few options
depending on your version of Cabal.  For modern cabal with v2/nix-style
builds, add `--enable-coverage` to the configure step, and the
results will be located at
`./dist-newstyle/build/$ARCH/$GHC/bytestring-lexing-$VERSION/opt/hpc/vanilla/html/bytestring-lexing-$VERSION/hpc_index.html`.
For v1/classic builds, add `--enable-coverage` to the configure
step and also add `--keep-tix-files` to the test step, and the
results are instead located at
`./dist/hpc/vanilla/html/bytestring-lexing-$VERSION/hpc_index.html`.
For very old versions of Cabal, you must use `--enable-library-coverage`
in lieu of `--enable-coverage`.


### Benchmarks

If you want to run the benchmarking code, then do:

    $> cd bytestring-lexing/bench
    $> cabal configure
    $> cabal build
    $> for b in isSpace numDigits packDecimal readDecimal readExponential ceilEightThirds; do
           ./dist/build/bench-${b}/bench-${b} -o ${b}.html;
       done && open *.html

Of course, you needn't run all the benchmarking programs if you
don't want. Notably, these benchmarks are artefacts of the development
of the library. They are not necessarily the most up-to-date
reflection of the library itself, nor of other Haskell libraries
we've compared against in the past.


## Portability

An attempt has been made to keep this library portable. However,
we do make use of two simple language extensions. Both of these
would be easy enough to remove, but they should not pose a significant
portability burden. If they do in fact pose a burden for your
compiler, contact the maintainer.

* ScopedTypeVariables - the `decimalPrecision` function in
    `Data.ByteString.Lex.Fractional` uses ScopedTypeVariables for
    efficiency; namely to ensure that the constant function
    `decimalPrecision` need only compute its result once (per type),
    and that its result has no data dependency on the proxy argument.
* BangPatterns - are used to make the code prettier and to "improve"
    code coverage over the equivalent semantics via the following
    idiom:
    
        foo x ... z
            | x `seq` ... `seq` z `seq` False = error "impossible"
            | otherwise = ...
    
    BangPatterns are supported in GHC as far back as [version
    6.6.1][ghc-bangpatterns], and are also supported by
    [JHC][jhc-bangpatterns] and [UHC][uhc-bangpatterns]. As of 2010,
    they were [not supported by Hugs][hugs-bangpatterns]; but alas
    Hugs is pretty much dead now.

[ghc-bangpatterns]: 
    https://downloads.haskell.org/~ghc/6.6.1/docs/html/users_guide/sec-bang-patterns.html
[jhc-bangpatterns]:
    http://repetae.net/computer/jhc/manual.html#code-options
[uhc-bangpatterns]:
    https://github.com/UU-ComputerScience/uhc-js/issues/1
[hugs-bangpatterns]: 
    https://mail.haskell.org/pipermail/haskell-cafe/2010-July/079946.html


## Changes: Version 0.5.0 (2015-05-06) vs 0.4.3 (2013-03-21)

I've completely overhauled the parsers for fractional numbers.

The old `Data.ByteString.Lex.Double` and `Data.ByteString.Lex.Lazy.Double`
modules have been removed, as has their reliance on Alex as a build
tool. I know some users were reluctant to use bytestring-lexing
because of that dependency, and forked their own version of
bytestring-lexing-0.3.0's integral parsers. This is no longer an
issue, and those users are requested to switch over to using
bytestring-lexing.

The old modules are replaced by the new `Data.ByteString.Lex.Fractional`
module. This module provides two variants of the primary parsers.
The `readDecimal` and `readExponential` functions are very simple
and should suffice for most users' needs. The `readDecimalLimited`
and `readExponentialLimited` are variants which take an argument
specifying the desired precision limit (in decimal digits). With
care, the limited-precision parsers can perform far more efficiently
than the unlimited-precision parsers. Performance aside, they can
also be used to intentionally restrict the precision of your program's
inputs.


## Benchmarks: Version 0.5.0 (2015-05-06)

The Criterion output of the benchmark discussed below, [is available
here](https://github.com/wrengr/bytestring-lexing/blob/master/bench/html/readExponential-0.5.0_ereshkigal.html).
The main competitors we compare against are the previous version
of bytestring-lexing (which already surpassed text and
attoparsec/scientific) and bytestring-read which was the previous
best-in-show.

The unlimited-precision parsers provide 3.3x to 3.9x speedup over
the `readDouble` function from bytestring-lexing-0.4.3.3, as well
as being polymorphic over all `Fractional` values. For `Float`/`Double`:
these functions have essentially the same performance as bytestring-read
on reasonable inputs (1.07x to 0.89x), but for inputs which have
far more precision than `Float`/`Double` can handle these functions
are much slower than bytestring-read (0.30x 'speedup'). However,
for `Rational`: these functions provide 1.26x to 1.96x speedup
compared to bytestring-read.

The limited-precision parsers do even better, but require some care
to use properly. For types with infinite precision (e.g., `Rational`)
we can pass in an 'infinite' limit by passing the length of the
input string plus one. For `Rational`: doing so provides 1.5x speedup
over the unlimited-precision parsers (and 1.9x to 3x speedup over
bytestring-read), because we can avoid intermediate renormalizations.
Whether other unlimited precision types would see the same benefit
remains an open question.

For types with inherently limited precision (e.g., `Float`/`Double`),
we could either pass in an 'infinite' limit or we could pass in the
actual inherent limit. For types with inherently limited precision,
passing in an 'infinite' limit degrades performance compared to the
unlimited-precision parsers (0.51x to 0.8x 'speedup'). Whereas,
passing in the actual inherent limit gives 1.3x to 4.5x speedup
over the unlimited-precision parsers. They also provide 1.2x to
1.4x speedup over bytestring-read; for a total of 5.1x to 14.4x
speedup over bytestring-lexing-0.4.3.3!


## Links

* [Website](https://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/bytestring-lexing)
* [GitHub](https://github.com/wrengr/bytestring-lexing)
