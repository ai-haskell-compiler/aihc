logfloat
========
[![CI Status](https://github.com/wrengr/logfloat/actions/workflows/ci.yml/badge.svg)](https://github.com/wrengr/logfloat/actions?query=workflow%3Aci+-event%3Apull_request)
[![Hackage version](https://img.shields.io/hackage/v/logfloat.svg?style=flat)](https://hackage.haskell.org/package/logfloat)
[![Stackage LTS version](https://stackage.org/package/logfloat/badge/lts)](https://stackage.org/lts/package/logfloat)
[![Stackage Nightly version](https://stackage.org/package/logfloat/badge/nightly)](https://stackage.org/nightly/package/logfloat)

This package provides a type for storing numbers in the log-domain,
primarily useful for preventing underflow when multiplying many
probabilities as in HMMs and other probabilistic models. The package
also provides modules for dealing with floating numbers correctly.

As of version 0.13.3, we've dropped support for Hugs and GHC < 7.8.
Nothing major has changed, so they should still work; it's just
that they're no longer officially supported. (In practice, GHC 7.6
still works just fine, and is still being tested against. It's just
that this support will be droped in the future, once it becomes a
burden.) Thus, this version of the library provides a transitional
point between backwards compatability and adding new features (see
below).

## Install

In general, this is a simple package and should be easy to install.
The specifics are a bit murky however, since we use CPP and the FFI
and want to retain portability, and due to the rapid evolution of
Cabal and other development tools. However, you should be able to
use the standard:

    $> cabal install logfloat


### FFI Problems

The logfloat package uses the FFI to access functions in libm to
improve accuracy. These functions are part of the ISO C 1999 standard
and are supported on most systems, however they're not part of the
ANSI C standard nor the System V standard and so they might be
unavailable on some systems. If you encounter errors during build
about not being able to find `<math.h>`, not having prototypes for
functions, or linking errors, then you are probably on such a system.

In order to use pure Haskell and disable the FFI, pass -f-useFFI
during the configure phase to disable the useFFI flag. This will
degrade the accuracy of certain operations, but should still compile
cleanly.

If you run into errors like "error: 'log1p' redeclared as different
kind of symbol" this is due to a bug in GHC 6.10.1 where you can't
use both -fvia-C and the FFI. This bug should be circumvented for
the moment, but resolving it is still a work in progress. See the
bug report (which is resolved in GHC 6.10.2):

    http://hackage.haskell.org/trac/ghc/ticket/3117


### Windows FFI

The logfloat package builds and installs cleanly with GHC 8.0 and
later on Windows (without needing Cygwin nor Mingw/Msys), and the
installed package can be used in compiled programs without any
issues.

Prior to GHC 8.0, the package does not work from the GHCi debugger
by default and gives errors like "can't load .so/.DLL for: m (addDLL:
could not load DLL)". This is a long-standing issue with GHCi having
to do with the fact that, on Windows, the so-called "libm" file does
not actually contain anything (as it does on POSIX) and the C
functions we use are instead placed in `libmingwex.a` (which comes
bundled with GHC). The problem is that `ghc` (the compiler) knows
to pull in `libmingwex.a`, whereas `ghci` (the interactive debugger)
does not. All of this is true at least as far back as Windows XP
and GHC 6.10.1. Some more information can be found in the ticket:

    https://ghc.haskell.org/trac/ghc/ticket/3242

The most reliable workaround at this point, alas, is to compile the
library with FFI support disabled in order to be able to use it in
GHCi, and then to recompile with FFI enabled whenever you need to
ship a compiled program.

A less-reliable workaround (i.e., needs to be tailored for your
system based on your paths, and you'll need to have `gcc` installed)
which avoids the need for recompilation is to generate the DLL
yourself by running the following two commands and then placing the
resulting `m.dll` into your path.

    ar -x libmingwex.a
	gcc -shared *.o -o m.dll


### Testing

If you want to run the test suite, use the following standard method
(with `runhaskell Setup.hs` in lieu of `cabal`, if necessary):

    $> cd logfloat
    $> cabal configure --enable-tests --enable-coverage
    $> cabal build
    $> cabal test --keep-tix-files

The results of the code coverage are in
`./dist/hpc/vanilla/html/logfloat-$VERSION/hpc_index.html`.  If
you're not interested in the coverage of the test suite, then you
needn't pass the `--enable-coverage` nor `--keep-tix-files` flags.
Note that older versions of cabal used the flag name
`--enable-library-coverage` instead of `--enable-coverage`. And
IIRC hpc integration in cabal was broken for ghc-7.6.


### Haddock Problems

In Cabal 1.2 there is a bug in the handling of building Haddock
documentation when CPP is involved. These issues have been fixed
in Cabal 1.6, but here are the instructions if you're on older
systems.

In Cabal 1.2 the cpp-options field is not passed to Haddock, and
therefore any macros defined there are not seen, which can cause
Haddock to fail. The old workaround was to define CPP macros in the
ghc-options field which *does* get passed to Haddock. This is now
considered bad style and is forbidden by Hackage. It appears that
passing --haddock-option=... or --ghc-option=... flags during
configure does not have the same effect as defining the field.

Therefore, in order to properly compile Haddock documentation on
Cabal 1.2, you should go into logfloat.cabal and uncomment the
ghc-options fields which declare CPP macros -D__USE_FFI__ and
-D__HUGS__=200609. If you know of a better workaround for this
configuration, contact the maintainer.


### Building for Hugs (September 2006)

If you're feeling adventurous and want to try and get the library
to compile under Hugs, here's how you used to be able to compile
things:

    runhaskell Setup.hs configure --hugs  \
        --with-cpphs="`which cpphs-hugs`" \
        --ffihugs-options="-98 +o"        \
        --ffihugs-option=-F'cpp -P -traditional -D__HUGS__=200609 -D__USE_FFI__'
    runhaskell Setup.hs build
    runhaskell Setup.hs copy
    runhaskell Setup.hs register

If you need to disable the FFI due to issues with not being able
to find <math.h>, not having prototypes for functions, or linking
errors, be sure *not* to pass -D__USE_FFI__ to the cpp filter for
ffihugs.

Notably, Hugs installs cpphs under the name "cpphs-hugs" by default.
The `which` command will try to resolve the location, assuming it's
on your $PATH. If it isn't, then change the --with-cpphs= flag to
point to where cpphs is installed on your system.

Some additional details about difficulties with building for Hugs
can be found in this blog post and the bug reports:

    http://winterkoninkje.livejournal.com/60707.html
    http://hackage.haskell.org/trac/hackage/ticket/526
    http://hackage.haskell.org/trac/hackage/ticket/527

And while previous versions of Cabal could build this package (with
the above commandline to work around those bugs), Hugs support is
broken in Cabal 1.8 (including at least 1.8.0.2 through 1.8.0.6):

    http://hackage.haskell.org/trac/hackage/ticket/633


## Changes: Version 0.13.3+ (2015-03-29) vs 0.12.1 (2010-03-19)

* Monomorphized `logFloat`, `logToLogFloat`, `fromLogFloat`, and
`logFromLogFloat`: that is, they all take/return `Double` now. The
change was made to help reduce the need for explicit type signatures.
It shouldn't really affect most users, since it seems noone was
really making use of the polymorphism provided by previous versions.
To get the previous behavior back, just explicitly add calls to
`realToFrac` wherever necessary.

* Fixed some instances to get them to compile under the new role-based
type system of GHC 7.10

* Cleaned up various extraneous rewrite rules, specializations, etc

* Added the functions `sum`, `product`, and `pow`. Both sum and
product preserve more precision than the fold-based definitions in
the Prelude. Moreover, sum is *much* faster than the Prelude version,
since it only requires crossing the log/exp boundary n+1 times,
instead of 2\*(n-1) times. The only downside is that sum requires
two passes over the input and thus is not amenable to list fusion.

* (Version 0.13.3.2; 2015-08-06) Fixed the `Show LogFloat` instance
to produce parentheses in the right place.


## Upcoming changes (0.14+)

* Since the `Data.Number.RealToFrac` module is no longer required
by any of the others, it will probably be forked off to a separate
package in order to improve portability of the rest of the package
by removing the need for MPTCs.

* There's long been clamoring for adding a
vector:`Data.Vector.Unboxed.Unbox` instance. I've been reluctant
to add such an instance due to wanting to retain backwards compatibility
and portability. Having dropped support for Hugs and older versions
of GHC, I'm now willing to add them in.

The logfloat library is conceptually quite simple, and thus to
whatever extent possible I'd still like to retain portability to
non-GHC compilers. So if you are interested in using logfloat with
another compiler/interpreter but run into problems (e.g., due to
the type families required by the vector library), please get in
touch and I'll try to get things to work.


## Compatibility / Portability

The package is compatible with GHC 7.8.3 and 7.10.1. It may still
compile with older versions of GHC (or even Hugs!), however they
are no longer officially supported.

The package is not compatible with nhc98 and Yhc because
`Data.Number.RealToFrac` uses MPTCs. However, that module is no
longer required by any others, and all the other modules should be
compatible with these compilers. Thus, it should be fairly easy to
port. If you do so, please let me know and I'll try to incorporate
support for them.


## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/logfloat)
* [GitHub](https://github.com/wrengr/logfloat)
