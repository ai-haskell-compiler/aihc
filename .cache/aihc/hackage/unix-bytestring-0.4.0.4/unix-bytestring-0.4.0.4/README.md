unix-bytestring
===============
[![CI Status](https://github.com/wrengr/unix-bytestring/actions/workflows/ci.yml/badge.svg)](https://github.com/wrengr/unix-bytestring/actions?query=workflow%3Aci+-event%3Apull_request)
[![Hackage version](https://img.shields.io/hackage/v/unix-bytestring.svg?style=flat)](https://hackage.haskell.org/package/unix-bytestring) 
[![Stackage LTS version](https://stackage.org/package/unix-bytestring/badge/lts)](https://stackage.org/lts/package/unix-bytestring)
[![Stackage Nightly version](https://stackage.org/package/unix-bytestring/badge/nightly)](https://stackage.org/nightly/package/unix-bytestring)

Unix/Posix-specific functions for `ByteString`s.

This package provides `ByteString` file-descriptor based I/O API,
designed loosely after the `String` file-descriptor based I/O API
in `System.Posix.IO`. The functions here wrap standard C implementations
of the functions specified by the ISO/IEC 9945-1:1990 (“POSIX.1”)
and X/Open Portability Guide Issue 4, Version 2 (“XPG4.2”)
specifications.

Note that this package doesn't require the unix package as a
dependency. But you'll need it in order to get your hands on an
`Fd`, so we're not offering a complete replacement.


## Install

In general, this is a simple package and should be easy to install.
You must have hsc2hs installed in order to compile the package (but
then you probably already do). With the cabal-install program you
can just do the standard:

    $> cabal install unix-bytestring


### FFI Problems

The unix-bytestring package uses standard POSIX header files
`<sys/types.h>`, `<sys/uio.h>`, and `<unistd.h>`. If Cabal has
difficulty finding these files or reports another error, be sure
your include path variables are correct. If the problem persists,
contact the maintainer.


### Building for GHC (6.8 and above)

Nothing special to mention. 


### Building for Hugs (September 2006)

I haven't actually compiled this for Hugs because I don't have a
new enough version of Cabal for it, but I don't forsee any difficulties.
If you do compile this for Hugs, let the maintainer know how it
went.

When compiling for Hugs, see the following bugs for Cabal's interaction
with ffihugs. These bugs do not currently affect this package, but
this notice is here in case they affect future versions. For more
details and a minimal regression suite, see:

    <http://community.haskell.org/~wren/cabal-ffihugstest/>


(1) <http://hackage.haskell.org/trac/hackage/ticket/526>
Options in hugs-options aren't passed through to ffihugs, most
importantly -98 and +o are the ones we'd like to pass. For enabling
the +o flag Hugs-Sept06 does not honor:
    pragma    {-# LANGUAGE OverlappingInstances #-}
    pragma    {-# OPTIONS_HUGS +o #-}
    cabal     extensions: OverlappingInstances
And the -98 flag has similar issues. Therefore this is a real
problem.

Immediate solution: The options set in hugs-options should be passed
to ffihugs as well. As of Cabal 1.6 they are not passed (verified
by Duncan Coutts). The two programs accept all the same options,
so this is valid.

Ideal solution: Based on the extensions field, Cabal should
automatically determine whether -98 and +o need to be enabled (for
both hugs and ffihugs).


(2) <http://hackage.haskell.org/trac/hackage/ticket/527>
If CPP is being used in conjunction with FFI, then cpp/cpphs is not
called before ffihugs is called. Thus, users must pass an -F flag
to ffihugs in order to declare a code filter (and must pass all
cpp-options to -F manually). For example:

    --ffihugs-option=-F'cpp -P -traditional -D__HUGS__ -D__BLAH__'

This requires duplicating the build specifications, which defeats
the point of Cabal. Also it leads to tricky issues about ensuring
the proper level of quoting/escaping. (e.g. using the plural,
--ffihugs-options=..., breaks it. Wrapping the -F'cpp...' in double
quotes breaks it.)


## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/unix-bytestring)
* [GitHub](https://github.com/wrengr/unix-bytestring)
