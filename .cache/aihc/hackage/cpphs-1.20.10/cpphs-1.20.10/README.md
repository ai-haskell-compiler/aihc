[![Hackage version](https://img.shields.io/hackage/v/cpphs.svg?label=Hackage)](http://hackage.haskell.org/package/cpphs)
[![Stackage LTS version](https://www.stackage.org/package/cpphs/badge/lts?label=Stackage)](https://www.stackage.org/package/cpphs)
[![cpphs on Stackage Nightly](https://stackage.org/package/cpphs/badge/nightly)](https://stackage.org/nightly/package/cpphs)
[![Cabal build](https://github.com/haskell-pkg-janitors/cpphs/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-pkg-janitors/cpphs/actions)

cpphs
=====

A liberalised re-implementation of `cpp`, the C pre-processor.

`cpphs` is a re-implementation of the C pre-processor that is both more compatible with Haskell,
and itself written in Haskell so that it can be distributed with compilers.

This version of the C pre-processor is pretty-much feature-complete and compatible with traditional (K&R) pre-processors.
Additional features include: a plain-text mode; an option to unlit literate code files; and an option to turn off macro-expansion.

Usage
-----

    cpphs  [filename | -Dsym | -Dsym=val | -Ipath]+  [-Ofile]
           [ --include=file ]*
           [ --nomacro | --noline | --nowarn | --strip | --strip-eol |
             --pragma | --text | --hashes | --layout | --unlit |
             --linepragma ]*
           [ --cpp compatopts ]

For fuller details, see [docs/index.html](docs/index.html).

If you want to use `cpphs` as a completely drop-in replacement for the
real cpp, that is, to accept the same arguments, and have broadly
the same behaviour in response to them, then use the `--cpp` compatibility
option.


Copyright
---------

(c) 2004-2017 Malcolm Wallace (Malcolm.Wallace@me.com)


Licence
-------

These library modules are distributed under the terms of the LGPL.
The application module `cpphs.hs` is GPL.

This software comes with no warranty.  Use at your own risk.

If you have a commercial use for cpphs, and feel the terms of the (L)GPL
are too onerous, you have the option of distributing unmodified binaries
(only, not sources) under the terms of a different licence (see
LICENCE-commercial).


Website
-------

Description see: https://hackage.haskell.org/package/cpphs

Old homepage: https://archives.haskell.org/projects.haskell.org/cpphs/
