# Multiarg

This is multiarg, a library of combinators to parse command lines.

For "released" code see Hackage:

http://hackage.haskell.org/package/multiarg

multiarg is on Github:

http://www.github.com/massysett/multiarg

## Building

If you obtained this code through Hackage, just build it using the
ordinary Cabal command:

cabal install

If you obtain this code on Github, you will first need to generate the
Cabal file and generate the tests.  This will require that you install
two libraries:

cabal install cartel quickpull

Then run this script to generate the Cabal file and the tests:

sh generate

## Versioning

multiarg releases are numbered in accordance with the Haskell
Package Versioning Policy.

Currently the multiarg library depends only on the "base" package, so
multiarg should have wide compatibility with different compilers and
sets of libraries.  The tests have some additional dependencies.

## Build history

If you're having trouble building multiarg, try looking at the
travis-ci build history at:

https://travis-ci.org/massysett/multiarg

It shows successful builds and the versions of any package
dependencies that were installed when that build succeeded, so it
might help you diagnose any dependency issues.

[![Build Status](https://travis-ci.org/massysett/multiarg.svg?branch=master)](https://travis-ci.org/massysett/multiarg)

## Similar libraries

Of course there are many command-line parsing modules and libraries
out there; here are some comparisons.

[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative):
very featureful with a well thought-out interface.  Builds help for
you.  I often use this if it meets my needs.  From what I can tell,
though, it strips out information about the relative ordering of the
words from the command line; for instance, if the user typed "hello
--opt1 --opt2", you cannot tell whether she entered "--opt1" before
she entered "--opt2".  Also, from what I can tell it cannot easily
parse options that take more than one argument.

[GetOpt](https://hackage.haskell.org/package/base-4.7.0.2/docs/System-Console-GetOpt.html):
comes with the base libraries, so you don't have to install anything
extra, which gives it a huge advantage.  Keeps information about the
relative ordering of the words from the command line.  Cannot easily
parse options that take more than one argument.

[cmdargs](https://hackage.haskell.org/package/cmdargs): after multiple
passes through the Haddocks I could never make any sense of this
library at all, which must be a reflection of my level of Haskell
ignorance.

More comparisons are at the [Haskell
Wiki](https://www.haskell.org/haskellwiki/Command_line_option_parsers).
