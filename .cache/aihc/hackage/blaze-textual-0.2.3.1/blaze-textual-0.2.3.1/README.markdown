# Welcome to blaze-textual

blaze-textual is a fast Haskell library for rendering common Haskell
datatypes in text form using the
[blaze-builder](http://hackage.haskell.org/package/blaze-builder)
library.

# Important note for users of GHCi and Template Haskell

To achieve excellent performance for rendering floating point numbers,
this package can optionally use the
[double-conversion](http://hackage.haskell.org/package/double-conversion)
package.

Unfortunately, due to bugs in GHC, some uses of GHCi and Template
Haskell can crash if the double-conversion package is used.  As a
result, this package's use of double-conversion is disabled by default.

* [5289: Can't use ghci with a library linked against
  libstdc++](http://hackage.haskell.org/trac/ghc/ticket/5289) (fixed
  in GHC 7.2.1).

* [5386: GHCi crashes with SIGFPE when using double-conversion
  package](http://hackage.haskell.org/trac/ghc/ticket/5386) (not yet
  fixed at the time of writing)

If you enable use of double-conversion and are affected by these
problems, you should expect the 5289 crash to look like this:

    Loading package double-conversion-0.2.0.0 ... can't load .so/.DLL for: stdc++

The 5386 crash causes GHCi to die with a floating point exception
(SIGFPE).

To work around these bugs, this package includes an alternative,
slower, floating point conversion that is written in pure Haskell.
Although it is 10 times slower than the double-conversion package, it
is the default because it does not crash.

If you don't use GHCi or Template Haskell, and you want to force
the use of double-conversion, you can reinstall this package by
disabling the `native` flag with `cabal`:

    cabal install -f-native --reinstall

Afterwards, you will also need to reinstall any downstream packages
that depend on this one, e.g. the [aeson JSON
library](http://hackage.haskell.org/package/aeson):

    cabal install aeson --reinstall

# Join in!

We are happy to receive bug reports, fixes, documentation
enhancements, and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/bos/blaze-textual/issues).

Master [git repository](http://github.com/bos/blaze-textual):

* `git clone git://github.com/bos/blaze-textual.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/blaze-textual):

* `hg clone http://bitbucket.org/bos/blaze-textual`

(You can create and contribute changes using either git or Mercurial.)

Authors
-------

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
