# `libffi`
[![Hackage](https://img.shields.io/hackage/v/libffi.svg)][Hackage: libffi]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/libffi.svg)](http://packdeps.haskellers.com/reverse/libffi)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/remiturk/libffi/workflows/Haskell-CI/badge.svg)](https://github.com/remiturk/libffi/actions?query=workflow%3AHaskell-CI)

[Hackage: libffi]:
  http://hackage.haskell.org/package/libffi
  "libffi package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

A binding to `libffi`, allowing C functions of types only known at runtime to be called from Haskell.

# Notes on GHC's bundling of `libffi`

This library makes a somewhat unusual choice: by default, it does not
explicitly declare a dependency against the `libffi` C library. This is because
most binary distributions of GHC—that is, GHCs configured without the
`--with-system-libffi` option—bundle their own copies of `libffi`. One of these
copies is statically linked, and another of these copies is dynamically linked.
Moreover, whenever GHC compiles an executable, it will always pass the
necessary flags to link against its static copy of `libffi`, as the GHC runtime
system depends on it.

When GHC bundles its own copies of `libffi`, if you were to declare, say, an
`extra-libraries: ffi` dependency, then it would not behave in the way that you
would expect. This is because:

1. The linker flags to link against GHC's static copy of `libffi` always come
   first in the final linking step when compiling an executable. As a result,
   declaring an `extra-libraries: ffi` dependency won't make much of a
   difference, since GHC will always statically link against its own copy of
   `libffi` anyway due to the order of linker flags.

2. Moreover, declaring an `extra-libraries: ffi` dependency can have the
   unfortunate side effect of declaring an _unused_ dynamic dependency against
   `libffi`. Even worse is the fact that the version of dynamically linked
   `libffi` that comes with your operating system may differ from the version
   of dynamically linked `libffi` that GHC bundles. When the version numbers
   differ, this can lead to the compiled executable failing at runtime with
   mysterious errors such as:

   ```
   error while loading shared libraries: libffi.so.7: cannot open shared object file: No such file or directory
   ```

   For more information on this point, see
   [GHC#15397](https://gitlab.haskell.org/ghc/ghc/-/issues/15397).

Observation (2) means that when GHC is configured with `--with-system-libffi`,
it is inherently fragile to use `extra-libraries: ffi`. This is an unfortunate
situation, but there is not much that one can do about this short of fixing
GHC#15397 upstream. A workaround would be to configure GHC with
`--with-system-libffi`, but practically speaking, the vast majority of GHC
binary distributions do not configure this way. This includes all versions of
GHC that `ghcup` distributes, so unless we want to exclude most GHC users, we
need some kind of workaround for this issue.

Our workaround is to rely on observation (1). That is, because GHC always
passes flags to the linker to link against its own static copy of `libffi`, we
can always assume that GHC will handle the `libffi` dependency for us. As a
result, the default behavior for this library is to enable the
`+ghc-bundled-libffi` flag, which means that the library will not declare an
external dependency on `libffi` at all. This is rather unusual, but then again,
GHC bundling its own copies of `libffi` is also unusual. (To our knowledge,
this is the _only_ C library that GHC bundles in this fashion.)

We have tested out `+ghc-bundled-libffi` on Windows, macOS, and Linux, and it
works as expected. If you encounter any linking oddities with
`+ghc-bundled-libffi`, please file an issue.

It is worth re-emphasizing that `+ghc-bundled-libffi` will only work if you are
using a binary distribution of GHC that was not configured with the
`--with-system-libffi` option. If you _are_ using such a GHC, then you will
need to use `-ghc-bundle-libffi` (note the minus sign) to disable the flag and
link against your operating system's copy of `libffi`. Unfortunately, `cabal`
does not provide a way to detect whether GHC was configured with
`--with-system-libffi` or not, so the burden is on users to enable or disable
`ghc-bundle-libffi` as appropriate.
