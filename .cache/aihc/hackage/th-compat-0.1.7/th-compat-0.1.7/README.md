# `th-compat`
[![Hackage](https://img.shields.io/hackage/v/th-compat.svg)][Hackage: th-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-compat.svg)](http://packdeps.haskellers.com/reverse/th-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/haskell-compat/th-compat/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-compat/th-compat/actions?query=workflow%3AHaskell-CI)

[Hackage: th-compat]:
  http://hackage.haskell.org/package/th-compat
  "th-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package defines a `Language.Haskell.TH.Syntax.Compat`
module, which backports the `Quote` and `Code` types to
work across a wide range of `template-haskell` versions.
On recent versions of `template-haskell` (2.17.0.0 or
later), this module simply reexports `Quote` and `Code`
from `Language.Haskell.TH.Syntax`. Refer to the Haddocks
for `Language.Haskell.TH.Syntax.Compat` for examples of
how to use this module.

## Quick Start Guide

Let's say you have a library that offers a `foo :: Q (TExp a)`,
you want to make it compatible with the new `Code` type,
and you intend that `foo` is spliced directly in to user code.

Use `SpliceQ` as a type alias for the return of your
function. This is `Q (TExp a)` prior to GHC 9, and `Code Q a`
after.  This allows your code to be spliced in regardless of
GHC version.

Use `liftSplice` to convert a `m (TExp a)` into a `Splice m a`.

Use `examineSplice` before typed quoters. This will allow
a typed quasiquotation to work regardless of GHC version.

When splicing in a `TExp a` value into a typed quoter, use `expToSplice`.

For a real life example, consider [this conversion, from this PR](https://github.com/parsonsmatt/discover-instances/pull/2):

```haskell
discoverInstances
    :: forall c. (Typeable c)
    => Q (TExp [SomeDict c])
discoverInstances = do
    let className = show (typeRep (Proxy @c))
    instanceDecs <- reifyInstances (mkName className) [VarT (mkName "a")]

    dicts <- fmap listTE $ traverse decToDict instanceDecs

    [|| concat $$(pure dicts) ||]

listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType

decToDict :: InstanceDec -> Q (TExp [SomeDict c])
```

With GHC 9, this will have the following problems:

1. `reifyInstances` operates in `Q`, not `Code`, so it will not type check with the `[|| concat $$(pure dicts) ||]` line.
2. We cannot call `pure` in `Code`, since `Code` is not an applicative.
3. Typed quasiquotes return a `Quote m => Code m a`, not `Q (TExp a)`.

To fix these problems, we make the following diff:

```diff
 discoverInstances
     :: forall c. (Typeable c)
-    => Q (TExp [SomeDict c])
+    => SpliceQ [SomeDict c]
- discoverInstances = do
+ discoverInstances = liftSplice $ do
     let className = show (typeRep (Proxy @c))
     instanceDecs <- reifyInstances (mkName className) [VarT (mkName "a")]

     dicts <- fmap listTE $ traverse decToDict instanceDecs

-     [|| concat $$(pure dicts) ||]
+     examineSplice [|| concat $$(expToSplice dicts) ||]
```

The above pattern should work to ensure that code is compatible across a wide range of GHC versions.
