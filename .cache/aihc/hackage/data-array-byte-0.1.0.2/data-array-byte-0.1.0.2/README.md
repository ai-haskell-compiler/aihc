# data-array-byte

Compatibility layer for [`Data.Array.Byte`](https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html), providing boxed wrappers for [`ByteArray#`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html#t:ByteArray-35-) and [`MutableByteArray#`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html#t:MutableByteArray-35-) and relevant instances for GHC < 9.4. Include it into your Cabal file:

```cabal
build-depends: base
if impl(ghc < 9.4)
  build-depends: data-array-byte
```

and then `import Data.Array.Byte` unconditionally.
