# fusion-plugin-types

[![Hackage](https://img.shields.io/hackage/v/fusion-plugin-types.svg?style=flat)](https://hackage.haskell.org/package/fusion-plugin-types)
![](https://github.com/composewell/fusion-plugin-types/workflows/Haskell%20CI/badge.svg)

## Motivation

This package provides types needed to run the fusion-plugin
plugin. This package is separated from the
[fusion-plugin](https://github.com/composewell/fusion-plugin) package
so that library authors can annotate the types that `fusion-plugin`
should try to fuse but avoid the `ghc` dependency in the library
itself.

## Using the package

To enable support for using the `fusion-plugin` plugin, add this
package to your `build-depends` and annotate your types with `Fuse`
type from `Fusion.Plugin.Types` module.

```haskell
import Fusion.Plugin.Types (Fuse (..))

{-# ANN type Step Fuse #-}
data Step s a = Yield a s | Skip s | Stop
```

## See Also

* [fusion-plugin](https://github.com/composewell/fusion-plugin)
