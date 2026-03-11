# `mtl-compat`
[![Hackage](https://img.shields.io/hackage/v/mtl-compat.svg)][Hackage: mtl-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/mtl-compat.svg)](http://packdeps.haskellers.com/reverse/mtl-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/haskell-compat/mtl-compat.svg)](https://travis-ci.org/haskell-compat/mtl-compat)

[Hackage: mtl-compat]:
  http://hackage.haskell.org/package/mtl-compat
  "mtl-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package backports the `Control.Monad.Except` module from `mtl` (if using `mtl-2.2.0.1` or earlier), which reexports the `ExceptT` monad transformer and the `MonadError` class.

This package should only be used if there is a need to use the `Control.Monad.Except` module specifically. If you just want `mtl` class instances for `ExceptT`, use `transformers-compat` instead, since `mtl-compat` does nothing but reexport the instances from that package.

Note that unlike how `mtl-2.2` or later works, the `Control.Monad.Except` module defined in this package exports all of `ExceptT`'s monad class instances. Therefore, you may have to declare `import Control.Monad.Except ()` at the top of your file to get all of the `ExceptT` instances in scope.
