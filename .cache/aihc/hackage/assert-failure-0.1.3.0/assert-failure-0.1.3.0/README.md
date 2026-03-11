assert-failure [![Hackage](https://img.shields.io/hackage/v/assert-failure.svg)](https://hackage.haskell.org/package/assert-failure)
==============

This library contains syntactic sugar that makes it easier
to write simple contracts with 'assert' and 'error'
and report the values that violate contracts.
The original 'assert' function is here re-exported for convenience.

Make sure to enable assertions for your cabal package, e.g., by setting

    ghc-options: -fno-ignore-asserts

in your .cabal file. Otherwise, some of the functions will have
no effect at all.

The library emerged from the chaos of the tons of assertions
(sometimes augmented by comments and trace printouts) and 'error' calls in
the [LambdaHack](http://hackage.haskell.org/package/LambdaHack) game engine.
