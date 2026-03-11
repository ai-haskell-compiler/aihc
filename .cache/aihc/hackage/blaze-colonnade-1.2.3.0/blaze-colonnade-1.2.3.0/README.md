To run doctests, first make sure `doctest` is on the `PATH` (i.e. `cabal install doctest`), then run the following commands:

```
cabal repl --build-depends=QuickCheck,profunctors --with-ghc=doctest --repl-options="-fno-warn-orphans -Wno-x-partial" blaze-colonnade
```
