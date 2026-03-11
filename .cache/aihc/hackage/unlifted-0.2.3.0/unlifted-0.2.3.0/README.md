# unlifted

Sadly, the library cannot define types like `Maybe#` because of
[GHC issue #17178](https://gitlab.haskell.org/ghc/ghc/issues/17178).
However, there are other fun things like `Bool#` and `ShortText#`
that we are able to provide. This is intended for use with `vext`.
