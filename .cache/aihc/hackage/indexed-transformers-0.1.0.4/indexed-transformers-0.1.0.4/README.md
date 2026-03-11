# indexed-transformers

An [Atkey indexed monad](https://bentnib.org/paramnotions-jfp.pdf)
is a `Functor` [enriched category](https://ncatlab.org/nlab/show/enriched+category).
An indexed monad transformer transforms a `Monad` into an indexed monad.

This library provides
  - a typeclass for indexed monad transformers
  - qualified do notation to use with them
  - and instances for the
    - free indexed monad transformer
    - continuation indexed monad transformer
    - state indexed monad transfomer
    - writer indexed monad transformer
