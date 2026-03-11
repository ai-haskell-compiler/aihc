[![build](https://github.com/sergv/prettyprinter-combinators/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/prettyprinter-combinators/actions/workflows/haskell-ci.yaml)

This is a set of utilities for the Haskell `prettyrinter` package.
Most notable is automatic deriving of `Pretty` instance from the
`Generic` instance, e.g.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Prettyprinter.Generics

data Foo a b = Bar Int | Baz a b
  deriving (Generic)

instance (Pretty a, Pretty b) => Pretty (Foo a b) where
  pretty = ppGeneric

printed :: Doc ann
printed = pretty $ Baz (Bar 10 :: Foo () ()) [1..22]
```

which would put following into `printed`:

```
Baz
  Bar 10
  [ 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22 ]
```
