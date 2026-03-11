# Show combinators [![Hackage](https://img.shields.io/hackage/v/show-combinators.svg)](https://hackage.haskell.org/package/show-combinators) [![Build Status](https://travis-ci.org/Lysxia/show-combinators.svg)](https://travis-ci.org/Lysxia/show-combinators)

A minimal set of convenient combinators to write `Show` instances.

```haskell
data MyType a
  = C a a                   -- a regular constructor
  | a :+: a                 -- an infix constructor
  | R { f1 :: a, f2 :: a }  -- a record

infixl 4 :+:

instance Show a => Show (MyType a) where
  showsPrec = flip precShows where
    precShows (C a b) = showCon "C" @| a @| b
    precShows (c :+: d) = showInfix ":+:" 4 c d
    precShows (R {f1 = e, f2 = f}) =
      showRecord "R" ("f1" .=. e &| "f2" .=. f)
```
