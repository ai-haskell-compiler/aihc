This Haskell library provides string interpolation in quasiquotes, allowing you to build strings like this:

```haskell
animal = "snake"
location = "a tree"
[exon|#{animal} in #{location}|]
-- "snake in a tree"
```

Each step of the process is customizable based on the result type of the quote, making it possible to construct strings
for arbitrary types.
For example, `String -> String` is the type used by `showsPrec`, which can be a bit of a hassle to write:

```haskell
data Record =
  Record {
    number :: Int,
    maybeNumber :: Maybe Int,
    value :: Value
  }

instance Show Record where
  showsPrec d Record {..} =
    showParen (d > 10) [exon|Record #{showsPrec 11 number} #{showsPrec 11 maybeNumber} #{showsPrec 11 value}|]
```

[hackage]: https://hackage.haskell.org/package/exon/docs/Exon.html
