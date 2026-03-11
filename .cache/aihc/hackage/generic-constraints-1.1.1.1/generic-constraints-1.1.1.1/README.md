# Standalone deriving without boiler-plate

Write short and concise contexts based on generics.

Instead of writing boiler-plate standalone deriving clauses in the form of

```Haskell
deriving instance [Various Eq Constraints Here] => Instance Eq MyType
```

With generic-constraints you can use

```Haskell
deriving instance Constraints MyType Eq => Eq MyType
```

Or, using TH, simply

```Haskell
makeDeriving ''Eq ''MyType
```

And for several classes and types:

```Haskell
makeDerivings [''Eq, ''Ord, ''Show] [''MyType, ''MyOtherType]
```

## Credits

This library was extracted from the [one-liner](http://hackage.haskell.org/package/one-liner) library by Sjoerd Visscher and Xia Li-yao.
