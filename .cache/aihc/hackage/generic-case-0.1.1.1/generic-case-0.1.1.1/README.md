[![Haskell CI](https://github.com/fpringle/generic-case/actions/workflows/haskell.yml/badge.svg)](https://github.com/fpringle/generic-case/actions/workflows/haskell.yml)

# Generic case analysis functions

"Case analysis" functions are those which take one function for each constructor of a sum type, examine a value of that type, and call the relevant function depending on which constructor was used to build that type. Examples include
[maybe](https://hackage.haskell.org/package/base/docs/Data-Maybe.html#v:maybe),
[either](https://hackage.haskell.org/package/base/docs/Data-Either.html#v:either) and
[bool](https://hackage.haskell.org/package/base/docs/Data-Bool.html#v:bool). `generic-case` gives you these functions for any type which implements `Generic` from [generics-sop](https://hackage.haskell.org/package/generics-sop/docs/Generics-SOP.html#t:Generic).

```haskell
maybe :: forall a r. r -> (a -> r) -> Maybe a -> r
maybe = gcaseR @(Maybe a)
```

For more detailed documentation, see [Generics.Case](https://hackage.haskell.org/package/generics-case/docs/Generics-Case.html).
