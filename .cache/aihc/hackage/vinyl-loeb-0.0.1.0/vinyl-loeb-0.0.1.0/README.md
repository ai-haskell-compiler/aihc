# vinyl-loeb

This package defines a version of loeb's theorem for
[vinyl](https://hackage.haskell.org/package/vinyl).

```{.haskell}
rloeb :: RMap xs => Rec ((->) (Rec f xs) :. f) xs  -> Rec f xs
rloeb x = go where go = rmap (($ go) . getCompose) x
```

This can be used to fill up an extensible record lazily using
data from the result of the record itself.

```{.haskell}
type F = [Text, Text]

k :: Rec Thunk F -> Text
k (Thunk f :& _) = f <> "bar"

f :: Rec ((->) (Rec Thunk F) :. Thunk) F
f = (Compose $ const $ Thunk $ "foo") :& Compose (Thunk . k) :& RNil

-- > rloeb f
-- {"foo", "foobar"}
```
