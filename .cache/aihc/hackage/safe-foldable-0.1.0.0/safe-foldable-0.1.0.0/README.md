# safe-foldable
Provides `defaulting` and `mayhap` to safely wrap functions on `Foldable`s that are partial with null data.

```
λ. minimum []
*** Exception: Prelude.minimum: empty list
λ. minimum [3,1,2]
1

λ. defaulting 0 minimum []
0
λ. defaulting 0 minimum [3,1,2]
1

λ. mayhap minimum []
Nothing
λ. mayhap minimum [3,1,2]
Just 1
```
