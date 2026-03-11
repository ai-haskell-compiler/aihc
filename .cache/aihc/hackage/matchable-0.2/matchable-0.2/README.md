[![Haskell CI](https://github.com/viercc/matchable/actions/workflows/haskell.yml/badge.svg)](https://github.com/viercc/matchable/actions/workflows/haskell.yml)

## matchable

This package defines a type class `Matchable`,
which provides `zipMatch` operation for zipping two values of a
container type.

The `zipMatch` operation can fail. It returns the zipped value wrapped
in `Maybe`. Specifically, `zipMatch` returns zipped value if and only if two arguments
have the exactly same shape.

### Example

``` haskell
>>> zipMatch [1,2] ['a','b']
Just [(1,'a'), (2,'b')]
>>> zipMatch [1,2,3] ['a','b']
Nothing
```

See [examples](https://github.com/viercc/matchable/blob/master/example/README.md) also.
