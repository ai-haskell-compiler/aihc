# trimdent

`Trimdent` is a simple library for smartly trimming and unindenting strings.

```haskell
>>> import Text.RawString.QQ (r)
>>> trimdent [r|func add(x int, y int) int {
                  return x + y
                }
             |]
"func add(x int, y int) int {\n\
\  return x + y\n\
\}"
```

## Why is this useful?

This library is useful when you are writing your own quasi quoter and want to
sanitize whitespace.

## How does it compare to other libraries?

* [Neat-interpolation](https://hackage.haskell.org/package/neat-interpolation)
  exposes the same functionality but only in an interpolating quasi quoter.
  This means that you can't easily use it in your own quoters.
* [raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq-1.1)
  gives quasi quoters to express multi-line strings, but they don't do any
  trimming.
