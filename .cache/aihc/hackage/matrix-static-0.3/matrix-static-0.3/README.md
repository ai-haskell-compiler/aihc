[![Build Status](https://travis-ci.org/wchresta/matrix-static.svg?branch=master)](https://travis-ci.org/wchresta/matrix-static)
[![Hackage](https://img.shields.io/hackage/v/matrix-static.svg)](https://hackage.haskell.org/package/matrix-static)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/matrix-static.svg)](http://packdeps.haskellers.com/reverse/matrix-static)

# matrix-static

A static wrapper around the [matrix](https://hackage.haskell.org/package/matrix) library. It provides a data type `Matrix m n a` derived from `matrix`'s `Matrix a` with additional information about the matix dimension `m n` as type-level Nat's.

An alternative to this library is [static-tensor](https://hackage.haskell.org/package/static-tensor-0.2.1.0/docs/Data-Matrix-Static.html) that might better fit your needs. Choose `matrix-static` if you are familiar with `matrix` and want more guarantees at compile time. Choose `static-tensor` for most other use cases.

(Almost) all functions provided by `Data.Matrix` are wrapped. These wrappers guarantee during compile time that the matrix dimension are correct. As such, runtime errors due to mismatching matrix dimensions are minimized. Also, some performance improvements are achieved by not having to do dimension checks during runtime.

Some functions take indices `i` as parameters (e.g. `Matrix.Data.mapCol`).
```haskell
mapCol :: (Int -> a -> a) -> Int -> Matrix a -> Matrix a
```

For these, a decision has to be made. Either the runtime index `i` is kept at the runtime level, or it is moved to the type-level. If it remains at the run-time level, it is kept as a parameter but the correctness of the index cannot be guaranteed by the compiler (e.g. there will be an error if the index is -1). If it is moved to the type-level as a type-parameter, the compiler can guarantee the correctnes of the index, but it is not dependent on any values during runtime:

As such, `Data.Matrix.Static.mapCol` chooses to go the safe route and provide `j` as a type level parameter:
```haskell
mapCol :: forall j m n a. (KnownNat j, KnownNat m, 1 <= j, j <= n)
       => (Int -> a -> a) -> Matrix m n a -> Matrix m n a
```

Since it might be important to be able to provide mapCol with a run-time index, the following function is also provided:

```haskell
mapColUnsafe :: (Int -> a -> a) -> Int -> Matrix m n a -> Matrix m n a
```

Of course, it will cause an error if the given index does not match the matrix
dimensions. In some cases, the result of a run-time variant of a function is wrapped
in `Maybe` and thus becomes safe. In general, all functions in the library that do not guarantee that no error will be thrown have the postfix Unsafe.

## Usage example

Note, if you want to provide type-level paramters similar to run-time parameters, make sure to use the `DataKinds` and `TypeApplicatios` extensions. Then, you can give type-level parameters using `@` as demonstrated instead of giving the full type.

```haskell
>>> :set -XDataKinds -XTypeApplications
>>> fromList [1..9] :: Maybe (Matrix 3 3 Int)
Just ┌       ┐
│ 1 2 3 │
│ 4 5 6 │
│ 7 8 9 │
└       ┘
>>> fromList [1..8] :: Maybe (Matrix 3 3 Int)
Nothing

>>> fromJust $ fromList [1..8] :: Matrix 2 4 Int
┌     ┐
│ 1 2 │
│ 3 4 │
│ 5 6 │
│ 7 8 │
└     ┘

>>> a = fromListUnsafe @4 @4 [1..16]
>>> b = fromListUnsafe @4 @2 [4..12]
>>> b .* a

<interactive>:6:6: error:
    * Couldn't match type `4' with `2'
      Expected type: Matrix 2 4 a
        Actual type: Matrix 4 4 a
    * In the second argument of `(.*)', namely `a'
      In the expression: b .* a
      In an equation for `it': it = b .* a
>>> a .* b
┌         ┐
│  80  90 │
│ 192 218 │
│ 304 346 │
│ 416 474 │
└         ┘
```
