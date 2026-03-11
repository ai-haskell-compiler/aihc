peano - Lazy unary natural numbers
==================================

This package provides natural numbers in unary notation
```haskell
data Peano
  = Zero
  | Succ Peano
```
implementing `Num`, `Ord` etc.

Purpose
-------

One application is to check whether the length of a (potentially long) list is greater than a (small) number.

E.g., without optimization (`-O 0`),
```haskell
genericLength (replicate (10 ^ 6) True) >= (5 :: Peano)
```
outperforms the same test for `5 :: Int` by a factor of 10⁵, see [benchmark](bench/Main.hs):
```
  length
    Peano: OK
      450  ns ±  45 ns
    Int:   OK
      136  ms ± 4.1 ms
```
