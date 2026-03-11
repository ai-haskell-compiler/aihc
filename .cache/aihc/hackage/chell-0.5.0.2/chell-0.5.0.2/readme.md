Chell is a simple and intuitive library for automated testing.
It natively supports assertion-based testing, and can use companion libraries
such as `chell-quickcheck` to support more complex testing strategies.

An example test suite, which verifies the behavior of arithmetic operators.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Test.Chell

tests_Math :: Suite
tests_Math = suite "math" [test_Addition, test_Subtraction]

test_Addition :: Test
test_Addition = assertions "addition" $ do
  $expect (equal (2 + 1) 3)
  $expect (equal (1 + 2) 3)

test_Subtraction :: Test
test_Subtraction = assertions "subtraction" $ do
  $expect (equal (2 - 1) 1)
  $expect (equal (1 - 2) (-1))

main :: IO ()
main = defaultMain [tests_Math]
```

```
$ ghc --make chell-example.hs
$ ./chell-example
PASS: 2 tests run, 2 tests passed
```
