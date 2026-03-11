tasty-kat
=========

[![Build Status](https://travis-ci.org/vincenthz/tasty-kat.png?branch=master)](https://travis-ci.org/vincenthz/tasty-kat)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

[Tasty-kat](http://hackage.haskell.org/package/tasty-kat) provides support for
KAT (Known Answer Tests) testing.  KAT files provides input and output tests
for some functions for example, for testing the following function:

    r == a + b

A KAT file could be:

```ini
[2 digits addition]

a = 10
b = 20
r = 30

a = 11
b = 21
r = 32
```

This is somewhat similar to the [tasty-golden](http://hackage.haskell.org/package/tasty-golden)
package, but instead of generating files and comparing output file to a golden file,
tasty-kat loads input and output in test vectors and run specific function on it.

Documentation: [tasty-kat on hackage](http://hackage.haskell.org/package/tasty-kat)

```haskell
import Test.Tasty
import Test.Tasty.KAT
main = do
    kat <- testKatLoad "path/to/KAT" katLoaderSimple
    defaultMain [ testKatDetailed "kat-name" kat testKat ]
  where testAddition group kvs =
            case sequence $ map (flip lookup kvs) ["a","b","r"] of
                Nothing      -> error "invalid vector"
                Just [a,b,r] -> let a = read as :: Int
                                    b = read bs :: Int
                                    r = read rs :: Int
                                 in return (a + b == r)
```

The detail output with 'testKatDetailed' looks like:

```shell
    add
      1:    OK
      2:    OK
    sub
      1:    OK
    add
      1:    OK
    base64
      1:    OK
      2:    OK
```

The grouped output with 'testKatGrouped' looks like:

```shell
    add:    OK
      2 tests succeed
    sub:    OK
      1 tests succeed
    add:    OK
      1 tests succeed
    base64: OK
      2 tests succeed
```

