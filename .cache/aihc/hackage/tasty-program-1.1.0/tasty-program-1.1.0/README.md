tasty-program
=============

This package provides a function that extends
[tasty](http://hackage.haskell.org/package/tasty) testing framework
with capability to test whether an external program runs
successfully. This package is inspired by
[test-framework-program](http://hackage.haskell.org/package/test-framework-program)
package by Adam Wick.

tasty-program provides basic functionality of running a program with
specified set of command line parameters, and optionally a working
directory, and checking the exit code. Program's output and error
streams are ignored.

You can download latest stable release from
[Hackage](http://hackage.haskell.org/package/tasty-program)

## Example

Given `foo.hs` source file:

```haskell
module Foo where

foo :: Int
foo = 5
```

you can test whether GHC can compile it:

```haskell
module Main (
  main
 ) where

import Test.Tasty
import Test.Tasty.Program

main :: IO ()
main = defaultMain $ testGroup "Compilation with GHC" $ [
    testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"] Nothing
  ]
```
