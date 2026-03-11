ParamTree
=========
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Hackage](https://img.shields.io/hackage/v/paramtree.svg)](https://hackage.haskell.org/package/paramtree)
[![hackage-ci](https://matrix.hackage.haskell.org/api/v2/packages/paramtree/badge)](https://matrix.hackage.haskell.org/#/package/paramtree)
[![Build Status](https://travis-ci.org/merijn/paramtree.svg)](https://travis-ci.org/merijn/paramtree)

**ParamTree** library for generating labelled test/benchmark trees from sets of
parameters. Example usecases include criterion benchmark trees or tasty test
trees.

Example
-------

```haskell
import Test.Tasty
import Test.Tasty.HUnit

genTestCase :: Int -> Bool -> Char -> String -> TestTree
genTestCase i b c name = testCase name $ {- your code here -}

params = simpleParam "Int" [1,2]
       . simpleParam "Bool" [True]
       . simpleParam "Char" "xyz"

main :: IO ()
main = defaultMain $ testTree genTestCase params
  where
    testTree = growTree (Just "/") testGroup "my tests"
```

This generates a tasty TestTree with all combinations of values passed to
`genTestCase`. If the `Maybe String` argument is provided like in the above
example, groups with a single entry, such as "Bool" get collapsed into their
parent groups. So instead of a "1 Int" group containing a "True Bool" group
they get collapsed into a single "1 Int/True Bool" group, where the "/"
separator is the one specified by `Just "/"`.
