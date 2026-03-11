Edit Distance: Vector
=====================

[![Build Status][badge]][status]

This is a small library for calculating the edit distance and edit script
between two vectors. It is generic enough that you should be able to use it
with vectors containing *values* of any type you like, with *changes* described
by any type you like, and with *costs* represented by any type you like (with a
few restrictions).

Installing
----------

The `edit-distance-vector` package is a normal Haskell library and can be
installed using the Cabal package management tool.

````{bash}
cabal update
cabal install edit-distance-vector
````

`edit-distance-vector` is [automatically tested][status] on GHC versions 7.4.2,
7.6.3, 7.8.3, 7.10.1, 8.0.2 using the Travis CI service.

Usage
-----

The interface to `edit-distance-vector` is very small; just import
`Data.Vector.Distance`, create a `Params` value with the correct operations to
deal with your types, and pass this to `leastChanges` along with your
`Vector`s.

````{haskell}
import           Data.Monoid

import qualified Data.Vector          as V
import           Data.Vector.Distance

-- | Editing vectors of 'Char' values, with '(String, Int, Char)' describing
--   changes, and the additive monoid of 'Int' describing costs.
str :: Params Char (String, Int, Char) (Sum Int)
str = Params
    { equivalent = (==)
    , delete     = \i c    -> ("delete", i, c)
    , insert     = \i c    -> ("insert", i, c)
    , substitute = \i c c' -> ("replace", i, c')
    , cost = const (Sum 1)
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

main :: IO ()
main = do
    print $ leastChanges str (V.fromList "I am thomas")
                             (V.fromList "My name is Thomas")
````

(See `test/sample.hs` for a version of this code that is compiled
by the automated test suite.)

[badge]: https://travis-ci.org/thsutton/edit-distance-vector.svg?branch=master
[status]: https://travis-ci.org/thsutton/edit-distance-vector
