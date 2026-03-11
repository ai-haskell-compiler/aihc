-- Do not edit! Automatically created with doctest-extract.
module DocTest.Main where

import qualified DocTest.Data.Array.Comfort.Shape
import qualified DocTest.Data.Array.Comfort.Storable.Unchecked
import qualified DocTest.Data.Array.Comfort.Storable
import qualified DocTest.Data.Array.Comfort.Storable.Dim2
import qualified DocTest.Data.Array.Comfort.Boxed.Unchecked
import qualified DocTest.Data.Array.Comfort.Boxed

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    DocTest.Data.Array.Comfort.Shape.test
    DocTest.Data.Array.Comfort.Storable.Unchecked.test
    DocTest.Data.Array.Comfort.Storable.test
    DocTest.Data.Array.Comfort.Storable.Dim2.test
    DocTest.Data.Array.Comfort.Boxed.Unchecked.test
    DocTest.Data.Array.Comfort.Boxed.test
