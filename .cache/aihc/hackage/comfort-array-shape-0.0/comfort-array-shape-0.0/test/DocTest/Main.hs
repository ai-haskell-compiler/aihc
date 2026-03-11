-- Do not edit! Automatically created with doctest-extract.
module DocTest.Main where

import qualified DocTest.Data.Array.Comfort.Shape.Static
import qualified DocTest.Data.Array.Comfort.Shape.Extra

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    DocTest.Data.Array.Comfort.Shape.Static.test
    DocTest.Data.Array.Comfort.Shape.Extra.test
