-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified DocTest.Data.List.Reverse.StrictElement
import qualified DocTest.Data.List.Reverse.StrictSpine
import qualified DocTest.Data.List.Reverse.Private
import qualified DocTest.Data.List.Match.Private
import qualified DocTest.Data.List.HT.Private
import qualified DocTest.Data.Monoid.HT
import qualified DocTest.Data.Maybe.HT
import qualified DocTest.Data.Bool.HT.Private
import qualified DocTest.Data.Function.HT.Private
import qualified DocTest.Data.Ix.Enum

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    DocTest.Data.List.Reverse.StrictElement.test
    DocTest.Data.List.Reverse.StrictSpine.test
    DocTest.Data.List.Reverse.Private.test
    DocTest.Data.List.Match.Private.test
    DocTest.Data.List.HT.Private.test
    DocTest.Data.Monoid.HT.test
    DocTest.Data.Maybe.HT.test
    DocTest.Data.Bool.HT.Private.test
    DocTest.Data.Function.HT.Private.test
    DocTest.Data.Ix.Enum.test
