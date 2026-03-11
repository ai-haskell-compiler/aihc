-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Numeric.Interpolation.Type
import qualified Test.Numeric.Interpolation.NodeList
import qualified Test.Numeric.Interpolation.Piece
import qualified Test.Numeric.Interpolation.Piecewise

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Numeric.Interpolation.Type.test
    Test.Numeric.Interpolation.NodeList.test
    Test.Numeric.Interpolation.Piece.test
    Test.Numeric.Interpolation.Piecewise.test
