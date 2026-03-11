-- Do not edit! Automatically created with doctest-extract.
module Main where

import qualified Test.Game.Labyrinth
import qualified Test.Game.Mastermind
import qualified Test.Game.Utility

import qualified Test.DocTest.Driver as DocTest

main :: IO ()
main = DocTest.run $ do
    Test.Game.Labyrinth.test
    Test.Game.Mastermind.test
    Test.Game.Utility.test
