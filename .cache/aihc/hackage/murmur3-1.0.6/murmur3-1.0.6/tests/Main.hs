module Main where

import Test.Framework (defaultMain)

import qualified Data.Hash.Murmur.Units (tests)

main :: IO ()
main = defaultMain (Data.Hash.Murmur.Units.tests)

