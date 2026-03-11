{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [
	"-isrc",
	"src/Data/List/Length.hs",
	"src/Data/List/Range.hs",
	"src/Data/List/Range/Nat.hs" ]
