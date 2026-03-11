{- |
Module      : PrintMem
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

A command that computes and prints the memory usage of some processes
-}
module Main (main) where

import System.MemInfo (getChoices, printProcs)


main :: IO ()
main = getChoices >>= printProcs
