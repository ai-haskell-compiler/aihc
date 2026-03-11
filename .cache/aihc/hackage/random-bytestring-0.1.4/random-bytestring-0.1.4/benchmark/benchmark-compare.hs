{-# LANGUAGE UnicodeSyntax #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Main
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental
--
-- -------------------------------------------------------------------------- --

module Main
( main
) where

import Benchmarks
import Criterion.Main
import Implementations

main ∷ IO ()
main = defaultMain $ compareBenchmarks allImplementations

