{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}

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

import Control.Concurrent

import Benchmarks
import Implementations

main ∷ IO ()
main = mapM_ run allImplementations
  where
    run (Impl label act) = do
        putStrLn label
        !_ ← act $ 1024 * 1024 * 5
        threadDelay 500000
        return ()


