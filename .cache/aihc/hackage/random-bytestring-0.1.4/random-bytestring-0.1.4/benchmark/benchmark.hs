{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Main
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017-2018
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
import qualified Data.ByteString.Random.MWC as MWC
#ifdef MIN_VERSION_pcg_random
import qualified Data.ByteString.Random.PCG as PCG
#endif

main ∷ IO ()
main = defaultMain $ compareBenchmarks
    [ Impl "random-bytestring+mwc" MWC.random
#ifdef MIN_VERSION_pcg_random
    , Impl "random-bytestring+pcg" PCG.random
#endif
    ]

