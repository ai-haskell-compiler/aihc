{-# LANGUAGE UnicodeSyntax #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Benchmarks
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental
--
-- -------------------------------------------------------------------------- --

module Benchmarks
( Impl(..)
, benchmarks
, largeBenchmarks
, compareBenchmarks
) where

import Criterion
import Control.Concurrent.Async
import Data.ByteString
import Data.Monoid
import Numeric.Natural

data Impl = Impl String (Natural → IO ByteString)

benchmarks ∷ [Impl] → [Benchmark]
benchmarks impls =
    [ bgroup "single-threaded"
        [ singleThreaded impls 1024
        , singleThreaded impls (1024 * 1024)
        ]
    , bgroup "multi-threaded"
        [ concurrent impls 10 1024
        , concurrent impls 10 (1024 * 1024)
        ]
    ]

largeBenchmarks ∷ [Impl] → [Benchmark]
largeBenchmarks impls =
    [ bgroup "single-threaded"
        [ singleThreaded impls 1024
        , singleThreaded impls (1024 * 1024)
        , singleThreaded impls (1024 * 1024 * 10)
        -- , singleThreaded impls (1024 * 1024 * 100)
        ]
    , bgroup "multi-threaded"
        [ concurrent impls 10 1024
        , concurrent impls 10 (1024 * 1024)
        , concurrent impls 10 (1024 * 1024 * 10)
        -- , concurrent impls 10 (1024 * 1024 * 100)
        ]
    ]

compareBenchmarks ∷ [Impl] → [Benchmark]
compareBenchmarks impls =
    [ singleThreaded impls (1024 * 1024)
    , singleThreaded impls (1024 * 1024 * 10)
    -- , singleThreaded impls (1024 * 1024 * 100)
    ]

concurrent ∷ [Impl] → Natural → Natural → Benchmark
concurrent impls c n = bgroup (show c <> "-" <> show n <> "-bytes")
    [ cgo label f | (Impl label f) ←  impls ]
  where
    cgo s f = bench s $ nfIO $
        forConcurrently [0..(c-1)] (const $ f n)

singleThreaded ∷ [Impl] → Natural → Benchmark
singleThreaded impls n = bgroup (show n <> "-bytes")
    [ bench label $ nfIO $ f n | (Impl label f) ←  impls ]

