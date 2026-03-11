{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.DiGraph.Test
-- Copyright: Copyright © 2019 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Data.DiGraph.Test
(
-- * Test Properties
  properties
) where

import Data.Bifunctor
import Data.Hashable

import Numeric.Natural

import Test.QuickCheck

-- internal modules

import Data.DiGraph

-- -------------------------------------------------------------------------- --
-- Test Properties

prefixProperties :: String -> [(String, Property)] -> [(String, Property)]
prefixProperties p = fmap $ first (p <>)

properties_undirected :: Eq a => Hashable a => DiGraph a -> [(String, Property)]
properties_undirected g =
    [ ("isDiGraph", property $ isDiGraph g)
    , ("isIrreflexive", property $ isIrreflexive g)
    , ("isSymmetric", property $ isSymmetric g)
    ]

properties_directed :: Eq a => Hashable a => DiGraph a -> [(String, Property)]
properties_directed = filter ((/=) "isSymmetric" . fst) . properties_undirected

properties_emptyGraph :: Natural -> [(String, Property)]
properties_emptyGraph n = prefixProperties ("emptyGraph of order " <> show n <> ": ")
    $ ("order == " <> show n, order g === n)
    : ("size == 0", size g === 0)
    : properties_undirected g
  where
    g = emptyGraph n

properties_singletonGraph :: [(String, Property)]
properties_singletonGraph = prefixProperties "singletonGraph: "
    $ ("order == 1", order g === 1)
    : ("size == 0", symSize g === 0)
    : ("outDegree == 0", maxOutDegree g === 0)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 0", diameter g === Just 0)
    : properties_undirected g
  where
    g = singleton

properties_petersenGraph :: [(String, Property)]
properties_petersenGraph = prefixProperties "petersenGraph: "
    $ ("order == 10", order g === 10)
    : ("size == 15", symSize g === 15)
    : ("outDegree == 3", maxOutDegree g === 3)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 2", diameter g === Just 2)
    : properties_undirected g
  where
    g = petersenGraph

properties_twentyChainGraph :: [(String, Property)]
properties_twentyChainGraph = prefixProperties "twentyChainGraph: "
    $ ("order == 20", order g === 20)
    : ("size == 30", symSize g === 30)
    : ("outDegree == 3", maxOutDegree g === 3)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 3", diameter g === Just 3)
    : properties_undirected g
  where
    g = twentyChainGraph

properties_hoffmanSingletonGraph :: [(String, Property)]
properties_hoffmanSingletonGraph = prefixProperties "HoffmanSingletonGraph: "
    $ ("order == 50", order g === 50)
    : ("size == 175", symSize g === 175)
    : ("outDegree == 7", maxOutDegree g === 7)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 2", diameter g === Just 2)
    : properties_undirected g
  where
    g = hoffmanSingleton

properties_pentagon :: [(String,Property)]
properties_pentagon = prefixProperties "Pentagon: "
    $ ("order == 5",order g === 5)
    : ("size == 5",symSize g === 5)
    : ("outDegree == 1",maxOutDegree g === 1)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 4", diameter g === Just 4)
    : properties_directed g
  where
    g = pentagon

properties_ascendingCube :: [(String,Property)]
properties_ascendingCube = prefixProperties "Ascending Cube: "
    $ ("order == 8",order g === 8)
    : ("size == 12",symSize g === 12)
    : ("outDegree == 3",maxOutDegree g === 3)
    : ("isIrRegular", property $ not $ isRegular g)
    : ("diameter == Nothing", diameter g === Nothing)
    : properties_directed g
  where
    g = ascendingCube

properties_d3k4 :: [(String, Property)]
properties_d3k4 = prefixProperties "d3k4: "
    $ ("order == 38", order g === 38)
    : ("size == 57", symSize g === 57)
    : ("outDegree == 3", maxOutDegree g === 3)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 4", diameter g === Just 4)
    : properties_undirected g
  where
    g = d3k4

properties_d4k3 :: [(String, Property)]
properties_d4k3 = prefixProperties "d4k3: "
    $ ("order == 41", order g === 41)
    : ("size == 82", symSize g === 82)
    : ("outDegree == 4", maxOutDegree g === 4)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 3", diameter g === Just 3)
    : properties_undirected g
  where
    g = d4k3

properties_d4k4 :: [(String, Property)]
properties_d4k4 = prefixProperties "d4k4: "
    $ ("order == 98", order g === 98)
    : ("size == 196", symSize g === 196)
    : ("outDegree == 4", maxOutDegree g === 4)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 4", diameter g === Just 4)
    : properties_undirected g
  where
    g = d4k4

properties_d5k3 :: [(String, Property)]
properties_d5k3 = prefixProperties "d5k3: "
    $ ("order == 72", order g === 72)
    : ("size == 180", symSize g === 180)
    : ("outDegree == 5", maxOutDegree g === 5)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 3", diameter g === Just 3)
    : properties_undirected g
  where
    g = d5k3

properties_d5k4 :: [(String, Property)]
properties_d5k4 = prefixProperties "d5k4: "
    $ ("order == 212", order g === 212)
    : ("size == 530", symSize g === 530)
    : ("outDegree == 5", maxOutDegree g === 5)
    : ("isRegular", property $ isRegular g)
    : ("diameter == 4", diameter g === Just 4)
    : properties_undirected g
  where
    g = d5k4

-- | Test Properties.
--
properties :: [(String, Property)]
properties = (concat :: [[(String, Property)]] -> [(String, Property)])
    [ properties_emptyGraph 0
    , properties_emptyGraph 2
    , properties_singletonGraph
    , properties_petersenGraph
    , properties_twentyChainGraph
    , properties_hoffmanSingletonGraph
    , properties_pentagon
    , properties_ascendingCube
    , properties_d3k4
    , properties_d4k3
    , properties_d4k4
    , properties_d5k3
    , properties_d5k4
    ]
