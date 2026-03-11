{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.DiGraph.Random.Test
-- Copyright: Copyright © 2019 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Data.DiGraph.Random.Test
( RegularGraph(..)
, Gnp(..)
, RandomGraph(..)

-- * Test Properties
, properties
) where

import Data.Foldable
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import Data.Hashable
import Data.Massiv.Array (Array(..), Comp(..), Ix2(..), U, makeArray)
import qualified Data.Massiv.Array as M
import Data.Proxy

import GHC.Generics
import GHC.TypeNats

#if !MIN_VERSION_base(4,16,0)
import Numeric.Natural
#endif

import Test.QuickCheck

import Text.Printf

-- internal modules

import Data.DiGraph
import qualified Data.DiGraph.FloydWarshall as FW
import Data.DiGraph.Random

-- -------------------------------------------------------------------------- --
-- Utils

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- -------------------------------------------------------------------------- --
-- Arbitrary Graph Instances

-- | Uniformily distributed regular graph in edge list representation. The type
-- parameter is the degree of the graph. The size parameter of the QuickCheck
-- Arbirary instance is the order of the graph.
--
type role RegularGraph phantom
newtype RegularGraph (d :: Nat) = RegularGraph { getRegularGraph :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance KnownNat d => Arbitrary (RegularGraph d) where
    arbitrary = fmap RegularGraph
        $ scale (if d > (0 :: Int) then (+ (d + 1)) else id)
        $ sized $ \n -> maybe discard return =<< rrg choose (int n) d
      where
        d :: Num a => a
        d = int $ natVal (Proxy @d)


-- | Random graph in the \(G_{n,p}\) model in edge list representation. The type
-- parameter is the size of the graph. The size parameter of the QuickCheck
-- Arbitrary instance is the expected size of the graph (number of edges).
--
-- Notes:
--
-- For \(n * (n-1) * p / 2) = m\) the distributions \(G_{n,p}\) and \(G_{n,m}\)
-- have very similar properties.
--
type role Gnp phantom
newtype Gnp (n :: Nat) = Gnp { getGnp :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance KnownNat n => Arbitrary (Gnp n) where
    arbitrary = sized $ \m -> Gnp <$> gnp choose n (p m)
      where
        -- m = p * n * (n-1) / 2
        p :: Int -> Double
        p m = min 1 ((2 * int m) / (n * (n - 1)))

        n :: Num a => a
        n = int $ natVal $ Proxy @n

-- | The uniform distribution over all undirected graphs in edge list
-- representation. This is the \(G_{n,1/2})\) random graph.
--
-- Note for graphs in the \(G_{n,1/2}\) model many important properties are
-- highly concentrated around their expectation. For instances, almost all
-- graphs of a large enough order \(n\) are connected. Therefore, in many cases
-- this is not a good testing model for test spaces where only a low coverage is
-- possible.
--
newtype RandomGraph = RandomGraph { getRandomGraph :: DiGraph Int }
    deriving (Show, Eq, Ord, Generic)

instance Arbitrary RandomGraph where
    arbitrary = sized $ \n -> RandomGraph <$> gnp choose (int n) 0.5

{-
-- | Random graph in the \(G_{n,m}\) model. The type parameter is the order
-- of the graph. The \(m\) parameter free at runtime and can be chosen
-- set to the @size@ parameter in 'Arbitrary' instances.
--
-- The random graph \(G_{n,m}\) is the uniform distribution over graphs of
-- order \(n\) and size \(m\).
--
type role Gnm phantom
newtype Gnm (n :: Nat) = Gnm { getGnm :: DiGraph Int }

instance (KnownNat n) => Arbitrary (Gnm n) where
    arbitrary = sized $ \m -> d
-}

-- -------------------------------------------------------------------------- --
-- Dijkstra's Shortest Path Algorithm
--

-- | On sparse Graphs Dijkstra's algorithm is, generally, much faster than
-- Floyd-Warshall. The FGL implementation, however, is very inefficient. So,
-- this is almost certainly slower than the implementation of Floyd-Warshall
-- above.
--
-- We include this only for testing the correctness short test path matrix
-- implementation in "Data.DiGraph" which is based on Floyd-Warshall.
--
-- All entries of the result matrix are either whole numbers or @Infinity@.
--
fglShortestPaths :: G.Graph g => g Int Int -> Array U Ix2 Double
fglShortestPaths g = makeArray Seq (M.Sz (n :. n)) $ \(i :. j) ->
    maybe (1/0) realToFrac $ G.getDistance j (sp i)
  where
    sp i = G.spTree i g
    n = G.order g

fglDiameter :: G.Graph g => g Int Int -> Maybe Natural
fglDiameter g = if M.isEmpty sps
    then Just 0
    else let x = round $ M.maximum' sps
        in if x == round (1/0 :: Double) then Nothing else Just x
  where
    sps = fglShortestPaths g

toFglGraph :: G.Graph g => DiGraph Int -> g Int Int
toFglGraph g = G.mkGraph vs es
  where
    vs = [(i,i) | i <- toList (vertices g)]
    es = concatMap (\(x,y) -> [(x, y, 1)]) $ edges g

-- -------------------------------------------------------------------------- --
-- Tools for collecting coverage data

collect_graphStats
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order of in the graph distribution
    -> DiGraph Int
    -> prop
    -> Property
collect_graphStats n g
    = collect (orderClass (n + 1) g)
    . collect (sizeClass (n + 1) g)
    . collect (densityClass g)
    . collect (diameterClass g)

-- | For undirected graphs. In steps of tenth.
--
densityClass :: Eq a => Hashable a => DiGraph a -> String
densityClass g
    | order g == 0 = "density: null graph"
    | otherwise = printf "density: %.1f" d
  where
    o :: Num a => a
    o = int (order g)

    d :: Double
    d = 2 * int (size g) / (o * (o - 1))

diameterClass :: Eq a => Hashable a => DiGraph a -> String
diameterClass g = "diameter: " <> show (diameter g)

orderClass :: Natural -> DiGraph a -> String
orderClass n g = classifyByRange "order" n (order g)

-- | undirected size
--
sizeClass :: Eq a => Hashable a => Natural -> DiGraph a -> String
sizeClass n g = classifyByRange "size" (n * (n - 1) `div` 2) (size g)

classifyByRange :: String -> Natural -> Natural -> String
classifyByRange s n x = printf "%s: (%d, %d)" s l u
  where
    l = (x * 10 `div` n) * (n `div` 10)
    u = ((x * 10 `div` n) + 1) * (n `div` 10)

-- -------------------------------------------------------------------------- --
-- Property Utils

graphProperty
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order in the graph distribution
    -> (DiGraph Int -> prop)
    -> Property
graphProperty maxN p = mapSize (`mod` int maxN) $ \(RandomGraph g) ->
    collect_graphStats maxN g $ p g

rrgProperty
    :: Testable prop
    => Natural
        -- ^ maximum value for the graph order of in the graph distribution
    -> (DiGraph Int -> prop)
    -> Property
rrgProperty maxN p = mapSize (`mod` int maxN) $ \(RegularGraph g :: RegularGraph 3) ->
    collect_graphStats maxN g $ p g

-- | The first type parameter is the static order of the graph. The size of the
-- QuichCheck Arbitrary instances is the expected size of the graph (number of
-- edges).
--
gnpProperty
    :: forall (n :: Nat) prop
    . KnownNat n
    => Testable prop
    => (DiGraph Int -> prop)
    -> Property
gnpProperty p = property $ \(Gnp g :: Gnp n) ->
    collect_graphStats (natVal $ Proxy @n) g $ p g

-- -------------------------------------------------------------------------- --
-- Properties of graph algorithms

prop_shortestPaths :: DiGraph Int -> Property
prop_shortestPaths g = fglShortestPaths fglG === M.computeAs M.U (M.map fst m)
  where
    fglG = toFglGraph @G.Gr g
    denseG = FW.fromAdjacencySets $ adjacencySets g
    FW.ShortestPathMatrix m = FW.floydWarshall denseG

prop_diameter :: DiGraph Int -> Property
prop_diameter g = fglDiameter (toFglGraph @G.Gr g) === diameter g

properties_randomGraph :: [(String, Property)]
properties_randomGraph = prefix "uniform random graph" <$>
    [ ("isDiGraph", graphProperty 20 isDiGraph)
    , ("isSymmetric", graphProperty 20 isSymmetric)
    , ("shortestPaths", graphProperty 20 prop_shortestPaths)
    , ("diameter", graphProperty 20 prop_diameter)
    ]

properties_gnp :: [(String, Property)]
properties_gnp = prefix "Gnp random graph" <$>
    [ ("isDiGraph", gnpProperty @20 isDiGraph)
    , ("isSymmetric", gnpProperty @20 isSymmetric)
    , ("shortestPaths", gnpProperty @20 prop_shortestPaths)
    , ("diameter", gnpProperty @20 prop_diameter)
    ]

properties_rrg :: [(String, Property)]
properties_rrg = prefix "random regular graph" <$>
    [ ("isDiGraph", rrgProperty 20 isDiGraph)
    , ("isRegular", rrgProperty 20 isRegular)
    , ("isSymmetric", rrgProperty 20 isSymmetric)
    , ("shortestPaths", rrgProperty 20 prop_shortestPaths)
    , ("diameter", rrgProperty 20 prop_diameter)
    ]

prefix :: String -> (String, b) -> (String, b)
prefix a (b, c) = (a <> "." <> b, c)

-- | Test properties.
--
properties :: [(String, Property)]
properties = mconcat
    [ properties_randomGraph
    , properties_gnp
    , properties_rrg
    ]
