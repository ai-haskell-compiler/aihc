{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: DiGraph
-- Copyright: Copyright © 2018-2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Directed graphs in adjacency set representation. The implementation is based
-- on "Data.HashMap.Strict" and "Data.HashSet" from the [unordered-containers
-- package](https://hackage.haskell.org/package/unordered-containers)
--
-- Undirected graphs are represented as symmetric, irreflexive directed graphs.
--
module Data.DiGraph
( DiGraph
, DiEdge
, adjacencySets
, vertices
, edges
, adjacents
, incidents

-- * Construction and Modification of Graphs
, insertEdge
, fromEdges
, insertVertex
, mapVertices
, union
, transpose
, symmetric
, fromList
, unsafeFromList

-- * Predicates
, isDiGraph
, isAdjacent
, isRegular
, isSymmetric
, isIrreflexive
, isEdge
, isVertex

-- * Properties
, order
, size
, diSize
, symSize
, outDegree
, inDegree
, maxOutDegree
, maxInDegree
, minOutDegree
, minInDegree

-- * Distances, Shortest Paths, and Diameter
, ShortestPathCache
, shortestPathCache
, shortestPath
, shortestPath_
, distance
, distance_
, diameter
, diameter_

-- * Graphs
, emptyGraph
, singleton
, clique
, pair
, triangle
, cycle
, diCycle
, line
, diLine
, petersenGraph
, petersonGraph
, twentyChainGraph
, hoffmanSingleton
, pentagon
, ascendingCube
, d3k4
, d4k4
, d4k3
, d5k3
, d5k4
) where

import Control.Arrow
import Control.DeepSeq
import Control.Monad

import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup
import Data.Traversable
import Data.Tuple

import GHC.Generics

import Numeric.Natural

import Prelude hiding (cycle)

-- internal modules

import qualified Data.DiGraph.FloydWarshall as FW

-- -------------------------------------------------------------------------- --
-- Utils

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- -------------------------------------------------------------------------- --
-- Graph

-- | Directed Edge.
--
type DiEdge a = (a, a)

-- | Adjacency set representation of directed graphs.
--
-- It is assumed that each target of an edge is also explicitly a vertex in the
-- graph.
--
-- It is not generally required that graphs are irreflexive, but all concrete
-- graphs that are defined in this module are irreflexive.
--
-- Undirected graphs are represented as symmetric directed graphs.
--
newtype DiGraph a = DiGraph { unGraph :: HM.HashMap a (HS.HashSet a) }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

instance (Hashable a, Eq a) => Semigroup (DiGraph a) where
    (DiGraph a) <> (DiGraph b) = DiGraph (HM.unionWith (<>) a b)
    {-# INLINE (<>) #-}

instance (Hashable a, Eq a) => Monoid (DiGraph a) where
    mempty = DiGraph mempty
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-- | A predicate that asserts that every target of an edge is also a vertex in
-- the graph. Any graph that is constructed without using unsafe methods is
-- guaranteed to satisfy this predicate.
--
isDiGraph :: Eq a => Hashable a => DiGraph a -> Bool
isDiGraph g@(DiGraph m) = HS.null (HS.unions (HM.elems m) `HS.difference` vertices g)
{-# INLINE isDiGraph #-}

-- | The adjacency sets of a graph.
--
adjacencySets :: DiGraph a -> HM.HashMap a (HS.HashSet a)
adjacencySets = unGraph
{-# INLINE adjacencySets #-}

-- | The set of vertices of the graph.
--
vertices :: DiGraph a -> HS.HashSet a
vertices = HS.fromMap . HM.map (const ()) . unGraph
{-# INLINE vertices #-}

-- | The set edges of the graph.
--
edges :: Eq a => Hashable a => DiGraph a -> HS.HashSet (DiEdge a)
edges = HS.fromList . concatMap (traverse HS.toList) . HM.toList . unGraph
{-# INLINE edges #-}

-- | The set of adjacent pairs of a graph.
--
adjacents :: Eq a => Hashable a => a -> DiGraph a -> HS.HashSet a
adjacents a (DiGraph g) = g HM.! a
{-# INLINE adjacents #-}

-- | The set of incident edges of a graph.
--
incidents :: Eq a => Hashable a => a -> DiGraph a -> [(a, a)]
incidents a g = [ (a, b) | b <- toList (adjacents a g) ]
{-# INLINE incidents #-}

-- -------------------------------------------------------------------------- --
-- Constructing and Modifying Graphs

-- | Construct a graph from adjacency lists.
--
fromList :: Eq a => Hashable a => [(a,[a])] -> DiGraph a
fromList l = foldr insertVertex es (fst <$> l)
  where
    es = fromEdges [ (a,b) | (a, bs) <- l, b <- bs ]
{-# INLINE fromList #-}

-- | Unsafely construct a graph from adjacency lists.
--
-- This function assumes that the input includes a adjacency list of each vertex
-- that appears in a adjacency list of another vertex. Generally, 'fromList'
-- should be preferred.
--
unsafeFromList :: Eq a => Hashable a => [(a,[a])] -> DiGraph a
unsafeFromList = DiGraph . HM.map HS.fromList . HM.fromList
{-# INLINE unsafeFromList #-}

-- | Construct a graph from a foldable structure of edges.
--
fromEdges :: Eq a => Hashable a => Foldable f => f (a, a) -> DiGraph a
fromEdges = foldr insertEdge mempty
{-# INLINE fromEdges #-}

-- | The union of two graphs.
--
union :: Eq a => Hashable a => DiGraph a -> DiGraph a -> DiGraph a
union = (<>)
{-# INLINE union #-}

-- | Map a function over all vertices of a graph.
--
mapVertices :: Eq b => Hashable b => (a -> b) -> DiGraph a -> DiGraph b
mapVertices f = DiGraph . HM.fromList . fmap (f *** HS.map f) . HM.toList . unGraph
{-# INLINE mapVertices #-}

-- | Transpose a graph, i.e. reverse all edges of the graph.
--
transpose :: Eq a => Hashable a => DiGraph a -> DiGraph a
transpose g = (DiGraph $ mempty <$ unGraph g)
    `union` (fromEdges . HS.map swap $ edges g)

-- | Symmetric closure of a graph.
--
symmetric :: Eq a => Hashable a => DiGraph a -> DiGraph a
symmetric g = g <> transpose g
{-# INLINE symmetric #-}

-- | Insert an edge. Returns the graph unmodified if the edge is already in the
-- graph. Non-existing vertices are added.
--
insertEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> DiGraph a
insertEdge (a,b) = DiGraph
    . HM.insertWith (<>) a [b]
    . HM.insertWith (<>) b []
    . unGraph
{-# INLINE insertEdge #-}

-- | Insert a vertex. Returns the graph unmodified if the vertex is already in
-- the graph.
--
insertVertex :: Eq a => Hashable a => a -> DiGraph a -> DiGraph a
insertVertex a = DiGraph . HM.insertWith (<>) a [] . unGraph
{-# INLINE insertVertex #-}

-- -------------------------------------------------------------------------- --
-- Properties

-- | The order of a graph is the number of vertices.
--
order :: DiGraph a -> Natural
order = int . HS.size . vertices
{-# INLINE order #-}

-- | Directed Size. This the number of edges of the graph.
--
diSize :: Eq a => Hashable a => DiGraph a -> Natural
diSize = int . HS.size . edges
{-# INLINE diSize #-}

-- | Directed Size. This the number of edges of the graph.
--
size :: Eq a => Hashable a => DiGraph a -> Natural
size = int . HS.size . edges
{-# INLINE size #-}

-- | Undirected Size of a graph. This is the number of edges of the symmetric
-- closure of the graph.
--
symSize :: Eq a => Hashable a => DiGraph a -> Natural
symSize g = diSize (symmetric g) `div` 2
{-# INLINE symSize #-}

-- | The number of outgoing edges of vertex in a graph.
--
outDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
outDegree (DiGraph g) a = int . HS.size $ g HM.! a
{-# INLINE outDegree #-}

-- | The maximum out-degree of the vertices of a graph.
--
maxOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxOutDegree g = maximum $ HS.map (outDegree g) (vertices g)
{-# INLINE maxOutDegree #-}

-- | The minimum out-degree of the vertices of a graph.
--
minOutDegree :: Eq a => Hashable a => DiGraph a -> Natural
minOutDegree g = minimum $ HS.map (outDegree g) (vertices g)
{-# INLINE minOutDegree #-}

-- | The number of incoming edges of vertex in a graph.
--
inDegree :: Eq a => Hashable a => DiGraph a -> a -> Natural
inDegree g = outDegree (transpose g)
{-# INLINE inDegree #-}

-- | The maximum in-degree of the vertices of a graph.
--
maxInDegree :: Eq a => Hashable a => DiGraph a -> Natural
maxInDegree = maxOutDegree . transpose
{-# INLINE maxInDegree #-}

-- | The minimum in-degree of the vertices of a graph.
--
minInDegree :: Eq a => Hashable a => DiGraph a -> Natural
minInDegree = minOutDegree . transpose
{-# INLINE minInDegree #-}

-- -------------------------------------------------------------------------- --
-- Predicates

-- | Return whether a graph is regular, i.e. whether all vertices have the same
-- out-degree. Note that the latter implies that all vertices also have the same
-- in-degree.
--
isRegular :: DiGraph a -> Bool
isRegular = (== 1)
    . length
    . L.group
    . fmap (HS.size . snd)
    . HM.toList
    . unGraph
{-# INLINE isRegular #-}

-- | Return whether a graph is symmetric, i.e. whether for each edge \((a,b)\)
-- there is also the edge \((b,a)\) in the graph.
--
isSymmetric :: Hashable a => Eq a => DiGraph a -> Bool
isSymmetric g = all checkVertex $ HM.toList $ unGraph g
  where
    checkVertex (a, e) = all (\x -> isAdjacent x a g) e
{-# INLINE isSymmetric #-}

-- | Return whether a graph is irreflexive. A graph is irreflexive if for each
-- edge \((a,b)\) it holds that \(a \neq b\), i.e there are no self-loops in the
-- graph.
--
isIrreflexive :: Eq a => Hashable a => DiGraph a -> Bool
isIrreflexive = not . any (uncurry HS.member) . HM.toList . unGraph
{-# INLINE isIrreflexive #-}

-- | Return whether a vertex is contained in a graph.
--
isVertex :: Eq a => Hashable a => a -> DiGraph a -> Bool
isVertex a = HM.member a . unGraph
{-# INLINE isVertex #-}

-- | Return whether an edge is contained in a graph.
--
isEdge :: Eq a => Hashable a => DiEdge a -> DiGraph a -> Bool
isEdge (a, b) = maybe False (HS.member b) . HM.lookup a . unGraph
{-# INLINE isEdge #-}

-- | Return whether two vertices are adjacent in a graph.
--
isAdjacent :: Eq a => Hashable a => a -> a -> DiGraph a -> Bool
isAdjacent = curry isEdge
{-# INLINE isAdjacent #-}

-- -------------------------------------------------------------------------- --
-- Distances, Shortest Paths, and Diameter

-- | The shortest path matrix of a graph.
--
-- The shortest path matrix of a graph can be used to efficiently query the
-- distance and shortest path between any two vertices of the graph. It can also
-- be used to efficiently compute the diameter of the graph.
--
-- Computing the shortest path matrix is expensive for larger graphs. The matrix
-- is computed using the Floyd-Warshall algorithm. The space and time complexity
-- is quadratic in the /order/ of the graph. For sparse graphs there are more
-- efficient algorithms for computing distances and shortest paths between the
-- nodes of the graph.
--
data ShortestPathCache a = ShortestPathCache
    { _spcMatrix :: {-# UNPACK #-} !FW.ShortestPathMatrix
        -- ^ The shortest path matrix of a graph.
    , _spcIndices :: !(HM.HashMap a Int)
        -- ^ mapping from vertices of the graph to indices in the shortest path
        -- matrix.
    , _spcVertices :: !(HM.HashMap Int a)
        -- ^ mapping from indices in the shortest path matrix to vertices in the
        -- graph.
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Compute the shortest path matrix for a graph. The result can be used to
-- efficiently query the distance and shortest path between any two vertices of
-- the graph. It can also be used to efficiently compute the diameter of the
-- graph.
--
shortestPathCache :: Eq a => Hashable a => DiGraph a -> ShortestPathCache a
shortestPathCache g = ShortestPathCache m vmap rvmap
  where
    m = FW.floydWarshall $ FW.fromAdjacencySets (unGraph ig)
    ig = mapVertices (vmap HM.!) g
    vmap = HM.fromList $ zip (HS.toList $ vertices g) [0..]
    rvmap = HM.fromList $ zip [0..] (HS.toList $ vertices g)

-- | Compute the Diameter of a graph, i.e. the maximum length of a shortest path
-- between two vertices in the graph.
--
-- This is expensive to compute for larger graphs. If also the shortest paths or
-- distances are needed, one should use 'shortestPathCache' to cache the result
-- of the search and use the 'diameter_', 'shortestPath_', and 'distance_' to
-- query the respective results from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
diameter :: Eq a => Hashable a => DiGraph a -> Maybe Natural
diameter = diameter_ . shortestPathCache
{-# INLINE diameter #-}

-- | Compute the Diameter of a graph from a shortest path matrix. The diameter
-- of a graph is the maximum length of a shortest path between two vertices in
-- the graph.
--
diameter_ :: ShortestPathCache a -> Maybe Natural
diameter_ (ShortestPathCache m _ _) = round <$> FW.diameter m
{-# INLINE diameter_ #-}

-- | Compute the shortest path between two vertices of a graph.
--
-- | This is expensive for larger graphs. If more than one path is needed one
-- should use 'shortestPathCache' to cache the result of the search and use
-- 'shortestPath_' to query paths from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
shortestPath :: Eq a => Hashable a => a -> a -> DiGraph a -> Maybe [a]
shortestPath src trg = shortestPath_ src trg . shortestPathCache
{-# INLINE shortestPath #-}

-- | Compute the shortest path between two vertices from the shortest path
-- matrix of a graph.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
shortestPath_ :: Eq a => Hashable a => a -> a -> ShortestPathCache a -> Maybe [a]
shortestPath_ src trg (ShortestPathCache c m r)
    = fmap ((HM.!) r) <$> FW.shortestPath c (m HM.! src) (m HM.! trg)
{-# INLINE shortestPath_ #-}

-- | Compute the distance between two vertices of a graph.
--
-- | This is expensive for larger graphs. If more than one distance is needed
-- one should use 'shortestPathCache' to cache the result of the search and use
-- 'distance_' to query paths from the cache.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
distance :: Eq a => Hashable a => a -> a -> DiGraph a -> Maybe Natural
distance src trg = distance_ src trg . shortestPathCache
{-# INLINE distance #-}

-- | Compute the distance between two vertices from the shortest path matrix of
-- a graph.
--
-- The algorithm is optimized for dense graphs. For large sparse graphs a more
-- efficient algorithm should be used.
--
distance_ :: Eq a => Hashable a => a -> a -> ShortestPathCache a -> Maybe Natural
distance_ src trg (ShortestPathCache c m _)
    = round <$> FW.distance c (m HM.! src) (m HM.! trg)
{-# INLINE distance_ #-}

-- -------------------------------------------------------------------------- --
-- Concrete Graph

-- | The empty graph on @n@ nodes. This is the graph of 'order' @n@ and 'size'
-- @0@.
--
emptyGraph :: Natural -> DiGraph Int
emptyGraph n = unsafeFromList [ (i, []) | i <- [0 .. int n - 1] ]

-- | Undirected clique.
--
clique :: Natural -> DiGraph Int
clique i = unsafeFromList
    [ (a, b)
    | a <- [0 .. int i - 1]
    , let b = [ x | x <- [0 .. int i - 1] , x /= a ]
    ]

-- | The (irreflexive) singleton graph.
--
singleton :: DiGraph Int
singleton = clique 1

-- | Undirected pair.
--
pair :: DiGraph Int
pair = clique 2

-- | Undirected triangle.
--
triangle :: DiGraph Int
triangle = clique 3

-- | Directed cycle.
--
diCycle :: Natural -> DiGraph Int
diCycle n = unsafeFromList [ (a, [(a + 1) `mod` int n]) | a <- [0 .. int n - 1] ]

-- | Undirected cycle.
--
cycle :: Natural -> DiGraph Int
cycle = symmetric . diCycle

-- | Directed line.
--
diLine :: Natural -> DiGraph Int
diLine n = unsafeFromList [ (a, [ a + 1 | a /= int n - 1]) | a <- [0 .. int n - 1] ]

-- | Undirected line.
--
line :: Natural -> DiGraph Int
line = symmetric . diLine

-- | The Petersen graph.
--
-- * order: 10
-- * size: 30
-- * degree: 3
-- * diameter: 2
--
petersenGraph :: DiGraph Int
petersenGraph = DiGraph
    [ (0, [2,3,5])
    , (1, [3,4,6])
    , (2, [4,0,7])
    , (3, [0,1,8])
    , (4, [1,2,9])
    , (5, [0,6,9])
    , (6, [1,5,7])
    , (7, [2,6,8])
    , (8, [3,7,9])
    , (9, [4,8,5])
    ]

petersonGraph :: DiGraph Int
petersonGraph = petersenGraph
{-# DEPRECATED petersonGraph "Use petersenGraph instead" #-}

-- | The "twenty chain" graph.
--
-- * order: 20
-- * size: 60
-- * degree: 3
-- * diameter: 3
--
twentyChainGraph :: DiGraph Int
twentyChainGraph = pentagram `union` decagon `union` connections
  where
    pentagram = mapVertices (+ 5) $ pentagon2pentagram $ cycle 5
    decagon =  mapVertices (+ 10) $ cycle 10
    connections = fromEdges $ HS.fromList $ mconcat
        [ [(i, x), (x, i)]
        | i <- [0..4]
        , x <- [i + 5, i + 10, i + 15]
        ]
    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

-- | Hoffman-Singleton Graph.
--
-- The Hoffman-Singleton graph is a 7-regular graph with 50 vertices and 175
-- edges. It's the largest graph of max-degree 7 and diameter 2. Cf.
-- [https://en.wikipedia.org/wiki/Hoffman–Singleton_graph]()
--
hoffmanSingleton :: DiGraph Int
hoffmanSingleton = pentagons `union` pentagrams `union` connections
  where
    pentagons = mconcat
        [ mapVertices (p_off i) $ cycle 5 | i <- [0 .. 4] ]
    pentagrams = mconcat
        [ mapVertices (q_off i) $ pentagon2pentagram $ cycle 5 | i <- [0 .. 4] ]

    p_off h = (+) (25 + 5 * h)
    q_off i = (+) (5 * i)

    pentagon2pentagram = mapVertices $ \case
        0 -> 0
        1 -> 3
        2 -> 1
        3 -> 4
        4 -> 2
        _ -> error "invalid vertex"

    connections = fromEdges $ HS.fromList $ mconcat
        [ [(a, b), (b, a)]
        | h <- [0 .. 4]
        , j <- [0 .. 4]
        , let a = p_off h j
        , i <- [0 .. 4]
        , let b = q_off i ((h * i + j) `mod` 5)
        ]

pentagon :: DiGraph Int
pentagon = fromEdges $ HS.fromList [(i,(i+1) `mod` 5) | i <- [0..4] ]

ascendingCube :: DiGraph (Int,Int,Int)
ascendingCube = fromEdges $ HS.fromList $ mconcat
  [ [ ((0,a,b),(1,a,b))
    , ((a,0,b),(a,1,b))
    , ((a,b,0),(a,b,1))
    ]
  | a <- [0,1]
  , b <- [0,1]
  ]

-- | Graph of order 38 with degree 3 and diameter 4
--
-- This graph is described by Alegre et. al. in
--
-- Alegre, Fiol, and Yebra, Journal of Graph Theory, Vol 10 (1986), 219--224.
--
-- Note this is is not the original d3k4 graph of order 38 from Karl Doty (1982).
--
-- Buse proved in 2000 that the order of 38 is optimal for degree 3 and diameter
-- 4 in
--
-- Buse, Discrete Applied Mathematics, Vol 101 (2000), 53--16.
--
-- Details:
--
-- * order: 38
-- * size: 57
-- * degree: 3
-- * diameter: 4
-- * average distance: 3.108
-- * regular: yes
--
d3k4 :: DiGraph Int
d3k4 = mconcat [ c7 i | i <- [0..4] ] <> connections
  where
    -- The construction is based on 5 copies of a cluster of 7 vertices
    c7 :: Int -> DiGraph Int
    c7 i = mapVertices (\v -> v + (i * 7)) $ fromList
        [ (0, [1])
        , (1, [0, 2, 3])
        , (2, [1])
        , (3, [1, 4])
        , (4, [3, 5, 6])
        , (5, [4])
        , (6, [4])
        ]

    -- The clusters are connected by the following edges
    connections :: DiGraph Int
    connections = symmetric $ fromList
        $
            [ (a * 7, [n + 6, p + 6])
            | a <- [0..4]
            , let n = 7 * rem (a + 2) 5
            , let p = 7 * rem (a + 3) 5
            ]
        <>
            [ (a * 7 + 2, [n + 5, p + 5])
            | a <- [0..4]
            , let n = 7 * rem (a + 1) 5
            , let p = 7 * rem (a + 4) 5
            ]
        <>
            [ (35, [36])
            , (36, [37])
            , (0+3, [35])
            , (7+3, [36])
            , (14+3, [35])
            , (21+3, [37])
            , (28+3, [37])
            ]

-- | Graph of order 41 found by Allwright 1992
--
d4k3 :: DiGraph Int
d4k3 = DiGraph
    [ (0, [1, 5, 25, 37])
    , (1, [0, 2, 3, 4])
    , (2, [1, 19, 23, 30])
    , (3, [1, 24, 18, 12])
    , (4, [1, 13, 35, 17])
    , (5, [0, 6, 10, 34])
    , (6, [5, 7, 8, 9])
    , (7, [6, 28, 38, 24])
    , (8, [6, 17, 23, 29])
    , (9, [6, 22, 18, 30])
    , (10, [5, 11, 15, 31])
    , (11, [10, 12, 13, 14])
    , (12, [3, 11, 29, 33])
    , (13, [4, 11, 28, 22])
    , (14, [11, 27, 38, 23])
    , (15, [10, 20, 16, 37])
    , (16, [15, 17, 18, 19])
    , (17, [4, 8, 16, 32])
    , (18, [3, 9, 16, 27])
    , (19, [2, 16, 33, 28])
    , (20, [15, 25, 21, 34])
    , (21, [20, 22, 23, 24])
    , (22, [9, 13, 21, 36])
    , (23, [2, 8, 14, 21])
    , (24, [3, 7, 21, 32])
    , (25, [0, 20, 26, 31])
    , (26, [25, 29, 28, 27])
    , (27, [14, 18, 26, 35])
    , (28, [7, 13, 19, 26])
    , (29, [8, 12, 26, 36])
    , (30, [2, 9, 31, 40])
    , (31, [30, 32, 25, 10])
    , (32, [17, 24, 31, 39])
    , (33, [19, 34, 40, 12])
    , (34, [33, 35, 20, 5])
    , (35, [4, 27, 34, 39])
    , (36, [22, 29, 37, 40])
    , (37, [36, 38, 0, 15])
    , (38, [7, 14, 37, 39])
    , (39, [32, 40, 35, 38])
    , (40, [33, 30, 39, 36])
    ]

-- | From Geoffrey Exoo's list. A solution of the degree-diameter
-- problem with 98 vertices. The graph is regular with degree 4
-- and has diameter 4.
--
d4k4 :: DiGraph Int
d4k4 = DiGraph
    [ (0, [31, 91, 65, 49])
    , (1, [30, 90, 64, 48])
    , (2, [33, 93, 67, 51])
    , (3, [32, 92, 66, 50])
    , (4, [35, 95, 69, 53])
    , (5, [34, 94, 68, 52])
    , (6, [37, 97, 57, 55])
    , (7, [36, 96, 56, 54])
    , (8, [39, 85, 59, 43])
    , (9, [38, 84, 58, 42])
    , (10, [41, 87, 61, 45])
    , (11, [40, 86, 60, 44])
    , (12, [29, 89, 63, 47])
    , (13, [28, 88, 62, 46])
    , (14, [63, 94, 35, 73])
    , (15, [62, 95, 34, 72])
    , (16, [65, 96, 37, 75])
    , (17, [64, 97, 36, 74])
    , (18, [67, 84, 39, 77])
    , (19, [66, 85, 38, 76])
    , (20, [69, 86, 41, 79])
    , (21, [68, 87, 40, 78])
    , (22, [57, 88, 29, 81])
    , (23, [56, 89, 28, 80])
    , (24, [59, 90, 31, 83])
    , (25, [58, 91, 30, 82])
    , (26, [61, 92, 33, 71])
    , (27, [60, 93, 32, 70])
    , (28, [13, 23, 78, 49])
    , (29, [12, 22, 79, 48])
    , (30, [1, 25, 80, 51])
    , (31, [0, 24, 81, 50])
    , (32, [3, 27, 82, 53])
    , (33, [2, 26, 83, 52])
    , (34, [5, 15, 70, 55])
    , (35, [4, 14, 71, 54])
    , (36, [7, 17, 72, 43])
    , (37, [6, 16, 73, 42])
    , (38, [9, 19, 74, 45])
    , (39, [8, 18, 75, 44])
    , (40, [11, 21, 76, 47])
    , (41, [10, 20, 77, 46])
    , (42, [61, 9, 89, 37])
    , (43, [60, 8, 88, 36])
    , (44, [63, 11, 91, 39])
    , (45, [62, 10, 90, 38])
    , (46, [65, 13, 93, 41])
    , (47, [64, 12, 92, 40])
    , (48, [67, 1, 95, 29])
    , (49, [66, 0, 94, 28])
    , (50, [69, 3, 97, 31])
    , (51, [68, 2, 96, 30])
    , (52, [57, 5, 85, 33])
    , (53, [56, 4, 84, 32])
    , (54, [59, 7, 87, 35])
    , (55, [58, 6, 86, 34])
    , (56, [23, 83, 7, 53])
    , (57, [22, 82, 6, 52])
    , (58, [25, 71, 9, 55])
    , (59, [24, 70, 8, 54])
    , (60, [27, 73, 11, 43])
    , (61, [26, 72, 10, 42])
    , (62, [15, 75, 13, 45])
    , (63, [14, 74, 12, 44])
    , (64, [17, 77, 1, 47])
    , (65, [16, 76, 0, 46])
    , (66, [19, 79, 3, 49])
    , (67, [18, 78, 2, 48])
    , (68, [21, 81, 5, 51])
    , (69, [20, 80, 4, 50])
    , (70, [34, 59, 89, 27])
    , (71, [35, 58, 88, 26])
    , (72, [36, 61, 91, 15])
    , (73, [37, 60, 90, 14])
    , (74, [38, 63, 93, 17])
    , (75, [39, 62, 92, 16])
    , (76, [40, 65, 95, 19])
    , (77, [41, 64, 94, 18])
    , (78, [28, 67, 97, 21])
    , (79, [29, 66, 96, 20])
    , (80, [30, 69, 85, 23])
    , (81, [31, 68, 84, 22])
    , (82, [32, 57, 87, 25])
    , (83, [33, 56, 86, 24])
    , (84, [18, 81, 9, 53])
    , (85, [19, 80, 8, 52])
    , (86, [20, 83, 11, 55])
    , (87, [21, 82, 10, 54])
    , (88, [22, 71, 13, 43])
    , (89, [23, 70, 12, 42])
    , (90, [24, 73, 1, 45])
    , (91, [25, 72, 0, 44])
    , (92, [26, 75, 3, 47])
    , (93, [27, 74, 2, 46])
    , (94, [14, 77, 5, 49])
    , (95, [15, 76, 4, 48])
    , (96, [16, 79, 7, 51])
    , (97, [17, 78, 6, 50])
    ]

-- | Graph of order 72 found by Geoffrey Exoo 1998
--
-- * order: 72
-- * degree 5
-- * diameter 3
--
-- adjacency lists for vertices 0 to 71:
--
d5k3 :: DiGraph Int
d5k3 = DiGraph
    [ (0, [62, 59, 45, 51, 71])
    , (1, [63, 48, 46, 52, 60])
    , (2, [64, 49, 47, 53, 61])
    , (3, [65, 50, 36, 54, 62])
    , (4, [66, 51, 37, 55, 63])
    , (5, [67, 52, 38, 56, 64])
    , (6, [68, 53, 39, 57, 65])
    , (7, [69, 54, 40, 58, 66])
    , (8, [70, 55, 41, 59, 67])
    , (9, [71, 56, 42, 48, 68])
    , (10, [60, 57, 43, 49, 69])
    , (11, [61, 58, 44, 50, 70])
    , (12, [42, 24, 32, 39, 62])
    , (13, [43, 25, 33, 40, 63])
    , (14, [44, 26, 34, 41, 64])
    , (15, [45, 27, 35, 42, 65])
    , (16, [46, 28, 24, 43, 66])
    , (17, [47, 29, 25, 44, 67])
    , (18, [36, 30, 26, 45, 68])
    , (19, [37, 31, 27, 46, 69])
    , (20, [38, 32, 28, 47, 70])
    , (21, [39, 33, 29, 36, 71])
    , (22, [40, 34, 30, 37, 60])
    , (23, [41, 35, 31, 38, 61])
    , (24, [16, 12, 61, 63, 30])
    , (25, [17, 13, 62, 64, 31])
    , (26, [18, 14, 63, 65, 32])
    , (27, [19, 15, 64, 66, 33])
    , (28, [20, 16, 65, 67, 34])
    , (29, [21, 17, 66, 68, 35])
    , (30, [22, 18, 67, 69, 24])
    , (31, [23, 19, 68, 70, 25])
    , (32, [12, 20, 69, 71, 26])
    , (33, [13, 21, 70, 60, 27])
    , (34, [14, 22, 71, 61, 28])
    , (35, [15, 23, 60, 62, 29])
    , (36, [3, 21, 18, 49, 59])
    , (37, [4, 22, 19, 50, 48])
    , (38, [5, 23, 20, 51, 49])
    , (39, [6, 12, 21, 52, 50])
    , (40, [7, 13, 22, 53, 51])
    , (41, [8, 14, 23, 54, 52])
    , (42, [9, 15, 12, 55, 53])
    , (43, [10, 16, 13, 56, 54])
    , (44, [11, 17, 14, 57, 55])
    , (45, [0, 18, 15, 58, 56])
    , (46, [1, 19, 16, 59, 57])
    , (47, [2, 20, 17, 48, 58])
    , (48, [1, 9, 47, 37, 54])
    , (49, [2, 10, 36, 38, 55])
    , (50, [3, 11, 37, 39, 56])
    , (51, [4, 0, 38, 40, 57])
    , (52, [5, 1, 39, 41, 58])
    , (53, [6, 2, 40, 42, 59])
    , (54, [7, 3, 41, 43, 48])
    , (55, [8, 4, 42, 44, 49])
    , (56, [9, 5, 43, 45, 50])
    , (57, [10, 6, 44, 46, 51])
    , (58, [11, 7, 45, 47, 52])
    , (59, [0, 8, 46, 36, 53])
    , (60, [10, 35, 1, 22, 33])
    , (61, [11, 24, 2, 23, 34])
    , (62, [0, 25, 3, 12, 35])
    , (63, [1, 26, 4, 13, 24])
    , (64, [2, 27, 5, 14, 25])
    , (65, [3, 28, 6, 15, 26])
    , (66, [4, 29, 7, 16, 27])
    , (67, [5, 30, 8, 17, 28])
    , (68, [6, 31, 9, 18, 29])
    , (69, [7, 32, 10, 19, 30])
    , (70, [8, 33, 11, 20, 31])
    , (71, [9, 34, 0, 21, 32])
    ]

d5k4 :: DiGraph Int
d5k4 = DiGraph
    [ (0, [104, 189, 163, 145, 67])
    , (1, [105, 190, 164, 146, 68])
    , (2, [53, 191, 165, 147, 69])
    , (3, [54, 192, 166, 148, 70])
    , (4, [55, 193, 167, 149, 71])
    , (5, [56, 194, 168, 150, 72])
    , (6, [57, 195, 169, 151, 73])
    , (7, [58, 196, 170, 152, 74])
    , (8, [59, 197, 171, 153, 75])
    , (9, [60, 198, 172, 154, 76])
    , (10, [61, 199, 173, 155, 77])
    , (11, [62, 200, 174, 156, 78])
    , (12, [63, 201, 175, 157, 79])
    , (13, [64, 202, 176, 158, 80])
    , (14, [65, 203, 177, 106, 81])
    , (15, [66, 204, 178, 107, 82])
    , (16, [67, 205, 179, 108, 83])
    , (17, [68, 206, 180, 109, 84])
    , (18, [69, 207, 181, 110, 85])
    , (19, [70, 208, 182, 111, 86])
    , (20, [71, 209, 183, 112, 87])
    , (21, [72, 210, 184, 113, 88])
    , (22, [73, 211, 185, 114, 89])
    , (23, [74, 159, 186, 115, 90])
    , (24, [75, 160, 187, 116, 91])
    , (25, [76, 161, 188, 117, 92])
    , (26, [77, 162, 189, 118, 93])
    , (27, [78, 163, 190, 119, 94])
    , (28, [79, 164, 191, 120, 95])
    , (29, [80, 165, 192, 121, 96])
    , (30, [81, 166, 193, 122, 97])
    , (31, [82, 167, 194, 123, 98])
    , (32, [83, 168, 195, 124, 99])
    , (33, [84, 169, 196, 125, 100])
    , (34, [85, 170, 197, 126, 101])
    , (35, [86, 171, 198, 127, 102])
    , (36, [87, 172, 199, 128, 103])
    , (37, [88, 173, 200, 129, 104])
    , (38, [89, 174, 201, 130, 105])
    , (39, [90, 175, 202, 131, 53])
    , (40, [91, 176, 203, 132, 54])
    , (41, [92, 177, 204, 133, 55])
    , (42, [93, 178, 205, 134, 56])
    , (43, [94, 179, 206, 135, 57])
    , (44, [95, 180, 207, 136, 58])
    , (45, [96, 181, 208, 137, 59])
    , (46, [97, 182, 209, 138, 60])
    , (47, [98, 183, 210, 139, 61])
    , (48, [99, 184, 211, 140, 62])
    , (49, [100, 185, 159, 141, 63])
    , (50, [101, 186, 160, 142, 64])
    , (51, [102, 187, 161, 143, 65])
    , (52, [103, 188, 162, 144, 66])
    , (53, [2, 111, 117, 39, 178])
    , (54, [3, 112, 118, 40, 179])
    , (55, [4, 113, 119, 41, 180])
    , (56, [5, 114, 120, 42, 181])
    , (57, [6, 115, 121, 43, 182])
    , (58, [7, 116, 122, 44, 183])
    , (59, [8, 117, 123, 45, 184])
    , (60, [9, 118, 124, 46, 185])
    , (61, [10, 119, 125, 47, 186])
    , (62, [11, 120, 126, 48, 187])
    , (63, [12, 121, 127, 49, 188])
    , (64, [13, 122, 128, 50, 189])
    , (65, [14, 123, 129, 51, 190])
    , (66, [15, 124, 130, 52, 191])
    , (67, [16, 125, 131, 0, 192])
    , (68, [17, 126, 132, 1, 193])
    , (69, [18, 127, 133, 2, 194])
    , (70, [19, 128, 134, 3, 195])
    , (71, [20, 129, 135, 4, 196])
    , (72, [21, 130, 136, 5, 197])
    , (73, [22, 131, 137, 6, 198])
    , (74, [23, 132, 138, 7, 199])
    , (75, [24, 133, 139, 8, 200])
    , (76, [25, 134, 140, 9, 201])
    , (77, [26, 135, 141, 10, 202])
    , (78, [27, 136, 142, 11, 203])
    , (79, [28, 137, 143, 12, 204])
    , (80, [29, 138, 144, 13, 205])
    , (81, [30, 139, 145, 14, 206])
    , (82, [31, 140, 146, 15, 207])
    , (83, [32, 141, 147, 16, 208])
    , (84, [33, 142, 148, 17, 209])
    , (85, [34, 143, 149, 18, 210])
    , (86, [35, 144, 150, 19, 211])
    , (87, [36, 145, 151, 20, 159])
    , (88, [37, 146, 152, 21, 160])
    , (89, [38, 147, 153, 22, 161])
    , (90, [39, 148, 154, 23, 162])
    , (91, [40, 149, 155, 24, 163])
    , (92, [41, 150, 156, 25, 164])
    , (93, [42, 151, 157, 26, 165])
    , (94, [43, 152, 158, 27, 166])
    , (95, [44, 153, 106, 28, 167])
    , (96, [45, 154, 107, 29, 168])
    , (97, [46, 155, 108, 30, 169])
    , (98, [47, 156, 109, 31, 170])
    , (99, [48, 157, 110, 32, 171])
    , (100, [49, 158, 111, 33, 172])
    , (101, [50, 106, 112, 34, 173])
    , (102, [51, 107, 113, 35, 174])
    , (103, [52, 108, 114, 36, 175])
    , (104, [0, 109, 115, 37, 176])
    , (105, [1, 110, 116, 38, 177])
    , (106, [188, 195, 101, 14, 95])
    , (107, [189, 196, 102, 15, 96])
    , (108, [190, 197, 103, 16, 97])
    , (109, [191, 198, 104, 17, 98])
    , (110, [192, 199, 105, 18, 99])
    , (111, [193, 200, 53, 19, 100])
    , (112, [194, 201, 54, 20, 101])
    , (113, [195, 202, 55, 21, 102])
    , (114, [196, 203, 56, 22, 103])
    , (115, [197, 204, 57, 23, 104])
    , (116, [198, 205, 58, 24, 105])
    , (117, [199, 206, 59, 25, 53])
    , (118, [200, 207, 60, 26, 54])
    , (119, [201, 208, 61, 27, 55])
    , (120, [202, 209, 62, 28, 56])
    , (121, [203, 210, 63, 29, 57])
    , (122, [204, 211, 64, 30, 58])
    , (123, [205, 159, 65, 31, 59])
    , (124, [206, 160, 66, 32, 60])
    , (125, [207, 161, 67, 33, 61])
    , (126, [208, 162, 68, 34, 62])
    , (127, [209, 163, 69, 35, 63])
    , (128, [210, 164, 70, 36, 64])
    , (129, [211, 165, 71, 37, 65])
    , (130, [159, 166, 72, 38, 66])
    , (131, [160, 167, 73, 39, 67])
    , (132, [161, 168, 74, 40, 68])
    , (133, [162, 169, 75, 41, 69])
    , (134, [163, 170, 76, 42, 70])
    , (135, [164, 171, 77, 43, 71])
    , (136, [165, 172, 78, 44, 72])
    , (137, [166, 173, 79, 45, 73])
    , (138, [167, 174, 80, 46, 74])
    , (139, [168, 175, 81, 47, 75])
    , (140, [169, 176, 82, 48, 76])
    , (141, [170, 177, 83, 49, 77])
    , (142, [171, 178, 84, 50, 78])
    , (143, [172, 179, 85, 51, 79])
    , (144, [173, 180, 86, 52, 80])
    , (145, [174, 181, 87, 0, 81])
    , (146, [175, 182, 88, 1, 82])
    , (147, [176, 183, 89, 2, 83])
    , (148, [177, 184, 90, 3, 84])
    , (149, [178, 185, 91, 4, 85])
    , (150, [179, 186, 92, 5, 86])
    , (151, [180, 187, 93, 6, 87])
    , (152, [181, 188, 94, 7, 88])
    , (153, [182, 189, 95, 8, 89])
    , (154, [183, 190, 96, 9, 90])
    , (155, [184, 191, 97, 10, 91])
    , (156, [185, 192, 98, 11, 92])
    , (157, [186, 193, 99, 12, 93])
    , (158, [187, 194, 100, 13, 94])
    , (159, [23, 130, 123, 49, 87])
    , (160, [24, 131, 124, 50, 88])
    , (161, [25, 132, 125, 51, 89])
    , (162, [26, 133, 126, 52, 90])
    , (163, [27, 134, 127, 0, 91])
    , (164, [28, 135, 128, 1, 92])
    , (165, [29, 136, 129, 2, 93])
    , (166, [30, 137, 130, 3, 94])
    , (167, [31, 138, 131, 4, 95])
    , (168, [32, 139, 132, 5, 96])
    , (169, [33, 140, 133, 6, 97])
    , (170, [34, 141, 134, 7, 98])
    , (171, [35, 142, 135, 8, 99])
    , (172, [36, 143, 136, 9, 100])
    , (173, [37, 144, 137, 10, 101])
    , (174, [38, 145, 138, 11, 102])
    , (175, [39, 146, 139, 12, 103])
    , (176, [40, 147, 140, 13, 104])
    , (177, [41, 148, 141, 14, 105])
    , (178, [42, 149, 142, 15, 53])
    , (179, [43, 150, 143, 16, 54])
    , (180, [44, 151, 144, 17, 55])
    , (181, [45, 152, 145, 18, 56])
    , (182, [46, 153, 146, 19, 57])
    , (183, [47, 154, 147, 20, 58])
    , (184, [48, 155, 148, 21, 59])
    , (185, [49, 156, 149, 22, 60])
    , (186, [50, 157, 150, 23, 61])
    , (187, [51, 158, 151, 24, 62])
    , (188, [52, 106, 152, 25, 63])
    , (189, [0, 107, 153, 26, 64])
    , (190, [1, 108, 154, 27, 65])
    , (191, [2, 109, 155, 28, 66])
    , (192, [3, 110, 156, 29, 67])
    , (193, [4, 111, 157, 30, 68])
    , (194, [5, 112, 158, 31, 69])
    , (195, [6, 113, 106, 32, 70])
    , (196, [7, 114, 107, 33, 71])
    , (197, [8, 115, 108, 34, 72])
    , (198, [9, 116, 109, 35, 73])
    , (199, [10, 117, 110, 36, 74])
    , (200, [11, 118, 111, 37, 75])
    , (201, [12, 119, 112, 38, 76])
    , (202, [13, 120, 113, 39, 77])
    , (203, [14, 121, 114, 40, 78])
    , (204, [15, 122, 115, 41, 79])
    , (205, [16, 123, 116, 42, 80])
    , (206, [17, 124, 117, 43, 81])
    , (207, [18, 125, 118, 44, 82])
    , (208, [19, 126, 119, 45, 83])
    , (209, [20, 127, 120, 46, 84])
    , (210, [21, 128, 121, 47, 85])
    , (211, [22, 129, 122, 48, 86])
    ]
