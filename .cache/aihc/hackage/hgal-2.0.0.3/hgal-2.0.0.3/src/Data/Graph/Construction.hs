{-| Module      :  Data.Graph.Construction
    Copyright   :  (c) Jean-Philippe Bernardy 2003
    License     :  GPL

    Maintainer  :  JeanPhilippe.Bernardy@gmail.com
    Stability   :  proposal
    Portability :  GHC


Various functions to build graphs.

-}


module Data.Graph.Construction (hCubeG, cycleG, prismG, productG, linearG, arcG,
                                starG, unionG, undirG, tensorG, kG, cliqueG,
                                emptyG
                               ) where

import Data.Graph
import Data.Array (bounds, (!))


arcG :: Graph
arcG = undirG $ buildG (0,1) [(0,1)]

vertexG :: Graph
vertexG = buildG (0, 0) []

-- triG :: Graph
-- triG = cycleG 3

-- cubeG :: Graph
-- cubeG = hCubeG 3

prismG :: Int -> Graph
prismG n = productG arcG (cycleG n)

hCubeG :: Int -> Graph
hCubeG n = powerG n arcG

powerG :: Int -> Graph -> Graph
powerG n gr = foldr productG vertexG (replicate n gr)

kG :: Int -> Int -> Graph
kG n m = undirG $ buildG (1, n+m) [(x,y) | x <- [1..n], y <- [n+1..n+m]]

linearG :: Int -> Graph
linearG n = buildG (1,n)         [(i, i+1) | i <- [1..n-1] ]

emptyG :: Int -> Graph
emptyG n = buildG (1,n) []

cycleG :: Int -> Graph
cycleG n = buildG (1,n) ((n,1) : [(i, i+1) | i <- [1..n-1] ])

starG :: (Vertex, Vertex) -> Graph
starG (l,h) = buildG (l,h) [(l,i) | i <- [l+1..h]]

cliqueG :: (Vertex, Vertex) -> Graph
cliqueG (l,h)
    | l == h = buildG (l,h) []
    | l <  h = unionG (starG (l,h)) (cliqueG (l+1, h))
    | otherwise = error "cliqueG not defined on input."

unionG :: Graph -> Graph -> Graph
unionG g1 g2 = buildG (low, high) (edges g1 ++ edges g2)
               where low = min low1 low2
                     high = max high1 high2
                     (low1, high1) = bounds g1
                     (low2, high2) = bounds g2

tensorG :: [Int] -> Graph
tensorG = foldr (productG . linearG) vertexG

undirG :: Graph -> Graph
undirG g = unionG g (transposeG g)

type PVertex = (Vertex, Vertex)


isNeighbour :: Graph -> Vertex -> Vertex -> Bool
isNeighbour gr n1 n2 = n2 `elem` gr!n1

gen1 :: Graph -> Graph -> (Vertex, Vertex) -> (Vertex, Vertex) -> Bool
gen1 g1 g2 (x1, x2) (y1, y2) =
        isNeighbour g1 x1 y1 && x2 == y2 ||
        isNeighbour g2 x2 y2 && x1 == y1

-- gen2 g1 g2 (x1, x2) (y1, y2) =
--         isNeighbour g1 x1 y1 ||
--         isNeighbour g2 x2 y2

-- gen3 g1 g2 (x1, x2) (y1, y2) =
--      isNeighbour g1 x1 y1 &&
--      isNeighbour g2 x2 y2

productGen :: (Graph -> Graph -> PVertex -> PVertex -> Bool) -> Graph -> Graph -> Graph
productGen f g1 g2 =
        buildG bnds [ (renumber v1, renumber v2) | v1 <- vx, v2 <- vx, f g1 g2 v1 v2]
  where vx = [ (x, y) | x <- vertices1, y <- vertices2 ]
        vertices1 = vertices g1
        vertices2 = vertices g2
        (low1, high1) = bounds g1
        (low2, high2) = bounds g2
        renumber (v1, v2) = (v1-low1) + (high1-low1+1) * (v2-low2)
        bnds = (renumber (low1, low2), renumber (high1, high2))

productG :: Graph -> Graph -> Graph
productG = productGen gen1

