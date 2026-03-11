{- | 
    Module      :  Data.Graph.Automorphism
    Copyright   :  (c) Jean-Philippe Bernardy 2003, 2008
    License     :  GPL

    Maintainer  :  JeanPhilippe.Bernardy@gmail.com
    Stability   :  proposal
    Portability :  GHC


   implementation of the canonic labeling of graphs + automorphism group.

  The implementation is based on:
  Brendan D. McKay, PRACTICAL GRAPH ISOMORPHISM,
  in Congressus Numerantium,
  Vol. 30 (1981), pp. 45-87.


NOTE: Usage of implicit automorphisms, as described on page 62, is not implemented here.

TODO:
  - as GHC 6.6, use Sequence instead of appends at end.
  - skip first automorphism found; it is identity.
  - try not relabeling the graphs

-}

module Data.Graph.Automorphism(canonicGraph, canonicGraph0, autGenerators,
                               automorphisms, isIsomorphic, debugTree, withUnitPartition) where

import Data.Graph(Graph, Vertex)
import Data.Array
import Data.List (sort, isPrefixOf)
import Control.Monad (when)
import Control.Monad.ST
import Data.Graph.Partition
import Data.Graph.Permutation
import Data.STRef
import Data.Tree

-- relabel a graph, given a discrete partition
relabel :: Graph -> Partition -> Graph
relabel gr partition = applyPerm simplePermutation gr
    where simplePermutation = array bnds (zip (map head partition) (range bnds))
          bnds = bounds gr


-----------------------------------------------
-- The following manages the nests of partitions

initialPartition :: Partition -> Graph -> Partition
initialPartition pie gr = refine gr (unitPartition $ bounds gr) pie

{- Not currently used:
discretePartition :: Graph -> Partition
discretePartition gr = map (: []) (range $ bounds gr)

splittingCell :: Partition -> Cell
splittingCell = head . filter (not . isSingleton)
-}

splitPartition :: Partition -> [(Vertex, Partition)]
splitPartition [] = []
splitPartition (c1:cs1) =
    if isSingleton c1
        then [(v, c1:cs2) | (v,cs2) <- splitPartition cs1]
        else [(v, c2:[v]:cs1) | (v, c2) <- splitCell c1]


-- splitCell [x,y,z] = [(x,[y,z]), (y,[x,z]), (z, [x,y])]
splitCell :: Cell -> [(Vertex, Cell)]
splitCell [] = []
splitCell (v:c) = (v, c) : [(v2, v:c2) | (v2, c2) <- splitCell c]


childPartitions :: Graph -> Partition -> [(Vertex, Partition)]
childPartitions gr part =
    [(n, refine gr p [[n]]) | (n,p) <- splitPartition part]

partitionTree :: Partition -> Graph -> Tree Partition
partitionTree userPartition gr = tree (initialPartition userPartition gr)
    where tree p = Node p (map (tree . snd) (childPartitions gr p))

annotateTree :: (a -> b) -> Tree a -> Tree (a,b)
annotateTree f = fmap f'
    where f' x = (x, f x)

debugTree :: Partition -> Graph -> IO ()
debugTree userPartition gr = putStrLn $ drawTree $ fmap show $ annotateTree (lambda gr) $ partitionTree userPartition gr

-----------------------------------------
-- Simple version of the Nauty algorithm
-- (No use of automorphism information)

-- | All paths from root to leaves
paths :: Tree t -> [[t]]
paths (Node x []) = [[x]]
paths (Node x cs) = map (x:) (concatMap paths cs)

-- | Returns a canonic labeling of the graph (slow -- but dead simple implementation).
-- This implementation serves documentation and debugging purposes.
canonicGraph0 :: Partition -> Graph -> Graph
canonicGraph0 userPartition gr0 = snd . minimum . map fct . paths . partitionTree userPartition $ gr
    where gr = sort <$> gr0
          fct nu = (lambda_ gr nu, relabel gr (last nu))



------------------------------------
-- Nauty algorithm

forWhile :: Monad m => [a] -> m Bool -> (a -> m ()) -> m ()
forWhile []     _    _      = return ()
forWhile (v:vs) cond action = action v >> cond >>= \c -> when c (forWhile vs cond action)

firstNoCommon :: (Eq a) => [a] -> [a] -> Maybe a
firstNoCommon _ [] = Nothing
firstNoCommon [] (v:_) = Just v
firstNoCommon (v1:v1s) (v2:v2s)
    | v1 == v2 = firstNoCommon v1s v2s
    | otherwise = Just v2

maybeElem :: (Eq t) => Maybe t -> [t] -> Bool
maybeElem Nothing _  = True
maybeElem (Just v) l = v `elem` l

-- tells if l1 is included in l2
included :: Eq a => [a] -> [a] -> Bool
l1 `included` l2 = all (`elem` l2) l1

leftMostNode :: Graph -> Partition -> (Partition, [Indicator], [Vertex])
leftMostNode gr pi1 = case childPartitions gr pi1 of
    ((v1, pi2):_) -> let (nu, ls, path) = leftMostNode gr pi2
                         in (nu, lambda gr pi1 : ls, v1 : path)
    [] -> (pi1, [lambda gr pi1], [])


-- nu = current node
-- zeta = 1st terminal node
-- rho = best guess at the node leading to canonical labelling
-- Lambda = indicator function for a node (usually written xLambda)
-- theta = orbit partiton of the automorphism group found yet
-- gamma = automorphism found
-- psi = store for automorphisms (gamma) found, in the form of (fix gamma, mcr gamma)

-- returns the graph relabelled, canonically. See McKay for details.
nauty :: Partition -> Graph -> ST s ([Permutation], Graph)
nauty userPartition gr0 =
    do {
       ;let gr = sort <$> gr0
       ;let graphBounds = bounds gr
       ;let relabeling p1 p2 = permBetween graphBounds (map head p1) (map head p2)
       -- return the relabelling defined by the mapping between two discrete partitions
       ;thetaRef <- newSTRef (listArray graphBounds (range graphBounds), range graphBounds)
       ;let root = initialPartition userPartition gr
       ;let (zeta, zetaLambda, zetaPath) = leftMostNode gr root
       ;let grZeta = relabel gr zeta
       ;rhoRef <- newSTRef (zeta, zetaLambda, grZeta)
       ;psi <- newSTRef []
       ;let
        {
--       exploreNode :: Partition -> [Vertex] -> [Indicator] -> ST s ();
         exploreNode nu nuPath nuLambda =
         do {
            ;let
            {foundTerminalNode =
             do {
                ;let grNu = relabel gr nu
                ;(if (nuLambda, grNu) == (zetaLambda, grZeta)
                  then foundAutomorphism (relabeling zeta nu)
                  else do
                        {
                         (rho, rhoLambda, grRho) <- readSTRef rhoRef
                        ;case compare (nuLambda, grNu) (rhoLambda, grRho) of
                           {
                            LT -> writeSTRef rhoRef (nu, nuLambda, grNu); -- "better" solution found
                            EQ -> foundAutomorphism (relabeling rho nu); -- no better, but use automorphism
                            GT -> return (); -- no luck
                           }
                        }
                 )
                };

             foundAutomorphism gamma =
             do {
                 -- update psi
                ; modifySTRef psi (gamma:)
                 -- update theta
                ;(thetaOld, _) <- readSTRef thetaRef
                ;let theta = mergePerms gamma thetaOld
                ;writeSTRef thetaRef (theta, mcr $ orbitsFromPerm theta)
                };

--           exploreSubnode :: (Vertex, Partition) -> ST s ();
             exploreSubnode (v, pie) =
             do {
                ;automs <- readSTRef psi
                -- pruning is explained on pages 60-61.
                ;let fixingAutomsMcrs = [mcr (orbitsFromPerm gamma) |
                                         gamma <- drop 1 automs, nuPath `included` fixed gamma]
                                        -- drop the 1st automorphism because it is always identity
                ;when ((v `elem`) `all` fixingAutomsMcrs)
                          (exploreNode pie (nuPath ++ [v]) (nuLambda ++ [lambda gr pie]))
                };

             test1 =
             do {
                ;(_, mcrTheta) <- readSTRef thetaRef
                ;return (maybeElem (firstNoCommon zetaPath nuPath) mcrTheta)
                };

            };
            ;(_, rhoLambda, _) <- readSTRef rhoRef
            ;when (nuLambda <= rhoLambda || (nuLambda `isPrefixOf` zetaLambda)) $ do
              {
              ;let childNodes = childPartitions gr nu
              ;(if null childNodes
                then foundTerminalNode
                else forWhile childNodes test1 exploreSubnode)
              }
            };
        };
       ;exploreNode root [] [lambda gr root]
       ;autG <- readSTRef psi
       ;(_,_,canonicGr) <- readSTRef rhoRef
       ;return (autG, canonicGr)
       }



-- | Given a graph, return generators of its automorphism group, and its canonic labeling
automorphisms :: Partition -> Graph -> ([Permutation], Graph)
automorphisms userPartition graph = runST (nauty userPartition graph)

-- | Return the canonic version of a graph.
canonicGraph :: Partition -> Graph -> Graph
canonicGraph p gr = snd $ automorphisms p gr

-- | Tells whether two graphs are isomorphic
isIsomorphic :: Graph -> Graph -> Bool
isIsomorphic g1 g2 = bounds g1 == bounds g2 && canonicGraph p g1 == canonicGraph p g2
    where p = unitPartition (bounds g1)
          

-- | Returns generators of the automorphism group
autGenerators :: Partition -> Graph -> [Permutation]
autGenerators userPartition gr = fst $ automorphisms userPartition gr

withUnitPartition
  :: (Partition -> Array Vertex e -> t)
  -> Array Vertex e
  -> t
withUnitPartition f gr = f (unitPartition $ bounds gr) gr
