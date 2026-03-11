{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Cauldron.Graph.Algorithm
  ( reverseTopSort,
  )
where

import Cauldron.Graph
import Data.Foldable (for_)
import Data.Foldable qualified
import Data.Function ((&))
import Data.Graph qualified
import Data.List.NonEmpty
import Data.Map.Strict qualified
import Data.Sequence qualified
import Data.Set qualified

reverseTopSort :: (Ord a) => AdjacencyMap a -> Either (NonEmpty a) [a]
reverseTopSort g = do
  let theEdges = do
        (i, o) <- adjacencyList g
        [(i, i, o)]
      sccs = Data.Graph.stronglyConnComp theEdges
  for_ sccs $ \case
    Data.Graph.AcyclicSCC _ -> pure ()
    -- we should use 
    -- @Data.Graph.NECyclicSCC vs@
    -- for containers >= 0.7
    Data.Graph.CyclicSCC (Data.List.NonEmpty.fromList -> vs) -> do
      let aCycle = findCycleInSCC g vs
      Left aCycle
  let (g', nodeFromVertex, _) = Data.Graph.graphFromEdges theEdges
  Right $ do
    ves <- Data.Graph.reverseTopSort g'
    let (v, _, _) = nodeFromVertex ves
    [v]

findCycleInSCC :: (Ord a) => AdjacencyMap a -> NonEmpty a -> NonEmpty a
findCycleInSCC g scc@(start :| _) = go start (Data.Set.singleton start) (Data.Sequence.singleton start)
  where
    sccSet = Data.Set.fromList . Data.Foldable.toList $ scc
    isInScc = (`Data.Set.member` sccSet)
    am = adjacencyMap $ Cauldron.Graph.induce isInScc g
    firstChildOf v =
      case Data.Set.toList <$> Data.Map.Strict.lookup v am of
        Nothing -> error "findCycleInSCC: node not in adjacency map"
        -- In a SCC, all vertices should have at least one outgoing edge!
        Just [] -> error "findCycleInSCC: SCC node with no outgoing edge"
        Just (child : _) -> child
    go current visited cycleAcc =
      let child = firstChildOf current
       in if child `Data.Set.member` visited
            then
              Data.List.NonEmpty.fromList $ Data.Foldable.toList $ Data.Sequence.dropWhileL (/= child) cycleAcc
            else
              go child (visited & Data.Set.insert child) (cycleAcc Data.Sequence.|> child)
