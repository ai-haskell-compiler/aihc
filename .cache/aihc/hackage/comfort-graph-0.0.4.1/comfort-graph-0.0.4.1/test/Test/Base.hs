module Test.Base where

import qualified Data.Graph.Comfort as Graph
import Data.Graph.Comfort (Graph)

import qualified Data.Set as Set

import Control.Applicative (liftA2)

import qualified Test.QuickCheck as QC



type Edge = Graph.DirEdge
type Node = Int
type EdgeLabel = Integer
type NodeLabel = Char
type MonoGraph = Graph Edge Node EdgeLabel NodeLabel

newtype TestGraph = TestGraph {getTestGraph :: MonoGraph}
   deriving (Show)

instance QC.Arbitrary TestGraph where
   shrink (TestGraph g) =
      case Graph.nodeSet g of
         ns ->
            map (TestGraph . flip Graph.deleteNodeSet g .
                 Set.difference ns . Set.fromList) $
            QC.shrink $ Set.toList ns
   arbitrary = do
      nodes <- QC.arbitrary
      fmap TestGraph $
         if null nodes
            then return Graph.empty
            else do
               let genNode = QC.elements $ map fst nodes
               fmap (Graph.fromList nodes) $ QC.listOf $
                  liftA2 (,)
                     (liftA2 Graph.DirEdge genNode genNode) QC.arbitrary


data GraphAndEdge = GraphAndEdge MonoGraph (Edge Node)
   deriving (Show)

instance QC.Arbitrary GraphAndEdge where
   shrink (GraphAndEdge gr e) =
      map (\(TestGraph g) -> GraphAndEdge g e) $ QC.shrink $ TestGraph gr
   arbitrary =
      let makeGraph p consEdge = do
            TestGraph gr <- QC.suchThat QC.arbitrary $ p . getTestGraph
            fmap (GraphAndEdge gr) $ consEdge gr
      in  QC.oneof
            [makeGraph (not . null . Graph.edges) $ QC.elements . Graph.edges,
             makeGraph (not . null . Graph.nodes) $
                \gr ->
                   let selNode = QC.elements $ Graph.nodes gr
                   in  liftA2 Graph.DirEdge selNode selNode]
