-- Do not edit! Automatically created with doctest-extract from src/Data/Graph/Comfort.hs
{-# LINE 99 "src/Data/Graph/Comfort.hs" #-}

module Test.Data.Graph.Comfort where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 100 "src/Data/Graph/Comfort.hs" #-}
import     Test.Base

import     qualified Data.Graph.Comfort as Graph
import     qualified Data.Map as Map
import     qualified Data.Set as Set
import     qualified Data.Char as Char
import     Data.Graph.Comfort (Graph, DirEdge(DirEdge), UndirEdge(UndirEdge))
import     Data.Map (Map)
import     Data.Tuple.HT (mapSnd)

import     qualified Control.Monad.Trans.Class as MT
import     qualified Control.Monad.Trans.State as MS
import     Control.Applicative (pure)
import     Data.Functor.Identity (Identity(Identity), runIdentity)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>), (===))

deleteNodeIfExists     :: Node -> MonoGraph -> MonoGraph
deleteNodeIfExists     n gr =
       maybe gr (const $ Graph.deleteNode n gr) $ Graph.lookupNode n gr

isolated     :: (Graph.Edge e, Ord n) => Graph.Graph e n el nl -> n -> Bool
isolated     gr n = Set.null (Graph.adjacentEdgeSet gr n)

nodeAction     :: (Monad m) => NodeLabel -> MS.StateT NodeLabel m NodeLabel
nodeAction     x = do y <- MS.get; MS.put x; return y

evalTraverseNode     :: NodeLabel -> MonoGraph -> MonoGraph
evalTraverseNode     nl =
       flip MS.evalState nl . Graph.traverseNode nodeAction

edgeAction     :: (Monad m) => EdgeLabel -> MS.StateT EdgeLabel m EdgeLabel
edgeAction     x = MS.modify (x+) >> MS.get

evalTraverseEdge     :: EdgeLabel -> MonoGraph -> MonoGraph
evalTraverseEdge     el =
       flip MS.evalState el . Graph.traverseEdge edgeAction

evalTraverse     :: NodeLabel -> EdgeLabel -> MonoGraph -> MonoGraph
evalTraverse     nl el =
       flip MS.evalState el . flip MS.evalStateT nl .
       Graph.traverse nodeAction (MT.lift . edgeAction)


(*-*)     :: n -> n -> UndirEdge n
(*-*)     = UndirEdge

(*->)     :: n -> n -> DirEdge n
(*->)     = DirEdge

unlabGraph     ::
       (Graph.Edge edge, Ord (edge node), Ord node) =>
       [node] -> [edge node] -> Graph edge node () ()
unlabGraph     ns es =
       let label = map (flip (,) ()) in
       Graph.fromMap
          (Map.fromList $ label $ ns ++ map Graph.from es ++ map Graph.to es)
          (Map.fromList $ label es)

addReversedEdges     ::
       (Ord node) => Graph DirEdge node el nl -> Graph DirEdge node el nl
addReversedEdges     gr =
       Graph.fromMap
          (Graph.nodeLabels gr)
          (Map.union
            (Graph.edgeLabels gr)
            (Map.mapKeys (\(Graph.DirEdge f t) -> Graph.DirEdge t f) $
               Graph.edgeLabels gr))

genShuffledGraph     ::
       (Graph.Edge e, Functor e, Ord a) =>
       Graph e a el nl -> QC.Gen (Graph e Int el nl, Map a Int)
genShuffledGraph     gr = do
       shuffledNodes <- QC.shuffle $ Graph.nodes gr
       let nodeMap = Map.fromList $ zip shuffledNodes [(0::Int)..]
       let mapNode n = nodeMap Map.! n
       return (Graph.mapKeys mapNode (fmap mapNode) gr, nodeMap)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Graph.Comfort:418: "
{-# LINE 418 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 418 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> Graph.isConsistent (Graph.reverse gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:419: "
{-# LINE 419 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 419 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> Graph.reverse (Graph.reverse gr) === gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:470: "
{-# LINE 470 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 470 "src/Data/Graph/Comfort.hs" #-}
      Graph.isEmpty (Graph.empty :: MonoGraph)
  )
 DocTest.printPrefix "Data.Graph.Comfort:471: "
{-# LINE 471 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 471 "src/Data/Graph/Comfort.hs" #-}
      Graph.isConsistent (Graph.empty :: MonoGraph)
  )
 DocTest.printPrefix "Data.Graph.Comfort:525: "
{-# LINE 525 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 525 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) -> Graph.lookupEdge e gr === Map.lookup e (Graph.edgeLabels gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:543: "
{-# LINE 543 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 543 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) n -> Graph.lookupNode n gr === Map.lookup n (Graph.nodeLabels gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:594: "
{-# LINE 594 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 594 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) n -> Graph.isConsistent $ deleteNodeIfExists n gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:595: "
{-# LINE 595 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 595 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) n nl -> Graph.deleteNode n (Graph.insertNode n nl gr) === deleteNodeIfExists n gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:596: "
{-# LINE 596 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 596 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> let isolatedNodes = filter (isolated gr) $ Graph.nodes gr in not (null isolatedNodes) ==> QC.forAll (QC.elements isolatedNodes) $ \n nl -> Graph.insertNode n nl gr === Graph.insertNode n nl (Graph.deleteNode n gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:622: "
{-# LINE 622 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 622 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) -> Graph.isConsistent $ Graph.deleteEdge e gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:623: "
{-# LINE 623 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 623 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) el -> Graph.deleteEdge e (Graph.insertEdge e el gr) === Graph.deleteEdge e gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:624: "
{-# LINE 624 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 624 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) el -> Graph.insertEdge e el gr === Graph.insertEdge e el (Graph.deleteEdge e gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:635: "
{-# LINE 635 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 635 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) -> Graph.filterEdgeWithKey (\ei _ -> e/=ei) gr === Graph.deleteEdge e gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:688: "
{-# LINE 688 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 688 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) n nl -> Graph.isConsistent $ Graph.insertNode n nl gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:689: "
{-# LINE 689 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 689 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) n nl -> Graph.lookupNode n (Graph.insertNode n nl gr) === Just nl
  )
 DocTest.printPrefix "Data.Graph.Comfort:701: "
{-# LINE 701 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 701 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) el -> Graph.isConsistent $ Graph.insertEdge e el gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:702: "
{-# LINE 702 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 702 "src/Data/Graph/Comfort.hs" #-}
      \(GraphAndEdge gr e) el -> Graph.lookupEdge e (Graph.insertEdge e el gr) === Just el
  )
 DocTest.printPrefix "Data.Graph.Comfort:736: "
{-# LINE 736 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 736 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> gr === Graph.fromMap (Graph.nodeLabels gr) (Graph.edgeLabels gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:756: "
{-# LINE 756 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 756 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> Graph.mapNode id gr === gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:769: "
{-# LINE 769 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 769 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> Graph.mapEdge id gr === gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:805: "
{-# LINE 805 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 805 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) nl -> Graph.isConsistent $ evalTraverseNode nl gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:806: "
{-# LINE 806 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 806 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) -> runIdentity (Graph.traverseNode (Identity . Char.toUpper) gr) === Graph.mapNode Char.toUpper gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:819: "
{-# LINE 819 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 819 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) el -> Graph.isConsistent $ evalTraverseEdge el gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:820: "
{-# LINE 820 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 820 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) el -> runIdentity (Graph.traverseEdge (Identity . (el+)) gr) === Graph.mapEdge (el+) gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:831: "
{-# LINE 831 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 831 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) nl el -> Graph.isConsistent $ evalTraverse nl el gr
  )
 DocTest.printPrefix "Data.Graph.Comfort:832: "
{-# LINE 832 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 832 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) nl el -> evalTraverse nl el gr === evalTraverseNode nl (evalTraverseEdge el gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:833: "
{-# LINE 833 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 833 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) nl el -> evalTraverse nl el gr === evalTraverseEdge el (evalTraverseNode nl gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:834: "
{-# LINE 834 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 834 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) nl -> flip MS.evalState nl (Graph.traverseNode nodeAction gr) === flip MS.evalState nl (Graph.traverse nodeAction pure gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:835: "
{-# LINE 835 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 835 "src/Data/Graph/Comfort.hs" #-}
      \(TestGraph gr) el -> flip MS.evalState el (Graph.traverseEdge edgeAction gr) === flip MS.evalState el (Graph.traverse pure edgeAction gr)
  )
 DocTest.printPrefix "Data.Graph.Comfort:872: "
{-# LINE 872 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 872 "src/Data/Graph/Comfort.hs" #-}
      
   Graph.depthFirstSearch $
   Graph.fromList [(0,'A'),(1,'B')]
      [(Graph.DirEdge 1 0, 23), (Graph.DirEdge 0 (1::Int), 42::Integer)]
  )
  [ExpectedLine [LineChunk "[Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = []}]}]"]]
 DocTest.printPrefix "Data.Graph.Comfort:911: "
{-# LINE 911 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 911 "src/Data/Graph/Comfort.hs" #-}
    mapSnd Graph.nodes $ Graph.topologicalSort $ unlabGraph [] ['a'*->'a']
  )
  [ExpectedLine [LineChunk "(\"\",\"a\")"]]
 DocTest.printPrefix "Data.Graph.Comfort:913: "
{-# LINE 913 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 913 "src/Data/Graph/Comfort.hs" #-}
    mapSnd Graph.nodes $ Graph.topologicalSort $ unlabGraph [] ['a'*->'h', 'a'*->'p', 'g'*->'r', 'p'*->'h', 'r'*->'a']
  )
  [ExpectedLine [LineChunk "(\"graph\",\"\")"]]
 DocTest.printPrefix "Data.Graph.Comfort:915: "
{-# LINE 915 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 915 "src/Data/Graph/Comfort.hs" #-}
    mapSnd Graph.nodes $ Graph.topologicalSort $ unlabGraph [] ['h'*->'a', 'a'*->'p', 'g'*->'r', 'p'*->'h', 'r'*->'a']
  )
  [ExpectedLine [LineChunk "(\"gr\",\"ahp\")"]]
 DocTest.printPrefix "Data.Graph.Comfort:938: "
{-# LINE 938 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 938 "src/Data/Graph/Comfort.hs" #-}
    map Graph.nodes $ Graph.components $ unlabGraph ['d'] ['a'*->'p', 'g'*->'r', 'p'*->'h']
  )
  [ExpectedLine [LineChunk "[\"ahp\",\"d\",\"gr\"]"]]
 DocTest.printPrefix "Data.Graph.Comfort:940: "
{-# LINE 940 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 940 "src/Data/Graph/Comfort.hs" #-}
    map Graph.nodes $ Graph.components $ unlabGraph ['d'] ['a'*-*'p', 'g'*-*'r', 'p'*-*'h']
  )
  [ExpectedLine [LineChunk "[\"ahp\",\"d\",\"gr\"]"]]
 DocTest.printPrefix "Data.Graph.Comfort:985: "
{-# LINE 985 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 985 "src/Data/Graph/Comfort.hs" #-}
      
   Graph.stronglyConnectedComponents $
   Graph.fromList [(0,'A'),(1,'B')] [(Graph.DirEdge 0 (1::Int),42::Integer)]
  )
  [ExpectedLine [LineChunk "[fromList [0],fromList [1]]"]]
 DocTest.printPrefix "Data.Graph.Comfort:991: "
{-# LINE 991 "src/Data/Graph/Comfort.hs" #-}
 DocTest.example(
{-# LINE 991 "src/Data/Graph/Comfort.hs" #-}
    map Set.toAscList $ Graph.stronglyConnectedComponents $ unlabGraph ['d'] ['g'*->'r', 'r'*->'a', 'a'*->'g', 'a'*->'p', 'p'*->'h', 'h'*->'p']
  )
  [ExpectedLine [LineChunk "[\"agr\",\"d\",\"hp\"]"]]
 DocTest.printPrefix "Data.Graph.Comfort:997: "
{-# LINE 997 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 997 "src/Data/Graph/Comfort.hs" #-}
        
   \(TestGraph gr) ->
   QC.forAll (genShuffledGraph gr) $ \(shuffled, nodeMap) ->

   Set.fromList
      (map (Set.map (nodeMap Map.!)) $ Graph.stronglyConnectedComponents gr)
   ===
   Set.fromList (Graph.stronglyConnectedComponents shuffled)
  )
 DocTest.printPrefix "Data.Graph.Comfort:1011: "
{-# LINE 1011 "src/Data/Graph/Comfort.hs" #-}
 DocTest.property(
{-# LINE 1011 "src/Data/Graph/Comfort.hs" #-}
        
   \(TestGraph gr) ->
   Set.fromList (map Graph.nodeSet (Graph.components gr))
   ===
   Set.fromList (Graph.stronglyConnectedComponents (addReversedEdges gr))
  )
