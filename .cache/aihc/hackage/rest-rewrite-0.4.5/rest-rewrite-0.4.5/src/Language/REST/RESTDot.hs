{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is responsible for rendering GraphViz graphs corresponding to an
--   execution of the REST algorithm.
module Language.REST.RESTDot (
    PrettyPrinter(..)
  , ShowRejectsOpt(..)
  , writeDot
  ) where

import Data.List
import Data.Hashable
import qualified Data.Set as S
import qualified Data.HashSet as HS

import Language.REST.Dot
import Language.REST.Path

-- | Controls how rejected paths should be visualized
data ShowRejectsOpt =
    ShowRejectsWithRule     -- ^ Display rejected paths, and the rule that generated them
  | ShowRejectsWithoutRule  -- ^ Display rejected paths, but don't display the rule that generated them
  | HideRejects             -- ^ Do not show rejected paths
  deriving Eq

-- | Controls how rules, terms, orderings, and rejected paths should be displayed
data PrettyPrinter rule term ord = PrettyPrinter
  { printRule    :: rule -> String
  , printTerm    :: term -> String
  , printOrd     :: ord  -> String
  , showRejects  :: ShowRejectsOpt
  }

rejNodeID :: (Hashable rule, Hashable term, Hashable a) => GraphType -> Path rule term a -> term -> String
rejNodeID gt p term = getNodeID gt p ++ show (abs $ hash term)

rejectedNodes :: forall rule term a . (Hashable rule, Hashable term, Hashable a) =>
  GraphType -> PrettyPrinter rule term a -> Path rule term a -> S.Set Node
rejectedNodes _ pp _ | showRejects pp == HideRejects = S.empty
rejectedNodes gt pp p@(_steps, PathTerm {rejected}) = S.fromList $ map go (HS.toList rejected)
    where
        go :: (term, rule) -> Node
        go (rejTerm, _r) = Node (rejNodeID gt p rejTerm) (printTerm pp rejTerm) "dashed" "red"


getNodeID :: (Hashable rule, Hashable term, Hashable a) => GraphType -> Path rule term a -> String
getNodeID Tree p         = "node" ++ show (abs $ hash p)
getNodeID Dag (steps, t) =
    "node" ++ show (abs $ hash t) ++ "_" ++ show (length steps)
getNodeID Min (_, t)     = "node" ++ show (abs $ hash t)

-- This determines how to layout
endNode :: (Hashable rule, Hashable term, Hashable a)
  => GraphType -> PrettyPrinter rule term a -> Path rule term a -> Node
endNode gt pp p@(_, t) =
    let
        thisNodeID = getNodeID gt p
    in
        Node thisNodeID (printTerm pp (pathTerm t)) "solid" "black"

toEdges :: forall rule term a . (Hashable rule, Hashable term, Hashable a) =>
  GraphType -> PrettyPrinter rule term a -> Path rule term a -> S.Set Edge
toEdges gt pp path = allRej `S.union` S.fromList (zipWith (curry toEdge) subs (tail subs))
    where
        subs = subPaths path

        allRej = S.unions $ map rejEdges subs

        rejEdges :: Path rule term a -> S.Set Edge
        rejEdges p@(_, PathTerm _ rej) =
          if showRejects pp /= HideRejects
          then S.fromList $ map go (HS.toList rej)
          else S.empty
            where
                ruleText r =
                  if showRejects pp == ShowRejectsWithRule
                  then printRule pp r
                  else ""
                go (rejTerm, r) =
                    Edge (nodeID (endNode gt pp p)) (rejNodeID gt p rejTerm) (ruleText r) "red" " " "dotted"


        toEdge :: (Path rule term a, Path rule term a) -> Edge
        toEdge (p0, p1@(ts, _)) =
            let
                step        = last ts
                color       = if fromPLE step then "brown" else "darkgreen"
                esubLabel    = printOrd pp (ordering step)
                startNodeID = nodeID (endNode gt pp p0)
                endNodeID   = nodeID (endNode gt pp p1)
            in
                Edge startNodeID endNodeID (printRule pp (rule step)) color esubLabel "solid"

subPaths :: Path rule term a -> [Path rule term a]
subPaths p@(xs, _t) = map toPath (tail $ inits xs) ++ [p]
    where
        toPath ys = (init ys, term (last ys))

toNodes :: (Hashable rule, Hashable term, Hashable a) => GraphType -> PrettyPrinter rule term a -> Path rule term a -> S.Set Node
toNodes gt pp path =
    let
        r = S.unions $ map (rejectedNodes gt pp) (subPaths path)
        n = S.fromList (map (endNode gt pp) (subPaths path))
    in
        S.union r n

toGraph :: (Hashable rule, Hashable term, Hashable a) => GraphType -> PrettyPrinter rule term a -> S.Set (Path rule term a) -> DiGraph
toGraph gt pp paths =
    DiGraph "Rest" (unions (S.map (toNodes gt pp) paths)) (unions (S.map (toEdges gt pp) paths))
    where
      unions :: (Ord a, Eq a, Hashable a) => S.Set (S.Set a) -> S.Set a
      unions = S.unions . S.toList

-- | @writeDot name gt printer paths@ generates a graphViz graph from @paths@ with name @name@.
writeDot :: (Hashable rule, Hashable term, Ord a, Hashable a) =>
  String -> GraphType -> PrettyPrinter rule term a -> S.Set (Path rule term a) -> IO ()
writeDot name gt printer paths = mkGraph name (toGraph gt printer paths)

