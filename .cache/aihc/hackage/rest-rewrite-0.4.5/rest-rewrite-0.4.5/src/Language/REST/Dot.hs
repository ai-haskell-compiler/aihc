{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains functionality for generating GraphViz graphs
module Language.REST.Dot
  ( mkGraph
  , DiGraph(..)
  , Edge(..)
  , GraphType(..)
  , Node(..)
  , NodeID
  ) where

import GHC.Generics
import Data.Hashable
import Data.List
import qualified Data.Set as S
import Text.Printf
import System.Process

-- | A GraphViz directed graph
data DiGraph = DiGraph
  String -- ^ Filename
  (S.Set Node)
  (S.Set Edge);

type NodeID =  String

-- | The way the graph will be rendered
data GraphType =
    Tree -- ^ Standard representation
  | Dag  -- ^ In 'Dag', If two equal terms `n` steps from the root are the same, they are
         --   represented by the same node, even if they were reached via different
         --   paths
  | Min  -- ^ Each unique term is represented by the same node
  deriving (Read)

-- | A GraphViz node
data Node = Node
    { nodeID     :: NodeID
    , label      :: String
    , nodeStyle  :: String
    , labelColor :: String
    } deriving (Eq, Ord, Show, Generic, Hashable)

-- A GraphViz edge
data Edge = Edge
    { from      :: NodeID
    , to        :: NodeID
    , edgeLabel :: String
    , edgeColor :: String
    , subLabel  :: String
    , edgeStyle :: String
    } deriving (Eq, Ord, Show, Generic, Hashable)

nodeString :: Node -> String
nodeString (Node nid elabel style color) =
    printf "\t%s [label=\"%s\"\nstyle=\"%s\"\ncolor=\"%s\"];" nid elabel style color

edgeString :: Edge -> String
edgeString (Edge efrom eto elabel color esubLabel style) =
    let
        sub = escape esubLabel
        escape = concatMap go
            where
                go '\\' = "\\"
                go '\n' = "<br />"
                go '>'  = "&gt;"
                go '<'  = "&lt;"
                go o    = [o]
        labelPart =
          if elabel /= ""
          then printf "<font color =\"red\">%s</font>" (escape elabel)
          else ""
    in
        printf "\t%s -> %s [label = <%s<br/>%s>\ncolor=\"%s\"\nstyle=\"%s\"];" efrom eto labelPart sub color style

graphString :: DiGraph -> String
graphString (DiGraph name nodes edges) =
    printf "digraph %s {\n%s\n\n%s\n}" name nodesString edgesString
    where
        nodesString :: String
        nodesString = intercalate "\n" (map nodeString (S.toList nodes))

        edgesString :: String
        edgesString = intercalate "\n" (map edgeString (S.toList edges))


-- | @mkGraph name graph@ generates the @.dot@ file for @graph@, and renders
--   the resulting graph to a @png@ file using the @dot@ utility
mkGraph :: String -> DiGraph -> IO ()
mkGraph name graph = do
  let dotfile = printf "graphs/%s.dot" name
  let pngfile = printf "graphs/%s.png" name
  writeFile dotfile (graphString graph)
  result <- readProcessWithExitCode "dot" ["-Tpng", dotfile, "-o", pngfile] ""
  print result

