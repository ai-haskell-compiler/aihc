{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Module    :  Data.Graph.VisualizeAlternative
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Copies of some methods from @graphite@ package with only purpose
-- to replace the parameter 'Sfdp' by 'Dot' in 'plotDGraph' term.
module Data.Graph.VisualizeAlternative (plotDGraph, plotDGraphPng, toDirectedDot, sensibleDotParams) where

import Control.Concurrent (ThreadId, forkIO)
import Data.Graph.DGraph (DGraph, arcs)
import Data.Graph.Types (Arc (Arc), Graph, vertices)
import Data.GraphViz
  ( DotGraph,
    GlobalAttributes (GraphAttrs),
    GraphvizCanvas (Xlib),
    GraphvizCommand (Dot, Sfdp),
    GraphvizOutput (Png),
    GraphvizParams,
    PrintDot,
    addExtension,
    fmtEdge,
    globalAttributes,
    graphElemsToDot,
    isDirected,
    nonClusteredParams,
    runGraphvizCanvas,
    runGraphvizCommand,
  )
import Data.GraphViz.Attributes.Complete (Attribute (Label, Overlap), Label (StrLabel), Overlap (ScaleOverlaps))
import Data.Hashable (Hashable)
import qualified Data.Text.Lazy as TL
import Prelude (Bool (False, True), FilePath, IO, Ord, Show, String, show, ($), (<$>))

-- | A copy of @plotDGraph@ method from 'Data.Graph.Visualize' but the parameter 'Sfdp' is replaced by 'Dot'.
plotDGraph ::
  (Hashable v, Ord v, PrintDot v, Show v, Show e) =>
  DGraph v e ->
  IO ThreadId
plotDGraph g = forkIO $ runGraphvizCanvas Dot (toDirectedDot False g) Xlib

-- | A copy of @toDirectedDot@ method from 'Data.Graph.Visualize' but the parameter 'Sfdp' is replaced by 'Dot'.
plotDGraphPng ::
  (Hashable v, Ord v, PrintDot v, Show v, Show e) =>
  DGraph v e ->
  FilePath ->
  IO FilePath
plotDGraphPng g = addExtension (runGraphvizCommand Dot $ toDirectedDot False g) Png

-- | A copy of @toDirectedDot@ method from 'Data.Graph.Visualize'.
toDirectedDot ::
  (Hashable v, Ord v, Show v, Show e) =>
  Bool ->
  DGraph v e ->
  DotGraph v
toDirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
  where
    params = sensibleDotParams True labelEdges

-- | A copy of @sensibleDotParams@ method from 'Data.Graph.Visualize'.
sensibleDotParams ::
  Bool ->
  Bool ->
  GraphvizParams t l String () l
sensibleDotParams directed edgeLabeled =
  nonClusteredParams
    { isDirected = directed,
      globalAttributes =
        [ GraphAttrs [Overlap ScaleOverlaps]
        ],
      fmtEdge = edgeFmt
    }
  where
    edgeFmt (_, _, l) =
      [Label $ StrLabel $ TL.pack l | edgeLabeled]

-- | A copy of @labeledNodes@ method from 'Data.Graph.Visualize'.
labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> vertices g

-- | A copy of @labeledNodes@ method from 'Data.Graph.Visualize'.
labeledArcs :: (Hashable v, Show e) => DGraph v e -> [(v, v, String)]
labeledArcs g = (\(Arc v1 v2 attr) -> (v1, v2, show attr)) <$> arcs g
