{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  Debug.SimpleExpr.GraphUtils
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Tools for transforming simple expressions to graphs from @graphite@.
module Debug.SimpleExpr.GraphUtils
  ( -- * Conversion simple expressions to graphs
    exprToGraph,

    -- * Visualisation
    plotExpr,
    plotDGraphPng,

    -- * Auxiliary functions
    simpleExprToGraph,
    appendNodeToGraph,
  )
where

import Control.Concurrent (ThreadId)
import Data.Fix (Fix (..))
import Data.Graph.DGraph (DGraph, insertArc)
import Data.Graph.Types (Arc (..), empty, insertVertex, union)
import Data.Graph.VisualizeAlternative (plotDGraph, plotDGraphPng)
import Debug.SimpleExpr.Expr (Expr, SimpleExpr, SimpleExprF (..), content, dependencies)
import Prelude (IO, String, fmap, foldr, show, ($), (.))

-- | Transforms a simple expression to graph.
simpleExprToGraph :: SimpleExpr -> DGraph String ()
simpleExprToGraph (Fix e) = case e of
  NumberF n -> appendNodeToGraph (show n) [] graph
  VariableF c -> appendNodeToGraph c [] graph
  BinaryFuncF _ a b -> appendNodeToGraph (show (Fix e)) [show a, show b] graph
  SymbolicFuncF _ args' -> appendNodeToGraph (show (Fix e)) (fmap show args') graph
  where
    graph = exprToGraph $ dependencies (Fix e)

-- | Appends a node to a graph using string valued keys.
--
-- The first argumet is the new node name.
--
-- The second argument is the list of dependent nodes.
appendNodeToGraph :: String -> [String] -> DGraph String () -> DGraph String ()
appendNodeToGraph newNodeName depNodeNames graph = foldr addArc initGraph depNodeNames
  where
    addArc depName = insertArc (Arc depName newNodeName ())
    initGraph = insertVertex newNodeName graph

-- | Transforms an expression to graph.
--
-- ==== __Examples of usage__
--
-- >>> import Debug.SimpleExpr (variable)
-- >>> import NumHask ((+), (-))
--
-- >>> x = variable "x"
-- >>> y = variable "y"
-- >>> exprToGraph [x + y, x - y]
-- ...
--
-- We expect something like
-- @fromList [("y",[("x-y",()),("x+y",())]),("x-y",[]),("x",[("x-y",()),("x+y",())]),("x+y",[])]@
-- depending on the packages version version.
exprToGraph :: Expr d => d -> DGraph String ()
exprToGraph d = case content d of
  [] -> empty -- insertVertex (name e) empty
  [v] -> simpleExprToGraph v
  (v : vs) -> simpleExprToGraph v `union` exprToGraph vs -- insertArc newArcV addedV where

-- | Visualizes an expression.
--
-- ==== __Examples of usage__
--
-- >>> import Debug.SimpleExpr (number, variable)
-- >>> import NumHask ((+), (-))
-- >>> import Data.Graph.VisualizeAlternative (plotDGraphPng)
--
-- @>>> plotExpr (number 1 + variable "x")@
--
-- ![1+x](doc/images/demo1.png)
--
-- >>> x = variable "x"
-- >>> y = variable "y"
--
-- @>>> plotExpr [x + y, x - y]@
--
-- ![x+y,x-y](doc/images/demo2.png)
plotExpr :: Expr d => d -> IO ThreadId
plotExpr = plotDGraph . exprToGraph
