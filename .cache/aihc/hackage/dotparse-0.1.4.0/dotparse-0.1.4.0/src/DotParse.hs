{-# OPTIONS_HADDOCK prune #-}

-- | Parser & Printer for the dot language of <http://www.graphviz.org/doc/info/lang.html graphviz>
--
-- See "DotParse.Examples" for usage.
module DotParse
  ( DotConfig (..),
    defaultDotConfig,
    DotParse (..),
    testDotParser,
    runDotParser,
    Graph (..),
    defaultGraph,
    attL,
    gattL,
    processDot,
    processDotWith,
    processGraph,
    processGraphWith,
    Strict (..),
    defStrict,
    Directed (..),
    defDirected,
    ID (..),
    label,
    Compass (..),
    Port (..),
    AttributeType (..),
    AttributeStatement (..),
    NodeStatement (..),
    EdgeID (..),
    EdgeOp (..),
    fromDirected,
    EdgeStatement (..),
    edgeID,
    edgeIDs,
    edgeIDsNamed,
    Statement (..),
    addStatement,
    addStatements,
    SubGraphStatement (..),

    -- * Conversion
    graphToChartWith,
    graphToChart,
    ChartConfig (..),
    defaultChartConfig,
    toStatements,
    toDotGraph,
    toDotGraphWith,
  )
where

import DotParse.Types
