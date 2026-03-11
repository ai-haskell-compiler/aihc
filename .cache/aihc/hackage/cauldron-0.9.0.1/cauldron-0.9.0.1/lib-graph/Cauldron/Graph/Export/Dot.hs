{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module     : Algebra.Graph.Export.Dot
-- Copyright  : (c) Andrey Mokhov 2016-2024
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines functions for exporting graphs in the DOT file format.
module Cauldron.Graph.Export.Dot
  ( -- * Graph attributes and style
    Attribute (..),
    Quoting (..),
    Style (..),
    defaultStyle,
    defaultStyleViaShow,

    -- * Export functions
    export,
  )
where

import Cauldron.Graph
import Cauldron.Graph.Export hiding (export)
import Cauldron.Graph.Export qualified as E
import Data.List (intersperse)
import Data.Monoid
import Data.String hiding (unlines)
import Prelude hiding (unlines)

-- | An attribute is just a key-value pair, for example @"shape" := "box"@.
-- Attributes are used to specify the style of graph elements during export.
data Attribute s = (:=) s s

-- TODO: Do we need other quoting styles, for example, 'SingleQuotes'?
-- TODO: Shall we use 'Quoting' for vertex names too?

-- | The style of quoting used when exporting attributes; 'DoubleQuotes' is the
-- default.
data Quoting = DoubleQuotes | NoQuotes

-- | The record 'Style' @a@ @s@ specifies the style to use when exporting a
-- graph in the DOT format. Here @a@ is the type of the graph vertices, and @s@
-- is the type of string to represent the resulting DOT document (e.g. String,
-- Text, etc.). The only field that has no obvious default value is
-- 'vertexName', which holds a function of type @a -> s@ to compute vertex
-- names. See the function 'export' for an example.
data Style a s = Style
  { -- | Name of the graph.
    graphName :: s,
    -- | Preamble (a list of lines) is added at the beginning of the DOT file body.
    preamble :: [s],
    -- | Graph style, e.g. @["bgcolor" := "azure"]@.
    graphAttributes :: [Attribute s],
    -- | Default vertex style, e.g. @["shape" := "diamond"]@.
    defaultVertexAttributes :: [Attribute s],
    -- | Default edge style, e.g. @["style" := "dashed"]@.
    defaultEdgeAttributes :: [Attribute s],
    -- | Compute a vertex name.
    vertexName :: a -> s,
    -- | Attributes of a specific vertex.
    vertexAttributes :: a -> [Attribute s],
    -- | Attributes of a specific edge.
    edgeAttributes :: a -> a -> [Attribute s],
    -- | The quoting style used for attributes.
    attributeQuoting :: Quoting
  }

-- | Default style for exporting graphs. The 'vertexName' field is provided as
-- the only argument; the other fields are set to trivial defaults.
defaultStyle :: (Monoid s) => (a -> s) -> Style a s
defaultStyle v = Style mempty [] [] [] [] v (const []) (\_ _ -> []) DoubleQuotes

-- | Default style for exporting graphs with 'Show'-able vertices. The
-- 'vertexName' field is computed using 'show'; the other fields are set to
-- trivial defaults.
--
-- @
-- defaultStyleViaShow = 'defaultStyle' ('fromString' . 'show')
-- @
defaultStyleViaShow :: (Show a, IsString s, Monoid s) => Style a s
defaultStyleViaShow = defaultStyle (fromString . show)

-- | Export a graph with a given style.
--
-- For example:
--
-- @
-- style :: 'Style' Int String
-- style = 'Style'
--     { 'graphName'               = \"Example\"
--     , 'preamble'                = ["  // This is an example", ""]
--     , 'graphAttributes'         = ["label" := \"Example\", "labelloc" := "top"]
--     , 'defaultVertexAttributes' = ["shape" := "circle"]
--     , 'defaultEdgeAttributes'   = 'mempty'
--     , 'vertexName'              = \\x   -> "v" ++ 'show' x
--     , 'vertexAttributes'        = \\x   -> ["color" := "blue"   | 'odd' x      ]
--     , 'edgeAttributes'          = \\x y -> ["style" := "dashed" | 'odd' (x * y)]
--     , 'attributeQuoting'        = 'DoubleQuotes' }
--
-- > putStrLn $ export style (1 * 2 + 3 * 4 * 5 :: 'Graph' Int)
--
-- digraph Example
-- {
--   // This is an example
--
--   graph [label=\"Example\" labelloc="top"]
--   node [shape="circle"]
--   "v1" [color="blue"]
--   "v2"
--   "v3" [color="blue"]
--   "v4"
--   "v5" [color="blue"]
--   "v1" -> "v2"
--   "v3" -> "v4"
--   "v3" -> "v5" [style="dashed"]
--   "v4" -> "v5"
-- }
-- @
export :: (IsString s, Monoid s, Ord a) => Style a s -> AdjacencyMap a -> s
export Style {..} g = E.render $ header <> body <> "}\n"
  where
    header = "digraph" <+> literal graphName <> "\n{\n"
    with x as = if null as then mempty else line (x <+> attributes attributeQuoting as)
    line s = indent 2 s <> "\n"
    body =
      unlines (map literal preamble)
        <> ("graph" `with` graphAttributes)
        <> ("node" `with` defaultVertexAttributes)
        <> ("edge" `with` defaultEdgeAttributes)
        <> E.export vDoc eDoc g
    label = doubleQuotes . literal . vertexName
    vDoc x = line $ label x <+> attributes attributeQuoting (vertexAttributes x)
    eDoc x y = line $ label x <> " -> " <> label y <+> attributes attributeQuoting (edgeAttributes x y)

-- | Export a list of attributes using a specified quoting style.
-- Example: @attributes DoubleQuotes ["label" := "A label", "shape" := "box"]@
-- corresponds to document: @[label="A label" shape="box"]@.
attributes :: (IsString s) => Quoting -> [Attribute s] -> Doc s
attributes _ [] = mempty
attributes q as = brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k := v) = literal k <> "=" <> quote (literal v)
    quote = case q of
      DoubleQuotes -> doubleQuotes
      NoQuotes -> id
