{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Example of Dot graph construction for the <https://hackage.haskell.org/package/chart-svg chart-svg> class heirarchy.
module DotParse.Examples.AST where

import Algebra.Graph.Labelled qualified as L
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List qualified as List
import Data.List.NonEmpty hiding (filter, head, length, map, zip, zipWith, (!!))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.String.Interpolate
import Data.These
import DotParse.Types
import GHC.Generics
import Optics.Core
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> :set -XOverloadedStrings

-- | A Haskell class and (informal) list of sub-components.
data SubComponents = SubComponents
  { classComponent :: ByteString,
    subComponents :: [ByteString]
  }
  deriving (Eq, Show, Ord, Generic)

-- | Relationship between a class, a sub-component and the class of the sub-component.
data ComponentEdge = ComponentEdge
  { edgeClassComponent :: ByteString,
    edgeSubComponent :: ByteString,
    subComponentClass :: ByteString,
    edgeLabel :: Maybe ByteString
  }
  deriving (Eq, Show, Ord, Generic)

-- | algebraic graph vertices
graphVs :: (Monoid a) => [SubComponents] -> L.Graph a (ByteString, ByteString)
graphVs cs =
  L.vertices $
    ((\x -> (x, x)) . view #classComponent <$> cs)
      <> (mconcat $ subs <$> cs)

-- | Convert sub-components to a list of class, subcomponent bytestring tuples.
subs :: SubComponents -> [(ByteString, ByteString)]
subs c = (view #classComponent c,) <$> view #subComponents c

-- | algebraic graph edges
graphEs :: [ComponentEdge] -> L.Graph (Maybe ByteString) (ByteString, ByteString)
graphEs es =
  L.edges ((\c -> (view #edgeLabel c, (view #edgeClassComponent c, view #edgeSubComponent c), (view #subComponentClass c, view #subComponentClass c))) <$> es)

-- | algebraic graph
graphAST :: [SubComponents] -> [ComponentEdge] -> L.Graph (Maybe ByteString) (ByteString, ByteString)
graphAST cs es =
  graphVs cs <> graphEs es

-- | Create a list of 'SubComponents' from a list of 'ComponentEdge's
fromCEs :: [ComponentEdge] -> [SubComponents]
fromCEs es = fmap (uncurry SubComponents) $ Map.toList $ Map.fromListWith (<>) ((\e -> (view #edgeClassComponent e, [view #edgeSubComponent e])) <$> es)

-- | Convert an algebraic Graph into dot record nodes
recordNodes :: L.Graph (Maybe ByteString) (ByteString, ByteString) -> [Statement]
recordNodes g = (\(s, cs) -> StatementNode $ NodeStatement (IDQuoted s) Nothing (Map.fromList ([(ID "label", IDQuoted (ls (s, cs)))] <> maybe [] (\url -> [(ID "URL", IDQuoted url)]) (toURL s)))) <$> supers
  where
    ls (s, cs) = B.intercalate "|" $ ("<x" <> s <> "> " <> s) : fmap (\y -> " <x" <> y <> "> " <> y) cs
    supers = (\(s, cs) -> (s, filter (/= s) cs)) <$> (Map.toList $ Map.fromListWith (++) ((\(s, c) -> (s, [c])) <$> L.vertexList g))

-- | Convert an algebraic Graph into dot edges
recordEdges :: Directed -> L.Graph (Maybe ByteString) (ByteString, ByteString) -> [Statement]
recordEdges d g =
  ( \(l, (s0, c0), (s1, c1)) ->
      StatementEdge $
        EdgeStatement
          (fromDirected d)
          (EdgeID (IDQuoted s0) (Just (Port (This (IDQuoted ("x" <> c0))))))
          (fromList [EdgeID (IDQuoted c1) (Just (Port (This (IDQuoted ("x" <> s1)))))])
          (Map.fromList [(ID "label", IDQuoted (fromMaybe "x" l))])
  )
    <$> L.edgeList g

-- | create Statements from a (edge labelled) algebraic graph
--
-- https://graphviz.org/Gallery/directed/datastruct.html
toStatementsRecord :: Directed -> L.Graph (Maybe ByteString) (ByteString, ByteString) -> [Statement]
toStatementsRecord d g =
  recordEdges d g <> recordNodes g

-- | Convert a node ID to a label for chart-svg charts
-- Doing this directly in dot doesn't quite work because the engines get the width of the link wrong.
toURL :: ByteString -> Maybe ByteString
toURL name = fmap (\i' -> [i|https://hackage.haskell.org/package/#{view #itemPackage i'}/docs/#{view #itemModule i'}.html\#t:#{view #item i'}|]) item
  where
    item = List.find ((== name) . view #item) itemModules

-- | AST 'Graph'
--
-- > gAST = dotAST allSC componentEdges
-- > C.writeFile "other/ast.dot" $ dotPrint defaultDotConfig gAST
-- > bsSvg <- processDotWith Directed ["-Tsvg"] (dotPrint defaultDotConfig gAST)
-- > C.writeFile "other/ast.svg" bsSvg
dotAST :: [SubComponents] -> [ComponentEdge] -> Graph
dotAST sc ce =
  defaultGraph
    & set (attL GraphType (ID "size")) (Just $ IDQuoted "5")
    & addStatements (toStatementsRecord Directed (graphAST sc ce))
    & attL NodeType (ID "shape")
    .~ Just (ID "record")
    & gattL (ID "rankdir")
    .~ Just (IDQuoted "LR")

-- | Link values
data ItemModule = ItemModule {item :: ByteString, itemModule :: ByteString, itemPackage :: ByteString} deriving (Eq, Show, Generic)

-- | List of link values
itemModules :: [ItemModule]
itemModules =
  [ ItemModule "ChartOptions" "Chart-Markup" "chart-svg",
    ItemModule "MarkupOptions" "Chart-Markup" "chart-svg",
    ItemModule "CssOptions" "Chart-Markup" "chart-svg",
    ItemModule "ChartTree" "Chart-Primitive" "chart-svg",
    ItemModule "Chart" "Chart-Primitive" "chart-svg",
    ItemModule "HudOptions" "Chart-Hud" "chart-svg",
    ItemModule "RenderStyle" "Chart-Markup" "chart-svg",
    ItemModule "ChartAspect" "Chart-Primitive" "chart-svg",
    ItemModule "ShapeRendering" "Chart-Markup" "chart-svg",
    ItemModule "PreferColorScheme" "Chart-Primitive" "chart-svg",
    ItemModule "Tree" "Data-Tree" "containers",
    ItemModule "Priority" "Chart-Hud" "chart-svg",
    ItemModule "TitleOptions" "Chart-Hud" "chart-svg",
    ItemModule "AxisOptions" "Chart-Hud" "chart-svg",
    ItemModule "LegendOptions" "Chart-Hud" "chart-svg",
    ItemModule "FrameOptions" "Chart-Hud" "chart-svg",
    ItemModule "Adjustments" "Chart-Hud" "chart-svg",
    ItemModule "Ticks" "Chart-Hud" "chart-svg",
    ItemModule "Tick" "Chart-Hud" "chart-svg",
    ItemModule "Place" "Chart-Hud" "chart-svg",
    ItemModule "TickStyle" "Chart-Hud" "chart-svg",
    ItemModule "AxisBar" "Chart-Hud" "chart-svg",
    ItemModule "HudChartSection" "Chart-Hud" "chart-svg",
    ItemModule "TickExtend" "Chart-Hud" "chart-svg",
    ItemModule "FormatN" "Data-FormatN" "formatn",
    ItemModule "FStyle" "Data-FormatN" "formatn",
    ItemModule "Colour" "Data-Colour" "chart-svg",
    ItemModule "Style" "Chart-Style" "chart-svg",
    ItemModule "EscapeText" "Chart-Style" "chart-svg",
    ItemModule "GlyphShape" "Chart-Style" "chart-svg",
    ItemModule "Anchor" "Chart-Style" "chart-svg",
    ItemModule "LineCap" "Chart-Style" "chart-svg",
    ItemModule "LineJoin" "Chart-Style" "chart-svg",
    ItemModule "ScaleP" "Chart-Style" "chart-svg",
    ItemModule "ChartData" "Chart-Primitive" "chart-svg",
    ItemModule "PathData" "Data-Path" "chart-svg",
    ItemModule "ArcInfo" "Data-Path" "chart-svg",
    ItemModule "Rect" "NumHask-Space-Rect" "numhask-space",
    ItemModule "Point" "NumHask-Space-Point" "numhask-space"
  ]

-- | list of chart-svg component edges
componentEdges :: [ComponentEdge]
componentEdges =
  [ ComponentEdge "ChartOptions" "markupOptions" "MarkupOptions" (Just ""),
    ComponentEdge "ChartOptions" "hudOptions" "HudOptions" (Just ""),
    ComponentEdge "ChartOptions" "chartTree" "ChartTree" (Just ""),
    ComponentEdge "MarkupOptions" "chartAspect" "ChartAspect" (Just ""),
    ComponentEdge "MarkupOptions" "cssOptions" "CssOptions" (Just ""),
    ComponentEdge "MarkupOptions" "renderStyle" "RenderStyle" (Just ""),
    ComponentEdge "CssOptions" "shapeRendering" "ShapeRendering" (Just ""),
    ComponentEdge "CssOptions" "preferColorScheme" "PreferColorScheme" (Just ""),
    ComponentEdge "HudOptions" "axes" "AxisOptions" (Just "each % #item"),
    ComponentEdge "HudOptions" "frames" "FrameOptions" (Just "each % #item"),
    ComponentEdge "HudOptions" "legends" "LegendOptions" (Just "each % #item"),
    ComponentEdge "HudOptions" "titles" "TitleOptions" (Just "each % #item"),
    ComponentEdge "HudOptions" "axes" "Priority" (Just "each % #priority"),
    ComponentEdge "HudOptions" "frames" "Priority" (Just "each % #priority"),
    ComponentEdge "HudOptions" "legends" "Priority" (Just "each % #priority"),
    ComponentEdge "HudOptions" "titles" "Priority" (Just "each % #priority"),
    ComponentEdge "AxisOptions" "axisBar" "AxisBar" (Just "_Just"),
    ComponentEdge "AxisOptions" "adjustments" "Adjustments" (Just "_Just"),
    ComponentEdge "AxisOptions" "ticks" "Ticks" (Just ""),
    ComponentEdge "AxisOptions" "place" "Place" (Just ""),
    ComponentEdge "AxisBar" "style" "Style" (Just ""),
    ComponentEdge "AxisBar" "anchorTo" "HudChartSection" (Just ""),
    ComponentEdge "Tick" "formatN'" "FormatN" (Just "_Just"),
    ComponentEdge "FormatN" "fstyle" "FStyle" (Just ""),
    ComponentEdge "Tick" "tickExtend'" "TickExtend" (Just "_Just"),
    ComponentEdge "Ticks" "tick" "Tick" (Just "_Just"),
    ComponentEdge "Ticks" "glyphTick" "TickStyle" (Just "_Just"),
    ComponentEdge "Ticks" "textTick" "TickStyle" (Just "_Just"),
    ComponentEdge "Ticks" "lineTick" "TickStyle" (Just "_Just"),
    ComponentEdge "TickStyle" "style" "Style" (Just ""),
    ComponentEdge "TickStyle" "anchorTo" "HudChartSection" (Just ""),
    ComponentEdge "FrameOptions" "frame" "Style" (Just "#frame % _Just"),
    ComponentEdge "FrameOptions" "anchorTo" "HudChartSection" (Just ""),
    ComponentEdge "TitleOptions" "style" "Style" (Just ""),
    ComponentEdge "TitleOptions" "place" "Place" (Just ""),
    ComponentEdge "TitleOptions" "anchor" "Anchor" (Just ""),
    ComponentEdge "ChartTree" "tree" "Tree" (Just ""),
    ComponentEdge "ChartTree" "charts'" "Chart" (Just "each"),
    ComponentEdge "Chart" "chartStyle" "Style" (Just ""),
    ComponentEdge "Chart" "chartData" "ChartData" (Just ""),
    ComponentEdge "ChartData" "rectData'" "Rect" (Just "_Just % each"),
    ComponentEdge "ChartData" "lineData'" "Point" (Just "_Just % each % each"),
    ComponentEdge "ChartData" "glyphData'" "Point" (Just "_Just % each"),
    ComponentEdge "ChartData" "textData'" "(Text,Point)" (Just "_Just % each"),
    ComponentEdge "(Text,Point)" "_2" "Point" (Just ""),
    ComponentEdge "ChartData" "pathData'" "PathData" (Just "_Just % each"),
    ComponentEdge "ChartData" "blankData'" "Rect" (Just "_Just % each"),
    ComponentEdge "LegendOptions" "textStyle" "Style" (Just ""),
    ComponentEdge "LegendOptions" "frame" "Style" (Just "_Just"),
    ComponentEdge "LegendOptions" "place" "Place" (Just ""),
    ComponentEdge "LegendOptions" "scaleP" "ScaleP" (Just ""),
    ComponentEdge "LegendOptions" "legendCharts" "Chart" (Just "each % _2 % each"),
    ComponentEdge "PathData" "ArcP" "ArcInfo" (Just "(ArcP arcinfo _)"),
    ComponentEdge "Style" "color" "Colour" (Just ""),
    ComponentEdge "Style" "borderColor" "Colour" (Just ""),
    ComponentEdge "Style" "scaleP" "ScaleP" (Just ""),
    ComponentEdge "Style" "anchor" "Anchor" (Just ""),
    ComponentEdge "Style" "translate" "Point" (Just "_Just"),
    ComponentEdge "Style" "escapeText" "EscapeText" (Just ""),
    ComponentEdge "Style" "frame" "Style" (Just "_Just"),
    ComponentEdge "Style" "lineCap" "LineCap" (Just "_Just"),
    ComponentEdge "Style" "lineJoin" "LineJoin" (Just "_Just"),
    ComponentEdge "Style" "glyphShape" "GlyphShape" (Just "")
  ]

-- | list of chart-svg subcomponents
allSC :: [SubComponents]
allSC =
  [ SubComponents
      { classComponent = "AxisBar",
        subComponents =
          [ "style",
            "size",
            "buffer",
            "overhang",
            "anchorTo"
          ]
      },
    SubComponents
      { classComponent = "AxisOptions",
        subComponents =
          [ "axisBar",
            "adjustments",
            "ticks",
            "place"
          ]
      },
    SubComponents
      { classComponent = "Chart",
        subComponents =
          [ "chartStyle",
            "chartData"
          ]
      },
    SubComponents
      { classComponent = "ChartData",
        subComponents =
          [ "rectData'",
            "lineData'",
            "glyphData'",
            "textData'",
            "pathData'",
            "blankData'"
          ]
      },
    SubComponents
      { classComponent = "ChartOptions",
        subComponents =
          [ "markupOptions",
            "hudOptions",
            "chartTree"
          ]
      },
    SubComponents
      { classComponent = "ChartTree",
        subComponents =
          [ "tree",
            "charts'"
          ]
      },
    SubComponents
      { classComponent = "FrameOptions",
        subComponents =
          [ "frame",
            "anchorTo",
            "buffer"
          ]
      },
    SubComponents
      { classComponent = "HudOptions",
        subComponents =
          [ "axes",
            "frames",
            "legends",
            "titles"
          ]
      },
    SubComponents
      { classComponent = "MarkupOptions",
        subComponents =
          [ "markupHeight",
            "chartAspect",
            "cssOptions",
            "renderStyle"
          ]
      },
    SubComponents
      { classComponent = "(Text,Point)",
        subComponents =
          [ "_1",
            "_2"
          ]
      },
    SubComponents
      { classComponent = "TickStyle",
        subComponents =
          [ "style",
            "anchorTo",
            "buffer"
          ]
      },
    SubComponents
      { classComponent = "Ticks",
        subComponents =
          [ "tick",
            "glyphTick",
            "textTick",
            "lineTick"
          ]
      },
    SubComponents
      { classComponent = "TitleOptions",
        subComponents =
          [ "text",
            "style",
            "place",
            "anchor",
            "buffer"
          ]
      },
    SubComponents
      { classComponent = "RenderStyle",
        subComponents =
          [ "Compact",
            "Indented"
          ]
      },
    SubComponents
      { classComponent = "CssOptions",
        subComponents =
          [ "shapeRendering",
            "preferColorScheme",
            "fontFamilies",
            "cssExtra"
          ]
      },
    SubComponents
      { classComponent = "ChartAspect",
        subComponents =
          [ "FixedAspect",
            "CanvasAspect",
            "ChartAspect",
            "UnscaledAspect"
          ]
      },
    SubComponents
      { classComponent = "HudChartSection",
        subComponents =
          [ "CanvasSection",
            "CanvasStyleSection",
            "HudSection",
            "HudStyleSection"
          ]
      },
    SubComponents
      { classComponent = "Adjustments",
        subComponents =
          [ "maxXRatio",
            "maxYRatio",
            "angledRatio",
            "allowDiagonal"
          ]
      },
    SubComponents
      { classComponent = "Tick",
        subComponents =
          [ "TickNone",
            "TickLabels",
            "TickRound",
            "TickExact",
            "TickPlaced",
            "numTicks'",
            "formatN'",
            "tickExtend'"
          ]
      },
    SubComponents
      { classComponent = "TickExtend",
        subComponents =
          [ "TickExtend",
            "NoTickExtend"
          ]
      },
    SubComponents
      { classComponent = "FStyle",
        subComponents =
          [ "FSDecimal",
            "FSExponent",
            "FSComma",
            "FSFixed Int",
            "FSPercent",
            "FSDollar",
            "FSPrec",
            "FSCommaPrec",
            "FSNone"
          ]
      },
    SubComponents
      { classComponent = "FormatN",
        subComponents =
          [ "fstyle",
            "sigFigs",
            "maxDistinguishIterations",
            "addLPad",
            "cutRightZeros"
          ]
      },
    SubComponents
      { classComponent = "ShapeRendering",
        subComponents =
          [ "UseGeometricPrecision",
            "UseCssCrisp",
            "NoShapeRendering"
          ]
      },
    SubComponents
      { classComponent = "PreferColorScheme",
        subComponents =
          [ "PreferHud",
            "PreferDark",
            "PreferLight",
            "PreferNormal"
          ]
      },
    SubComponents
      { classComponent = "Place",
        subComponents =
          [ "PlaceLeft",
            "PlaceRight",
            "PlaceTop",
            "PlaceBottom",
            "PlaceAbsolute"
          ]
      },
    SubComponents
      { classComponent = "LegendOptions",
        subComponents =
          [ "legendSize",
            "buffer",
            "vgap",
            "hgap",
            "textStyle",
            "innerPad",
            "outerPad",
            "frame",
            "place",
            "scaleChartsBy",
            "scaleP",
            "legendCharts"
          ]
      },
    SubComponents
      { classComponent = "Anchor",
        subComponents =
          [ "AnchorMiddle",
            "AnchorStart",
            "AnchorEnd"
          ]
      },
    SubComponents
      { classComponent = "Point",
        subComponents =
          [ "_x",
            "_y"
          ]
      },
    SubComponents
      { classComponent = "PathData",
        subComponents =
          [ "StartP",
            "LineP",
            "CubicP",
            "QuadP",
            "ArcP"
          ]
      },
    SubComponents
      { classComponent = "ArcInfo",
        subComponents =
          [ "radii",
            "phi",
            "large",
            "clockwise"
          ]
      },
    SubComponents
      { classComponent = "Style",
        subComponents =
          [ "size",
            "borderSize",
            "color",
            "borderColor",
            "scaleP",
            "anchor",
            "rotation",
            "translate",
            "escapeText",
            "frame",
            "lineCap",
            "lineJoin",
            "dasharray",
            "dashoffset",
            "hsize",
            "vsize",
            "vshift",
            "glyphShape"
          ]
      },
    SubComponents
      { classComponent = "ScaleP",
        subComponents =
          [ "NoScaleP",
            "ScalePX",
            "ScalePY",
            "ScalePMinDim",
            "ScalePArea"
          ]
      },
    SubComponents
      { classComponent = "Colour",
        subComponents =
          [ "opac'",
            "lightness'",
            "chroma'",
            "hue'"
          ]
      },
    SubComponents
      { classComponent = "EscapeText",
        subComponents =
          [ "EscapeText",
            "NoEscapeText"
          ]
      },
    SubComponents
      { classComponent = "LineCap",
        subComponents =
          [ "LineCapButt",
            "LineCapRound",
            "LineCapSquare"
          ]
      },
    SubComponents
      { classComponent = "LineJoin",
        subComponents =
          [ "LineJoinMiter",
            "LineJoinBevel",
            "LineJoinRound"
          ]
      },
    SubComponents
      { classComponent = "GlyphShape",
        subComponents =
          [ "CircleGlyph",
            "SquareGlyph",
            "EllipseGlyph",
            "RectSharpGlyph",
            "RectRoundedGlyph",
            "TriangleGlyph",
            "VLineGlyph",
            "HLineGlyph",
            "PathGlyph"
          ]
      }
  ]
