[![img](https://img.shields.io/hackage/v/dotparse.svg)](https://hackage.haskell.org/package/dotparse) [![img](https://github.com/tonyday567/dotparse/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/dotparse/actions/workflows/haskell-ci.yml)


# Introduction

Parsing and printing for the dot language of graphviz.

-   A close rendition of the [dot language specification](http://www.graphviz.org/doc/info/lang.html)
-   Supports inexact printing . parsing round trip; forgetting comments, separator choice and whitespace.
-   Treats attribute keys and values as ByteStrings. No Type safety of Attributes is attempted.
-   Uses command-line graphviz programs \`dot\` and \`neato\` to augment dot graph specifications
-   Supports conversion from and to [algebraic-graphs](https://hackage.haskell.org/package/algebraic-graphs).
-   Support rendering dot graphs using [chart-svg](https://hackage.haskell.org/package/chart-svg).
-   Uses [flatparse](https://hackage.haskell.org/package/flatparse) for speedy parsing.


## Reference

Graphviz documentation:

[Graphviz Main Page](https://www.graphviz.org/)

[Graphviz Visual Editor](http://magjac.com/graphviz-visual-editor/)

[Attributes | Graphviz](http://www.graphviz.org/doc/info/attrs.html)

[Dot Grammar](http://www.graphviz.org/pdf/dot.1.pdf)

Graphviz practical examples:

<https://renenyffenegger.ch/notes/tools/Graphviz/examples/index>

<https://renenyffenegger.ch/notes/tools/Graphviz/attributes/label/HTML-like/index>


## Similar projects

The [graphviz](https://hackage.haskell.org/package/graphviz) library aims for comprehensive typing of graphviz attributes and syntax. As a result, it is quite large and somewhat incomplete. In contrast, dotparse parsing is simpler, more robust and faster. It is also somewhat tied to fgl and I wanted to try a different graph library.

[dotgen](https://hackage.haskell.org/package/dotgen) is a dot graph printer but not a parser. It supports a monadic style of printing. Specifically, it supports generation of unique names if that is an important feature of the problem domain.


## development wish list

Target [calligraphy](https://hackage.haskell.org/package/calligraphy) for enhanced source code visualization.

Broaden support to include fgl and containers.

Support parsing of library graphs from cabal.

Explore [tagged partial birectional isomorphism](https://kowainik.github.io/posts/2019-01-14-tomland#tagged-partial-bidirectional-isomorphism) style.

Steal design ideas from [jordan](https://hackage.haskell.org/package/jordan).


# process pipelines


## algebraic-graphs

Starting with a Graph from algebraic-graphs:

    import qualified Algebra.Graph as G
    :{
    exAGraph :: G.Graph Int
    exAGraph =
      G.edges $
        [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
          <> [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
          <> [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]
    :}

    ghci| ghci| ghci| ghci| ghci| ghci| ghci|

    exAGraph

    Overlay (Connect (Vertex 0) (Vertex 1)) (Overlay (Connect (Vertex 1) (Vertex 2)) (Overlay (Connect (Vertex 2) (Vertex 3)) (Overlay (Connect (Vertex 3) (Vertex 4)) (Overlay (Connect (Vertex 4) (Vertex 5)) (Overlay (Connect (Vertex 5) (Vertex 0)) (Overlay (Connect (Vertex 0) (Vertex 6)) (Overlay (Connect (Vertex 0) (Vertex 12)) (Overlay (Connect (Vertex 1) (Vertex 7)) (Overlay (Connect (Vertex 1) (Vertex 13)) (Overlay (Connect (Vertex 2) (Vertex 8)) (Overlay (Connect (Vertex 2) (Vertex 14)) (Overlay (Connect (Vertex 3) (Vertex 9)) (Overlay (Connect (Vertex 3) (Vertex 15)) (Overlay (Connect (Vertex 4) (Vertex 10)) (Overlay (Connect (Vertex 4) (Vertex 16)) (Overlay (Connect (Vertex 5) (Vertex 11)) (Overlay (Connect (Vertex 5) (Vertex 17)) (Overlay (Connect (Vertex 2) (Vertex 18)) (Overlay (Connect (Vertex 2) (Vertex 19)) (Overlay (Connect (Vertex 15) (Vertex 18)) (Overlay (Connect (Vertex 15) (Vertex 19)) (Overlay (Connect (Vertex 18) (Vertex 3)) (Connect (Vertex 19) (Vertex 3))))))))))))))))))))))))


## dotparse Graph

Convert to a dotparse Graph

    exGraph = defaultGraph & addStatements (toStatements Directed (Char8.pack . show <$> exAGraph))


## dotPrint

Encode graph as a ByteString (prior to processing via graphviz)

    BS.putStr (dotPrint defaultDotConfig exGraph)

    digraph {
        node [height=0.5;shape=circle]
        graph [overlap=false;size="1!";splines=spline]
        edge [arrowsize=0.5]
        "9"
        "8"
        "7"
        "6"
        "5"
        "4"
        "3"
        "2"
        "19"
        "18"
        "17"
        "16"
        "15"
        "14"
        "13"
        "12"
        "11"
        "10"
        "1"
        "0"
        "5" -> "17"
        "5" -> "11"
        "5" -> "0"
        "4" -> "5"
        "4" -> "16"
        "4" -> "10"
        "3" -> "9"
        "3" -> "4"
        "3" -> "15"
        "2" -> "8"
        "2" -> "3"
        "2" -> "19"
        "2" -> "18"
        "2" -> "14"
        "19" -> "3"
        "18" -> "3"
        "15" -> "19"
        "15" -> "18"
        "1" -> "7"
        "1" -> "2"
        "1" -> "13"
        "0" -> "6"
        "0" -> "12"
        "0" -> "1"
        }


## processDotWith

Directly create an SVG from the dotparse Graph

    (\b f -> processDotWith Directed ["-Tsvg", "-o", "other/" <> f <> ".svg"] b) (dotPrint defaultDotConfig exGraph) "exdirect"

![img](other/exdirect.svg)


## processDot

ByteString of the processed Graph

    BS.putStr =<< processDot Directed (dotPrint defaultDotConfig exInt)


## processGraph

Graph augmented by graphviz

    exGraphAugmented <- processGraph exGraph
    :t exGraphAugmented

    exGraphAugmented :: Graph


## graphToChartWith

SVG production via chart-svg

    import Chart (writeChartOptions)
    writeChartOptions "other/exga.svg" (graphToChart exGraphAugmented)

![img](other/exga.svg)


# Development


## imports

    :reload
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -Wno-type-defaults
    :set -Wno-x-partial
    :set -XImportQualifiedPost
    import Chart
    import Optics.Core
    import FlatParse.Basic qualified as FP
    import qualified Data.ByteString as BS
    import qualified Data.Text as Text
    import qualified Data.ByteString.Char8 as Char8
    import Algebra.Graph qualified as G
    import Data.Monoid
    import GHC.Exts
    import DotParse
    import DotParse.Examples
    import DotParse.Examples.AST
    import DotParse.Examples.NumHask
    import Data.Proxy
    print "ok"

    [6 of 7] Compiling DotParse.Examples.NumHask ( src/DotParse/Examples/NumHask.hs, interpreted ) [Source file changed]
    Ok, 7 modules loaded.
    "ok"


## testAll

Round-trip test

    testAll

    ex0
    ex1
    ex2
    ex3
    ex4
    ex5
    ex6
    ex7
    ex8
    ex9
    ex10
    ex11
    ex12
    ex13
    ex14
    ex15


## numhask

    g <- processGraph (dotGraphNH Directed)
    writeChartOptions "other/nh12.svg" (graphToChartWith (defaultChartConfig & set #chartVshift (-4) & set #textSize 12) toLinkNH g)


# chart-svg AST

    g1 = dotAST allSC componentEdges
    BS.writeFile "other/chart-svg-ast.dot" $ dotPrint defaultDotConfig g1

    dot other/chart-svg-ast.dot -Tsvg >other/chart-svg-ast.svg

