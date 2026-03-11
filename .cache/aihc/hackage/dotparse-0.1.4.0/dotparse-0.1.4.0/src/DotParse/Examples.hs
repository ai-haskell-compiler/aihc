{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Examples of conversion from dot ByteStrings
--
-- Most examples from https://renenyffenegger.ch/notes/tools/Graphviz/examples/index
module DotParse.Examples where

import Algebra.Graph qualified as G
import Control.Monad (zipWithM_)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate
import DotParse
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> import Data.Proxy
-- >>> :set -XOverloadedStrings
-- >>> import Data.String.Interpolate
-- >>> import qualified Data.ByteString.Char8 as Char8
-- >>> import Optics.Core
-- >>> import Chart

-- * examples

-- | minimal definition
--
-- >>> runDotParser ex0 :: Graph
-- Graph {strict = Last {getLast = Just NoMergeEdges}, directed = Last {getLast = Just UnDirected}, graphid = Last {getLast = Nothing}, nodeAttributes = fromList [], graphAttributes = fromList [], edgeAttributes = fromList [], globalAttributes = fromList [], nodes = [], edges = [], subgraphs = []}
--
-- >>> testDotParser (Proxy :: Proxy Graph) defaultDotConfig ex0
ex0 :: ByteString
ex0 =
  [i|
graph {}
|]

-- |
-- ![Example](other/ex1.svg)
ex1 :: ByteString
ex1 =
  [i|
digraph D {
    A [shape=diamond]
    B [shape=box]
    C [shape=circle]
    A -> B [style=dashed, color=grey]
    A -> C [color="black:invis:black"]
    A -> D [penwidth=5, arrowhead=none]
    }
|]

-- |
-- ![Example](other/ex2.svg)
ex2 :: ByteString
ex2 =
  [i|
digraph D {

    node [fontname="Arial"];

    node_A [shape=record    label="shape=record|{above|middle|below}|right"];
    node_B [shape=plaintext label="shape=plaintext|{curly|braces and|bars without}|effect"];

}
|]

-- |
-- ![Example](other/ex3.svg)
ex3 :: ByteString
ex3 =
  [i|
digraph D {
  A -> {B, C, D} -> {F}
}
|]

-- |
--
-- ![Example](other/ex4.svg)
ex4 :: ByteString
ex4 =
  [i|
digraph L {

  node [shape=record fontname=Arial];

  a  [label="one\ntwo three\nfour five six seven\n"]
  b  [label="one\ntwo three\nfour five six seven"]
  c  [label="one\rtwo three\rfour five six seven\r"]

  a -> b -> c

}
|]

-- |
--
-- ![Example](other/ex5.svg)
ex5 :: ByteString
ex5 =
  [i|
digraph D {

  label = "The foo, the bar and the baz";
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};


}
|]

-- |
--
-- ![Example](other/ex6.svg)
ex6 :: ByteString
ex6 =
  [i|
digraph D {

  label = <The <font color='red'><b>foo</b></font>,<br/> the <font point-size='20'>bar</font> and<br/> the <i>baz</i>>;
  labelloc = "t"; // place the label at the top (b seems to be default)

  node [shape=plaintext]

  FOO -> {BAR, BAZ};

}
|]

-- |
--
-- ![Example](other/ex7.svg)
ex7 :: ByteString
ex7 =
  [i|
digraph R {

  { rank=same rA sA tA }
  { rank=same uB vB wB }


   rA -> sA;
   sA -> vB;
   t  -> rA;
   uB -> vB;
   wB -> u;
   wB -> tA;

}
|]

-- |
--
-- ![Example](other/ex8.svg)
ex8 :: ByteString
ex8 =
  [i|
digraph Q {

  nd_1   [label = "Node 1"];
  nd_2   [label = "Node 2"];
  nd_3_a [label = "Above Right Node 3"];
  nd_3_l [label = "Left of Node 3"];
  nd_3   [label = "Node 3"];
  nd_3_r [label = "Right of Node 3"];
  nd_4   [label = "Node 4"];


  nd_3_a -> nd_3_r;
  nd_1 -> nd_2 -> nd_3 -> nd_4;

  subgraph cluster_R {

    {rank=same nd_3_l nd_3 nd_3_r}

    nd_3_l -> nd_3 -> nd_3_r [color=grey arrowhead=none];

  }

}
|]

-- |
--
-- ![Example](other/ex9.svg)
ex9 :: ByteString
ex9 =
  [i|
digraph D {

  subgraph cluster_p {
    label = "Parent";

    subgraph cluster_c1 {
      label = "Child one";
      a;

      subgraph cluster_gc_1 {
        label = "Grand-Child one";
         b;
      }
      subgraph cluster_gc_2 {
        label = "Grand-Child two";
          c;
          d;
      }

    }

    subgraph cluster_c2 {
      label = "Child two";
      e;
    }
  }
}
|]

-- |
--
-- ![Example](other/ex10.svg)
ex10 :: ByteString
ex10 =
  [i|
digraph H {

  aHtmlTable [
   shape=plaintext
   color=blue      // The color of the border of the table
   label=<

     <table border='1' cellborder='0'>
       <tr><td>col 1</td><td>foo</td></tr>
       <tr><td>COL 2</td><td>bar</td></tr>
     </table>

  >];

}|]

-- |
--
-- ![Example](other/ex11.svg)
ex11 :: ByteString
ex11 =
  [i|
digraph {

  tbl [

    shape=plaintext
    label=<

      <table border='0' cellborder='1' color='blue' cellspacing='0'>
        <tr><td>foo</td><td>bar</td><td>baz</td></tr>
        <tr><td cellpadding='4'>
          <table color='orange' cellspacing='0'>
            <tr><td>one  </td><td>two  </td><td>three</td></tr>
            <tr><td>four </td><td>five </td><td>six  </td></tr>
            <tr><td>seven</td><td>eight</td><td>nine </td></tr>
          </table>
        </td>
        <td colspan='2' rowspan='2'>
          <table color='pink' border='0' cellborder='1' cellpadding='10' cellspacing='0'>
            <tr><td>eins</td><td>zwei</td><td rowspan='2'>drei<br/>sechs</td></tr>
            <tr><td>vier</td><td>fünf</td>                             </tr>
          </table>
        </td>
        </tr>

        <tr><td>abc</td></tr>

      </table>

    >];

}
|]

-- |
--
-- ![Example](other/ex12.svg)
ex12 :: ByteString
ex12 =
  [i|
digraph D {

  node [shape=plaintext]

  some_node [
   label=<
     <table border="0" cellborder="1" cellspacing="0">
       <tr><td bgcolor="yellow">Foo</td></tr>
       <tr><td bgcolor="lightblue"><font color="\#0000ff">Bar</font></td></tr>
       <tr><td bgcolor="\#f0e3ff"><font color="\#ff1020">Baz</font></td></tr>
     </table>>
  ];
}
|]

-- |
--
-- ![Example](other/ex13.svg)
ex13 :: ByteString
ex13 =
  [i|
digraph H {

  aHtmlTable [
   shape=plaintext
   label=<

     <table border='1' cellborder='0' style='rounded'>
       <tr><td>col 1</td><td>foo</td></tr>
       <tr><td>COL 2</td><td>bar</td></tr>
     </table>

  >];

}
|]

-- |
--
-- ![Example](other/ex14.svg)
ex14 :: ByteString
ex14 =
  [i|
digraph H {

  parent [
   shape=plaintext
   label=<
     <table border='1' cellborder='1'>
       <tr><td colspan="3">The foo, the bar and the baz</td></tr>
       <tr><td port='port_one'>First port</td><td port='port_two'>Second port</td><td port='port_three'>Third port</td></tr>
     </table>
  >];

  child_one [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>1</td></tr>
     </table>
  >];

  child_two [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>2</td></tr>
     </table>
  >];

  child_three [
   shape=plaintext
   label=<
     <table border='1' cellborder='0'>
       <tr><td>3</td></tr>
     </table>
  >];

  parent:port_one   -> child_one;
  parent:port_two   -> child_two;
  parent:port_three -> child_three;

}
|]

-- |
--
-- ![Example](other/ex15.svg)
ex15 :: ByteString
ex15 =
  [i|
digraph D {

  node [shape=plaintext fontname="Sans serif" fontsize="8"];

  task_menu [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 1</b></td></tr>
     <tr><td align="left">Choose Menu</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_ingredients [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 2</b></td></tr>
     <tr><td align="left">Buy ingredients</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_invitation [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 4</b></td></tr>
     <tr><td align="left">Send invitation</td></tr>
     <tr><td align="left"><font color="darkgreen">done</font></td></tr>
   </table>>];

  task_cook [ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 5</b></td></tr>
     <tr><td align="left">Cook</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];

  task_table[ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 3</b></td></tr>
     <tr><td align="left">Lay table</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];

  task_eat[ label=<
   <table border="1" cellborder="0" cellspacing="1">
     <tr><td align="left"><b>Task 6</b></td></tr>
     <tr><td align="left">Eat</td></tr>
     <tr><td align="left"><font color="red">todo</font></td></tr>
   </table>>];


  task_menu        -> task_ingredients;
  task_ingredients -> task_cook;
  task_invitation  -> task_cook;
  task_table       -> task_eat;
  task_cook        -> task_eat;

}
|]

-- | Test all the examples
testAll :: IO ()
testAll =
  zipWithM_
    (>>)
    ( putStrLn
        <$> [ "ex0",
              "ex1",
              "ex2",
              "ex3",
              "ex4",
              "ex5",
              "ex6",
              "ex7",
              "ex8",
              "ex9",
              "ex10",
              "ex11",
              "ex12",
              "ex13",
              "ex14",
              "ex15"
            ]
    )
    ( testDotParser (Proxy :: Proxy Graph) defaultDotConfig
        <$> [ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12, ex13, ex14, ex15]
    )

-- | Render all the examples
svgAll :: IO ()
svgAll =
  zipWithM_
    (>>)
    ( putStrLn
        <$> [ "ex0",
              "ex1",
              "ex2",
              "ex3",
              "ex4",
              "ex5",
              "ex6",
              "ex7",
              "ex8",
              "ex9",
              "ex10",
              "ex11",
              "ex12",
              "ex13",
              "ex14",
              "ex15"
            ]
    )
    ( zipWith
        (\b f -> processDotWith Directed ["-Tsvg", "-oother/" <> f <> ".svg"] b)
        [ex0, ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex11, ex12, ex13, ex14, ex15]
        [ "ex0",
          "ex1",
          "ex2",
          "ex3",
          "ex4",
          "ex5",
          "ex6",
          "ex7",
          "ex8",
          "ex9",
          "ex10",
          "ex11",
          "ex12",
          "ex13",
          "ex14",
          "ex15"
        ]
    )

-- | algebraic graph example
--
-- > exGraph = defaultGraph & addStatements (toStatements Directed (Char8.pack . show <$> exAGraph))
-- > exGraphAugmented <- processGraph exGraph
-- > writeChartOptions "other/exga.svg" (graphToChart exGraphAugmented)
--
-- ![augmentation example](other/exga.svg)
exAGraph :: G.Graph Int
exAGraph =
  G.edges $
    [(v, (v + 1) `mod` 6) | v <- [0 .. 5]]
      <> [(v, v + k) | v <- [0 .. 5], k <- [6, 12]]
      <> [(2, 18), (2, 19), (15, 18), (15, 19), (18, 3), (19, 3)]
