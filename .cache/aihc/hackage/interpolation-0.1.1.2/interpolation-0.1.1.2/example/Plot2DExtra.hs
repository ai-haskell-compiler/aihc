module Plot2DExtra where

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import Data.Tuple.HT (mapFst)


functions ::
   (Atom.C x, Atom.C y, Tuple.C x, Tuple.C y) =>
   [x] -> [(String, x -> y)] -> Plot2D.T x y
functions scale =
   Plot2D.functionsWithLineSpec Graph2D.lines scale .
   map (mapFst (flip LineSpec.title LineSpec.deflt))
