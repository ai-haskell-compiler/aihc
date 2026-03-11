module Graphics.Gnuplot.Frame.OptionSet.Histogram (
   clustered,
   clusteredGap,
   errorbars,
   errorbarsGap,
   errorbarsGapLineWidth,
   rowstacked,
   columnstacked,
   ) where

import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option

import qualified Graphics.Gnuplot.Value.Atom as Atom

import Graphics.Gnuplot.Private.FrameOptionSet (T, )


clustered ::
   (Atom.C x, Atom.C y) =>
   T (Graph2D.T x y) -> T (Graph2D.T x y)
clustered =
   OptionSet.add Option.styleHistogram ["clustered"]

clusteredGap ::
   (Atom.C x, Atom.C y) =>
   Double -> T (Graph2D.T x y) -> T (Graph2D.T x y)
clusteredGap gapSize =
   OptionSet.add Option.styleHistogram ["clustered", "gap", show gapSize]

errorbars ::
   (Atom.C x, Atom.C y) =>
   T (Graph2D.T x y) -> T (Graph2D.T x y)
errorbars =
   OptionSet.add Option.styleHistogram ["errorbars"]

errorbarsGap ::
   (Atom.C x, Atom.C y) =>
   Double -> T (Graph2D.T x y) -> T (Graph2D.T x y)
errorbarsGap gapSize =
   OptionSet.add Option.styleHistogram ["errorbars", "gap", show gapSize]

errorbarsGapLineWidth ::
   (Atom.C x, Atom.C y) =>
   Double -> Double -> T (Graph2D.T x y) -> T (Graph2D.T x y)
errorbarsGapLineWidth gapSize width =
   OptionSet.add Option.styleHistogram ["errorbars", "gap", show gapSize, show width]

rowstacked ::
   (Atom.C x, Atom.C y) =>
   T (Graph2D.T x y) -> T (Graph2D.T x y)
rowstacked =
   OptionSet.add Option.styleHistogram ["rowstacked"]

columnstacked ::
   (Atom.C x, Atom.C y) =>
   T (Graph2D.T x y) -> T (Graph2D.T x y)
columnstacked =
   OptionSet.add Option.styleHistogram ["columnstacked"]
