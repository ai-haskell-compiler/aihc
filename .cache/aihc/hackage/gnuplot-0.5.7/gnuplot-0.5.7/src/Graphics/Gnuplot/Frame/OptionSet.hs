module Graphics.Gnuplot.Frame.OptionSet (
   OptionSet.T,
   deflt,

   OptionSet.add,
   OptionSet.remove,
   OptionSet.boolean,
   OptionSet.addBool,

   size,
   sizeRatio,
   sizeSquare,
   title,
   key,
   keyInside,
   keyOutside,
   xRange2d,
   yRange2d,
   xRange3d,
   yRange3d,
   zRange3d,
   xLabel,
   yLabel,
   zLabel,
   xTicks2d,
   yTicks2d,
   xTicks3d,
   yTicks3d,
   zTicks3d,
   xLogScale,
   yLogScale,
   zLogScale,
   grid,
   gridXTicks,
   gridYTicks,
   gridZTicks,
   xFormat,
   yFormat,
   zFormat,

   view,
   viewMap,

   boxwidthRelative,
   boxwidthAbsolute,
   ) where


import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.Graph as Graph

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import Graphics.Gnuplot.Private.FrameOptionSet (T, )

import Graphics.Gnuplot.Utility (quote, commaConcat, )

import qualified Data.List as List


deflt :: Graph.C graph => T graph
deflt = Graph.defltOptions


size :: Graph.C graph => Double -> Double -> T graph -> T graph
size x y =
   OptionSet.add Option.sizeScale [show x ++ ", " ++ show y]

sizeRatio :: Graph.C graph => Double -> T graph -> T graph
sizeRatio r =
   OptionSet.add Option.sizeRatio ["ratio", show r]

sizeSquare :: Graph.C graph => T graph -> T graph
sizeSquare =
   OptionSet.add Option.sizeRatio ["square"]

title :: Graph.C graph => String -> T graph -> T graph
title text =
   OptionSet.add Option.title [quote text]

key :: Graph.C graph => Bool -> T graph -> T graph
key = OptionSet.boolean Option.keyShow

keyInside :: Graph.C graph => T graph -> T graph
keyInside = OptionSet.add Option.keyPosition ["inside"]

keyOutside :: Graph.C graph => T graph -> T graph
keyOutside = OptionSet.add Option.keyPosition ["outside"]

{-
xRange :: Graph.C graph => (Double, Double) -> T graph -> T graph
xRange = range Option.xRange

yRange :: Graph.C graph => (Double, Double) -> T graph -> T graph
yRange = range Option.yRange

zRange :: (Double, Double) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zRange = range Option.zRange
-}

xRange2d ::
   (Atom.C x, Atom.C y, Tuple.C x) =>
   (x, x) -> T (Graph2D.T x y) -> T (Graph2D.T x y)
xRange2d = range Option.xRangeBounds

yRange2d ::
   (Atom.C x, Atom.C y, Tuple.C y) =>
   (y, y) -> T (Graph2D.T x y) -> T (Graph2D.T x y)
yRange2d = range Option.yRangeBounds


xRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C x) =>
   (x, x) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
xRange3d = range Option.xRangeBounds

yRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C y) =>
   (y, y) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
yRange3d = range Option.yRangeBounds

zRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C z) =>
   (z, z) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zRange3d = range Option.zRangeBounds


range ::
   (Atom.C a, Tuple.C a, Graph.C graph) =>
   Option.T -> (a, a) -> T graph -> T graph
range opt (x,y) =
   OptionSet.add opt
      [showString "[" . atomText x .
       showString ":" . atomText y $
       "]"]

atomText ::
   (Atom.C a, Tuple.C a) =>
   a -> ShowS
atomText x =
   case Tuple.text x of
      [s] -> s
      _ -> error "OptionSet.fromSingleton: types of Atom class must generate single representation texts"


xLabel :: Graph.C graph => String -> T graph -> T graph
xLabel = label Option.xLabelText

yLabel :: Graph.C graph => String -> T graph -> T graph
yLabel = label Option.yLabelText

zLabel ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   String -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zLabel = label Option.zLabelText

label :: Graph.C graph => Option.T -> String -> T graph -> T graph
label opt x =
   OptionSet.add opt [quote x]


xFormat :: Graph.C graph => String -> T graph -> T graph
xFormat = format Option.xFormat

yFormat :: Graph.C graph => String -> T graph -> T graph
yFormat = format Option.yFormat

zFormat ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   String -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zFormat = format Option.zFormat

format :: Graph.C graph => Option.T -> String -> T graph -> T graph
format opt x =
   OptionSet.add opt [quote x]


{- |
Set parameters of viewing a surface graph.
See <info:gnuplot/view>
-}
view ::
   Double {- ^ rotateX -} ->
   Double {- ^ rotateZ -} ->
   Double {- ^ scale -} ->
   Double {- ^ scaleZ -} ->
   T (Graph3D.T x y z) -> T (Graph3D.T x y z)
view rotateX rotateZ scale scaleZ =
   OptionSet.add Option.view
      [commaConcat [show rotateX, show rotateZ, show scale, show scaleZ]]

{- |
Show flat pixel map.
-}
viewMap :: T (Graph3D.T x y z) -> T (Graph3D.T x y z)
viewMap =
   OptionSet.add Option.view ["map"]


xTicks2d ::
   (Atom.C x, Atom.C y, Tuple.C x) =>
   [(String, x)] -> T (Graph2D.T x y) -> T (Graph2D.T x y)
xTicks2d = ticks Option.xTickLabels

yTicks2d ::
   (Atom.C x, Atom.C y, Tuple.C y) =>
   [(String, y)] -> T (Graph2D.T x y) -> T (Graph2D.T x y)
yTicks2d = ticks Option.yTickLabels


xTicks3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C x) =>
   [(String, x)] -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
xTicks3d = ticks Option.xTickLabels

yTicks3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C y) =>
   [(String, y)] -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
yTicks3d = ticks Option.yTickLabels

zTicks3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C z) =>
   [(String, z)] -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zTicks3d = ticks Option.zTickLabels


ticks ::
   (Atom.C a, Tuple.C a, Graph.C graph) =>
   Option.T -> [(String, a)] -> T graph -> T graph
ticks opt labels =
   OptionSet.add opt
      [('(' :) $ foldr ($) ")" $
       List.intersperse (showString ", ") $
       map
          (\(lab,pos) ->
             showString (quote lab) .
             showString " " .
             atomText pos)
          labels]


xLogScale :: Graph.C graph => T graph -> T graph
xLogScale = OptionSet.add Option.xLogScale []

yLogScale :: Graph.C graph => T graph -> T graph
yLogScale = OptionSet.add Option.yLogScale []

zLogScale ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zLogScale = OptionSet.add Option.zLogScale []


grid :: Graph.C graph => Bool -> T graph -> T graph
grid b =
   OptionSet.addBool Option.gridXTicks b .
   OptionSet.addBool Option.gridYTicks b .
   OptionSet.addBool Option.gridZTicks b

gridXTicks :: Graph.C graph => Bool -> T graph -> T graph
gridXTicks = OptionSet.addBool Option.gridXTicks

gridYTicks :: Graph.C graph => Bool -> T graph -> T graph
gridYTicks = OptionSet.addBool Option.gridYTicks

gridZTicks ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   Bool -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
gridZTicks = OptionSet.addBool Option.gridZTicks


boxwidthRelative ::
   (Graph.C graph) =>
   Double -> T graph -> T graph
boxwidthRelative width =
   OptionSet.add Option.boxwidth [show width, "relative"]

boxwidthAbsolute ::
   (Graph.C graph) =>
   Double -> T graph -> T graph
boxwidthAbsolute width =
   OptionSet.add Option.boxwidth [show width, "absolute"]

{-
cornerToColor :: CornersToColor -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)

type3d :: Plot3dType -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
-}
