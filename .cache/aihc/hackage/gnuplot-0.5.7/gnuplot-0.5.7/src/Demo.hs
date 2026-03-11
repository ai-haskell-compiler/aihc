module Main where

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import Graphics.Gnuplot.Value.Tuple (Label(Label))

import qualified Data.Time as Time

import qualified Paths_gnuplot as Path
import System.FilePath ((</>), )

import Control.Monad (liftM2, )
import Data.Array (listArray, )
import Data.Foldable (foldMap, )
import Data.Monoid (mappend, mconcat, )
import Data.Tuple.HT (mapFst)


simple2d :: Plot2D.T Double Double
simple2d =
   Plot2D.function Graph2D.lines
      (linearScale 100 (-10,10)) sin

circle2d :: Plot2D.T Double Double
circle2d =
   Plot2D.parameterFunction Graph2D.lines
      (linearScale 24 (-pi,pi))
      (\t -> (cos t, sin t))

list2d :: Plot2D.T Int Integer
list2d =
   Plot2D.list Graph2D.listPoints [0,1,1,2,3,5,8,13]

labels2d :: Plot2D.T Double Double
labels2d =
   Plot2D.list Graph2D.labels $
   flip map [1..12] $
   \t -> (let w = fromInteger t*2*pi/12 in (sin w, cos w), Label $ show t)

candle2d :: Plot2D.T Time.Day Double
candle2d =
   Plot2D.list Graph2D.candleSticks $
   zip [(Time.fromGregorian 2008 01 01) ..] $
   flip map (linearScale 32 (0,2*pi)) $
   \t -> (-sin t, -2*sin t, 2*sin t, sin t)

image2d :: Plot2D.T Double Double
image2d =
   case linearScale 300 (-20,20) of
      xs ->
         Plot2D.function Graph2D.image (liftM2 (,) xs xs) $
            \(x,y) -> cos (sqrt (x*x+x*y+y*y))

overlay2d :: Frame.T (Graph2D.T Double Double)
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ defltOpts) $
   Plot2D.function Graph2D.lines (linearScale 100 (-pi,pi)) cos
   `mappend`
   circle2d

histogram2d :: Frame.T (Graph2D.T Int Double)
histogram2d =
   Frame.cons (
      Opts.title "Comparison of how well software revisions perform on each hardware version" $
      Histogram.clusteredGap 2 $
      Opts.boxwidthAbsolute 0.9 $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.keyOutside $
      Opts.xTicks2d
         [("220", 0), ("320", 1), ("420", 2), ("520", 3), ("620", 4), ("720", 5)] $
      Opts.deflt) $
   foldMap (\(title,dat) ->
      fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms dat) $
   ("1.0011", [102, 213, 378, 408, 840,  920]) :
   ("1.0012", [152, 233, 388, 433, 850, 1200]) :
   ("1.0013", [172, 263, 408, 463, 860, 1500]) :
   ("1.0014", [192, 293, 458, 483, 890, 2000]) :
   []

names2d :: Frame.T (Graph2D.T Double Double)
names2d =
   let domain = (-10,10)
       frameOpts =
          Opts.xRange2d domain $
          Opts.yRange2d (-1.5,1.5) $
          Opts.deflt
   in  Frame.cons frameOpts $
       Plot2D.functionsWithLineSpec Graph2D.lines (linearScale 100 domain) $
       map (mapFst (flip LineSpec.title LineSpec.deflt)) $
       ("sin", sin) :
       ("cos", cos) :
       ("tan", tan) :
       []

filled2d :: Frame.T (Graph2D.T Double Double)
filled2d =
   let scale = linearScale 100 (-10,10)
       wave style (lower,upper) =
          Plot2D.function style scale $
          (\y -> (lower*y, upper*y)) . sin
   in  Frame.cons (Opts.key False Opts.deflt) $ mconcat $
       wave Graph2D.filledStripeAbove (1.0, 1.3) :
       wave Graph2D.filledStripeBelow (1.0, 1.3) :
       wave Graph2D.filledStripe      (0.7, 1.0) :
       []

file2d :: FilePath -> FilePath -> Frame.T (Graph2D.T Int Double)
file2d path file =
   let lineSpec =
          Graph2D.lineSpec $
          LineSpec.title "runtimes" $
          LineSpec.lineWidth 2.5 $
          LineSpec.deflt
       frameOpts =
          Opts.xLabel "input size" $
          Opts.yLabel "runtime (ms)" $
          Opts.title ("Graph from " ++ file) $
          Opts.deflt
   in  Frame.cons frameOpts $ fmap lineSpec $
       Plot2D.pathFromFile Graph2D.lines (path </> file) 1 2

mixed2d :: MultiPlot.T
mixed2d =
   MultiPlot.simpleFromPartArray $
   listArray ((0,0), (0,2)::(Int,Int)) $
   MultiPlot.partFromPlot circle2d :
   MultiPlot.partFromFrame
      (Frame.cons (Opts.sizeSquare $ Opts.xFormat "%m-%d" $ defltOpts) $
       candle2d) :
   MultiPlot.partFromPlot list2d :
   []

{-
The size settings of overlay2d interfer badly with the other plots,
because 'unset' does not restore the size to the multiplot settings.
-}
size2d :: MultiPlot.T
size2d =
   MultiPlot.simpleFromPartArray $
   listArray ((0,0), (0,2)::(Int,Int)) $
   MultiPlot.partFromPlot candle2d :
   MultiPlot.partFromFrame
      (Frame.cons (Opts.yLabel "Fibonacci" $ defltOpts) list2d) :
   MultiPlot.partFromPlot candle2d :
   []

defltOpts :: Graph.C graph => Opts.T graph
defltOpts = Opts.key False $ Opts.deflt

wave3d :: Frame.T (Graph3D.T Double Double Double)
wave3d =
   let meshNodes = linearScale 20 (-2,2)
   in  Frame.cons
          (Opts.grid True $
           Opts.view 42 17 1 1 $
           Opts.xRange3d (-2.5,2.5) $
           Opts.yRange3d (-2.5,2.5) $
           defltOpts) $
       Plot3D.surface meshNodes meshNodes (\x y -> cos(x*x+y*y))

lissajous3d :: Frame.T (Graph3D.T Double Double Double)
lissajous3d =
   let t = linearScale 300 (-pi, pi)
       f n = map (sin . (*) n) t
   in  Frame.cons defltOpts $
       Plot3D.cloud Graph3D.lines $
       zip3 (f 3) (f 4) (f 9)

multiplot :: MultiPlot.T
multiplot =
   let (prefix,suffix) =
          splitAt 7 $
          map MultiPlot.partFromFrame $
          map (\k ->
             Frame.cons (Opts.xRange2d (-1,1) defltOpts) $
             Plot2D.parameterFunction Graph2D.lines
                (linearScale 48 (-pi,pi::Double))
                (\t -> (cos (t + pi/7*fromInteger k), sin (2*t)))) $
          [0..13]
--       center = MultiPlot.partFromFrame Frame.empty
       center = MultiPlot.partFromFrame wave3d
   in  MultiPlot.title "multiplot of 2d and 3d graphics" $
       MultiPlot.simpleFromPartArray $
       listArray ((0,0), (2,4)::(Int,Int)) $
       prefix ++ center : suffix


main :: IO ()
main = sequence_ $
   GP.plotDefault simple2d :
   GP.plotDefault list2d :
   GP.plotDefault labels2d :
   GP.plotDefault candle2d :
   GP.plotDefault image2d :
   GP.plotDefault histogram2d :
   GP.plotDefault names2d :
   GP.plotDefault filled2d :
   GP.plotDefault overlay2d :
   (GP.plotDefault . flip file2d "runtime.data"
      =<< fmap (</> "data") Path.getDataDir) :
   GP.plotDefault mixed2d :
   GP.plotDefault size2d :
   GP.plotDefault wave3d :
   GP.plotDefault lissajous3d :
   GP.plotDefault multiplot :
   []
