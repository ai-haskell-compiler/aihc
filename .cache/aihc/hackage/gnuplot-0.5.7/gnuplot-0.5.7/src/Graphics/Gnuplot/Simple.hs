{- |
This is a simple monolithic interface to gnuplot
that can be used as is in GHCi or Hugs.
We do not plan to support every feature of gnuplot here,
instead we provide an advanced modularized interface
in "Graphics.Gnuplot.Advanced".

Here is a pretty simple example:

> Graphics.Gnuplot.Simple> plotFunc [] (linearScale 1000 (-10,10::Double)) sin

This was formerly part of the htam package.
-}
module Graphics.Gnuplot.Simple (
    Attribute(..),
    Size(..),
    Aspect(..),

    LineAttr(..),
    LineSpec(..),
    PlotType(..),

    PlotStyle(..),

    linearScale,
    defaultStyle,

    terminal,

    plotList,
    plotListStyle,
    plotLists,
    plotListsStyle,
    plotFunc,
    plotFuncs,
    plotPath,
    plotPaths,
    plotPathStyle,
    plotPathsStyle,
    plotParamFunc,
    plotParamFuncs,
    plotDots,

    Plot3dType(..),
    CornersToColor(..),
    Attribute3d(..),
    plotMesh3d,
    plotFunc3d,

    epspdfPlot,
    inclPlot,
  ) where

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph2D as Graph2D
import qualified Graphics.Gnuplot.Private.Graph2DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.Plot as Plot

import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

{-
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
-}

import qualified Graphics.Gnuplot.Private.Terminal as Terminal

import Graphics.Gnuplot.Utility
   (quote, commaConcat, semiColonConcat, showTriplet, linearScale, )

import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.State as MS

import qualified Graphics.Gnuplot.Private.Command as Cmd
import System.Process (rawSystem, )
import Control.Functor.HT (void, )

import qualified Data.List.Reverse.StrictElement as ListRev
import Data.Foldable (foldMap, )
import Data.Maybe (listToMaybe, mapMaybe, isNothing, )


-- * User front-end

data Attribute =
     Custom String [String]  -- ^ anything that is allowed after gnuplot's @set@ command
   | EPS    FilePath
   | PNG    FilePath
   | Terminal Terminal.T     -- ^ you cannot use this, call 'terminal' instead
   | Grid   (Maybe [String])
   | Key    (Maybe [String])
   | Border (Maybe [String])
   | XTicks (Maybe [String])
   | YTicks (Maybe [String])
   | Size   (Size)
   | Aspect (Aspect)
   | BoxAspect (Aspect)
   | LineStyle Int [LineAttr]
   | Title  String
   | XLabel String
   | YLabel String
   | ZLabel String
   | XRange (Double, Double)
   | YRange (Double, Double)
   | ZRange (Double, Double)
   | Palette [(Double, (Double, Double, Double))]
   | ColorBox (Maybe [String])
   | XTime
   | XFormat String

data Size =
     Scale    Double
   | SepScale Double Double

data Aspect =
     Ratio Double
   | NoRatio

{- |
Be careful with 'LineTitle'
which can only be used as part of gnuplot's @plot@ command
but not as part of @set@.
That is,

> plotList [LineStyle 0 [LineTitle "foobar"]] [0,5..100::Double]

will leave you with an invalid gnuplot script, whereas

> plotListStyle [] (defaultStyle {lineSpec = CustomStyle [LineTitle "foobar"]}) [0,5..100::Double]

does what you want.

The 'Int' types would be better enumerations
but their interpretations depend on the gnuplot output type. :-(
-}
data LineAttr =
     LineType  Int
   | LineWidth Double
   | PointType Int
   | PointSize Double
   | LineTitle String

data LineSpec =
     DefaultStyle Int
   | CustomStyle  [LineAttr]

data PlotType =
     Lines
   | Points
   | LinesPoints
   | Impulses
   | Dots
   | Steps
   | FSteps
   | HiSteps
   | ErrorBars
   | XErrorBars
   | YErrorBars
   | XYErrorBars
   | ErrorLines
   | XErrorLines
   | YErrorLines
   | XYErrorLines
   | Boxes
   | FilledCurves
   | BoxErrorBars
   | BoxXYErrorBars
   | FinanceBars
   | CandleSticks
   | Vectors
   | PM3d

data PlotStyle = PlotStyle { plotType :: PlotType, lineSpec :: LineSpec }



defaultStyle :: PlotStyle
defaultStyle = PlotStyle Lines (CustomStyle [])


terminal :: Terminal.C term => term -> Attribute
terminal =
   Terminal . Terminal.canonical


-- * plot functions

list :: (Tuple.C a) => [a] -> Plot2D.T Double Double
list = Plot2D.list (GraphType.Cons "lines")
-- list = Plot2D.list GraphType.listLines

{- |
> plotList [] (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
-}
plotList ::
   (Tuple.C a) =>
   [Attribute] -> [a] -> IO ()
plotList attrs =
   plot2d attrs . list

{- |
> plotListStyle [] (defaultStyle{plotType = CandleSticks}) (Plot2D.functionToGraph (linearScale 32 (0,2*pi::Double)) (\t -> (-sin t, -2*sin t, 2*sin t, sin t)))
-}
plotListStyle ::
   (Tuple.C a) =>
   [Attribute] -> PlotStyle -> [a] -> IO ()
plotListStyle attrs style =
   plot2d attrs . setPlotStyle style . list

plotLists ::
   (Tuple.C a) =>
   [Attribute] -> [[a]] -> IO ()
plotLists attrs xss =
   plot2d attrs (foldMap list xss)

plotListsStyle ::
   (Tuple.C a) =>
   [Attribute] -> [(PlotStyle, [a])] -> IO ()
plotListsStyle attrs =
   plot2d attrs .
   foldMap (\(style,xs) -> setPlotStyle style $ list xs)

{- |
> plotFunc [] (linearScale 1000 (-10,10)) sin
-}
plotFunc ::
   (Atom.C a, Tuple.C a) =>
   [Attribute] -> [a] -> (a -> a) -> IO ()
plotFunc attrs args f =
   plot2d attrs (Plot2D.function GraphType.lines args f)

{- |
> plotFuncs [] (linearScale 1000 (-10,10)) [sin, cos]
-}
plotFuncs ::
   (Atom.C a, Tuple.C a) =>
   [Attribute] -> [a] -> [a -> a] -> IO ()
plotFuncs attrs args fs =
   plot2d attrs (Plot2D.functions GraphType.lines args fs)

plotPath ::
   (Tuple.C a) =>
   [Attribute] -> [(a,a)] -> IO ()
plotPath attrs =
   plot2d attrs . list

plotPaths ::
   (Tuple.C a) =>
   [Attribute] -> [[(a,a)]] -> IO ()
plotPaths attrs xss =
   plot2d attrs (foldMap list xss)

plotPathStyle ::
   (Tuple.C a) =>
   [Attribute] -> PlotStyle -> [(a,a)] -> IO ()
plotPathStyle attrs style =
   plot2d attrs . setPlotStyle style . list

plotPathsStyle ::
   (Tuple.C a) =>
   [Attribute] -> [(PlotStyle, [(a,a)])] -> IO ()
plotPathsStyle attrs =
   plot2d attrs .
   foldMap (\(style,xs) -> setPlotStyle style $ list xs)

{- |
> plotParamFunc [] (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
plotParamFunc ::
   (Atom.C a, Tuple.C a) =>
   [Attribute] -> [a] -> (a -> (a,a)) -> IO ()
plotParamFunc attrs args f =
   plot2d attrs (Plot2D.parameterFunction GraphType.lines args f)

{- |
> plotParamFuncs [] (linearScale 1000 (0,2*pi)) [\t -> (sin (2*t), cos t), \t -> (cos t, sin (2*t))]
-}
plotParamFuncs ::
   (Atom.C a, Tuple.C a) =>
   [Attribute] -> [a] -> [a -> (a,a)] -> IO ()
plotParamFuncs attrs args fs =
   plot2d attrs $
   foldMap (Plot2D.parameterFunction GraphType.lines args) fs


plotDots ::
   (Atom.C a, Tuple.C a) =>
   [Attribute] -> [(a,a)] -> IO ()
plotDots attrs xs =
   plot2d attrs (Plot2D.list GraphType.dots xs)



data Plot3dType =
     Surface
   | ColorMap

data CornersToColor =
     Mean
   | GeometricMean
   | Median
   | Corner1
   | Corner2
   | Corner3
   | Corner4

data Attribute3d =
     Plot3dType     Plot3dType
   | CornersToColor CornersToColor


{- |
> let xs = [-2,-1.8..2::Double] in plotMesh3d [] [] (do x <- xs; return (do y <- xs; return (x,y,cos(x*x+y*y))))

> let phis = linearScale 30 (-pi, pi :: Double) in plotMesh3d [] [] (do phi <- phis; return (do psi <- phis; let r = 5 + sin psi in return (r * cos phi, r * sin phi, cos psi)))
-}
plotMesh3d ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [Attribute] -> [Attribute3d] -> [[(x,y,z)]] -> IO ()
plotMesh3d attrs pt dat =
   plot3d attrs pt (Plot3D.mesh dat)

{- |
> let xs = [-2,-1.8..2::Double] in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
-}
plotFunc3d ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [Attribute] -> [Attribute3d] -> [x] -> [y] -> (x -> y -> z) -> IO ()
plotFunc3d attrs pt xArgs yArgs f =
   plot3d attrs pt (Plot3D.surface xArgs yArgs f)



-- * For inclusion of gnuplot graphics in LaTeX documents using lhs2TeX

{-| Redirects the output of a plotting function to an EPS file
    and additionally converts it to PDF. -}
epspdfPlot ::
      FilePath
   -> ([Attribute] -> IO ())  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO ()
epspdfPlot filename plot =
   do plot (EPS (filename++".eps") : Key Nothing : [])
      void $ rawSystem "epstopdf" [filename++".eps"]

{-| Creates an EPS and a PDF graphics
    and returns a string that can be inserted into a LaTeX document
    to include this graphic.

    Different from GHCi, Hugs doesn't output a return value from an IO monad.
    So you must wrap it with a 'putStr'.
    Nevertheless this implementation which returns the LaTeX command as string
    is the most flexible one. -}
inclPlot ::
      FilePath
   -> ([Attribute] -> IO ())  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO String
inclPlot filename plot =
   do epspdfPlot filename plot
      return ("\\includegraphics{"++filename++"}")



-- * Internal functions

attrToProg :: Attribute -> String
attrToProg (Custom attribute parameters) =
   "set " ++ attribute ++ " " ++ unwords parameters

attrToProg (Terminal term) =
   semiColonConcat $ Terminal.format term

attrToProg (EPS filename) =
   "set terminal postscript eps; " ++  -- latex
   "set output " ++ quote filename

attrToProg (PNG filename) =
   "set terminal png; " ++
   "set output " ++ quote filename

attrToProg (Grid   (Just x))     = "set grid " ++ unwords x
attrToProg (Grid   Nothing)      = "set nogrid"
attrToProg (Key    (Just x))     = "set key " ++ unwords x
attrToProg (Key    Nothing)      = "set nokey"
attrToProg (Border (Just x))     = "set border " ++ unwords x
attrToProg (Border Nothing)      = "set noborder"
attrToProg (XTicks (Just x))     = "set xtics " ++ unwords x
attrToProg (XTicks Nothing)      = "set noxtics"
attrToProg (YTicks (Just x))     = "set ytics " ++ unwords x
attrToProg (YTicks Nothing)      = "set noytics"
attrToProg (Size (Scale c))      = "set size " ++ show c
attrToProg (Size (SepScale x y)) = "set size " ++ show x ++ ", " ++ show y
attrToProg (Aspect (Ratio r))    = "set size ratio " ++ show (-r)
attrToProg (Aspect (NoRatio))    = "set size noratio"
attrToProg (BoxAspect (Ratio r)) = "set size ratio " ++ show r
attrToProg (BoxAspect (NoRatio)) = "set size noratio"
attrToProg (LineStyle num style) =
   "set linestyle " ++ show num ++ " " ++
   LineSpec.toString (lineAttrRecord style LineSpec.deflt)
attrToProg (Title  title_)       = "set title " ++ quote title_
attrToProg (XLabel label)        = "set xlabel " ++ quote label
attrToProg (YLabel label)        = "set ylabel " ++ quote label
attrToProg (ZLabel label)        = "set zlabel " ++ quote label
attrToProg (XRange _)            = ""  -- xrange is handled in plot command
attrToProg (YRange _)            = ""  -- yrange is handled in plot command
attrToProg (ZRange _)            = ""  -- zrange is handled in plot command
attrToProg (Palette colors) =
   "set palette defined (" ++
     commaConcat (map (\(idx,c) -> show idx ++ " " ++ showTriplet c) colors) ++ ")"
attrToProg (ColorBox (Just x))     = "set colorbox " ++ unwords x
attrToProg (ColorBox Nothing)      = "unset colorbox"
attrToProg XTime                   = "set xdata time; set timefmt \"%s\""
attrToProg (XFormat fmt)           = "set format x " ++ quote fmt

xRangeFromAttr, yRangeFromAttr, zRangeFromAttr ::
   Attribute -> Maybe (Double, Double)
xRangeFromAttr (XRange rng) = Just rng
xRangeFromAttr _            = Nothing

yRangeFromAttr (YRange rng) = Just rng
yRangeFromAttr _            = Nothing

zRangeFromAttr (ZRange rng) = Just rng
zRangeFromAttr _            = Nothing

extractRanges :: [Attribute] -> String
extractRanges attrs =
   let ranges = map (listToMaybe . flip mapMaybe attrs)
                    [xRangeFromAttr, yRangeFromAttr, zRangeFromAttr]
       showRng (l,r) = "[" ++ show l ++ ":" ++ show r ++ "]"
   in  unwords $ map (maybe "[:]" showRng) (ListRev.dropWhile isNothing ranges)

interactiveTerm :: [Attribute] -> Bool
interactiveTerm =
   all $ \attr ->
      case attr of
         Terminal term -> Terminal.interactive term
         PNG _ -> False
         EPS _ -> False
         _ -> True



plotTypeToGraph :: PlotType -> Graph2D.Type -- GraphType.T
plotTypeToGraph t =
   case t of
      Lines          -> "lines"
      Points         -> "points"
      LinesPoints    -> "linespoints"
      Impulses       -> "impulses"
      Dots           -> "dots"
      Steps          -> "steps"
      FSteps         -> "fsteps"
      HiSteps        -> "histeps"
      ErrorBars      -> "errorbars"
      XErrorBars     -> "xerrorbars"
      YErrorBars     -> "yerrorbars"
      XYErrorBars    -> "xyerrorbars"
      ErrorLines     -> "errorlines"
      XErrorLines    -> "xerrorlines"
      YErrorLines    -> "yerrorlines"
      XYErrorLines   -> "xyerrorlines"
      Boxes          -> "boxes"
      FilledCurves   -> "filledcurves"
      BoxErrorBars   -> "boxerrorbars"
      BoxXYErrorBars -> "boxxyerrorbars"
      FinanceBars    -> "financebars"
      CandleSticks   -> "candlesticks"
      Vectors        -> "vectors"
      PM3d           -> "pm3d"


plot3dTypeToString :: Plot3dType -> String
plot3dTypeToString Surface  = ""
plot3dTypeToString ColorMap = "map"

cornersToColorToString :: CornersToColor -> String
cornersToColorToString Mean          = "mean"
cornersToColorToString GeometricMean = "geomean"
cornersToColorToString Median        = "median"
cornersToColorToString Corner1       = "c1"
cornersToColorToString Corner2       = "c2"
cornersToColorToString Corner3       = "c3"
cornersToColorToString Corner4       = "c4"

attribute3dToString :: Attribute3d -> String
attribute3dToString (Plot3dType     pt)  = plot3dTypeToString pt
attribute3dToString (CornersToColor c2c) =
   "corners2color " ++cornersToColorToString c2c



plot2d ::
   (Atom.C x, Atom.C y) =>
   [Attribute] -> Plot2D.T x y -> IO ()
plot2d attrs plt =
   runGnuplot attrs "plot" plt

setPlotStyle :: PlotStyle -> Plot2D.T x y -> Plot2D.T x y
setPlotStyle ps =
   fmap (Graph2D.typ (plotTypeToGraph $ plotType ps) .
         Graph2D.lineSpec (lineSpecRecord $ lineSpec ps))


plot3d ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   [Attribute] -> [Attribute3d] -> Plot3D.T x y z -> IO ()
plot3d attrs pt plt =
   runGnuplot
      (Custom "pm3d" (map attribute3dToString pt) : attrs) "splot" plt


lineSpecRecord :: LineSpec -> LineSpec.T
lineSpecRecord (DefaultStyle n) =
   LineSpec.lineStyle n LineSpec.deflt
lineSpecRecord (CustomStyle ls) =
   lineAttrRecord ls LineSpec.deflt

lineAttrRecord :: [LineAttr] -> LineSpec.T -> LineSpec.T
lineAttrRecord =
   flip $ foldl (flip $ \attr ->
      case attr of
         LineType  n -> LineSpec.lineType  n
         LineWidth w -> LineSpec.lineWidth w
         PointType n -> LineSpec.pointType n
         PointSize s -> LineSpec.pointSize s
         LineTitle s -> LineSpec.title     s
      )

runGnuplot ::
   Graph.C graph =>
   [Attribute] -> String -> Plot.T graph -> IO ()
runGnuplot attrs cmd (Plot.Cons mp) =
   void $ Cmd.asyncIfInteractive (interactiveTerm attrs) $ Cmd.run $ \dir ->
      let files = MR.runReader (MS.evalStateT mp 0) dir
      in  (map attrToProg attrs ++
           [cmd ++ " " ++
            extractRanges attrs ++ " " ++
            commaConcat (plotFileStatements files)],
           files)

plotFileStatements ::
   Graph.C graph => [Plot.File graph] -> [String]
plotFileStatements =
   concatMap
      (\(Plot.File filename _ grs) ->
         map (\gr -> quote filename ++ " " ++ Graph.toString gr) grs)
