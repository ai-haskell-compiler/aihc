module Graphics.Gnuplot.Private.Graph2DType where

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import Graphics.Gnuplot.Value.Tuple (Label)
import Prelude hiding (lines, )


{- |
The type parameter @x@ is for the values on the X axis,
@y@ of the Y axis and
@a@ is the type of the plotted data.
The type @a@ is a pair in case of points in the plane,
or a more complex tuple in case of error plots and the like.
-}
newtype T x y a = Cons String

tupleSize :: (Tuple.C a) => T x y a -> Tuple.ColumnCount a
tupleSize _ = Tuple.columnCount


{-
See info:/gnuplot/set_style_rectangle
-}

listLines       :: (Atom.C y) => T Int y y
listPoints      :: (Atom.C y) => T Int y y
listLinesPoints :: (Atom.C y) => T Int y y
listImpulses    :: (Atom.C y) => T Int y y
listDots        :: (Atom.C y) => T Int y y
histograms      :: (Atom.C y) => T Int y y

xErrorBarsRelative   :: (Atom.C x, Atom.C y) => T x y ((x,y),x)
yErrorBarsRelative   :: (Atom.C x, Atom.C y) => T x y ((x,y),y)
xyErrorBarsRelative  :: (Atom.C x, Atom.C y) => T x y ((x,y),(x,y))
xErrorBarsAbsolute   :: (Atom.C x, Atom.C y) => T x y ((x,y),(x,x))
yErrorBarsAbsolute   :: (Atom.C x, Atom.C y) => T x y ((x,y),(y,y))
xyErrorBarsAbsolute  :: (Atom.C x, Atom.C y) => T x y ((x,y),((x,x),(y,y)))

xErrorLinesRelative  :: (Atom.C x, Atom.C y) => T x y ((x,y),x)
yErrorLinesRelative  :: (Atom.C x, Atom.C y) => T x y ((x,y),y)
xyErrorLinesRelative :: (Atom.C x, Atom.C y) => T x y ((x,y),(x,y))
xErrorLinesAbsolute  :: (Atom.C x, Atom.C y) => T x y ((x,y),(x,x))
yErrorLinesAbsolute  :: (Atom.C x, Atom.C y) => T x y ((x,y),(y,y))
xyErrorLinesAbsolute :: (Atom.C x, Atom.C y) => T x y ((x,y),((x,x),(y,y)))

lines          :: (Atom.C x, Atom.C y) => T x y (x,y)
points         :: (Atom.C x, Atom.C y) => T x y (x,y)
linesPoints    :: (Atom.C x, Atom.C y) => T x y (x,y)
impulses       :: (Atom.C x, Atom.C y) => T x y (x,y)
dots           :: (Atom.C x, Atom.C y) => T x y (x,y)
steps          :: (Atom.C x, Atom.C y) => T x y (x,y)
fSteps         :: (Atom.C x, Atom.C y) => T x y (x,y)
hiSteps        :: (Atom.C x, Atom.C y) => T x y (x,y)
errorBars      :: (Atom.C x, Atom.C y) => T x y (x,y)
errorLines     :: (Atom.C x, Atom.C y) => T x y (x,y)
boxes          :: (Atom.C x, Atom.C y) => T x y (x,y)
filledCurves   :: (Atom.C x, Atom.C y) => T x y (x,y)
{-
boxErrorBars   :: (Atom.C x, Atom.C y) => T x y (x,y)
boxXYErrorBars :: (Atom.C x, Atom.C y) => T x y (x,y)
-}
financeBars    :: (Atom.C x, Atom.C y) => T x y (x,(y,y,y,y))
candleSticks   :: (Atom.C x, Atom.C y) => T x y (x,(y,y,y,y))
vectors        :: (Atom.C x, Atom.C y) => T x y ((x,y),(x,y))
image          :: (Atom.C x, Atom.C y, Atom.C z) => T x y ((x, y), z)

filledStripe      :: (Atom.C x, Atom.C y) => T x y (x,(y,y))
filledStripeAbove :: (Atom.C x, Atom.C y) => T x y (x,(y,y))
filledStripeBelow :: (Atom.C x, Atom.C y) => T x y (x,(y,y))

labels          :: (Atom.C x, Atom.C y) => T x y ((x,y),Label)


listLines       = Cons "lines"
listPoints      = Cons "points"
listLinesPoints = Cons "linespoints"
listImpulses    = Cons "impulses"
listDots        = Cons "dots"
histograms      = Cons "histograms"

xErrorBarsRelative   = Cons "xerrorbars"
yErrorBarsRelative   = Cons "yerrorbars"
xyErrorBarsRelative  = Cons "xyerrorbars"
xErrorBarsAbsolute   = Cons "xerrorbars"
yErrorBarsAbsolute   = Cons "yerrorbars"
xyErrorBarsAbsolute  = Cons "xyerrorbars"

xErrorLinesRelative  = Cons "xerrorlines"
yErrorLinesRelative  = Cons "yerrorlines"
xyErrorLinesRelative = Cons "xyerrorlines"
xErrorLinesAbsolute  = Cons "xerrorlines"
yErrorLinesAbsolute  = Cons "yerrorlines"
xyErrorLinesAbsolute = Cons "xyerrorlines"

lines          = Cons "lines"
points         = Cons "points"
linesPoints    = Cons "linespoints"
impulses       = Cons "impulses"
dots           = Cons "dots"
steps          = Cons "steps"
fSteps         = Cons "fsteps"
hiSteps        = Cons "histeps"
errorBars      = Cons "errorbars"
errorLines     = Cons "errorlines"
boxes          = Cons "boxes"
filledCurves   = Cons "filledcurves"
{-
boxErrorBars   = Cons "boxerrorbars"
boxXYErrorBars = Cons "boxxyerrorbars"
-}
financeBars    = Cons "financebars"
candleSticks   = Cons "candlesticks"
vectors        = Cons "vectors"
image          = Cons "image"

filledStripe      = Cons "filledcurves"
filledStripeAbove = Cons "filledcurves above"
filledStripeBelow = Cons "filledcurves below"

labels       = Cons "labels"


toString :: T x y a -> String
toString (Cons t) = t
