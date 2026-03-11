{- |
If you add a new option,
please mind to add a default value to FrameOptionSet.deflt, too.
-}
module Graphics.Gnuplot.Private.FrameOption where

{- |
Every option represents an internal state in gnuplot.
It is altered with gnuplot's set command.
The first field in 'T' is the name of the option
and the name of the according internal state in gnuplot.

Sometimes the addressed state is not explicitly mentioned
but is expressed by the syntax of the values.
E.g. you can write @set grid xtics@ and @set grid noxtics@,
but both commands refer to the same internal boolean variable,
that we like to call @xtics@.
It is important that the gnuplot Haskell bindings know
that these two set commands refer to the same gnuplot state,
since we want to simulate a stateless functional interface
in front of a stateful imperative one.

In case of a such a hidden state,
we manage an identifier in the second field of 'T'.
It is mainly used for distinguishing different hidden states,
that are accessed by the same @set@ variable.
This second field may not contain valid gnuplot identifiers,
however you might use the field for formatting boolean options
using 'Graphics.Gnuplot.Frame.OptionSet.addBool'.
-}
data T = Cons String String
   deriving (Eq, Ord, Show)

{- |
Constructs a generic option from Strings for the first and second field of 'T'.

This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
-}
custom :: String -> String -> T
custom = Cons

grid   :: String -> T; grid   = Cons "grid"
size   :: String -> T; size   = Cons "size"
key    :: String -> T; key    = Cons "key"
border :: String -> T; border = Cons "border"
pm3d   :: String -> T; pm3d   = Cons "pm3d"

xRange :: String -> T; xRange = Cons "xrange"
yRange :: String -> T; yRange = Cons "yrange"
zRange :: String -> T; zRange = Cons "zrange"

xLabel :: String -> T; xLabel = Cons "xlabel"
yLabel :: String -> T; yLabel = Cons "ylabel"
zLabel :: String -> T; zLabel = Cons "zlabel"

xTicks :: String -> T; xTicks = Cons "xtics"
yTicks :: String -> T; yTicks = Cons "ytics"
zTicks :: String -> T; zTicks = Cons "ztics"

xLogScale :: T; xLogScale = Cons "logscale x" ""
yLogScale :: T; yLogScale = Cons "logscale y" ""
zLogScale :: T; zLogScale = Cons "logscale z" ""


title :: T; title = Cons "title" ""
view  :: T; view  = Cons "view" ""

xFormat :: T; xFormat = Cons "format x" ""
yFormat :: T; yFormat = Cons "format y" ""
zFormat :: T; zFormat = Cons "format z" ""

timeFmt :: T; timeFmt = Cons "timefmt" ""

xData :: T; xData = Cons "xdata" ""
yData :: T; yData = Cons "ydata" ""
zData :: T; zData = Cons "zdata" ""


sizeScale :: T; sizeScale = size "scale"
sizeRatio :: T; sizeRatio = size "ratio"
keyShow     :: T; keyShow     = key "show"
keyPosition :: T; keyPosition = key "position"

xRangeBounds :: T; xRangeBounds = xRange "bounds"
yRangeBounds :: T; yRangeBounds = yRange "bounds"
zRangeBounds :: T; zRangeBounds = zRange "bounds"

xLabelText :: T; xLabelText = xLabel "text"
yLabelText :: T; yLabelText = yLabel "text"
zLabelText :: T; zLabelText = zLabel "text"

xTickLabels :: T; xTickLabels = xTicks "labels"
yTickLabels :: T; yTickLabels = yTicks "labels"
zTickLabels :: T; zTickLabels = zTicks "labels"

gridXTicks :: T; gridXTicks = grid "xtics"
gridYTicks :: T; gridYTicks = grid "ytics"
gridZTicks :: T; gridZTicks = grid "ztics"

boxwidth :: T; boxwidth = Cons "boxwidth" ""

styleFillSolid  :: T; styleFillSolid  = Cons "style fill" "solid"
styleFillBorder :: T; styleFillBorder = Cons "style fill" "border"
styleHistogram  :: T; styleHistogram  = Cons "style histogram" ""
