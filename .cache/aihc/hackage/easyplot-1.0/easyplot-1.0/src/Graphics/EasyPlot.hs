{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, IncoherentInstances #-}

-- | A simple wrapper to the gnuplot command line utility.
--
-- Typically you will invoke a plot like so:
--
-- > plot X11 $ Data2D [Title "Sample Data"] [] [(1, 2), (2, 4), ...]
--
-- To plot a function, use the following:
--
-- > plot X11 $ Function2D [Title "Sine and Cosine"] [] (\x -> sin x * cos x)
--
-- There is also a shortcut available - the following plots the sine function:
--
-- > plot X11 sin
--
-- Output can go into a file, too (See 'TerminalType'):
--
-- > plot (PNG "plot.png") (sin . cos)
--
-- Haskell functions are plotted via a set of tuples obtained form the function.
-- If you want to make use of gnuplots mighty function plotting functions you can
-- pass a 'Gnuplot2D' or 'Gnuplot3D' object to plot.
--
-- > plot X11 $ Gnuplot2D [Color Blue] [] "2**cos(x)"
--
-- For 3D-Plots there is a shortcut available by directly passing a String:
--
-- > plot X11 "x*y"
--
-- Multiple graphs can be shown simply by passing a list of these:
--
-- > plot X11 [ Data2D [Title "Graph 1", Color Red] [] [(x, x ** 3) | x <- [-4,-3.9..4]]
-- >          , Function2D [Title "Function 2", Color Blue] [] (\x -> negate $ x ** 2) ]
--
-- For 3D Graphs it is useful to be able to interact with the graph (See 'plot'' and 'GnuplotOption'):
--
-- > plot' [Interactive] X11 $ Gnuplot3D [Color Magenta] [] "x ** 2 + y ** 3"
--
-- If you want to know the command that SimplePlot uses to plot your graph,
-- turn on debugging:
--
-- > plot' [Debug] X11 $ Gnuplot3D [Color Magenta] [] "x ** 4 + y ** 3"
-- > > set term x11 persist; splot x ** 4 + y ** 3 lc rgb "magenta"
module Graphics.EasyPlot (

    -- * Plotting
    Plot (plot, plot'),

    -- * Graphs for 2D and 3D plots
    Graph2D (..), Graph3D (..),

    -- * Configuration and other options
    TerminalType (..),
    Color (..), Style (..), -- Style2D (..),
    Option (..), Option2D (..), Option3D (..),
    GnuplotOption (..)

    ) where

import Numeric (showHex)
import Data.Char (toUpper)
import Data.List (sortBy, nubBy)
import System.Cmd (rawSystem)
import System.Exit (ExitCode (ExitSuccess))

-- | TerminalType determines where the output of gnuplot should go.
data TerminalType = Aqua    -- ^ Output on Mac OS X (Aqua Terminal).
                  | Windows -- ^ Output for MS Windows.
                  | X11     -- ^ Output to the X Window System.
                  | PS FilePath -- ^ Output into a Postscript file.
                  | EPS FilePath -- ^ Output into an EPS file.
                  | PNG FilePath -- ^ Output as Portable Network Graphic into file.
                  | PDF FilePath -- ^ Output as Portable Document Format into a file.
                  | SVG FilePath -- ^ Output as Scalable Vector Graphic into a file.
                  | GIF FilePath -- ^ Output as Graphics Interchange Format into a file.
                  | JPEG FilePath -- ^ Output into a JPEG file.
                  | Latex FilePath -- ^ Output as LaTeX.

-- | The Style of a graph.
data Style = Lines  -- ^ points in the plot are interconnected by lines.
           | Points -- ^ data points are little cross symbols.
           | Dots   -- ^ data points are real dots (approx the size of a pixel).
           | Impulses
           | Linespoints

-- | The Color of a graph.
data Color = Red | Blue | Green | Yellow | Orange | Magenta | Cyan
           | DarkRed | DarkBlue | DarkGreen | DarkYellow | DarkOrange | DarkMagenta | DarkCyan
           | LightRed | LightBlue | LightGreen | LightMagenta
           | Violet | White | Brown | Grey | DarkGrey | Black
           | RGB Int Int Int -- ^ a custom color

data Style2D = Boxerrorbars | Boxes | Boxyerrorbars
             | Filledcurves | Financebars | Fsteps | Histeps | Histograms
             | Steps | Xerrorbars | Xyerrorbars | Yerrorbars | Xerrorlines
             | Xyerrorlines | Yerrorlines

-- | Options on how to render a graph.
data Option = Style Style   -- ^ The style for a graph.
            | Title String  -- ^ The title for a graph in a plot (or a filename like @plot1.dat@).
            | Color Color   -- ^ The line-color for the graph (or if it consist of 'Dots' or 'Points' the color of these)

-- | Options which are exclusively available for 2D plots.
data Option2D x y = Range x x -- ^ Plots the function for the specified x range
                  | For [x]   -- ^ Plots the function only for the given x values
                  | Step x    -- ^ Uses the given step-size for plotting along the x-axis

-- | Options which are exclusively available for 3D plots.
data Option3D x y z = RangeX x x -- ^ Plots the function for the specified x range
                    | RangeY y y -- ^ Plots the function for the specified y range
                    | ForX [x]   -- ^ Plots the function only for the given x values
                    | ForY [y]   -- ^ Plots the function only for the given y values
                    | StepX x    -- ^ Uses the given step-size for plotting along the x-axis
                    | StepY y    -- ^ Uses the given step-size for plotting along the y-axis

-- | A two dimensional set of data to plot.
data Graph2D x y =
      Function2D   [Option] [Option2D x y] (x -> y)
      -- ^ plots a Haskell function @x -> y@

    | Data2D       [Option] [Option2D x y] [(x, y)]
      -- ^ plots a set of tuples.

    | Gnuplot2D    [Option] [Option2D x y] String
      -- ^ plots a custom function passed to Gnuplot (like @x**2 + 10@)

-- | A three dimensional set of data to plot.
data Graph3D x y z =
      Function3D   [Option] [Option3D x y z] (x -> y -> z)
      -- ^ plots a Haskell function @x -> y -> z@

    | Data3D       [Option] [Option3D x y z] [(x, y, z)]
      -- ^ plots a set of triples.

    | Gnuplot3D    [Option] [Option3D x y z] String
      -- ^ plots a custom function passed to Gnuplot (like @x*y@)

-- | Options which can be used with 'plot''
data GnuplotOption = Interactive -- ^ keeps gnuplot open, so that you can interact with the plot (only usefull with 'X11')
                   | Debug       -- ^ prints the command used for running gnuplot.
    deriving Eq

-- | Provides the plot function for different kinds of graphs (2D and 3D)
class Plot a where

    -- | Do a plot to the terminal (i.e. a window will open and your plot can be seen)
    plot :: TerminalType -- ^ The terminal to be used for output.
            -> a         -- ^ The graph to plot. A 'Graph2D' or 'Graph3D' or a list of these.
            -> IO Bool   -- ^ Whether the plot was successfull or not.

    plot = plot' []

    plot' :: [GnuplotOption]
            -> TerminalType
            -> a
            -> IO Bool

-- | 'plot' can be used to plot a single 'Graph2D'.
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot (Graph2D x y) where
    plot' options term graph = plot' options term [graph]

-- | 'plot' can be used to plot a list of 'Graph2D'.
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot [Graph2D x y] where
    plot' options term graphs = exec options [toString term] "plot" options' datasources
        where   (options', datasources) = unzip $ map prepare graphs
                prepare (Gnuplot2D  opt opt2d g) = (opts $ sanitize opt, Right $ g)
                prepare (Data2D     opt opt2d d) = (opts $ sanitize opt, Left  $ toString d)
                prepare (Function2D opt opt2d f) = (opt', Left $ plotData)
                    where   (opt', plotData) = render2D opt opt2d f

-- | 'plot' can be used to plot a single 'Graph3D'.
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot (Graph3D x y z) where
    plot' options term graph = plot' options term [graph]

-- | 'plot' can be used to plot a list of 'Graph3D'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [Graph3D x y z] where
    plot' options term graphs = exec options [toString term] "splot" options' datasources
        where   (options', datasources) = unzip $ map prepare graphs
                prepare (Gnuplot3D  opt opt3d g) = (opts $ sanitize opt, Right $ g)
                prepare (Data3D     opt opt3d d) = (opts $ sanitize opt, Left  $ toString d)
                prepare (Function3D opt opt3d f) = (opt', Left $ plotData)
                    where   (opt', plotData) = render3D opt opt3d f

-- | A 2D function can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot (x -> y) where
    plot' options term f = plot' options term $ Function2D [] [] f

-- | A list of 2D functions can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot [x -> y] where
    plot' options term fs = plot' options term $ map (Function2D [] []) fs

-- | A 3D function can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot (x -> y -> z) where
    plot' options term f = plot' options term $ Function3D [] [] f

-- | A list of 3D functions can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [x -> y -> z] where
    plot' options term fs = plot' options term $ map (Function3D [] []) fs

-- | A list of tuples can be plotted directly using 'plot'
instance (Fractional x, Enum x, Num x, Show x, Num y, Show y) => Plot [(x, y)] where
    plot' options term d = plot' options term $ Data2D [] [] d

-- | A list of triples can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [(x, y, z)] where
    plot' options term d = plot' options term $ Data3D [] [] d

-- | plot accepts a custom string which is then to be interpreted by gnu plot.
--   The function will be interpreted as 'Gnuplot3D'.
instance Plot String where
    plot' options term g = plot' options term $ Gnuplot3D [] [] g

-- | plots mutliple 3D functions using gnuplots native function parser
--   and renderer. The String will be interpreted as 'Gnuplot3D'.
instance Plot [String] where
    plot' options term g = plot' options term $ map (Gnuplot3D [] []) g

-- | INTERNAL: Prepares 2D plots of haskell functions.
render2D opt opt2d f = (opts $ sanitize (opt ++ [Style Lines]), plot2D f)
    where   plot2D f = toString [(x, f x) | x <- maybe [x1,sx..x2] id $ for opt2d]

            (x1, x2) = range opt2d
            sx       = x1 + step opt2d

-- | INTERNAL: Prepares 3D plots of haskell functions.
render3D opt opt3d f = (opts $ sanitize (opt), plot3D f)
    where   plot3D f = toString [(x, y, f x y) | x <- xs, y <- ys]

            xs = maybe [x1,sx..x2] id $ forX opt3d
            ys = maybe [y1,sy..y2] id $ forY opt3d

            ((x1, x2), (y1, y2)) = (rangeX opt3d, rangeY opt3d)
            (sx, sy) = (x1 + stepX opt3d, y1 + stepY opt3d)


for [] = Nothing
for ((For xs) : _) = Just xs
for (_ : xs) = for xs

range [] = (-5, 5)
range ((Range x1 x2) : _) = (x1, x2)
range (_ : xs) = range xs

step [] = 0.05
step ((Step x) : _) = x
step (_ : xs) = step xs


forX [] = Nothing
forX ((ForX xs) : _) = Just xs
forX (_ : xs) = forX xs

forY [] = Nothing
forY ((ForY ys) : _) = Just ys
forY (_ : ys) = forY ys

rangeX [] = (-5, 5)
rangeX ((RangeX x1 x2) : _) = (x1, x2)
rangeX (_ : xs) = rangeX xs

rangeY [] = (-5, 5)
rangeY ((RangeY y1 y2) : _) = (y1, y2)
rangeY (_ : ys) = rangeY ys

stepX [] = 0.1
stepX ((StepX x) : _) = x
stepX (_ : xs) = stepX xs

stepY [] = 0.1
stepY ((StepY y) : _) = y
stepY (_ : ys) = stepY ys


-- | INTERNAL: Sanitizes options given via Graph-Objects
sanitize = sortBy ord . nubBy dup
    where   ord a b
                | dup a b = EQ
                | True    = ord' a b
            ord' (Style _) (Title _) = LT
            ord' (Style _) (Color _) = LT
            ord' (Color _) (Title _) = GT
            ord' a b
                | ord' b a == LT = GT
                | True           = LT
            dup (Title _) (Title _) = True
            dup (Style _) (Style _) = True
            dup (Color _) (Color _) = True
            dup _ _                 = False

-- | INTERNAL: Translates options into gnuplot commands
opts [] = ""
opts [x] = toString x
opts (x:xs) = toString x ++ " " ++ opts xs

-- | INTERNAL: Invokes gnuplot.
--
-- Can be invoked like so:
--
-- > exec ["set terminal x11 persist"] "splot" ["with lines", "with points"] [Left "1 0 2\n2 1 1", Left "2 0 3\n1 1 2"]
--
-- or so:
--
-- > exec ["set terminal x11 persist"] "splot" ["width lines", "with lines"] [Right "x*y", Right "sin(x) + cos(y)"]
exec :: [GnuplotOption] -> [String] -> String -> [String] -> [Either String String] -> IO Bool
exec options preamble plotfunc plotops datasets =
    do
        let filenames = zipWith (\x y -> x ++ show y ++ ".dat")
                                (cycle ["plot"]) [1..length datasets]

        mapM (uncurry writeFile) (zip filenames (map (either id id) datasets))

        let datasources = zipWith (\x y -> either (const (Left x)) Right y) filenames datasets

            file y x = "\"" ++ x ++ "\" " ++ y
            func y x =         x ++ " "   ++ y

            plotcmds = zipWith (\x y -> either (file y) (func y) x) datasources plotops
            plotstmt = foldl1  (\x y -> x ++ ", " ++ y) plotcmds
            plotcmd  = foldl1  (\x y -> x ++ "; " ++ y)
                               (preamble ++ [plotfunc ++ " " ++ plotstmt])
        
            args = ["-e", plotcmd] ++ if Interactive `elem` options then ["-"] else []

        if Debug `elem` options then putStrLn plotcmd else return ()

        exitCode <- rawSystem "gnuplot" args

        return $ exitCode == ExitSuccess

-- | INTERNAL: Provides 'toString' for translating haskell types into gnuplot commands
--   (ordinary strings)
class GnuplotIdiom a where
    toString :: a -> String

instance (Num x, Show x, Num y, Show y) => GnuplotIdiom (x, y) where
    toString (x, y) = space $ shows x $ space $ show y

instance (Num x, Show x, Num y, Show y, Num z, Show z) => GnuplotIdiom (x, y, z) where
    toString (x, y, z) = space $ shows x $ space $ shows y $ space $ show z

space x = ' ' : x

instance GnuplotIdiom Style where
    toString x = case x of
        Lines   -> "with lines"
        Points  -> "with points"
        Dots    -> "with dots"
        Impulses -> "with impulses"
        Linespoints -> "with linespoints"

instance GnuplotIdiom Option where
    toString x = case x of
        Title t -> "title \"" ++ t ++ "\""
        Style s -> toString s
        Color c -> "lc rgb \"" ++ toString c ++ "\""

instance GnuplotIdiom x => GnuplotIdiom [x] where
    toString = unlines . map toString

instance GnuplotIdiom (TerminalType) where
    toString t = case t of
        PNG f   -> "set term png; set output \"" ++ f ++ "\""
        PDF f   -> "set term pdf enhanced; set output \"" ++ f ++ "\""
        SVG f   -> "set term svg dynamic; set output \"" ++ f ++ "\""
        GIF f   -> "set term gif; set output \"" ++ f ++ "\""
        JPEG f  -> "set term jpeg; set output \"" ++ f ++ "\""
        Latex f -> "set term latex; set output \"" ++ f ++ "\""
        EPS f   -> "set term postscript eps; set output \"" ++ f ++ "\""
        PS f    -> "set term postscript; set output \"" ++ f ++ "\""
        Aqua    -> "set term aqua"
        Windows -> "set term windows"
        X11     -> "set term x11 persist"

instance GnuplotIdiom (Color) where
    toString (RGB r g b) = '#' : map toUpper (showHex r $ showHex g $ showHex b "")
    toString color = case color of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"
        Orange -> "orange"
        Magenta -> "magenta"
        Cyan -> "cyan"
        DarkRed -> "dark-red"
        DarkBlue -> "dark-blue"
        DarkGreen -> "dark-green"
        DarkYellow -> "dark-yellow"
        DarkOrange -> "dark-orange"
        DarkMagenta -> "aark-magenta"
        DarkCyan -> "dark-cyan"
        LightRed -> "light-red"
        LightBlue -> "light-blue"
        LightGreen -> "light-green"
        LightMagenta -> "light-magenta"
        Violet -> "violet"
        Grey -> "grey"
        White -> "white"
        Brown -> "brown"
        DarkGrey -> "dark-grey"
        Black -> "black"

