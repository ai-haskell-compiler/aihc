module Graphics.Gnuplot.Frame (
   Frame.T,
   cons, simple, empty,
   ) where

import qualified Graphics.Gnuplot.Frame.OptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.Frame as Frame
import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.GraphEmpty as Empty
import qualified Graphics.Gnuplot.Private.Graph as Graph


cons :: OptionSet.T graph -> Plot.T graph -> Frame.T graph
cons = Frame.Cons

simple :: Graph.C graph => Plot.T graph -> Frame.T graph
simple = cons OptionSet.deflt

empty :: Frame.T Empty.T
empty = simple $ Plot.pure []
