module Graphics.Gnuplot.Private.Frame where

import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet

import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Data.Monoid (mappend, )


data T graph =
   Cons {
      option :: OptionSet.T graph,
      plot :: Plot.T graph
   }

instance Graph.C graph => Display.C (T graph) where
   toScript frame =
      (Plot.optionsToScript $ option frame)
      `mappend`
      (Plot.toScript $ plot frame)
