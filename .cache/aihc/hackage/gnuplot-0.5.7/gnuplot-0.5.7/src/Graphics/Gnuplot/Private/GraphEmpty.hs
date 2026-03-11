{-
This module is private since 'Cons' shall not be public.
However, 'Cons' is not needed at all
because the plot command (here @clear@)
is determined entirely by the graph type.
-}
module Graphics.Gnuplot.Private.GraphEmpty where

import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet


data T = Cons


instance Graph.C T where
   command = Graph.Command "clear"
   toString = const ""
   defltOptions =
      OptionSet.Cons OptionSet.deflt
