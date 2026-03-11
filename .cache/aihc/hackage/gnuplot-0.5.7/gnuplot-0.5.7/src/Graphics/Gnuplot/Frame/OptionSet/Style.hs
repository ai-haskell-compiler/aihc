module Graphics.Gnuplot.Frame.OptionSet.Style (
   fillSolid,
   fillBorder,
   fillBorderLineType,
   ) where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Graphics.Gnuplot.Private.FrameOptionSet (T, )

-- import Graphics.Gnuplot.Utility (quote, )


fillSolid :: Graph.C graph => T graph -> T graph
fillSolid =
   OptionSet.addBool Option.styleFillSolid True

fillBorder :: Graph.C graph => Bool -> T graph -> T graph
fillBorder =
   OptionSet.addBool Option.styleFillBorder

fillBorderLineType :: Graph.C graph => Int -> T graph -> T graph
fillBorderLineType n =
   OptionSet.add Option.styleFillBorder ["border", show n]
