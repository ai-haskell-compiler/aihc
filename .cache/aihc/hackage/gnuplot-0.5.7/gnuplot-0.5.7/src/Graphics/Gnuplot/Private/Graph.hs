module Graphics.Gnuplot.Private.Graph where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet

class C graph where
   command :: Command graph
   toString :: graph -> String
   defltOptions :: OptionSet.T graph

newtype Command graph = Command {commandString :: String}
