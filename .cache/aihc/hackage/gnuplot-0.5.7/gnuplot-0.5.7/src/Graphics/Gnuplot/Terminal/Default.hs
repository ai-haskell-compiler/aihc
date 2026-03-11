module Graphics.Gnuplot.Terminal.Default (
   T, cons,
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal


data T = Cons

cons :: T
cons = Cons


instance Terminal.C T where
   canonical _term =
      Terminal.Cons {
         Terminal.precommands = [],
         Terminal.options = [],
         Terminal.commands = [],
         Terminal.interactive = True  -- Always true? I don't know.
      }
