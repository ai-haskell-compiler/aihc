module Graphics.Gnuplot.Terminal.SVG (
   T, cons,
   encoding,
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import qualified Graphics.Gnuplot.Private.Encoding as Encoding
import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons {
      filename_ :: FilePath,
      encoding_ :: Maybe Encoding.T
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      encoding_ = Nothing
   }

{- |
Setting the encoding to anything different
from 'Graphics.Gnuplot.Encoding.locale'
makes only sense if you write your gnuplot files manually using this encoding.
-}
encoding :: Encoding.T -> T -> T
encoding enc term = term{encoding_ = Just enc}


-- private functions

instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.precommands = Encoding.formatMaybe $ encoding_ term,
         Terminal.options =
            "svg" :
            [],
         Terminal.commands =
            ["set output " ++ (quote $ filename_ term)],
         Terminal.interactive = False
      }
