module Graphics.Gnuplot.Private.Encoding where

import Graphics.Gnuplot.Utility (listFromMaybeWith)

newtype T = Cons String

format :: T -> String
format (Cons enc) = "set encoding " ++ enc

formatMaybe :: Maybe T -> [String]
formatMaybe = listFromMaybeWith format
