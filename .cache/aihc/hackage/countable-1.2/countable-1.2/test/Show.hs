module Show where

import Data.Function.Show ()
import Data.Searchable

showFunction :: (Show a, Finite a, Show b) => (a -> b) -> String
showFunction f = show f
