module Visualize.Data.Char.Number where

import qualified Data.Char.Number as Number
import qualified Data.Map as Map


visualize :: IO ()
visualize = do
   putStrLn $ unlines $ Map.elems $
      Map.mapWithKey
         (\ratio chr -> show (ratio::Rational) ++ '\t' : chr : [])
         Number.fractionMap
