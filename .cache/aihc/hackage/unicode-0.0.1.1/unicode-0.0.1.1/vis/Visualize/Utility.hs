module Visualize.Utility where

import qualified Data.List.HT as ListHT
import Data.List (intersperse, )


printGrid :: (a -> b -> Char) -> [a] -> [b] -> IO ()
printGrid f as bs =
   putStrLn $ unlines $ intersperse "" $ map (intersperse ' ') $
      ListHT.outerProduct f as bs
