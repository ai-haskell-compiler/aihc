
module Main (main) where

import Data.SortedList (SortedList)
import qualified Data.SortedList as SL
import Criterion.Main (defaultMain, bench, nf)

list :: SortedList Int
list = SL.take 100 $ SL.iterate (+1) 1

-- | Monotonically increasing function.
incf :: Int -> Int
incf x = 2 * x

-- | Monotonically decreasing function.
decf :: Int -> Int
decf x = (-2) * x

main :: IO ()
main = defaultMain
  [ bench "increasing/map"    $ nf (SL.map    incf) list
  , bench "increasing/mapDec" $ nf (SL.mapDec incf) list
  , bench "decreasing/map"    $ nf (SL.map    decf) list
  , bench "decreasing/mapDec" $ nf (SL.mapDec decf) list
    ]
