{- |
How many possibilities are there for representing an amount of n ct
by the Euro coins 1ct, 2ct, 5ct, 10ct, 20ct, 50ct, 100ct, 200ct?
-}
module Combinatorics.Coin where

import qualified Data.List as List
import qualified PowerSeries as PS


values :: [Int]
values = 1 : 2 : 5 : 10 : 20 : 50 : 100 : 200 : []

representationNumbersSingle :: Int -> [Integer]
representationNumbersSingle n =
   cycle (1 : List.replicate (n-1) 0)

representationNumbers :: [Integer]
representationNumbers =
   foldl PS.mul PS.one $
   map representationNumbersSingle values
