module Utils
(
  medianOf
) where

import Data.List (sort)

medianOf :: (Real a, Eq a) => [a] -> Maybe Double
medianOf []  = Nothing
medianOf [x] = Just . fromRational $ toRational x
medianOf xs  =
  let sorted = sort xs
      len    = length xs
    in case even len of
      True  -> let [x, y] = take 2 $ drop (len `div` 2 - 1) sorted
                in Just $ average x y
      False -> medianOf . take 1 $ drop (len `div` 2) sorted

average :: (Real a) => a -> a -> Double
average x l = (/2) . fromRational $ toRational (x + l)
