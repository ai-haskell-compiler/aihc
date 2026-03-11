module Combinatorics.Utility where

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct x y = sum (zipWith (*) x y)
