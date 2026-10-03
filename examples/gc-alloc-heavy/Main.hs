module Main (main) where

import Data.List (foldl')

-- A GC workload: many short-lived objects and almost no live data. Each
-- step builds a small tree and sums it at once, so a collection finds
-- nearly nothing to copy. The time of the program is the allocation rate.
data Tree = Leaf Int | Node Tree Tree

build :: Int -> Int -> Tree
build 0 seed = Leaf seed
build depth seed = Node (build (depth - 1) (seed * 2)) (build (depth - 1) (seed * 2 + 1))
{-# NOINLINE build #-}

total :: Tree -> Int
total (Leaf value) = value
total (Node left right) = total left + total right
{-# NOINLINE total #-}

step :: Int -> Int -> Int
step acc seed = acc + total (build 4 seed)

main :: IO ()
main = print (foldl' step 0 [1 .. 100000])
