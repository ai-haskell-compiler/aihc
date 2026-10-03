module Main (main) where

import Data.List (foldl')

-- A GC workload: a large live set that stays reachable while the program
-- allocates. A copying collector copies the whole set at each collection,
-- and a generational collector copies it once.
data Tree = Leaf | Node Tree Int Tree

insert :: Int -> Tree -> Tree
insert key Leaf = Node Leaf key Leaf
insert key node@(Node left value right)
  | key < value = Node (insert key left) value right
  | key > value = Node left value (insert key right)
  | otherwise = node

member :: Int -> Tree -> Bool
member _ Leaf = False
member key (Node left value right)
  | key < value = member key left
  | key > value = member key right
  | otherwise = True

size :: Tree -> Int
size Leaf = 0
size (Node left _ right) = size left + 1 + size right

-- The keys arrive in a scrambled order so the tree stays balanced enough.
keys :: [Int]
keys = [(index * 7919) `mod` 100003 | index <- [1 .. 100000]]

-- One round allocates a temporary list and asks the live tree about it.
probe :: Tree -> Int -> Int -> Int
probe tree acc round' = acc + length (filter (`member` tree) [round' * 3, round' * 3 + 1 .. round' * 3 + 2000])
{-# NOINLINE probe #-}

main :: IO ()
main = do
  let tree = foldl' (flip insert) Leaf keys
  print (size tree)
  print (foldl' (probe tree) 0 [1 .. 200])
