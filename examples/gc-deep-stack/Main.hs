module Main (main) where

-- A GC workload: a deep stack while the nursery fills many times. Each level
-- of the recursion allocates a small tree before it recurses, so collections
-- happen with up to 100000 frames live. A collector that walks every frame
-- at each collection does quadratic work here.
data Tree = Leaf Int | Node Tree Tree

build :: Int -> Int -> Tree
build 0 seed = Leaf seed
build depth seed = Node (build (depth - 1) (seed * 2)) (build (depth - 1) (seed * 2 + 1))
{-# NOINLINE build #-}

total :: Tree -> Int
total (Leaf value) = value
total (Node left right) = total left + total right
{-# NOINLINE total #-}

descend :: Int -> Int
descend 0 = 0
descend depth = total (build 3 depth) + descend (depth - 1)
{-# NOINLINE descend #-}

main :: IO ()
main = print (descend 100000)
