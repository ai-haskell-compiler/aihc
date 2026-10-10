module Main (main) where

-- A GC workload: deep stacks that grow with little or no allocation. The
-- runtime charges each new stack chunk to the nursery, so the program
-- collects after about one nursery of stack growth. Thus one minor
-- collection scans a bounded number of young stack chunks, also at the
-- bottom of a deep recursion.

-- A chain of lazy additions. The evaluation of the last one evaluates each
-- thunk below it before any addition gives its result.
data Box = Box Int

fill :: Int -> Box -> Box
fill 0 box = box
fill n (Box total) = fill (n - 1) (Box (total + n))
{-# NOINLINE fill #-}

unbox :: Box -> Int
unbox (Box total) = total

-- A recursion that is not in tail position, over a list that exists before
-- it starts. Each cell pushes one frame.
total :: [Int] -> Int
total [] = 0
total (x : xs) = x + total xs
{-# NOINLINE total #-}

-- A recursion that is not in tail position and that allocates one cell for
-- each 64 levels.
descend :: Int -> [Int] -> Int
descend 0 acc = length acc
descend n acc
  | n `rem` 64 == 0 = case n : acc of
      cell -> 1 + descend (n - 1) cell
  | otherwise = 1 + descend (n - 1) acc
{-# NOINLINE descend #-}

main :: IO ()
main = do
  print (unbox (fill 1000000 (Box 0)))
  let xs = [1 .. 1000000]
  print (length xs)
  print (total xs)
  print (descend 1000000 [])
