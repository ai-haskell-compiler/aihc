module Main (main) where

import Data.Algorithm.Assignment (assign)
import Weigh

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  benchCase 5 5
  benchCase 5 10
  benchCase 10 5
  benchCase 10 10
  benchCase 10 20
  benchCase 20 10
  benchCase 20 20
  benchCase 40 40

benchCase :: Int -> Int -> Weigh ()
benchCase a b =
  func name (uncurry (assign cost)) (as, bs)
  where
    name = "assign " ++ show a ++ " " ++ show b
    as = [1 .. a]
    bs = [1 .. b]
    cost x y = x `div` y - x `rem` y + (y * 3) `rem` 4
