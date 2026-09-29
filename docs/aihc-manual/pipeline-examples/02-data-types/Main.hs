module Main (main) where

data Shape
  = Circle Int
  | Rect Int Int

area :: Shape -> Int
area shape = case shape of
  Circle r -> 3 * r * r
  Rect w h -> w * h

main :: IO ()
main = print (area (Rect 3 4) + area (Circle 2))
