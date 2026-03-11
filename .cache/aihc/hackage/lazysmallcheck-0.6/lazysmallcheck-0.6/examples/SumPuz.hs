module SumPuz where

-- Cryptarithmetic solver from AFP 2003

import Data.List((\\))
import Char(isAlpha, chr, ord)
import Maybe(fromJust)

type Soln = [(Char, Int)]

solve :: String -> String
solve p =
  display p (solutions xs ys zs 0 [])
  where
  [xs,ys,zs] = map reverse (words (filter (`notElem` "+=") p))

display :: String -> [Soln] -> String
display p []    = "No solution!"
display p (s:_) =
  map soln p
  where
  soln c = if isAlpha c then chr (ord '0' + img s c) else c

rng :: Soln -> [Int]
rng = map snd

img :: Soln -> Char -> Int
img lds l = fromJust (lookup l lds)

bindings :: Char -> [Int] -> Soln -> [Soln]
bindings l ds lds =
  case lookup l lds of
  Nothing  -> map (:lds) (zip (repeat l) (ds \\ rng lds))
  Just d -> if d `elem` ds then [lds] else []

solutions :: String -> String -> String -> Int -> Soln -> [Soln]
solutions [] [] []  c lds = if c==0 then [lds] else []
solutions [] [] [z] c lds = if c==1 then bindings z [1] lds else []
solutions (x:xs) (y:ys) (z:zs) c lds =
  solns `ofAll`
  bindings y [(if null ys then 1 else 0)..9] `ofAll`
  bindings x [(if null xs then 1 else 0)..9] lds
  where  
  solns s = 
    solutions xs ys zs (xy `div` 10) `ofAll` bindings z [xy `mod` 10] s
    where    
    xy = img s x + img s y + c

infixr 5 `ofAll`
ofAll :: (a -> [b]) -> [a] -> [b]
ofAll = concatMap

-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

find :: String -> String -> String -> [Soln]
find xs ys zs = solutions (reverse xs) (reverse ys) (reverse zs) 0 []

val :: Soln -> String -> Int
val s "" = 0
val s xs = read (concatMap (show . img s) xs)

prop_Sound :: (String, String, String) -> Bool
prop_Sound (xs, ys, zs) =
      length xs == length ys
   && (diff == 0 || diff == 1)
   && not (null sols)
  --> and [ val s xs + val s ys == val s zs
          | s <- sols
          ]
  where
    sols = find xs ys zs
    diff = length zs - length xs
