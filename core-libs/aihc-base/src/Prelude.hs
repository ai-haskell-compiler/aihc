module Prelude
  ( Char,
    String,
    (++),
  )
where

data Char

type String = [Char]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)
