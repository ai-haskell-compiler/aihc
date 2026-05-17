module Prelude
  ( Bool (..),
    Char,
    List (..),
    String,
    (&&),
    (++),
    not,
    otherwise,
    (||),
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))

data Char

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)
