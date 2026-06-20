module Prelude
  ( Bool (..),
    Char,
    Eq (..),
    Integer,
    List (..),
    Num (..),
    String,
    (&&),
    (++),
    (/=),
    (==),
    not,
    otherwise,
    (||),
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import GHC.Integer (Integer)
import GHC.Num (Num (..))

data Char

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  x /= y = not (x == y)

instance (Eq a) => Eq [a] where
  [] == [] = True
  [] == (_ : _) = False
  (_ : _) == [] = False
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)
