module Prelude
  ( Bool (..),
    Char,
    Eq (..),
    IO (..),
    Int,
    List (..),
    Ord,
    Read,
    Show,
    String,
    ($),
    (&&),
    (++),
    (/=),
    (==),
    (>>),
    (>>=),
    fmap,
    not,
    otherwise,
    return,
    undefined,
    (||),
  )
where

data Bool = False | True

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && x = x

not :: Bool -> Bool
not False = True
not True = False

otherwise :: Bool
otherwise = True

infixr 2 ||

(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True

infixr 0 $

($) :: (a -> b) -> a -> b
($) f = f

data Char

data Int

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

newtype IO a
  = IO a

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

class Ord a

class Read a

class Show a

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

return :: a -> IO a
return = return

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) action next = action >>= next

(>>) :: IO a -> IO b -> IO b
(>>) action next = action >> next

fmap :: (a -> b) -> IO a -> IO b
fmap = fmap

undefined :: a
undefined = undefined
