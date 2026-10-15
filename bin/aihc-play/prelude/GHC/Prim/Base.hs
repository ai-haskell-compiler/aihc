{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# HLINT ignore "Use >>" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use tuple-section" #-}

-- | Functions and classes for the small Prelude of The Lazy Reader.
--
-- The module holds what a short example program reaches for: 'Maybe',
-- 'Either', the function combinators, the list functions, and the
-- 'Functor', 'Applicative' and 'Monad' classes with their instances for
-- 'Maybe', lists and 'IO'.
module GHC.Prim.Base
  ( Maybe (..),
    Either (..),
    String,
    Functor (..),
    Applicative (..),
    Monad (..),
    id,
    const,
    (.),
    flip,
    ($),
    otherwise,
    fst,
    snd,
    maybe,
    either,
    map,
    (++),
    filter,
    foldr,
    length,
    null,
    reverse,
    concat,
    concatMap,
    zip,
    sum,
    take,
    drop,
    replicate,
  )
where

import GHC.Classes (Eq (..), Ord (..), (&&))
import GHC.Num (Num (..))
import GHC.Types (Bool (..), Char, IO (..), Int, Ordering (..))

infixr 9 .

infixr 0 $

infixr 5 ++

infixl 4 <$>, <*>

infixl 1 >>=, >>

type String = [Char]

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

instance (Ord a) => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing (Just _) = LT
  compare (Just _) Nothing = GT
  compare (Just x) (Just y) = compare x y

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Right x == Right y = x == y
  _ == _ = False

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
f $ x = f x

otherwise :: Bool
otherwise = True

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

maybe :: b -> (a -> b) -> Maybe a -> b
maybe fallback _ Nothing = fallback
maybe _ f (Just x) = f x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter keep (x : xs)
  | keep x = x : filter keep xs
  | otherwise = filter keep xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

length :: [a] -> Int
length = go 0
  where
    go n [] = n
    go n (_ : xs) = go (n + 1) xs

null :: [a] -> Bool
null [] = True
null (_ : _) = False

reverse :: [a] -> [a]
reverse = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr (\x acc -> f x ++ acc) []

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

sum :: (Num a) => [a] -> a
sum = foldr (+) 0

take :: Int -> [a] -> [a]
take n (x : xs) | n > 0 = x : take (n - 1) xs
take _ _ = []

drop :: Int -> [a] -> [a]
drop n (_ : xs) | n > 0 = drop (n - 1) xs
drop _ xs = xs

replicate :: Int -> a -> [a]
replicate n x
  | n > 0 = x : replicate (n - 1) x
  | otherwise = []

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class (Applicative m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  m >> k = m >>= \_ -> k
  return :: a -> m a
  return = pure

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)
  _ <*> _ = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= k = k x

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure x = [x]
  fs <*> xs = concatMap (`map` xs) fs

instance Monad [] where
  xs >>= k = concatMap k xs

instance Functor IO where
  fmap f (IO m) = IO (\s -> case m s of (# s', a #) -> (# s', f a #))

instance Applicative IO where
  pure x = IO (\s -> (# s, x #))
  IO mf <*> IO mx = IO (\s -> case mf s of (# s', f #) -> case mx s' of (# s'', x #) -> (# s'', f x #))

instance Monad IO where
  IO m >>= k = IO (\s -> case m s of (# s', a #) -> case k a of IO n -> n s')
