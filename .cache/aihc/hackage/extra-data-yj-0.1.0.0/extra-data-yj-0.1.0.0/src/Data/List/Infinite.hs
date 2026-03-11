{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Infinite (
	-- * Definition
	Infinite(..), NonEmpty(..),
	-- * Basic functions
	append, uncons, concat,
	-- * List transformations
	intersperse, intercalate, subsequences,
	-- * Reducing lists (folds)
	foldr,
	-- * Scans
	scanl, scanl',
	-- * Infinite lists
	iterate, iterate', repeat, cycle, unfoldr,
	-- * Extracting sublits
	take, drop, splitAt, span, group, groupBy, inits,
	-- * Predicates
	isPrefixOf,
	-- * Searching
	partition,
	-- * Indexing lists
	index,
	-- * Zipping and unzipping lists
	zipWith, unzip,
	-- * "Set" operations
	delete, (\\),
	-- * Ordered lists
	insert, insertBy
	) where

import Prelude hiding (
	cycle, (++), concat, scanl, iterate, repeat,
	span, take, drop, splitAt, zipWith, unzip )

import Control.Arrow (first, second, (***))

import Data.List.NonEmpty (NonEmpty(..))

-- DEFINITION

infixr 5 :~

data Infinite a = a :~ Infinite a

instance Functor Infinite where f `fmap` (x :~ xs) = f x :~ (f <$> xs)

instance Applicative Infinite where
	pure x = x :~ pure x
	(f :~ fs) <*> (x :~ xs) = f x :~ (fs <*> xs)

instance Foldable Infinite where
	foldr op v (x :~ xs) = x `op` foldr op v xs

-- BASIC FUNCTIONS

append :: [a] -> Infinite a -> Infinite a
[] `append` ys = ys
(x : xs) `append` ys = x :~ (xs `append` ys)

uncons :: Infinite a -> (a, Infinite a)
uncons (x :~ xs) = (x, xs)

concat :: Infinite [a] -> Infinite a
concat ([] :~ xss) = concat xss
concat ((x : xs) :~ xss) = x :~ concat (xs :~ xss)

-- LIST TRANSFORMATIONS

intersperse :: a -> Infinite a -> Infinite a
intersperse x (y :~ ys) = y :~ x :~ intersperse x ys

intercalate :: [a] -> Infinite [a] -> Infinite a
intercalate xs (ys :~ yss) = ys `append` (xs `append` intercalate xs yss)

subsequences, nonEmptySubsequences :: Infinite a -> Infinite [a]
subsequences xs = [] :~ nonEmptySubsequences xs

nonEmptySubsequences (x :~ xs) =
	[x] :~ concat ((\ys -> [ys, x : ys]) <$> nonEmptySubsequences xs)

-- REDUCING LISTS (FOLDS)

-- SCANS

scanl, scanl' :: (b -> a -> b) -> b -> Infinite a -> Infinite b
scanl op z (x :~ xs) = z :~ scanl op (z `op` x) xs
scanl' op z (x :~ xs) = z `seq` z :~ scanl' op (z `op` x) xs

-- INFINITE LISTS

iterate, iterate' :: (a -> a) -> a -> Infinite a
iterate f x = x :~ iterate f (f x)
iterate' f x = x `seq` x :~ iterate f (f x)

repeat :: a -> Infinite a
repeat x = x :~ repeat x

cycle :: NonEmpty a -> Infinite a
cycle xs = ccl xs where
	ccl (y :| ys) = y :~ case ys of
		[] -> cycle xs
		(z : zs) -> ccl (z :| zs)

unfoldr :: (b -> (a, b)) -> b -> Infinite a
unfoldr f s = x :~ unfoldr f s' where (x, s') = f s

-- SUBLISTS

take :: Integral i => i -> Infinite a -> [a]
take n = fst . splitAt n

drop :: Integral i => i -> Infinite a -> Infinite a
drop n = snd . splitAt n

splitAt :: Integral i => i -> Infinite a -> ([a], Infinite a)
splitAt n xs | n < 1 = ([], xs)
splitAt n (x :~ xs) = (x :) `first` splitAt (n - 1) xs

span :: (a -> Bool) -> Infinite a -> ([a], Infinite a)
span p xa@(x :~ xs)
	| p x = (x :) `first` span p xs
	| otherwise = ([], xa)

group :: Eq a => Infinite a -> Infinite [a]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> Infinite a -> Infinite [a]
groupBy eq (x :~ xs) = (x : ys) :~ groupBy eq zs
	where (ys, zs) = span (x `eq`) xs

inits :: Infinite a -> Infinite [a]
inits (x :~ xs) = [] :~ ((x :) <$> inits xs)

-- PREDICATES

isPrefixOf :: Eq a => [a] -> Infinite a -> Bool
isPrefixOf [] _ = True
isPrefixOf (x : xs) (y :~ ys) = x == y && isPrefixOf xs ys

-- SEARCHING LISTS

partition :: (a -> Bool) -> Infinite a -> (Infinite a, Infinite a)
partition p (x :~ xs)
	| p x = (x :~) `first` partition p xs
	| otherwise = (x :~) `second` partition p xs

-- INDEXING LISTS

infixl 9 `index`

index :: Integral i => Infinite a -> i -> a
_ `index` n | n < 0 = error "negative index"
(x :~ _) `index` 0 = x
(_ :~ xs) `index` n = xs `index` (n - 1)

-- ZIPPING AND UNZIPPING LISTS

zipWith :: (a -> b -> c) -> Infinite a -> Infinite b -> Infinite c
zipWith op (x :~ xs) (y :~ ys) = (x `op` y) :~ zipWith op xs ys

unzip :: Infinite (a, b) -> (Infinite a, Infinite b)
unzip ((x, y) :~ xys) = (x :~) *** (y :~) $ unzip xys

-- SET OPERATIONS

delete :: Eq a => a -> Infinite a -> Infinite a
delete x (y :~ ys)
	| x == y = ys
	| otherwise = y :~ delete x ys

(\\) :: Eq a => Infinite a -> [a] -> Infinite a
(\\) = foldl $ flip delete

-- ORDERED LISTS

insert :: Ord a => a -> Infinite a -> Infinite a
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> Infinite a -> Infinite a
insertBy cmp x ya@(y :~ ys) = case cmp x y of
	GT -> y :~ insertBy cmp x ys
	_ -> x :~ ya
