-- This differs from intervals package: even an empty interval has offset (but not inf)

module Data.Interval
    ( Interval(..)
    , member
    , empty
    , contains
    ) where

data Interval a = I { offset :: !a, size :: !a }

instance (Ord a, Num a) => Eq (Interval a) where
    (==) (I o1 s1) (I o2 s2) | s1 >  0 && s2 >  0 = o1 == o2 && s1 == s2
                             | s1 <= 0 && s2 <= 0 = o1 == o2
                             | otherwise          = False

instance (Ord a, Num a, Show a) => Show (Interval a) where
    show (I o s) | s <= 0    = "empty @" ++ show o
                 | otherwise = show o ++ " ... " ++ show (o + s - 1)

empty :: (Ord a, Num a) => Interval a -> Bool
empty (I _ s) | s <= 0    = True
              | otherwise = False
{-# INLINE empty #-}

member :: (Ord a, Num a) => a -> Interval a -> Bool
member x i@(I o s) | empty i   = False
                   | otherwise = o <= x && x <= (o + s - 1)
{-# INLINE member #-}

contains :: (Ord a, Num a) => Interval a -> Interval a -> Bool
contains b a@(I ao as) | empty b   = False
                       | empty a   = ao `member` b
                       | otherwise = ao `member` b && (ao + as - 1) `member` b
{-# INLINE contains #-}
