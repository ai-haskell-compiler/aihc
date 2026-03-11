{-# LANGUAGE FlexibleInstances #-}
module Data.Ix.Shapable 
    ( Shapable(..)
    , rank
    , shape
    , shapeToStride
    , size
    ) where

import Data.Array.Base (numElements)
import Data.Array.IArray

-- | Determine the rank of an array.
rank :: (Shapable i, Ix i, IArray a e) => a i e -> Int
rank = sRank . fst . bounds

-- | Canonical representation of the shape.
-- The following properties hold:
--     'length . shape = rank'
--     'product . shape = size'
shape :: (Shapable i, Ix i, IArray a e) => a i e -> [Int]
shape = uncurry sShape . bounds

-- | How much the offset changes when you move one element in the given
-- direction.  Since arrays are in row-major order, 'last . shapeToStride = const 1'
shapeToStride :: [Int] -> [Int]
shapeToStride = scanr (*) 1 . tail

-- | Number of elements in the Array.
size :: (Ix i, IArray a e) => a i e -> Int
size = numElements

-- | We need this type class to distinguish between different tuples of Ix.
-- There are Shapable instances for homogenous Int tuples, but may Haddock
-- doesn't see them.
class Shapable i where
    sRank :: i -> Int
    sShape :: i -> i -> [Int]
    sBounds :: [Int] -> (i,i)

instance Shapable Int where
    sRank _ = 1
    sShape a a' = [rangeSize (a,a')]
    sBounds [a] = (0,a-1)
    sBounds _ = error "sBounds expected list length 1"

instance Shapable (Int,Int) where
    sRank _ = 2
    sShape (a,b) (a',b') = [rangeSize (a,a'), rangeSize (b,b')]
    sBounds [a,b] = ((0,0),(a-1,b-1))
    sBounds _ = error "sBounds expected list length 2"

instance Shapable (Int,Int,Int) where
    sRank _ = 3
    sShape (a,b,c) (a',b',c') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c')]
    sBounds [a,b,c] = ((0,0,0),(a-1,b-1,c-1))
    sBounds _ = error "sBounds expected list length 3"

instance Shapable (Int,Int,Int,Int) where
    sRank _ = 4
    sShape (a,b,c,d) (a',b',c',d') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')]
    sBounds [a,b,c,d] = ((0,0,0,0),(a-1,b-1,c-1,d-1))
    sBounds _ = error "sBounds expected list length 4"

instance Shapable (Int,Int,Int,Int,Int) where
    sRank _ = 5
    sShape (a,b,c,d,e) (a',b',c',d',e') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')
        , rangeSize (e,e')]
    sBounds [a,b,c,d,e] = ((0,0,0,0,0),(a-1,b-1,c-1,d-1,e-1))
    sBounds _ = error "sBounds expected list length 5"

instance Shapable (Int,Int,Int,Int,Int,Int) where
    sRank _ = 6
    sShape (a,b,c,d,e,f) (a',b',c',d',e',f') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')
        , rangeSize (e,e'), rangeSize (f,f')]
    sBounds [a,b,c,d,e,f] = ((0,0,0,0,0,0),(a-1,b-1,c-1,d-1,e-1,f-1))
    sBounds _ = error "sBounds expected list length 6"

instance Shapable (Int,Int,Int,Int,Int,Int,Int) where
    sRank _ = 7
    sShape (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')
        , rangeSize (e,e'), rangeSize (f,f'), rangeSize (g,g')]
    sBounds [a,b,c,d,e,f,g] = ((0,0,0,0,0,0,0),(a-1,b-1,c-1,d-1,e-1,f-1,g-1))
    sBounds _ = error "sBounds expected list length 7"

instance Shapable (Int,Int,Int,Int,Int,Int,Int,Int) where
    sRank _ = 8
    sShape (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')
        , rangeSize (e,e'), rangeSize (f,f'), rangeSize (g,g'), rangeSize (h,h')]
    sBounds [a,b,c,d,e,f,g,h] = ((0,0,0,0,0,0,0,0),(a-1,b-1,c-1,d-1,e-1,f-1,g-1,h-1))
    sBounds _ = error "sBounds expected list length 8"

instance Shapable (Int,Int,Int,Int,Int,Int,Int,Int,Int) where
    sRank _ = 9
    sShape (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i') =
        [rangeSize (a,a'), rangeSize (b,b'), rangeSize (c,c'), rangeSize (d,d')
        , rangeSize (e,e'), rangeSize (f,f'), rangeSize (g,g'), rangeSize (h,h')
        , rangeSize (i,i')]
    sBounds [a,b,c,d,e,f,g,h,i] = ((0,0,0,0,0,0,0,0,0)
                                  ,(a-1,b-1,c-1,d-1,e-1,f-1,g-1,h-1,i-1))
    sBounds _ = error "sBounds expected list length 9"

