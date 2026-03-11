{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric      #-}
module Data.Matrix.Sparse.Generic
    ( Zero(..)
    , CSR(..)
    , AssocList

    -- * Accessors
    -- ** length information
    , MG.dim
    , MG.rows
    , MG.cols

    -- ** Indexing
    , MG.unsafeIndex
    , (MG.!)
    , MG.takeRow
    , MG.takeColumn
    , MG.takeDiag

    -- * Construction
    , fromAscAL
    , MG.unsafeFromVector
    , MG.fromVector
    , MG.matrix
    , MG.fromLists
    , MG.fromRows
    , MG.empty

    -- * Conversions
    , MG.flatten
    , MG.toRows
    , MG.toColumns
    , MG.toList
    , MG.toLists
    ) where

import           Control.Monad                     (foldM, forM_, when)
import           Control.Monad.ST                  (runST)
import           Data.Bits                         (shiftR)
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Unboxed               as U
import           Text.Printf                       (printf)
import           GHC.Generics          (Generic)

import           Data.Matrix.Generic.Mutable (MMatrix)
import qualified Data.Matrix.Class as MG

class Eq a => Zero a where
    zero :: a

instance Zero Int where
    zero = 0

instance Zero Double where
    zero = 0.0

instance Eq a => Zero ([] a) where
    zero = []

-- | mutable sparse matrix not implemented
type instance MG.Mutable CSR = MMatrix

-- | Compressed Sparse Row (CSR) matrix
data CSR v a = CSR !Int  -- rows
                   !Int  -- cols
                   !(v a)  -- values
                   !(U.Vector Int)  -- column index of values
                   !(U.Vector Int)  -- row pointer
    deriving (Show, Read, Eq, Generic)

instance (Zero a, G.Vector v a) => MG.Matrix CSR v a where
    dim (CSR r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    unsafeIndex (CSR _ _ vec ci rp) (i,j) =
        case binarySearchByBounds ci j r0 r1 of
            Nothing -> zero
            Just k -> vec `G.unsafeIndex` k
      where
        r0 = rp `U.unsafeIndex` i
        r1 = rp `U.unsafeIndex` (i+1) - 1
    {-# INLINE unsafeIndex #-}

    unsafeFromVector (r,c) vec =
        CSR r c (G.generate n (G.unsafeIndex vec . U.unsafeIndex nz))
                (U.map (`mod` c) nz)
                (U.fromList . g . U.foldr f ((r-1,n-1), [n]) $ nz)
      where
        nz = U.filter (\i -> vec `G.unsafeIndex` i /= zero) . U.enumFromN 0 $ (r*c)
        f i ((!prev,!acc), xs) | stride == 0 = ((prev, acc-1), xs)
                               | otherwise = ((current, acc-1), replicate stride (acc+1) ++ xs)
          where
            stride = prev - current
            current = i `div` c
        g ((a, _), xs) | a == 0 = 0 : xs
                       | otherwise = replicate (a+1) 0 ++ xs
        n = U.length nz
    {-# INLINE unsafeFromVector #-}

    unsafeTakeRow (CSR _ c vec ci rp) i = G.fromList $ loop (-1) r0
      where
        loop !prev !n
            | n > r1 = replicate (c-prev-1) zero
            | otherwise = replicate (cur-prev-1) zero ++ (x : loop cur (n+1))
          where
            cur = ci `U.unsafeIndex` n
            x = vec `G.unsafeIndex` n
        r0 = rp `U.unsafeIndex` i
        r1 = rp `U.unsafeIndex` (i+1) - 1
    {-# INLINE unsafeTakeRow #-}

    thaw = undefined
    unsafeThaw = undefined
    freeze = undefined
    unsafeFreeze = undefined

type AssocList a = [((Int, Int), a)]

-- | Construct CSR from ascending association list. Items must be sorted first
-- by row index, and then by column index.
fromAscAL :: G.Vector v a => (Int, Int) -> Int -> AssocList a -> CSR v a
fromAscAL (r,c) n al = CSR r c values ci rp
  where
    (values, ci, rp) = runST $ do
        v <- GM.new n
        col <- GM.new n
        row <- GM.new (r+1)

        ((i,_),_) <- foldM (f v col row) ((-1,-1),0) al

        let stride = r - i
        forM_ [0..stride-1] $ \s -> GM.write row (r-s) n
        v' <- G.unsafeFreeze v
        col' <- G.unsafeFreeze col
        row' <- G.unsafeFreeze row
        return (v', col', row')

    f v col row ((i',j'), acc) ((i,j),x) =
        if i > i' || (i == i' && j > j')
           then do
               GM.write v acc x
               GM.write col acc j
               let stride = i - i'
               when (stride > 0) $ forM_ [0..stride-1] $ \s -> GM.write row (i-s) acc

               return ((i,j), acc+1)
           else error $ printf "Input must be sorted by row and then by column: (%d,%d) >= (%d,%d)" i' j' i j
{-# INLINE fromAscAL #-}

binarySearchByBounds :: U.Vector Int -> Int -> Int -> Int -> Maybe Int
binarySearchByBounds vec x = loop
  where
    loop !l !u
        | l > u = Nothing
        | x == x' = Just k
        | x < x' = loop l (k-1)
        | otherwise = loop (k+1) u
      where
        k = (u+l) `shiftR` 1
        x' = vec `U.unsafeIndex` k
{-# INLINE binarySearchByBounds #-}
