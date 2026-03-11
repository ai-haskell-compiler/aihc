{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Symmetric.Generic.Mutable
   ( -- * Mutable Matrix
     SymMMatrix(..)
   , C.dim
   , C.write
   , C.unsafeWrite
   , C.read
   , C.unsafeRead
   , C.new
   , C.replicate
   ) where

import           Control.Monad               (liftM)
import           Data.Bits                   (shiftR)
import qualified Data.Vector.Generic.Mutable as GM
import           Prelude                     hiding (read, replicate)

import qualified Data.Matrix.Class.Mutable as C

-- | mutable matrix
data SymMMatrix v s a = SymMMatrix !Int !(v s a)

instance GM.MVector v a => C.MMatrix SymMMatrix v a where
    dim (SymMMatrix n _) = (n,n)
    {-# INLINE dim #-}

    unsafeRead (SymMMatrix n v) (i,j) = GM.unsafeRead v (idx n i j)
    {-# INLINE unsafeRead #-}

    unsafeWrite (SymMMatrix n v) (i,j) = GM.unsafeWrite v (idx n i j)
    {-# INLINE unsafeWrite #-}

    new (r,c) | r /= c = error "colmumns /= rows"
              | otherwise = SymMMatrix r `liftM` GM.new ((r*(r+1)) `shiftR` 1)

    replicate (r,c) x
        | r /= c = error "colmumns /= rows"
        | otherwise = SymMMatrix r `liftM` GM.replicate ((r*(r+1)) `shiftR` 1) x

-- row major upper triangular indexing
idx :: Int -> Int -> Int -> Int
idx n i j | i <= j = (i * (2 * n - i - 1)) `shiftR` 1 + j
          | otherwise = (j * (2 * n - j - 1)) `shiftR` 1 + i
{-# INLINE idx #-}
