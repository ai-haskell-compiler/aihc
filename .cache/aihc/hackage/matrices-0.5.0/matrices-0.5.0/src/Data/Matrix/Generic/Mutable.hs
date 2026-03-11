{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Matrix.Generic.Mutable
   ( -- * Mutable Matrix
     MMatrix(..)
   , C.dim
   , takeRow
   , C.write
   , C.unsafeWrite
   , C.read
   , C.unsafeRead
   , C.new
   , C.replicate
   ) where

import           Control.Monad               (liftM)
import           Control.DeepSeq
import qualified Data.Vector.Generic.Mutable as GM
import           Prelude                     hiding (read, replicate)

import qualified Data.Matrix.Class.Mutable as C

-- | mutable matrix
data MMatrix v s a = MMatrix !Int !Int !Int !Int !(v s a)

instance (NFData (v s a)) => NFData (MMatrix v s a) where
 rnf (MMatrix _ _ _ _ vec) = rnf vec

instance GM.MVector v a => C.MMatrix MMatrix v a where
    dim (MMatrix r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    unsafeRead (MMatrix _ _ tda offset v) (i,j) = GM.unsafeRead v idx
      where idx = offset + i * tda + j
    {-# INLINE unsafeRead #-}

    unsafeWrite (MMatrix _ _ tda offset v) (i,j) = GM.unsafeWrite v idx
      where idx = offset + i * tda + j
    {-# INLINE unsafeWrite #-}

    new (r,c) = MMatrix r c c 0 `liftM` GM.new (r*c)
    {-# INLINE new #-}

    replicate (r,c) x = MMatrix r c c 0 `liftM` GM.replicate (r*c) x
    {-# INLINE replicate #-}

takeRow :: GM.MVector v a => MMatrix v m a -> Int -> v m a
takeRow (MMatrix _ c tda offset vec) i = GM.slice i' c vec
  where
    i' = offset + i * tda
{-# INLINE takeRow #-}
