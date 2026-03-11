{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Matrix.Class.Mutable
    ( MMatrix(..)
    , write
    , read
    ) where

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector.Generic.Mutable as GM
import           Prelude                     hiding (read)

class GM.MVector v a => MMatrix m v a where
    dim ::  m v s a -> (Int, Int)

    unsafeRead :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> s a

    unsafeWrite :: PrimMonad s => m v (PrimState s) a -> (Int, Int) -> a -> s ()

    -- | Create a mutable matrix without initialization
    new :: PrimMonad s => (Int, Int) -> s (m v (PrimState s) a)

    replicate :: PrimMonad s => (Int, Int) -> a -> s (m v (PrimState s) a)

    {-# MINIMAL dim, unsafeRead, unsafeWrite, new, replicate #-}

-- | Derived methods

write :: (PrimMonad s, MMatrix m v a)
      => m v (PrimState s) a -> (Int, Int) -> a -> s ()
write mat (i,j)
    | i < 0 || i >= r || j < 0 || j >= c = error "write: Index out of bounds"
    | otherwise = unsafeWrite mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE write #-}

read :: (PrimMonad s, MMatrix m v a)
     => m v (PrimState s) a -> (Int, Int) -> s a
read mat (i,j)
    | i <0 || i >= r || j < 0 || j >= c = error "read: Index out of bounds"
    | otherwise = unsafeRead mat (i,j)
  where
    (r,c) = dim mat
{-# INLINE read #-}
