{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- | "Data.Serialize" functions for "Data.Vector.Generic"
-- vectors. Orphan instances are provided for "Data.Vector",
-- "Data.Vector.Unboxed", "Data.Vector.Storable", and
-- "Data.Vector.Primitive" vectors.
--
-- The serialized format is an 'Int64' representing the length of the
-- vector, followed by the "Data.Serialize"d contents of each element.
--
-- Note that the functions in "Data.Vector.Storable.UnsafeSerialize"
-- perform much better when serialization does not need to account for
-- host endianness and word size.
module Data.Vector.Serialize (
    genericGetVector
  , genericPutVector
  , genericGetVectorWith
  , genericPutVectorWith
  ) where

import Control.Monad

import Data.Int (Int64)
import Data.Serialize (Get, Putter, Serialize(..))
import qualified Data.Vector           as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed   as VU
import qualified Data.Vector.Generic   as VG
import qualified Data.Vector.Storable  as VS

-- | Read a 'Data.Vector.Generic.Vector' using custom 'Get' for the
-- vector's elements.
genericGetVectorWith :: (VG.Vector v a) => Get a -> Get (v a)
{-# INLINE genericGetVectorWith #-}
genericGetVectorWith getter = do
  len64 <- get :: Get Int64
  when (len64 > fromIntegral (maxBound :: Int)) $
    fail "Host can't deserialize a Vector longer than (maxBound :: Int)"
  VG.replicateM (fromIntegral len64) getter

-- | Write a 'Data.Vector.Generic.Vector' using custom 'Putter' for
-- the vector's elements.
genericPutVectorWith :: (VG.Vector v a) => Putter a -> Putter (v a)
{-# INLINE genericPutVectorWith #-}
genericPutVectorWith putter v = do
  put ((fromIntegral $ VG.length v) :: Int64)
  VG.mapM_ putter v


-- | Write a 'Data.Vector.Generic.Vector'.
genericPutVector :: (Serialize a, VG.Vector v a) => Putter (v a)
{-# INLINE genericPutVector #-}
genericPutVector = genericPutVectorWith put

-- | Read a 'Data.Vector.Generic.Vector'.
genericGetVector :: (Serialize a, VG.Vector v a) => Get (v a)
{-# INLINE genericGetVector #-}
genericGetVector = genericGetVectorWith get


instance (Serialize a) => Serialize (V.Vector a) where
  get = genericGetVector ; put = genericPutVector
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (Serialize a, VP.Prim a) => Serialize (VP.Vector a) where
  get = genericGetVector ; put = genericPutVector
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (Serialize a, VU.Unbox a) => Serialize (VU.Vector a) where
  get = genericGetVector ; put = genericPutVector
  {-# INLINE get #-}
  {-# INLINE put #-}

instance (Serialize a, VS.Storable a) => Serialize (VS.Vector a) where
  get = genericGetVector ; put = genericPutVector
  {-# INLINE get #-}
  {-# INLINE put #-}
